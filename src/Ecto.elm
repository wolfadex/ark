module Ecto exposing
  ( World
  , Entity
  , empty
  , init
  , newEntities
  , entityToString
  , addComponent
  , removeComponent
  , addSystem
  , removeSystem
  , runSystems
  , addGlobal
  , removeGlobal
  )


import Dict exposing (Dict)
import Random exposing (Seed)
import Uuid exposing (Uuid)


type alias World c g =
  { entities : List Entity
  , components : Dict String (Dict String c)
  , systems : Dict String (List String, (Float -> (Dict String g, Dict String c) -> (Dict String g, Dict String c)))
  , globals : Dict String g
  }


type alias Description =
  String


type alias Entity =
  ( Uuid, Description )


empty : World c g
empty =
  { entities = []
  , components = Dict.empty
  , systems = Dict.empty
  , globals = Dict.empty
  }


init : List Entity -> World c g
init e =
  { empty | entities = e }


newEntities : Seed -> List String -> (List Entity, Seed)
newEntities initSeed descriptions =
  let
    ( results, lastSeed ) = List.foldl
      (\description ( result, seed ) ->
        let
          ( nextUuid, nextSeed ) =
            Random.step Uuid.uuidGenerator seed
        in
          ( ( nextUuid, description ) :: result, nextSeed )
      )
      ( [], initSeed )
      descriptions
  in
    (List.reverse results, lastSeed)


entityToString : Entity -> ( String, String )
entityToString ( uuid, description ) =
  ( Uuid.toString uuid, description )


addComponent : World c g -> Uuid -> String -> c -> World c g
addComponent world entity componentName component =
  let
    uuidAsString = (Uuid.toString entity)
    newEntityComponents =
      case Dict.get uuidAsString world.components of
        Just entityComponents ->
          Dict.insert componentName component entityComponents
        Nothing ->
          Dict.singleton componentName component
  in
    { world
    | components = Dict.insert uuidAsString newEntityComponents world.components
    }


removeComponent : World c g -> Uuid -> String -> World c g
removeComponent world entity componentName =
  let
    uuidAsString = (Uuid.toString entity)
    newEntityComponents =
      case Dict.get uuidAsString world.components of
        Just entityComponents ->
          Dict.remove componentName entityComponents
        Nothing ->
          Dict.empty
  in
    { world
    | components = Dict.insert uuidAsString newEntityComponents world.components
    }


addSystem : World c g -> String -> List String -> (Float -> (Dict String g, Dict String c) -> (Dict String g, Dict String c)) -> World c g
addSystem world systemName componentsRequired system =
  { world | systems = Dict.insert systemName (componentsRequired, system) world.systems }


removeSystem : World c g -> String -> World c g
removeSystem world systemName =
  { world | systems = Dict.remove systemName world.systems }


runSystems : World c g -> Float -> World c g
runSystems ({ systems, components, globals } as world) deltaTime =
  let
    (newGlobals, newComponents) =
      Dict.foldl (runSystem components deltaTime) (globals, components) systems  
  in
    { world
    | components = newComponents
    , globals = newGlobals
    }


runSystem : Dict String (Dict String c) -> Float -> String -> (List String, (Float -> (Dict String g, Dict String c) -> (Dict String g, Dict String c))) -> (Dict String g, Dict String (Dict String c)) -> (Dict String g, Dict String (Dict String c))
runSystem entitiesWithData deltaTime systemName (componentsRequired, system) (resGlobals, resComponents) =
  Dict.foldl
    (\uuid entityData (prevGlobals, prevComponents) ->
      let
        (nextGlobals, newEntityData) =
          if allAreMembersOf componentsRequired (Dict.keys entityData) then
            let
              (g, ed) = system deltaTime (prevGlobals, entityData)
            in
              (g, Dict.union ed entityData)
          else
            (prevGlobals, entityData)
      in
        (nextGlobals, Dict.insert uuid newEntityData prevComponents)
    )
    (resGlobals, resComponents)
    entitiesWithData


allAreMembersOf : List a -> List a -> Bool
allAreMembersOf lookingFor inList =
  List.all (\i -> List.member i inList) lookingFor


addGlobal : World c g -> String -> g -> World c g
addGlobal world globalName global =
  { world | globals = Dict.insert globalName global world.globals }


removeGlobal : World c g -> String -> World c g
removeGlobal ({ globals } as world) globalName =
  { world | globals = Dict.remove globalName globals }