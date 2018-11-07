module Ecto exposing
  ( World
  , Entity
  , empty
  , init
  , newEntities
  , entityToString
  )


import Dict exposing (Dict)
import Random exposing (Seed)
import Uuid exposing (Uuid)


type alias World c =
  { entities : List Entity
  --           Dict compName (Dict entity compData)
  , components : Dict String (Dict String c)
  }


type alias Description =
  String


type alias Entity =
  ( Uuid, Description )


empty : World c
empty =
  { entities = []
  , components = Dict.empty
  }


init : List Entity -> World c
init e =
  { empty | entities = e }


newEntities : List String -> List Entity
newEntities descriptions =
  let
    ( results, s ) = List.foldl
      (\description ( result, seed ) ->
        let
          ( nextUuid, nextSeed ) =
            Random.step Uuid.uuidGenerator seed
        in
          ( ( nextUuid, description ) :: result, nextSeed )
      )
      ( [], Random.initialSeed 0 )
      descriptions
  in
    List.reverse results


entityToString : Entity -> ( String, String )
entityToString ( uuid, description ) =
  ( Uuid.toString uuid, description )


addComponent : World c -> String -> Uuid -> c -> World c
addComponent world componentName entity component =
  let
    uuidAsString = (Uuid.toString entity)
    newComponentGroup =
      case Dict.get componentName world.components of
        Just componentGroup ->
          Dict.insert uuidAsString component componentGroup
        Nothing ->
          Dict.singleton uuidAsString component
  in
    { world
    | components = Dict.insert componentName newComponentGroup world.components
    }