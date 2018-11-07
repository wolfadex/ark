module Main exposing (main)


import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attrs
-- import Html.Events exposing (onClick)
import Random exposing (Seed)
import Ecto exposing (World, Entity)


---- Types ----


type alias Model =
  { world : World Component Global }


type Msg
  = NoOp
  | Tick Float


type Component
  = Position Float Float Float
  | Health Int
  

type alias Occupied = Bool


type Global
  = Grid (Array (Array (Int, Int, Occupied)))


---- Main ----


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


init : () -> (Model, Cmd Msg)
init flags =
  let
    initSeed = Random.initialSeed 0
    (newEntities, nextSeed) = Ecto.newEntities initSeed ["Carl"]
    newWorld = Ecto.init newEntities
    worldWithPosition =
      List.foldl initEntities newWorld newEntities
    worldWithMovement =
      Ecto.addSystem worldWithPosition "sysMove" ["locationOnGrid", "goalOnGrid","location"] sysMove
    worldWithGrid =
      Ecto.addGlobal worldWithMovement "grid" initializeGrid
  in
    ( { world = worldWithMovement
      }
    , Cmd.none
    )


initializeGrid : Global
initializeGrid =
  Grid
    (Array.initialize
      100
      (\x ->
        Array.initialize
          100
          (\y ->
            ( x
            , y
            , (x == 0 && y == 0)
            )
          )
      )
    )


initEntities : Entity -> World Component Global -> World Component Global
initEntities ( entity, description ) world =
  let
      currentPosWorld =
        Ecto.addComponent
          world
          entity
          "locationOnGrid"
          <| Position 0 0 0
      nextPosWorld =
        Ecto.addComponent
          currentPosWorld
          entity
          "goalOnGrid"
          <| Position 1 0 0
  in
    Ecto.addComponent
      nextPosWorld
      entity
      "location"
      <| Position 0 0 0


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
      [ onAnimationFrameDelta Tick
      ]


---- Update ----


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ world } as model) =
  case msg of
    NoOp ->
      ( model, Cmd.none )
    Tick deltaTime ->
      ( { model | world = Ecto.runSystems world deltaTime }
      , Cmd.none
      )


sysMove : Float -> (Dict String Global, Dict String Component) -> (Dict String Global, Dict String Component)
sysMove deltaTime (globals, entityData) =
  let
      -- Get data
      location =
        case Dict.get "location" entityData of
          Just (Position x y z) -> Position x y z
          _ -> Position 0 0 0

      locationOnGrid =
        case Dict.get "locationOnGrid" entityData of
          Just (Position x y z) -> Position x y z
          _ -> Position 0 0 0

      goalOnGrid =
        case Dict.get "goalOnGrid" entityData of
          Just (Position x y z) -> Position x y z
          _ -> Position 0 0 0

      -- Update data
      nextLocation =
        let
          (x, y, z) =
            case location of
              Position px py pz ->
                (px, py, pz)
              _ ->
                (0, 0, 0)
          nextX = x + 1 / deltaTime
          nextY = y
          (goalX, goalY) =
            case goalOnGrid of
              Position px py _ ->
                (px, py)
              _ ->
                (0, 0)
        in
          if nextX >= goalX then
            goalOnGrid
          else
            Position nextX y z
      
      updatedLocation = Dict.insert "location" nextLocation entityData

      updatedLocationOnGrid =
        Dict.insert
          "locationOnGrid"
          (if nextLocation == goalOnGrid then
            nextLocation
          else
            locationOnGrid)
          updatedLocation
      
      updatedGoalOnGrid =
        Dict.insert
          "goalOnGrid"
          (if nextLocation == goalOnGrid then
            let
              (x, y, z) =
                case goalOnGrid of
                  Position px py pz ->
                    (px, py, pz)
                  _ ->
                    (0, 0, 0)
            in
              Position (x + 1) y z
          else
            goalOnGrid)
          updatedLocationOnGrid
  in
    (globals, updatedGoalOnGrid)


---- View ----


view : Model -> Html Msg
view { world } =
  let
    { entities, components } = world 
  in
    Html.div
      []
      <| List.map
          (\e ->
            let
              ( uuid, description ) = Ecto.entityToString e
            in
              case Dict.get uuid components of
                Just entityData ->
                  let
                    (x, y) =
                      case Dict.get "location" entityData of
                        Just (Position px py _) ->
                          (px, py)
                        _ ->
                          (0, 0)
                  in
                    Html.div
                      [ Attrs.style "position" "absolute"
                      , Attrs.style "top" <| (String.fromFloat y) ++ "vh"
                      , Attrs.style "left" <| (String.fromFloat x) ++ "vw"
                      , Attrs.style "width" "10vw"
                      , Attrs.style "height" "10vh"
                      , Attrs.style "background" "red"
                      ]
                      []
                Nothing ->
                  Html.text ""
          )
          entities