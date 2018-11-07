module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Ecto exposing (World, Entity)


type alias Model =
  { world : World Component }


type Msg
  = NoOp


type Component
  = Position Int Int Int
  | Health Int


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
  ( { world =
        Ecto.init
          <| Ecto.newEntities [ "Carl" ]
    }
  , Cmd.none
  )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )


view : Model -> Html Msg
view { world } =
  let
    { entities } = world 
  in
    div []
      <| List.map
          (\e ->
            let
              ( uuid, description ) = Ecto.entityToString e
            in
              Html.text <| description ++ " : " ++ uuid
          )
          entities