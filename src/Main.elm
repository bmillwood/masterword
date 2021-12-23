module Main exposing (main)

import Browser
import Html exposing (Html)

type alias Flags = ()
type alias Model = ()
type alias Msg = ()

init : Flags -> (Model, Cmd Msg)
init () = ((), Cmd.none)

view : Model -> Html Msg
view () = Html.div [] []

update : Msg -> Model -> (Model, Cmd Msg)
update () () = ((), Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions () = Sub.none

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
