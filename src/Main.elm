module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events

import Words

type alias Flags = ()

type Model
  = Init
  | Playing
      { secret : List Char
      , guesses : List (List Char)
      }

type Msg
  = StartNewGame
  | GameStarted (List Char)

startNewGame : Cmd Msg
startNewGame = Cmd.map GameStarted Words.newWord

init : Flags -> (Model, Cmd Msg)
init () = (Init, startNewGame)

view : Model -> Html Msg
view model =
  Html.div
    []
    [ case model of
        Init ->
          Html.button
            [ Html.Events.onClick StartNewGame ]
            [ Html.text "Start new game" ]
        Playing { secret, guesses } ->
          Html.text (String.fromList secret)
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartNewGame -> (model, startNewGame)
    GameStarted newWord ->
      ( Playing { secret = newWord, guesses = [] }
      , Cmd.none
      )

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
