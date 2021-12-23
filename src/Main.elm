module Main exposing (main)

import Browser
import Browser.Dom
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Task

import Words
import Zipper

type alias Flags = ()

type Model
  = Init
  | Playing
      { secret : List Char
      , guesses : List (List Char)
      , nextGuess : List (Maybe Char)
      }

type Msg
  = DoNothing
  | StartNewGame
  | GameStarted (List Char)
  | UpdateGuess Int (Maybe Char)
  | SubmitGuess

startNewGame : Cmd Msg
startNewGame = Cmd.map GameStarted Words.newWord

init : Flags -> (Model, Cmd Msg)
init () = (Init, startNewGame)

guessCellId : Int -> String
guessCellId n = "guessCell" ++ String.fromInt n

submitId : String
submitId = "submitButton"

viewGuess : { secret : List Char, guess : List Char } -> Html Msg
viewGuess { secret, guess } =
  let
    cellForChar s g =
      Html.td
        (if s == g
        then [ Html.Attributes.style "background-color" "lightgreen" ]
        else if List.member g secret
        then [ Html.Attributes.style "background-color" "yellow" ]
        else [])
        [ Html.text (String.fromChar g) ]
  in
  Html.tr [] (List.map2 cellForChar secret guess)

view : Model -> Html Msg
view model =
  case model of
    Init -> Html.div [] [ Html.text "Game is starting (or failed to start)" ]
    Playing { secret, guesses, nextGuess } ->
      let
        useInput v =
          String.right 1 v
          |> String.toUpper
          |> String.toList
          |> List.head
        nextGuessChar index c =
          Html.td
            []
            [ Html.input
                [ Html.Attributes.id (guessCellId index)
                , Html.Attributes.value
                    (Maybe.withDefault "" (Maybe.map String.fromChar c))
                , Html.Attributes.style "width" "1em"
                , Html.Events.onInput
                    (\v -> UpdateGuess index (useInput v))
                ]
                []
            ]
        nextGuessRow =
          Html.tr
            []
            (List.indexedMap nextGuessChar nextGuess)
        guessRow guess = viewGuess { secret = secret, guess = guess }
        rows = List.reverse (nextGuessRow :: List.map guessRow guesses)
      in
      Html.div
        []
        [ Html.table [] rows
        , Html.button
            [ Html.Attributes.id submitId
            , Html.Events.onClick SubmitGuess
            ]
            [ Html.text "Guess" ]
        ]

allJust : List (Maybe a) -> Maybe (List a)
allJust maybes =
  case maybes of
    [] -> Just []
    x :: xs ->
      x |> Maybe.andThen (\y -> Maybe.map (\ys -> y :: ys) (allJust xs))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DoNothing -> (model, Cmd.none)
    StartNewGame -> (model, startNewGame)
    GameStarted newWord ->
      ( Playing
          { secret = newWord
          , guesses = []
          , nextGuess = List.map (always Nothing) newWord
          }
      , Cmd.none
      )
    UpdateGuess updateIndex newChar ->
      case model of
        Init -> (Init, Cmd.none)
        Playing p ->
          let
            zipperElt =
              Zipper.ofList p.nextGuess
              |> List.drop updateIndex
              |> List.head
            newGuess =
              case zipperElt of
                Nothing -> p.nextGuess
                Just (before, _, after) ->
                  Zipper.toList (before, newChar, after)
          in
          ( Playing { p | nextGuess = newGuess }
          , Task.onError
              (\_ -> Browser.Dom.focus submitId)
              (Browser.Dom.focus (guessCellId (updateIndex + 1)))
            |> Task.attempt (\_ -> DoNothing)
          )
    SubmitGuess ->
      case model of
        Init -> (Init, Cmd.none)
        Playing p ->
          case allJust p.nextGuess of
            Nothing -> (model, Cmd.none)
            Just guess ->
              ( Playing
                  { p
                  | guesses = guess :: p.guesses
                  , nextGuess = List.map (always Nothing) guess
                  }
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
