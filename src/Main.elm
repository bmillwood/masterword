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

type DomId
  = GuessCell Int
  | Submit

domIdToString : DomId -> String
domIdToString di =
  case di of
    GuessCell i -> "guessCell" ++ String.fromInt i
    Submit -> "submitButton"

idAttr : DomId -> Html.Attribute a
idAttr di = Html.Attributes.id (domIdToString di)

tryFocuses : List DomId -> Cmd Msg
tryFocuses dis =
  List.foldr
    (\di task -> Task.onError (\_ -> task) (Browser.Dom.focus (domIdToString di)))
    (Task.fail ())
    dis
  |> Task.attempt (\_ -> DoNothing)

correctStyle : Html.Attribute a
correctStyle = Html.Attributes.style "background-color" "lightgreen"

viewGuess : { secret : List Char, guess : List Char } -> Html Msg
viewGuess { secret, guess } =
  let
    cellForChar s g =
      Html.td
        (if s == g
        then [ correctStyle ]
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
        cellWidth = Html.Attributes.style "width" "1em"
        useInput v =
          String.right 1 v
          |> String.toUpper
          |> String.toList
          |> List.head
        alreadyGuessedCell c =
          Html.td
            [ cellWidth, correctStyle ]
            [ Html.text (String.fromChar c) ]
        yetToGuessCell index c =
          Html.td
            []
            [ Html.input
                [ idAttr (GuessCell index)
                , Html.Attributes.value
                    (Maybe.withDefault "" (Maybe.map String.fromChar c))
                , cellWidth
                , Html.Events.onInput
                    (\v -> UpdateGuess index (useInput v))
                ]
                []
            ]
        alreadyGuessed =
          case guesses of
            ls :: _ ->
              List.map2 (\lc s -> if lc == s then Just lc else Nothing) ls secret
            [] -> List.map (always Nothing) secret
        done =
          case allJust alreadyGuessed of
            Just _ -> True
            Nothing -> False
        nextGuessCell index (c, s, a) =
          case a of
            Just cc -> alreadyGuessedCell cc
            Nothing -> yetToGuessCell index c
        nextGuessRow =
          Html.tr
            []
            (List.indexedMap nextGuessCell
              (List.map3 (\g s a -> (g, s, a)) nextGuess secret alreadyGuessed))
        guessRow guess = viewGuess { secret = secret, guess = guess }
        rows =
          List.reverse
            ((if done then [] else [ nextGuessRow ]) ++ List.map guessRow guesses)
      in
      Html.div
        []
        [ Html.table [] rows
        , Html.button
            [ idAttr Submit
            , Html.Events.onClick (if done then StartNewGame else SubmitGuess)
            ]
            [ if done then Html.text "New" else Html.text "Guess" ]
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
              Zipper.ofList (List.indexedMap (\i x -> (i, x)) p.nextGuess)
              |> List.drop updateIndex
              |> List.head
            delete =
              case newChar of
                Nothing -> True
                Just _ -> False
            values = List.map (\(i, x) -> x)
            (newGuess, newId) =
              case zipperElt of
                Nothing -> (p.nextGuess, GuessCell 0)
                Just (before, (atIndex, _), after) ->
                  ( Zipper.toList (values before, newChar, values after)
                  , List.filterMap
                      (\(i, x) ->
                        case x of
                          Just _ -> Nothing
                          Nothing -> Just (GuessCell i))
                      (if delete then before else after)
                    |> List.head
                    |> Maybe.withDefault (if delete then GuessCell 0 else Submit)
                  )
          in
          ( Playing { p | nextGuess = newGuess }
          , tryFocuses [ newId ]
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
                  , nextGuess =
                      List.map2
                        (\g s -> if g == s then Just s else Nothing)
                        guess
                        p.secret
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
