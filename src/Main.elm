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

type alias Game =
  { secret : List Char
  , guesses : List (List Char)
  , nextGuess : List (Maybe Char)
  }

type Model
  = Init
  | Playing Game

alreadyGuessed : Game -> List (Maybe Char)
alreadyGuessed { secret, guesses, nextGuess } =
  case guesses of
    [] -> List.map (always Nothing) secret
    guess :: _ ->
      List.map2 (\g s -> if g == s then Just g else Nothing) guess secret

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

misplacedStyle : Html.Attribute a
misplacedStyle = Html.Attributes.style "background-color" "yellow"

viewGuess : { secret : List Char, guess : List Char } -> Html Msg
viewGuess { secret, guess } =
  let
    cellForChar s g =
      Html.td
        (if s == g
        then [ correctStyle ]
        else if List.member g secret
        then [ misplacedStyle ]
        else [])
        [ Html.text (String.fromChar g) ]
  in
  Html.tr [] (List.map2 cellForChar secret guess)

view : Model -> Html Msg
view model =
  case model of
    Init -> Html.div [] [ Html.text "Game is starting (or failed to start)" ]
    Playing ({ secret, guesses, nextGuess } as game) ->
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
        guessed = alreadyGuessed game
        done =
          case allJust guessed of
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
              (List.map3 (\g s a -> (g, s, a)) nextGuess secret guessed))
        guessRow guess = viewGuess { secret = secret, guess = guess }
        rows =
          List.reverse
            ((if done then [] else [ nextGuessRow ]) ++ List.map guessRow guesses)
        indicatorForChar c =
          if List.member (Just c) guessed
          then [ correctStyle ]
          else if not (List.any (List.member c) guesses)
          then []
          else if List.member c secret
          then [ misplacedStyle ]
          else [ Html.Attributes.style "color" "lightgrey" ]
        keyboardIndicatorCell c =
          Html.td
            (indicatorForChar c)
            [ Html.text (String.fromChar c) ]
        keyboardRows =
          List.map
            (Html.tr [] << List.map keyboardIndicatorCell << String.toList)
            [ "QWERTYUIOP"
            , "ASDFGHJKL"
            , "ZXCVBNM"
            ]
      in
      Html.div
        [ Html.Attributes.style "text-align" "center" ]
        [ Html.table
            [ Html.Attributes.style "margin" "auto" ]
            rows
        , Html.button
            [ idAttr Submit
            , Html.Events.onClick (if done then StartNewGame else SubmitGuess)
            ]
            [ if done then Html.text "New" else Html.text "Guess" ]
        , Html.table
            [ Html.Attributes.style "margin" "auto" ]
            keyboardRows
        , Html.p []
            [ Html.a
                [ Html.Attributes.href "https://github.com/bmillwood/masterword" ]
                [ Html.text "source on github" ]
            ]
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
            nextGuessWithCorrect =
              List.map2 (\g ag -> (g, ag)) p.nextGuess (alreadyGuessed p)
            zipperElt =
              List.indexedMap (\i (g, ag) -> (i, g, ag)) nextGuessWithCorrect
              |> Zipper.ofList
              |> List.drop updateIndex
              |> List.head
            isDelete =
              case newChar of
                Just _ -> False
                Nothing -> True
            guesses = List.map (\(i, g, ag) -> g)
            indexOfIncorrect (i, _, ag) =
              case ag of
                Nothing -> Just (GuessCell i)
                Just _ -> Nothing
            (newGuess, focuses) =
              case zipperElt of
                Nothing -> (p.nextGuess, [])
                Just (before, (atIndex, _, _), after) ->
                  ( Zipper.toList (guesses before, newChar, guesses after)
                  , if isDelete
                    then List.filterMap indexOfIncorrect before |> List.take 1
                    else
                      List.filterMap indexOfIncorrect after
                      |> List.head
                      |> Maybe.map List.singleton
                      |> Maybe.withDefault [ Submit ]
                  )
          in
          ( Playing { p | nextGuess = newGuess }
          , tryFocuses focuses
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
