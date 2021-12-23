module Zipper exposing (..)

type alias OfList a = (List a, a, List a)

ofList : List a -> List (OfList a)
ofList =
  let
    go before xs =
      case xs of
        [] -> []
        y :: ys ->
          (before, y, ys) :: go (y :: before) ys
  in
  go []

toList : OfList a -> List a
toList (before, x, after) =
  case before of
    [] -> x :: after
    b :: bs -> toList (bs, b, x :: after)
