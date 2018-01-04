module Utility exposing (..)


applyList : a -> List (a -> a) -> a
applyList x fs =
    case fs of
        [] ->
            x

        f :: rest ->
            applyList (f x) rest
