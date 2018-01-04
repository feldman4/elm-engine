module Types.Decode exposing (..)

import Types exposing (..)


toPiece : String -> Result String Piece
toPiece s =
    case s of
        "Pawn" ->
            Result.Ok Pawn

        "Queen" ->
            Result.Ok Queen

        _ ->
            Result.Err ("Failed to match Piece: " ++ s)


toOwner : String -> Maybe Player
toOwner s =
    case s of
        "White" ->
            Just White

        "Black" ->
            Just Black

        _ ->
            Nothing
