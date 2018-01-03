module Decode exposing (..)

import Types exposing (..)
import Parsers exposing (..)
import Setters exposing (..)
import Json.Decode as DE
import Json.Encode as JE
import Encode exposing (toJson)


simplePawn : { id : number, piece : Piece }
simplePawn =
    { piece = Pawn
    , id = 0
    }


jsonInput : String
jsonInput =
    toJson { id = 4, piece = Queen } |> JE.encode 4


start : DE.Decoder (List a)
start =
    DE.succeed []


finalDecoder : DE.Decoder (List ({ a | id : Int, piece : Piece } -> { a | id : Int, piece : Piece }))
finalDecoder =
    start
        |> addInt "id" setId Result.Ok
        |> addString "piece" setPiece toPiece


addFieldCustom :
    DE.Decoder c
    -> String
    -> (a -> b -> b)
    -> (c -> Result error a)
    -> DE.Decoder (List (b -> b))
    -> DE.Decoder (List (b -> b))
addFieldCustom jsonType field set cast =
    DE.field field jsonType |> DE.map (update set cast) |> another


addString :
    String
    -> (a -> b -> b)
    -> (String -> Result error a)
    -> DE.Decoder (List (b -> b))
    -> DE.Decoder (List (b -> b))
addString =
    addFieldCustom DE.string


addInt :
    String
    -> (a -> b -> b)
    -> (Int -> Result error a)
    -> DE.Decoder (List (b -> b))
    -> DE.Decoder (List (b -> b))
addInt =
    addFieldCustom DE.int


another : DE.Decoder a -> DE.Decoder (List a) -> DE.Decoder (List a)
another x xs =
    let
        f x xs =
            case x of
                Just x_ ->
                    x_ :: xs

                Nothing ->
                    xs
    in
        DE.map2 f (DE.maybe x) xs


c : { id : Int, piece : Piece }
c =
    jsonInput
        |> DE.decodeString finalDecoder
        |> Result.withDefault []
        |> applyList simplePawn


type alias MyObject a =
    { a | id : Int, piece : Piece }


x : List (String -> MyObject a -> MyObject a)
x =
    [ up "id" setId String.toInt
    , up "piece" setPiece toPiece
    ]
