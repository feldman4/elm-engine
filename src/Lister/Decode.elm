module Lister.Decode exposing (..)

import Types exposing (..)
import Types.Decode exposing (..)
import Setters exposing (..)
import Utility exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Lister.Encode exposing (toJson)


{-| Update record based on JSON. Requires adding a decoder pipeline call and
having a setter available for each field.
-}
finalDecoder : JD.Decoder (List (Update (SimpleObject a)))
finalDecoder =
    start
        |> addInt "id" setId Result.Ok
        |> addString "piece" setPiece toPiece


type alias SimpleObject a =
    { a | id : Int, piece : Piece }


type alias Update a =
    a -> a


simplePawn : SimpleObject {}
simplePawn =
    { piece = Pawn
    , id = 0
    }


jsonInput : String
jsonInput =
    toJson { id = 4, piece = Queen } |> JE.encode 4


start : JD.Decoder (List a)
start =
    JD.succeed []


addFieldCustom :
    JD.Decoder c
    -> String
    -> (a -> b -> b)
    -> (c -> Result error a)
    -> JD.Decoder (List (b -> b))
    -> JD.Decoder (List (b -> b))
addFieldCustom jsonType field set cast =
    JD.field field jsonType |> JD.map (Setters.update set cast) |> another


addString :
    String
    -> (a -> b -> b)
    -> (String -> Result error a)
    -> JD.Decoder (List (b -> b))
    -> JD.Decoder (List (b -> b))
addString =
    addFieldCustom JD.string


addInt :
    String
    -> (a -> b -> b)
    -> (Int -> Result error a)
    -> JD.Decoder (List (b -> b))
    -> JD.Decoder (List (b -> b))
addInt =
    addFieldCustom JD.int


another : JD.Decoder a -> JD.Decoder (List a) -> JD.Decoder (List a)
another x xs =
    let
        f x xs =
            case x of
                Just x_ ->
                    x_ :: xs

                Nothing ->
                    xs
    in
        JD.map2 f (JD.maybe x) xs


test : { id : Int, piece : Piece }
test =
    jsonInput
        |> JD.decodeString finalDecoder
        |> Result.withDefault []
        |> applyList simplePawn
