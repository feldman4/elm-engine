module Lister.Encode exposing (..)

import Types exposing (..)
import Dict
import Ast
import Ast.Expression
import Json.Encode as JE


toJson : a -> JE.Value
toJson x =
    x |> toString |> encode


toJsonString : a -> String
toJsonString =
    toJson >> JE.encode 1


defaultOpTable : Dict.Dict k v
defaultOpTable =
    Dict.empty


simplePawn : { id : number, piece : Piece }
simplePawn =
    { piece = Pawn
    , id = 0
    }


encode : String -> JE.Value
encode input =
    case input |> Ast.parseExpression defaultOpTable of
        Result.Err err ->
            JE.string ("parse error: " ++ (toString err))

        Result.Ok ( _, _, expr ) ->
            encodeElm expr


encodeElm : Ast.Expression.Expression -> JE.Value
encodeElm expr =
    case expr of
        Ast.Expression.Record contents ->
            JE.object (List.map (\( name, expr_ ) -> ( name, encodeElm expr_ )) contents)

        Ast.Expression.Float val ->
            JE.float val

        Ast.Expression.Integer val ->
            JE.int val

        Ast.Expression.String val ->
            JE.string val

        Ast.Expression.Variable vals ->
            case vals of
                val :: [] ->
                    JE.string val

                _ ->
                    vals |> List.map JE.string |> JE.list

        _ ->
            JE.string (toString expr)
