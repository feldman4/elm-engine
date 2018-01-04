module Decode.Shared exposing (..)

import Result.Extra
import Json.Decode as JD exposing (at, int, string, nullable)


-- ADDITIONAL DECODERS


nString : JD.Decoder String
nString =
    JD.oneOf [ JD.null "", int |> JD.map toString, JD.string ]


{-| Useful for decoding XML like <tag>4500 4500 4500</tag> into [4500, 4500, 4500].
<tag></tag> becomes the empty list.
-}
intStringList : JD.Decoder (List Int)
intStringList =
    let
        inner : String -> JD.Decoder (List Int)
        inner string =
            let
                result =
                    case string of
                        "" ->
                            Result.Ok []

                        _ ->
                            string
                                |> String.words
                                |> List.map String.toInt
                                |> Result.Extra.combine
            in
                case result of
                    Result.Ok x ->
                        JD.succeed x

                    Result.Err s ->
                        JD.fail s
    in
        nString |> JD.andThen inner
