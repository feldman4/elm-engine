module Test exposing (..)

import Json.Decode exposing (int, string, float, nullable, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


type alias User =
    { id : Int
    , email : Maybe String
    , name : String
    , percentExcited : Float
    }


userDecoder : Decoder User
userDecoder =
    decode User
        |> required "id" int
        |> required "email" (nullable string)
        -- `null` decodes to `Nothing`
        |> optional "name" string "(fallback if name is `null` or not present)"
        |> hardcoded 1.0


userDecoder2 : Decoder User
userDecoder2 =
    decode User
        |> required "id" int
        |> required "email" (nullable string)
        -- `null` decodes to `Nothing`
        |> optional "name" string "(fallback if name is `null` or not present)"
        |> hardcoded 1.0
