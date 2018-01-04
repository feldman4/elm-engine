module Remote exposing (..)

import Http
import Html
import Result.Extra
import Json.Decode as JD exposing (at, int, string)
import Json.Decode.Pipeline exposing (decode, custom)
import Xml.Extra exposing (Required(..), decodeXml, multipleTag, requiredTag)


-- TYPES AND DECODERS


type alias Map =
    { chipset_id : Int
    , width : Int
    , height : Int
    , lower_layer : List Int
    , upper_layer : List Int
    }


mapDecoder : JD.Decoder Map
mapDecoder =
    decode Map
        |> custom (at [ "chipset_id" ] int)
        |> custom (at [ "width" ] int)
        |> custom (at [ "height" ] int)
        |> custom (at [ "lower_layer" ] intStringList)
        |> custom (at [ "upper_layer" ] intStringList)


type alias Event =
    { name : String
    , x : Int
    , y : Int
    }


eventDecoder : JD.Decoder Event
eventDecoder =
    decode Event
        |> custom (at [ "name" ] string)
        |> custom (at [ "x" ] int)
        |> custom (at [ "y" ] int)


eventTagSpec : List ( String, Required )
eventTagSpec =
    [ ( "name", Required )
    , ( "x", Required )
    , ( "y", Required )
    ]


eventsDecoder : JD.Decoder (List Event)
eventsDecoder =
    (\d -> requiredTag "events" d [ ( "Event", Multiple ) ]) <|
        multipleTag "Event" eventDecoder eventTagSpec


decodeAllEventsXml : String -> Result Xml.Extra.Error (List Event)
decodeAllEventsXml xml =
    decodeXml xml "Map" eventsDecoder [ ( "events", Required ) ]


mapTagSpecs : List ( String, Required )
mapTagSpecs =
    [ ( "chipset_id", Required )
    , ( "width", Required )
    , ( "height", Required )
    , ( "lower_layer", Required )
    , ( "upper_layer", Required )
    ]


decodeMapXml : String -> Result Xml.Extra.Error Map
decodeMapXml xml =
    decodeXml xml "Map" mapDecoder mapTagSpecs


{-| Useful for decoding XML like <tag>4500 4500 4500</tag> into [4500, 4500, 4500].
-}
intStringList : JD.Decoder (List Int)
intStringList =
    let
        inner : String -> JD.Decoder (List Int)
        inner string =
            let
                result =
                    string |> String.words |> List.map String.toInt |> Result.Extra.combine
            in
                case result of
                    Result.Ok x ->
                        JD.succeed x

                    Result.Err s ->
                        JD.fail s
    in
        JD.andThen inner string


process : String -> String
process =
    -- decodeMapXml >> toString
    decodeAllEventsXml >> toString


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Response (Ok s) ->
            { model | value = s |> process } ! []

        Response (Err s) ->
            { model | error = Just s } ! []


main : Program Never { error : Maybe Http.Error, value : String } Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


localUrl : String
localUrl =
    "resources/Map0002.emu"


init : ( { error : Maybe a, value : String }, Cmd Msg )
init =
    let
        request =
            localUrl |> Http.getString
    in
        { value = "", error = Nothing } ! [ Http.send Response request ]


view : { b | error : a, value : String } -> Html.Html msg
view model =
    let
        a =
            model.value |> Html.text

        b =
            model.error |> toString |> Html.text

        f x =
            Html.ul [] [ x ]
    in
        Html.div [] (List.map f [ a, b ])


type alias Model =
    { value : String, error : Maybe Http.Error }


type Msg
    = Response (Result Http.Error String)


{-| Google drive hosting needs authentication, couldn't figure out public
solution.
-}
gDriveBase : String
gDriveBase =
    "https://docs.google.com/uc?export=download"


gDriveID : String
gDriveID =
    "1eNpVv5jgPB6bSot6dY0U_V9HhF1WF6IM"
