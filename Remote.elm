module Remote exposing (..)

import Http
import Html
import Result.Extra
import Json.Decode as JD exposing (at, int, string, nullable)
import Json.Decode.Pipeline exposing (decode, custom)
import Xml.Extra exposing (Required(..), decodeXml, multipleTag, requiredTag, optionalTag)


decodeMapXml : String -> Result Xml.Extra.Error Map
decodeMapXml xml =
    decodeXml xml "Map" mapDecoder mapTagSpecs



-- MAP


type alias Map =
    { chipset_id : Int
    , width : Int
    , height : Int
    , lower_layer : List Int
    , upper_layer : List Int
    , events : List Event
    }


mapTagSpecs : List ( String, Required )
mapTagSpecs =
    [ ( "chipset_id", Required )
    , ( "width", Required )
    , ( "height", Required )
    , ( "lower_layer", Required )
    , ( "upper_layer", Required )
    , ( "events", Required )
    ]


mapDecoder : JD.Decoder Map
mapDecoder =
    decode Map
        |> custom (at [ "chipset_id" ] int)
        |> custom (at [ "width" ] int)
        |> custom (at [ "height" ] int)
        |> custom (at [ "lower_layer" ] intStringList)
        |> custom (at [ "upper_layer" ] intStringList)
        |> custom eventsDecoder



-- EVENT


type alias Event =
    { name : String
    , x : Int
    , y : Int
    , pages : List EventPage
    }


eventTagSpec : List ( String, Required )
eventTagSpec =
    [ ( "name", Required )
    , ( "x", Required )
    , ( "y", Required )
    , ( "pages", Required )
    ]


eventDecoder : JD.Decoder Event
eventDecoder =
    decode Event
        |> custom (at [ "name" ] string)
        |> custom (at [ "x" ] int)
        |> custom (at [ "y" ] int)
        |> custom eventPagesDecoder


eventsDecoder : JD.Decoder (List Event)
eventsDecoder =
    (\d -> requiredTag "events" d [ ( "Event", Multiple ) ]) <|
        multipleTag "Event" eventDecoder eventTagSpec



-- EVENT PAGE


type alias EventPage =
    { event_commands : List EventCommand
    }


eventPageTagSpec : List ( String, Required )
eventPageTagSpec =
    [ ( "event_commands", Required )
    ]


eventPageDecoder : JD.Decoder EventPage
eventPageDecoder =
    decode EventPage
        |> custom eventCommandsDecoder


eventPagesDecoder : JD.Decoder (List EventPage)
eventPagesDecoder =
    (\d -> requiredTag "pages" d [ ( "EventPage", Multiple ) ]) <|
        multipleTag "EventPage" eventPageDecoder eventPageTagSpec



-- EVENT COMMAND


type alias EventCommand =
    { code : Int
    , indent : Int
    , string : String
    , parameters : List Int
    }


eventCommandTagSpec : List ( String, Required )
eventCommandTagSpec =
    [ ( "code", Required )
    , ( "indent", Required )
    , ( "string", Required )
    , ( "parameters", Required )
    ]


eventCommandDecoder : JD.Decoder EventCommand
eventCommandDecoder =
    decode EventCommand
        |> custom (at [ "code" ] int)
        |> custom (at [ "indent" ] int)
        |> custom (at [ "string" ] nString)
        |> custom (at [ "parameters" ] intStringList)


eventCommandsDecoder : JD.Decoder (List EventCommand)
eventCommandsDecoder =
    ((\d -> optionalTag "event_commands" d [ ( "EventCommand", Multiple ) ]) <|
        multipleTag "EventCommand" eventCommandDecoder eventCommandTagSpec
    )
        |> JD.map (Maybe.withDefault [])



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


process : String -> String
process =
    decodeMapXml >> toString


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
