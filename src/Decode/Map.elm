module Decode.Map exposing (decodeMapXml, Map)

import Http
import Html
import Json.Decode as JD exposing (at, int, string, nullable)
import Json.Decode.Pipeline exposing (decode, custom)
import Xml.Extra exposing (Required(..), decodeXml, multipleTag, requiredTag, optionalTag)
import Decode.Shared exposing (..)


decodeMapXml : String -> Result String Map
decodeMapXml xml =
    decodeXml xml "Map" mapDecoder mapTagSpecs
        |> Result.mapError toString


demoResource : String
demoResource =
    "/resources/Map0002.emu"



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



-- APP


process : String -> String
process =
    decodeMapXml >> toString


main : Program Never { error : Maybe Http.Error, value : String } Msg
main =
    Html.program
        { init = init demoResource
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Response (Ok s) ->
            { model | value = s |> process } ! []

        Response (Err s) ->
            { model | error = Just s } ! []


init : String -> ( { error : Maybe a, value : String }, Cmd Msg )
init url =
    let
        request =
            url |> Http.getString
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
