module Remote exposing (..)

import Http
import Html
import Xml
import Xml.Decode
import Xml.Query
import Result.Extra
import Json.Decode as JD exposing (at, int, string)
import Json.Decode.Pipeline exposing (decode, custom)
import Xml.Extra exposing (TagSpec, Required(..), decodeXml)


type alias Map =
    { chipset_id : Int
    , width : Int
    , height : Int

    -- , lower_layer : List Int
    }


simpleDecoder : JD.Decoder Map
simpleDecoder =
    decode Map
        |> custom (at [ "chipset_id" ] int)
        |> custom (at [ "width" ] int)
        |> custom (at [ "height" ] int)


simpleTagSpecs : List ( String, Required )
simpleTagSpecs =
    [ ( "chipset_id", Required )
    , ( "width", Required )
    , ( "height", Required )
    ]


decodeFuckingXml : String -> Result Xml.Extra.Error Map
decodeFuckingXml xml =
    decodeXml xml "Map" simpleDecoder simpleTagSpecs


eventDecoder : JD.Decoder Map
eventDecoder =
    let
        decodeMap =
            at [ "Map", "valu" ] << JD.index 1

        get field =
            at [ field, "value" ]

        getMap field =
            decodeMap << get field

        intStringList : String -> JD.Decoder (List Int)
        intStringList string =
            let
                result =
                    string |> String.words |> List.map String.toInt |> Result.Extra.combine
            in
                case result of
                    Result.Ok x ->
                        JD.succeed x

                    Result.Err s ->
                        JD.fail s

        f =
            JD.andThen intStringList string
    in
        decode Map
            |> custom (getMap "chipset_id" int)
            |> custom (getMap "width" int)
            |> custom (getMap "height" int)



-- |> custom f
-- |> custom (getMap "lower_layer" intStringList)


process : String -> String
process x =
    let
        xml =
            x
                |> Xml.Decode.decode
                |> Result.map (Xml.Query.tags "Map")
                |> Result.andThen (List.head >> Result.fromMaybe "no Map")

        lower_layer =
            xml
                |> Result.andThen (getFirstThing "lower_layer" listOfInt)

        width =
            xml |> Result.andThen (getFirstThing "width" Xml.Query.int)

        events =
            xml |> Result.map (Xml.Query.tags "Event")

        getEventId event =
            event |> getFirstThing "name" Xml.Query.string

        eventIds =
            events
                |> Result.map (List.map getEventId)
                |> Result.andThen Result.Extra.combine

        value =
            eventIds

        json =
            xml
                |> Result.map Xml.xmlToJson
                |> Result.andThen (JD.decodeValue eventDecoder)
    in
        -- value |> Result.map toString |> Result.Extra.merge
        -- json |> toString
        x |> decodeFuckingXml |> toString


getFirstThing :
    String
    -> (Xml.Value -> Result String b)
    -> Xml.Value
    -> Result String b
getFirstThing parentTag xmlQuery xml =
    xml
        |> Xml.Query.tags parentTag
        |> List.head
        |> Maybe.andThen stepIn
        |> Result.fromMaybe (parentTag ++ " not found")
        |> Result.andThen xmlQuery


listOfInt : Xml.Value -> Result String (List Int)
listOfInt =
    xmlQueryList String.toInt


xmlQueryList :
    (String -> Result String a)
    -> Xml.Value
    -> Result String (List a)
xmlQueryList stringToX value =
    value
        |> Xml.Query.string
        |> Result.map String.words
        |> Result.map (List.map stringToX)
        |> Result.andThen Result.Extra.combine


stepIn : Xml.Value -> Maybe Xml.Value
stepIn node =
    case node of
        Xml.Tag _ _ content ->
            Just content

        _ ->
            Nothing


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
