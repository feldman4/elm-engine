module Database exposing (decodeDatabaseXml)

import Http
import Html
import Json.Decode as JD exposing (at, int, string, nullable)
import Json.Decode.Pipeline exposing (decode, custom)
import Xml.Extra exposing (Required(..), decodeXml, multipleTag, requiredTag, optionalTag)
import Decode.Shared exposing (..)


decodeDatabaseXml : String -> Result Xml.Extra.Error Database
decodeDatabaseXml xml =
    decodeXml xml "Database" databaseDecoder databaseTagSpecs


demoResource : String
demoResource =
    "/resources/Easy_RT_mini.edb"


bypassDecoder : JD.Decoder a -> JD.Decoder a
bypassDecoder d =
    JD.index 1 (JD.field "LDB" d)



-- DATABASE


type alias Database =
    { chipsets : List Chipset
    , switches : List Switch
    }


databaseTagSpecs : List ( String, Required )
databaseTagSpecs =
    [ ( "chipsets", Required )
    , ( "switches", Required )
    ]


databaseDecoder : JD.Decoder Database
databaseDecoder =
    decode Database
        |> custom chipsetsDecoder
        |> custom switchesDecoder



-- CHIPSET


type alias Chipset =
    { name : String
    , chipset_name : String
    , terrain_data : List Int
    , passable_data_lower : List Int
    , passable_data_upper : List Int
    }


chipsetTagSpecs : List ( String, Required )
chipsetTagSpecs =
    [ ( "name", Required )
    , ( "chipset_name", Required )
    , ( "terrain_data", Required )
    , ( "passable_data_lower", Required )
    , ( "passable_data_upper", Required )
    ]


chipsetDecoder : JD.Decoder Chipset
chipsetDecoder =
    decode Chipset
        |> custom (at [ "name" ] string)
        |> custom (at [ "chipset_name" ] string)
        |> custom (at [ "terrain_data" ] intStringList)
        |> custom (at [ "passable_data_lower" ] intStringList)
        |> custom (at [ "passable_data_upper" ] intStringList)


chipsetsDecoder : JD.Decoder (List Chipset)
chipsetsDecoder =
    (\d -> requiredTag "chipsets" d [ ( "Chipset", Multiple ) ]) <|
        multipleTag "Chipset" chipsetDecoder chipsetTagSpecs



-- SWITCH


type alias Switch =
    { name : String
    }


switchTagSpecs : List ( String, Required )
switchTagSpecs =
    [ ( "name", Required )
    ]


switchDecoder : JD.Decoder Switch
switchDecoder =
    decode Switch
        |> custom (at [ "name" ] nString)


switchesDecoder : JD.Decoder (List Switch)
switchesDecoder =
    (\d -> requiredTag "switches" d [ ( "Switch", Multiple ) ]) <|
        multipleTag "Switch" switchDecoder switchTagSpecs



-- APP


process : String -> String
process =
    decodeDatabaseXml >> toString


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
