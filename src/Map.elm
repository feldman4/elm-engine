module Map exposing (..)

import Decode.Database
import Decode.Map
import Http
import Html


databaseResource : String
databaseResource =
    "/resources/Easy_RT_mini.elm.edb"


mapResource : String
mapResource =
    "/resources/Map0002.elm.emu"


type alias Tile =
    { chipset_id : Int, index : Int, position : ( Int, Int ) }


meshgrid : List a -> List b -> List ( a, b )
meshgrid xs ys =
    xs
        |> List.concatMap (\x -> List.map (\y -> ( x, y )) ys)


linspace : Float -> Float -> Int -> List Float
linspace start end n =
    List.range 0 n
        |> List.map toFloat
        |> List.map (\x -> x / (toFloat n * (end - start)))
        |> List.map (\x -> x + start)


linspaceEx : Float -> Float -> Int -> List Float
linspaceEx start end n =
    List.range 0 (n - 1)
        |> List.map toFloat
        |> List.map (\x -> x / (toFloat n * (end - start)))
        |> List.map (\x -> x + start)


mapToTiles : Decode.Map.Map -> List Tile
mapToTiles { height, width, lower_layer, chipset_id } =
    meshgrid (List.range 0 (height - 1)) (List.range 0 (width - 1))
        |> List.map2 (Tile chipset_id) lower_layer



-- tileToSprite : Tile -> Maybe Texture -> Game.TwoD.
-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = init ! [ loadDatabase, loadMap ]
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        LoadMap x ->
            case x |> Result.mapError toString |> Result.andThen Decode.Map.decodeMapXml of
                Ok x ->
                    { model | map = Just x } ! []

                Err s ->
                    { model | error = Just s } ! []

        LoadDatabase x ->
            case x |> Result.mapError toString |> Result.andThen Decode.Database.decodeDatabaseXml of
                Ok x ->
                    { model | database = Just x } ! []

                Err s ->
                    { model | error = Just s } ! []


init : Model
init =
    { map = Nothing, database = Nothing, error = Nothing }


loadDatabase : Cmd Msg
loadDatabase =
    databaseResource |> Http.getString |> Http.send LoadDatabase


loadMap : Cmd Msg
loadMap =
    mapResource |> Http.getString |> Http.send LoadMap


view : Model -> Html.Html msg
view model =
    let
        a =
            case model.database of
                Just database ->
                    List.length database.chipsets
                        |> toString
                        |> (++) "database, chipsets = "

                Nothing ->
                    "no database"

        b =
            case model.map of
                Just map ->
                    mapToTiles map
                        |> toString
                        |> (++) "map: "

                Nothing ->
                    "no map"

        c =
            model.error |> toString

        f x =
            Html.ul [] [ Html.text x ]
    in
        Html.div [] (List.map f [ a, b, c ])


type alias Model =
    { database : Maybe Decode.Database.Database
    , map : Maybe Decode.Map.Map
    , error : Maybe String
    }


type Msg
    = LoadMap (Result Http.Error String)
    | LoadDatabase (Result Http.Error String)
