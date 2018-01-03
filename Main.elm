module Main exposing (main)

import Html exposing (Html, text, div, ul, input)
import Html.Attributes as HA exposing (contenteditable, value)
import Setters exposing (..)
import Types exposing (..)
import Encode exposing (toJsonString)


-- main : Program Never (List a) msg


type alias Model =
    { objects : List Object }


main : Program Never Model msg
main =
    Html.program
        { init = init ! []
        , update = (\msg model -> model ! [])
        , subscriptions = (\model -> Sub.none)
        , view = view
        }


init : { objects : List Object }
init =
    { objects = [ whitePawn, blackQueen ] }


view : { b | objects : List a } -> Html msg
view model =
    div [] [ viewObjects model, viewLister model ]


viewLister : a -> Html msg
viewLister model =
    input [ value (toJsonString model) ] []


viewObjects : { b | objects : List a } -> Html msg
viewObjects { objects } =
    let
        entry x =
            ul [] [ text (toString x) ]
    in
        objects |> List.map entry |> div []


whitePawn : Object
whitePawn =
    { position = Just ( 2, 2 )
    , piece = Just Pawn
    , owner = Just White
    , id = Just 0
    }


blackQueen : Object
blackQueen =
    whitePawn
        |> j setPiece Queen
        |> j setPosition ( 4, 4 )
        |> setOwner Nothing


type alias Object =
    { position : Maybe ( Int, Int )
    , piece : Maybe Piece
    , owner : Maybe Player
    , id : Maybe Int
    }
