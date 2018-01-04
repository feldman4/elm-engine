module Types exposing (..)

import Math.Vector2 exposing (Vec2)


type Piece
    = Pawn
    | Queen


type Player
    = Black
    | White


type Entity
    = Entity
        { position : Maybe Vec2
        , orientation : Maybe Vec2
        , animation : Maybe (Animation Entity)
        , event : Maybe (Event Entity)
        , sprite : Maybe Sprite
        , inventory : Maybe Inventory
        }


type alias Sprite =
    String


type alias Animation a =
    Float -> a -> a


type alias Event a =
    a -> a


type alias Inventory =
    List String
