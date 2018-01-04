module Setters exposing (..)

-- TEXT SETTERS


update : (a -> b -> b) -> (c -> Result error a) -> c -> b -> b
update setField cast string object =
    case cast string of
        Result.Ok value ->
            object |> setField value

        Result.Err err ->
            object



-- HELPERS


j : (Maybe a -> b) -> a -> b
j =
    justify


justify : (Maybe a -> b) -> a -> b
justify f x =
    f (Just x)



-- SETTERS


setId : a -> { c | id : b } -> { c | id : a }
setId id object =
    { object | id = id }


setPosition : a -> { c | position : b } -> { c | position : a }
setPosition position object =
    { object | position = position }


setPiece : a -> { c | piece : b } -> { c | piece : a }
setPiece piece object =
    { object | piece = piece }


setOwner : a -> { c | owner : b } -> { c | owner : a }
setOwner owner object =
    { object | owner = owner }
