module Lister exposing (..)

import Dict exposing (Dict)
import String
import Setters exposing (..)


setters : Dict String (String -> { c | id : Int } -> { c | id : Int })
setters =
    Dict.fromList [ ( "id", up setId String.toInt ) ]
