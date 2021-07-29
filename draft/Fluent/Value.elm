module Fluent.Value exposing (Value, datetime, float, format, int, text)

import Time exposing (Posix, Zone)


type Value
    = Text String
    | Number Float
    | DateTime Posix Zone



-- TODO: Formatting options


format : Value -> String
format value =
    case value of
        Text txt ->
            txt

        Number num ->
            String.fromFloat num

        DateTime posix _ ->
            String.fromInt <| Time.posixToMillis posix


text : String -> Value
text =
    Text


int : Int -> Value
int =
    toFloat >> Number


float : Float -> Value
float =
    Number


datetime : Posix -> Zone -> Value
datetime =
    DateTime
