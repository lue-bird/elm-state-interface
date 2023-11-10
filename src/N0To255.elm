module N0To255 exposing (N0To255, fromPercentage, jsonDecoder, toJson)

{-| Opaque type :(
-}

import Json.Decode
import Json.Encode


type N0To255
    = N0To255 Int


toInt : N0To255 -> Int
toInt =
    \(N0To255 int) -> int


toPercentage : N0To255 -> Float
toPercentage =
    \n0To255 -> (n0To255 |> toInt |> Basics.toFloat) / 255


toJson : N0To255 -> Json.Encode.Value
toJson =
    \n0To255 ->
        n0To255 |> toInt |> Json.Encode.int


fromInt : Int -> N0To255
fromInt =
    \int -> Basics.clamp 0 255 int |> N0To255


fromIntOrError : Int -> Result String N0To255
fromIntOrError =
    \int ->
        if int <= -1 then
            Err "negative"

        else if int >= 256 then
            Err "greater than 255"

        else
            int |> N0To255 |> Ok


fromPercentage : Float -> N0To255
fromPercentage =
    \percentage ->
        percentage * 255 |> Basics.round |> fromInt


jsonDecoder : Json.Decode.Decoder N0To255
jsonDecoder =
    Json.Decode.andThen
        (\int ->
            case int |> fromIntOrError of
                Ok n0To255 ->
                    n0To255 |> Json.Decode.succeed

                Err error ->
                    error |> Json.Decode.fail
        )
        Json.Decode.int
