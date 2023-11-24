module Json.Decode.Local exposing (andMap)

import Json.Decode


andMap :
    Json.Decode.Decoder argument
    ->
        (Json.Decode.Decoder (argument -> constructed)
         -> Json.Decode.Decoder constructed
        )
andMap argumentDecoder =
    \functionDecoder ->
        Json.Decode.map2 (\f a -> f a)
            functionDecoder
            argumentDecoder
