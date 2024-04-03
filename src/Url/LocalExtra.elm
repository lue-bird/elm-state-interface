module Url.LocalExtra exposing (jsonDecoder)

import Json.Decode
import Url exposing (Url)


jsonDecoder : Json.Decode.Decoder Url
jsonDecoder =
    Json.Decode.andThen
        (\urlString ->
            case urlString |> Url.fromString of
                Nothing ->
                    "invalid URL" |> Json.Decode.fail

                Just urlParsed ->
                    urlParsed |> Json.Decode.succeed
        )
        Json.Decode.string
