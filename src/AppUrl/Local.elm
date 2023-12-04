module AppUrl.Local exposing (jsonDecoder)

import AppUrl exposing (AppUrl)
import Json.Decode
import Url


jsonDecoder : Json.Decode.Decoder AppUrl
jsonDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\appUrlString ->
                case appUrlString |> fromString of
                    Nothing ->
                        "invalid app-specific URL" |> Json.Decode.fail

                    Just appUrl ->
                        appUrl |> Json.Decode.succeed
            )


fromString : String -> Maybe AppUrl
fromString =
    \appUrlString ->
        if appUrlString |> String.startsWith "/" then
            ("https://dummy.com" ++ appUrlString)
                |> Url.fromString
                |> Maybe.map AppUrl.fromUrl

        else
            Nothing
