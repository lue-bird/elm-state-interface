module Json.Decode.LocalExtra exposing (onlyString, variant)

import Json.Decode


variant : String -> (Json.Decode.Decoder value -> Json.Decode.Decoder value)
variant name valueJsonDecoder =
    Json.Decode.map2 (\() variantValue -> variantValue)
        (Json.Decode.field "tag" (onlyString name))
        (Json.Decode.field "value" valueJsonDecoder)


onlyString : String -> Json.Decode.Decoder ()
onlyString specificAllowedString =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                if str == specificAllowedString then
                    () |> Json.Decode.succeed

                else
                    ([ "expected only \"", specificAllowedString, "\"" ] |> String.concat)
                        |> Json.Decode.fail
            )
