module Json.Encode.LocalExtra exposing (nullable, variant)

import Json.Encode


variant : ( String, Json.Encode.Value ) -> Json.Encode.Value
variant =
    \( tag, value ) ->
        Json.Encode.object
            [ ( "tag", tag |> Json.Encode.string )
            , ( "value", value )
            ]


nullable : (value -> Json.Encode.Value) -> (Maybe value -> Json.Encode.Value)
nullable valueToJson =
    \maybe ->
        case maybe of
            Nothing ->
                Json.Encode.null

            Just value ->
                value |> valueToJson
