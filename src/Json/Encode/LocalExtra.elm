module Json.Encode.LocalExtra exposing (nullable, variant)

import Json.Encode


variant : { tag : String, value : Json.Encode.Value } -> Json.Encode.Value
variant =
    \tagAndValue ->
        Json.Encode.object
            [ ( "tag", tagAndValue.tag |> Json.Encode.string )
            , ( "value", tagAndValue.value )
            ]


nullable : (value -> Json.Encode.Value) -> (Maybe value -> Json.Encode.Value)
nullable valueToJson =
    \maybe ->
        case maybe of
            Nothing ->
                Json.Encode.null

            Just value ->
                value |> valueToJson
