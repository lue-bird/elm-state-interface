module Time.LocalExtra exposing (posixJsonDecoder, posixToStructureId)

import Json.Decode
import StructuredId exposing (StructuredId)
import Time


posixToStructureId : Time.Posix -> StructuredId
posixToStructureId =
    \timePosix ->
        timePosix |> Time.posixToMillis |> StructuredId.ofInt


posixJsonDecoder : Json.Decode.Decoder Time.Posix
posixJsonDecoder =
    Json.Decode.map Time.millisToPosix Json.Decode.int
