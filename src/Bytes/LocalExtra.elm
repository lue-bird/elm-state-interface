module Bytes.LocalExtra exposing (fromUnsignedInt8List, toUnsignedInt8List)

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode


toUnsignedInt8List : Bytes -> List Int
toUnsignedInt8List =
    \bytes ->
        bytes
            |> Bytes.Decode.decode
                (unsignedInt8ListBytesDecoder (bytes |> Bytes.width))
            |> -- above decoder should never fail
               Maybe.withDefault []


unsignedInt8ListBytesDecoder : Int -> Bytes.Decode.Decoder (List Int)
unsignedInt8ListBytesDecoder length =
    Bytes.Decode.loop { remainingLength = length, elements = [] }
        (\soFar ->
            if soFar.remainingLength <= 0 then
                Bytes.Decode.succeed (Bytes.Decode.Done (soFar.elements |> List.reverse))

            else
                Bytes.Decode.map
                    (\byte ->
                        Bytes.Decode.Loop
                            { remainingLength = soFar.remainingLength - 1
                            , elements = byte :: soFar.elements
                            }
                    )
                    Bytes.Decode.unsignedInt8
        )


fromUnsignedInt8List : List Int -> Bytes
fromUnsignedInt8List =
    \uint8s ->
        uint8s
            |> List.map Bytes.Encode.unsignedInt8
            |> Bytes.Encode.sequence
            |> Bytes.Encode.encode
