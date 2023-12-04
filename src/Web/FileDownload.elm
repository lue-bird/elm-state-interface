module Web.FileDownload exposing (bytes)

{-| Helpers for downloading a dynamically generated file as part of an [`Interface`](Web#Interface).

Security note: Browsers require downloads to be initiated by a user event.
So rather than allowing malicious sites to put files on your computer however they please,
the user has to at least click a button first.
As a result, the following interfaces only work when they are triggered by some user event.

Note: There's no equivalent module for file select
since you can easily replicate the behavior using an input element with type file or file drop area modifiers,
see for example [mpizenberg/elm-file](https://dark.elm.dmy.fr/packages/mpizenberg/elm-file/latest/FileValue#load-files-with-an-input).

@docs bytes

-}

import Bytes exposing (Bytes)
import Bytes.Decode
import Rope
import Web


{-| An [`Interface`](Web#Interface) for downloading a given file
with a list of [`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/)s as its content,
a given type and and a given default name.

Replacement for [`File.Download.bytes`](https://dark.elm.dmy.fr/packages/elm/file/latest/File-Download#bytes)

-}
bytes : { name : String, mimeType : String, content : Bytes } -> Web.Interface state_
bytes fileDownloadConfig =
    Web.FileDownloadUnsignedInt8s
        { name = fileDownloadConfig.name
        , mimeType = fileDownloadConfig.mimeType
        , content =
            -- `Bytes` can't be diffed as their representation is magic and only exists in js land, so we need to convert it.
            fileDownloadConfig.content
                |> Bytes.Decode.decode
                    (byteListDecoder (fileDownloadConfig.content |> Bytes.width))
                |> -- above decoder should never fail
                   Maybe.withDefault []
        }
        |> Web.InterfaceWithoutReceive
        |> Rope.singleton


byteListDecoder : Int -> Bytes.Decode.Decoder (List Int)
byteListDecoder length =
    Bytes.Decode.loop ( length, [] )
        (\( n, elements ) ->
            if n <= 0 then
                Bytes.Decode.succeed (Bytes.Decode.Done (elements |> List.reverse))

            else
                Bytes.Decode.map
                    (\byte -> Bytes.Decode.Loop ( n - 1, byte :: elements ))
                    Bytes.Decode.unsignedInt8
        )
