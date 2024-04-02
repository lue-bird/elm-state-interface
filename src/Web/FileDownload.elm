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
import Bytes.LocalExtra
import Rope
import Web


{-| An [`Interface`](Web#Interface) for downloading a given file
with [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/) as its content,
a given type and and a given default name.

Replacement for [`File.Download.bytes`](https://dark.elm.dmy.fr/packages/elm/file/latest/File-Download#bytes)

-}
bytes : { name : String, mimeType : String, content : Bytes } -> Web.Interface future_
bytes fileDownloadConfig =
    Web.FileDownloadUnsignedInt8s
        { name = fileDownloadConfig.name
        , mimeType = fileDownloadConfig.mimeType
        , content = fileDownloadConfig.content |> Bytes.LocalExtra.toUnsignedInt8List
        }
        |> Rope.singleton
