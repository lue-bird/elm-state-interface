module Web.FileDownload exposing (bits)

{-| Helpers for downloading a dynamically generated file as part of an [`Interface`](Web#Interface).

Security note: Browsers require downloads to be initiated by a user event.
So rather than allowing malicious sites to put files on your computer however they please,
the user has to at least click a button first.
As a result, the following interfaces only work when they are triggered by some user event.

Note: There's no equivalent module for file select
since you can easily replicate the behavior using an input element with type file or file drop area modifiers,
see for example [mpizenberg/elm-file](https://dark.elm.dmy.fr/packages/mpizenberg/elm-file/latest/FileValue#load-files-with-an-input).

@docs bits

-}

import Bit exposing (Bit)
import Rope
import Web


{-| An [`Interface`](Web#Interface) for downloading a given file
with a list of [`Bit`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/)s as its content,
a given type and and a given default name.

Note: Doesn't use [elm/bytes](https://dark.elm.dmy.fr/packages/elm/bytes/latest/)
because they can't easily be diffed as their representation is magic and only exists in js land.
To create an encoder, you can use [the bits helpers in the modules of elm-morph](https://dark.elm.dmy.fr/packages/lue-bird/elm-morph/latest/)

Replacement for [`File.Download.bytes`](https://dark.elm.dmy.fr/packages/elm/file/latest/File-Download#bytes)

-}
bits : { name : String, mimeType : String, content : List Bit } -> Web.Interface state_
bits fileDownloadConfig =
    Web.FileDownloadBits fileDownloadConfig
        |> Rope.singleton
