module Web.Clipboard exposing (request, replaceBy)

{-| Helpers for clipboard interactions as part of an [`Interface`](Web#Interface)

@docs request, replaceBy

Note: To listen for [copy, cut and paste events](https://developer.mozilla.org/en-US/docs/Web/API/ClipboardEvent),
use [`Web.Dom.listenTo`](Web-Dom#listenTo)

-}

import Rope
import Web


{-| An [`Interface`](Web#Interface) for reading the textual contents of the system clipboard.

Note: uses [`navigator.clipboard.readText`](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/readText)

-}
request : Web.Interface String
request =
    Web.ClipboardRequest identity
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for setting the textual contents of the system clipboard.

Note: uses [`navigator.clipboard.writeText`](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/writeText)

-}
replaceBy : String -> Web.Interface future_
replaceBy replacement =
    Web.ClipboardReplaceBy replacement
        |> Rope.singleton
