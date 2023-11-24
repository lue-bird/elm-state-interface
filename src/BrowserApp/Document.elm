module BrowserApp.Document exposing (listenToEvent)

{-| Helpers for `document` interaction as part of an [`Interface`](BrowserApp#Interface)

@docs listenToEvent

-}

import BrowserApp
import Json.Decode
import Json.Decode.Local


{-| An [`Interface`](BrowserApp#Interface) that listens for a specific `document` event
like like keypress, keydown, keyup, click, mousemove, mousedown, mouseup
-}
listenToEvent : String -> BrowserApp.Interface Json.Decode.Value
listenToEvent eventName =
    BrowserApp.DocumentEventListen { eventName = eventName, on = identity }
        |> BrowserApp.InterfaceSingle
