module BrowserApp.Document exposing (eventListen)

{-| Helpers for `document` interaction as part of an [`Interface`](BrowserApp#Interface)

@docs eventListen

-}

import BrowserApp
import Json.Decode
import Json.Decode.Local


{-| An [`Interface`](BrowserApp#Interface) that listens for a specific `document` event
like like keypress, keydown, keyup, click, mousemove, mousedown, mouseup
-}
eventListen : String -> BrowserApp.Interface Json.Decode.Value
eventListen eventName =
    BrowserApp.DocumentEventListen { eventName = eventName, on = identity }
        |> BrowserApp.InterfaceSingle
