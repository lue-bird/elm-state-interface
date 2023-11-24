module BrowserApp.Console exposing (log)

{-| Helpers for console interactions as part of an [`Interface`](BrowserApp#Interface)

@docs log

-}

import BrowserApp


{-| Like [`Debug.log`](https://dark.elm.dmy.fr/packages/elm/core/latest/Debug#log)
as an [`Interface`](BrowserApp#Interface)
-}
log : String -> BrowserApp.Interface state_
log string =
    BrowserApp.ConsoleLog string
