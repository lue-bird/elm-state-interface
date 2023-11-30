module Web.Console exposing (log)

{-| Helpers for console interactions as part of an [`Interface`](Web#Interface)

@docs log

-}

import Rope
import Web


{-| Like [`Debug.log`](https://dark.elm.dmy.fr/packages/elm/core/latest/Debug#log)
as an [`Interface`](Web#Interface)
-}
log : String -> Web.Interface state_
log string =
    Web.ConsoleLog string |> Rope.singleton
