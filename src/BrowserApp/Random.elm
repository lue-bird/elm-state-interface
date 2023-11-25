module BrowserApp.Random exposing (unsignedInts)

{-| Helpers for randomness as part of an [`Interface`](BrowserApp#Interface)

@docs unsignedInts

-}

import BrowserApp
import Rope


{-| An [`Interface`](BrowserApp#Interface) for generating a given count of unsigned 32-bit `Int`s.
You can use these in all kinds of packages that allow creating an initial seed
from ints like [NoRedInk/elm-random-pcg-extended](https://dark.elm.dmy.fr/packages/NoRedInk/elm-random-pcg-extended/latest/Random-Pcg-Extended#initialSeed)

Note: uses [`window.crypto.getRandomValues`](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)

-}
unsignedInts : Int -> BrowserApp.Interface (List Int)
unsignedInts count =
    BrowserApp.RandomUnsignedIntsRequest { count = count, on = identity }
        |> Rope.singleton
