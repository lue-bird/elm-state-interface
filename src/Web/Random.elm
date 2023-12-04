module Web.Random exposing (unsignedInts)

{-| Helpers for randomness as part of an [`Interface`](Web#Interface)

@docs unsignedInts

-}

import Rope
import Web


{-| An [`Interface`](Web#Interface) for generating a given count of unsigned 32-bit `Int`s.
You can use these in all kinds of packages that allow creating an initial seed
from ints like [NoRedInk/elm-random-pcg-extended](https://dark.elm.dmy.fr/packages/NoRedInk/elm-random-pcg-extended/latest/Random-Pcg-Extended#initialSeed)

Note: uses [`window.crypto.getRandomValues`](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)

-}
unsignedInts : Int -> Web.Interface (List Int)
unsignedInts count =
    Web.RandomUnsignedIntsRequest { count = count, on = identity }
        |> Web.InterfaceWithReceive
        |> Rope.singleton
