module Web.Random exposing (unsignedInt32s)

{-| Helpers for randomness as part of an [`Interface`](Web#Interface).
Not familiar with random "generators"? [`elm/random`](https://package.elm-lang.org/packages/elm/random/latest)
explains it nicely!

Here's an example showing a number between 1 and 6 and a button to reroll
using [NoRedInk/elm-random-pcg-extended](https://dark.elm.dmy.fr/packages/NoRedInk/elm-random-pcg-extended/latest/)

    import Random.Pcg.Extended
    import Web.Dom

    type State
        = WaitingForInitialRandomness
        | DiceUiState { diceEyes : Int, seed : Random.Pcg.Extended.Seed }

    type DiceUiEvent
        = RerollClicked

    diceEyesRandomGenerator : Random.Pcg.Extended.Generator Int
    diceEyesRandomGenerator =
        Random.Pcg.Extended.int 1 6

    { initialState = WaitingForInitialRandomness
    , interface =
        \state ->
            case state of
                WaitingForInitialRandomness ->
                    Web.Random.unsignedInt32s 4
                        |> Web.futureMap
                            (\unsignedInt32s ->
                                let
                                    initialSeed : Random.Pcg.Extended.Seed
                                    initialSeed =
                                        Random.Pcg.Extended.initialSeed (unsignedInt32s |> List.head |> Maybe.withDefault 0) (unsignedInt32s |> List.drop 1)

                                    ( diceEyes, newSeed ) =
                                        Random.Pcg.Extended.step diceEyesRandomGenerator initialSeed
                                in
                                DiceUiState { diceEyes = diceEyes, seed = newSeed }
                            )

                DiceUiState randomStuff ->
                    Web.Dom.element "div"
                        []
                        [ randomStuff.diceEyes |> String.fromInt |> Web.Dom.text
                        , Web.Dom.element "button"
                            [ Web.Dom.listenTo "click"
                                |> Web.Dom.modifierFutureMap (\_ -> RerollClicked)
                            ]
                            [ "roll the dice" ]
                        ]
                        |> Web.Dom.render
                        |> Web.interfaceFutureMap
                            (\RerollClicked ->
                                let
                                    ( diceEyes, newSeed ) =
                                        Random.Pcg.Extended.step diceEyesRandomGenerator randomStuff.seed
                                in
                                DiceUiState { diceEyes = diceEyes, seed = newSeed }
                            )
    }

@docs unsignedInt32s

-}

import Rope
import Web


{-| An [`Interface`](Web#Interface) for generating a given count of cryptographically sound unsigned 32-bit `Int`s.
You can use these in all kinds of packages that allow creating an initial seed
from ints like [NoRedInk/elm-random-pcg-extended](https://dark.elm.dmy.fr/packages/NoRedInk/elm-random-pcg-extended/latest/Random-Pcg-Extended#initialSeed)

Note: uses [`window.crypto.getRandomValues`](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)

-}
unsignedInt32s : Int -> Web.Interface (List Int)
unsignedInt32s count =
    Web.RandomUnsignedInt32sRequest { count = count, on = identity }
        |> Rope.singleton
