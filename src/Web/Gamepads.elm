module Web.Gamepads exposing (request, changeListen)

{-| Observe connected [gamepads and other game controllers](Web#Gamepad)
(not including motion sensing, gesture recognition etc.).

    import Web.Time
    import Web

    interface =
        \state ->
            [ Web.Time.periodicallyListen (Duration.seconds (1 / 50))
                |> Web.interfaceFutureMap
                    ..simulate one tick using gamepad inputs..
            , [ Web.Gamepads.request, Web.Gamepads.changeListen ]
                |> Web.interfaceBatch
                |> Web.interfaceFutureMap (\gamepads -> { state | gamepads = gamepads })
            ]
                |> Web.interfaceBatch

If your gamepad isn't showing up in the list,
press some buttons. On some devices, only certain buttons will wake up the gamepad API (the shapes on PS3 controllers, for instance)

@docs request, changeListen

-}

import Dict exposing (Dict)
import Rope
import Web


{-| An [`Interface`](Web#Interface) for getting what gamepads are connected and how buttons and axis are activated.

The given `Dict` keys uniquely identify each device for the whole session.

-}
request : Web.Interface (Dict Int Web.Gamepad)
request =
    Web.GamepadsRequest identity
        |> Web.Request
        |> Web.InterfaceWithFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for detecting changes to what gamepads are connected and how buttons and axis are activated.

The given `Dict` keys uniquely identify each device for the whole session,
so you can for example check for removed and added gamepads using `Dict.diff`
or track one host device in the state.

Implementation note:
The [web gamepad API](https://developer.mozilla.org/en-US/docs/Web/API/Gamepad_API) does not offer a listener to detect changes.
So instead, we poll every 14ms (a bit more often than 60 times/second)
which is just a bit faster than Chrome's actual fetch rate (fetch rate is not part of the specification).

We want to avoid missed inputs before your next simulation tick,
so we just had to guess a good interval number.
<https://stackoverflow.com/a/51483510>

If you have issues with unresponsiveness, [open an issue](https://github.com/lue-bird/elm-state-interface/issues/new)

-}
changeListen : Web.Interface (Dict Int Web.Gamepad)
changeListen =
    Web.GamepadsChangeListen identity
        |> Web.Listen
        |> Web.InterfaceWithFuture
        |> Rope.singleton
