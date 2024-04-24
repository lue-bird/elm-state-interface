module Web.Audio.Parameter exposing (at, through)

{-| Build an [`AudioParameterTimeline`](Web#AudioParameterTimeline)

@docs at, through

-}

import Time
import Web


{-| Set it to a constant value. Add [`through`](#through) to make it transition from this starting value over time
-}
at : Float -> Web.AudioParameterTimeline
at valueAtTheStart =
    { startValue = valueAtTheStart, keyFrames = [] }


{-| Specify a key value at a given absolute point in time.
The parameter will then transition linearly between those points.

Let's define an audio function that fades in to 1 and then fades out until it's 0 again.

    import Duration
    import Time
    import Web.Audio.Parameter


    -- 1                ________
    --                /         \
    -- 0 ____________/           \_______
    --    t ->    fade in     fade out
    fadeInOut fadeInStartTime fadeOutEndTime audio =
        Web.Audio.Parameter.at 0
            |> Web.Audio.Parameter.through fadeInStartTime 1
            |> Web.Audio.Parameter.through (Duration.addTo fadeInStartTime Duration.second) 1
            |> Web.Audio.Parameter.through (Duration.subtractFrom fadeOutEndTime Duration.second) 1
            |> Web.Audio.Parameter.through fadeOutEndTime 0

  - 🧩 `Duration` is from [ianmackenzie/elm-units](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/)
  - 🧩 `Time` is from [elm/time](https://dark.elm.dmy.fr/packages/elm/time/latest/)

You do not have to worry about order.

-}
through : Time.Posix -> Float -> (Web.AudioParameterTimeline -> Web.AudioParameterTimeline)
through keyFrameMoment keyFrameValue =
    \soFar ->
        { startValue = soFar.startValue
        , keyFrames =
            { time = keyFrameMoment, value = keyFrameValue } :: soFar.keyFrames
        }
