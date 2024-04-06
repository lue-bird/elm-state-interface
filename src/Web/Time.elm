module Web.Time exposing
    ( posixRequest, zoneRequest, zoneNameRequest
    , periodicallyListen, onceAt
    )

{-| Helpers for [`elm/time`](https://dark.elm.dmy.fr/packages/elm/time/) primitives as part of an [`Interface`](Web#Interface).

@docs posixRequest, zoneRequest, zoneNameRequest
@docs periodicallyListen, onceAt

-}

import Duration exposing (Duration)
import Rope
import Time
import Web


{-| An [`Interface`](Web#Interface) for getting the current [POSIX time](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Posix).

Replacement for [`elm/time`'s `Time.now`](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#now).

-}
posixRequest : Web.Interface Time.Posix
posixRequest =
    Web.TimePosixRequest identity
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for getting a [`Time.Zone`](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Zone)
based on the current UTC offset.

Replacement for [`elm/time`'s `Time.here`](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#here).

-}
zoneRequest : Web.Interface Time.Zone
zoneRequest =
    Web.TimezoneOffsetRequest (\offset -> Time.customZone -offset [])
        |> Rope.singleton


{-| Intended for package authors.
An [`Interface`](Web#Interface) for using [`Intl.DateTimeFormat().resolvedOptions().timeZone`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/resolvedOptions#timezone)
to get names like `Europe/Moscow` or `America/Havana`.
From there you can look it up in any [IANA data](https://www.iana.org/time-zones) you loaded yourself.

Replacement for [`elm/time`'s `Time.getZoneName`](https://package.elm-lang.org/packages/elm/time/latest/Time#getZoneName).

-}
zoneNameRequest : Web.Interface String
zoneNameRequest =
    Web.TimezoneNameRequest identity
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for getting a reminder
once a given [point in time](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Posix) has been reached.

This lets you for example wait until it's 15 minutes before the event,
timeout a request or schedule a certain action to a specific time.

    import Web

    type RequestState result
        = NotAsked
        | BeforeTimeout { start : Time.Posix }
        | TimedOut
        | GotResult result

    { initialState = NotAsked
    , interface =
        \state ->
            [ case state of
                NotAsked ->
                    [ Web.Time.posixRequest
                        |> Web.interfaceFutureMap BeforeTimeout
                    , ..request.. |> Web.futureMap GotResult
                    ]

                BeforeTimeout requestTime ->
                    -- timeout after 10 seconds
                    Web.Time.onceAt (Duration.addTo requestTime (Duration.seconds 10))
                        |> Web.interfaceFutureMap (\_ -> TimedOut)

                TimedOut ->
                    ...

                GotResult result ->
                    ...
            ]
    }

  - 🧩 [`Duration` is from ianmackenzie/elm-units](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Duration)

You can abstract this in various ways like adding a

    withTimeout :
        ..Request result..
        -> Web.Interface (RequestState result)

where the result can be put into the "main state" and therefore cased on.

-}
onceAt : Time.Posix -> Web.Interface Time.Posix
onceAt pointInTime =
    Web.TimeOnce { pointInTime = pointInTime, on = identity }
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for getting the current time
every time a given [`Duration`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Duration) has passed.

Note: Do not use it for animations.
[`Web.Window.animationFrameListen`](Web-Window#animationFrameListen)
syncs up with repaints and will end up being much smoother for any moving visuals.

-}
periodicallyListen : Duration -> Web.Interface Time.Posix
periodicallyListen intervalDuration =
    Web.TimePeriodicallyListen
        { intervalDurationMilliSeconds = intervalDuration |> Duration.inMilliseconds |> Basics.round
        , on = identity
        }
        |> Rope.singleton
