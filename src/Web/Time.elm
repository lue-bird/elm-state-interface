module Web.Time exposing
    ( posixRequest, zoneRequest, zoneNameRequest
    , periodicallyListen
    )

{-| Helpers for [`elm/time`](https://dark.elm.dmy.fr/packages/elm/time/) primitives as part of an [`Interface`](Web#Interface).

@docs posixRequest, zoneRequest, zoneNameRequest
@docs periodicallyListen

-}

import Duration exposing (Duration)
import Rope
import Time
import Web


{-| An [`Interface`](Web#Interface) for getting the [POSIX time](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Posix)
at the moment it's added.

Replacement for [`elm/time`'s `Time.now`](https://package.elm-lang.org/packages/elm/time/latest/Time#now).

-}
posixRequest : Web.Interface Time.Posix
posixRequest =
    Web.TimePosixRequest identity
        |> Web.Request
        |> Web.InterfaceWithFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for getting a [`Time.Zone`](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Zone)
based on the current UTC offset.

Replacement for [`elm/time`'s `Time.here`](https://package.elm-lang.org/packages/elm/time/latest/Time#here).

-}
zoneRequest : Web.Interface Time.Zone
zoneRequest =
    Web.TimezoneOffsetRequest (\offset -> Time.customZone -offset [])
        |> Web.Request
        |> Web.InterfaceWithFuture
        |> Rope.singleton


{-| Intended for package authors.
Use `Intl.DateTimeFormat().resolvedOptions().timeZone` to try to get names like Europe/Moscow or America/Havana.
From there you can look it up in any [IANA data](https://www.iana.org/time-zones) you loaded yourself.

Replacement for [`elm/time`'s `Time.getZoneName`](https://package.elm-lang.org/packages/elm/time/latest/Time#getZoneName).

-}
zoneNameRequest : Web.Interface Time.ZoneName
zoneNameRequest =
    Web.TimezoneNameRequest identity
        |> Web.Request
        |> Web.InterfaceWithFuture
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
        |> Web.Listen
        |> Web.InterfaceWithFuture
        |> Rope.singleton
