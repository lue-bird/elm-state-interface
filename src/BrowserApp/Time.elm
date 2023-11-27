module BrowserApp.Time exposing
    ( posixRequest, zoneRequest, zoneNameRequest
    , periodicallyListen
    )

{-| Helpers for [`elm/time`](https://dark.elm.dmy.fr/packages/elm/time/latest/Time) primitives as part of an [`Interface`](BrowserApp#Interface)

@docs posixRequest, zoneRequest, zoneNameRequest
@docs periodicallyListen

-}

import BrowserApp
import Duration exposing (Duration)
import Rope
import Time


{-| Get the POSIX time at the moment when this interface is added.

Replacement for `elm/time`'s [`Time.now`](https://package.elm-lang.org/packages/elm/time/latest/Time#now).

-}
posixRequest : BrowserApp.Interface Time.Posix
posixRequest =
    BrowserApp.TimePosixRequest identity
        |> Rope.singleton


{-| Produce a `Zone` based on the current UTC offset.

Replacement for `elm/time`'s [`Time.here`](https://package.elm-lang.org/packages/elm/time/latest/Time#here).

-}
zoneRequest : BrowserApp.Interface Time.Zone
zoneRequest =
    BrowserApp.TimezoneOffsetRequest (\offset -> Time.customZone -offset [])
        |> Rope.singleton


{-| Intended for package authors.
Use `Intl.DateTimeFormat().resolvedOptions().timeZone` to try to get names like Europe/Moscow or America/Havana.
From there you can look it up in any [IANA data](https://www.iana.org/time-zones) you loaded yourself.

Replacement for `elm/time`'s [`Time.getZoneName`](https://package.elm-lang.org/packages/elm/time/latest/Time#getZoneName).

-}
zoneNameRequest : BrowserApp.Interface Time.ZoneName
zoneNameRequest =
    BrowserApp.TimezoneNameRequest identity
        |> Rope.singleton


{-| An [`Interface`](BrowserApp#Interface) that sends you the current time
every time a given [`Duration`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Duration) has passed.

Note: Do not use it for animations.
[`BrowserApp.Window.animationFrameListen`](BrowserApp-Window#animationFrameListen)
syncs up with repaints and will end up being much smoother for any moving visuals.

-}
periodicallyListen : Duration -> BrowserApp.Interface Time.Posix
periodicallyListen intervalDuration =
    BrowserApp.TimePeriodicallyListen
        { intervalDurationMilliSeconds = intervalDuration |> Duration.inMilliseconds |> Basics.round
        , on = identity
        }
        |> Rope.singleton
