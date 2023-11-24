module BrowserApp.Time exposing (currentRequest, zoneRequest, zoneNameRequest)

{-| Helpers for [`elm/time`](https://dark.elm.dmy.fr/packages/elm/time/latest/Time) primitives as part of an [`Interface`](BrowserApp#Interface)

@docs currentRequest, zoneRequest, zoneNameRequest

-}

import BrowserApp
import Time


{-| Get the POSIX time at the moment when this interface is added.

Replacement for `elm/time`'s [`Time.now`](https://package.elm-lang.org/packages/elm/time/latest/Time#now).

-}
currentRequest : BrowserApp.Interface Time.Posix
currentRequest =
    BrowserApp.TimeCurrentRequest identity
        |> BrowserApp.InterfaceSingle


{-| Produce a `Zone` based on the current UTC offset.

Replacement for `elm/time`'s [`Time.here`](https://package.elm-lang.org/packages/elm/time/latest/Time#here).

-}
zoneRequest : BrowserApp.Interface Time.Zone
zoneRequest =
    BrowserApp.TimezoneRequest identity
        |> BrowserApp.InterfaceSingle


{-| Use `Intl.DateTimeFormat().resolvedOptions().timeZone` to try to get names like Europe/Moscow or America/Havana.

Replacement for `elm/time`'s [`Time.getZoneName`](https://package.elm-lang.org/packages/elm/time/latest/Time#getZoneName).

-}
zoneNameRequest : BrowserApp.Interface Time.ZoneName
zoneNameRequest =
    BrowserApp.TimezoneNameRequest identity
        |> BrowserApp.InterfaceSingle
