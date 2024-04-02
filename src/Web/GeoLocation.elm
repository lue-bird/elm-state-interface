module Web.GeoLocation exposing (request, changeListen)

{-| Observe the [`GeoLocation`](Web#GeoLocation) as part of an [`Interface`](Web#Interface)
using the [web geolocation API](https://developer.mozilla.org/en-US/docs/Web/API/Geolocation_API).

@docs request, changeListen

-}

import Rope
import Web


{-| An [`Interface`](Web#Interface) for getting the current [position of the device](Web#GeoLocation)
-}
request : Web.Interface Web.GeoLocation
request =
    Web.GeoLocationRequest identity
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for detecting changes in the current [position of the device](Web#GeoLocation)
-}
changeListen : Web.Interface Web.GeoLocation
changeListen =
    Web.GeoLocationChangeListen identity
        |> Rope.singleton
