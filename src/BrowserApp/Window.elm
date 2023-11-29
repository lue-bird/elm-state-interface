module BrowserApp.Window exposing
    ( animationFrameListen
    , sizeRequest, eventListen, resizeListen
    )

{-| Helpers for `window` interaction as part of an [`Interface`](BrowserApp#Interface)

@docs animationFrameListen
@docs sizeRequest, eventListen, resizeListen

-}

import BrowserApp
import Json.Decode
import Json.Decode.Local
import Rope
import Time


{-| An [`Interface`](BrowserApp#Interface) that listens for a specific `window` event
-}
eventListen : String -> BrowserApp.Interface Json.Decode.Value
eventListen eventName =
    BrowserApp.WindowEventListen { eventName = eventName, on = identity }
        |> Rope.singleton


{-| An [`Interface`](BrowserApp#Interface) for getting the inner window width and height in pixels,
not including toolbars/scrollbars.
-}
sizeRequest : BrowserApp.Interface { width : Int, height : Int }
sizeRequest =
    BrowserApp.WindowSizeRequest identity
        |> Rope.singleton


{-| An [`Interface`](BrowserApp#Interface) that listens for changes to the inner window width and height.
-}
resizeListen : BrowserApp.Interface (Result Json.Decode.Error { width : Int, height : Int })
resizeListen =
    eventListen "resize"
        |> BrowserApp.map
            (\value ->
                value
                    |> Json.Decode.decodeValue
                        (Json.Decode.field "target"
                            (Json.Decode.succeed (\width height -> { width = width, height = height })
                                |> Json.Decode.Local.andMap (Json.Decode.field "innerWidth" Json.Decode.int)
                                |> Json.Decode.Local.andMap (Json.Decode.field "innerHeight" Json.Decode.int)
                            )
                        )
            )


{-| An [`Interface`](BrowserApp#Interface) that continuously listens for an animation frame.
This will be about 60 times per second, though 75, 120, and 144 are also widely used.
To balance this out in your animation, the [current time](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Posix) is provided each frame.

To get a delta, you could use [`BrowserApp.Time.posixRequest`](BrowserApp-Time#posixRequest)
to get a start time and check with e.g. [`Duration.from`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Duration#from) how far you've progressed in the timeline.

Note: To improve performance and battery life, most browsers pause these notifications when the app is running in a background tab or a hidden <iframe>.

Replacement for [`Browser.Events.onAnimationFrame`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Events#onAnimationFrame)

Note: uses [`window.requestAnimationFrame`](https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame).

-}
animationFrameListen : BrowserApp.Interface Time.Posix
animationFrameListen =
    BrowserApp.WindowAnimationFrameListen identity
        |> Rope.singleton
