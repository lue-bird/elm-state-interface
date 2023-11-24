module BrowserApp.Window exposing
    ( listenToEvent
    , listenToResize
    )

{-| Helpers for `window` interaction as part of an [`Interface`](BrowserApp#Interface)

@docs listenToEvent
@docs listenToResize

-}

import BrowserApp
import Json.Decode
import Json.Decode.Local


{-| An [`Interface`](BrowserApp#Interface) that listens for a specific `window` event
-}
listenToEvent : String -> BrowserApp.Interface Json.Decode.Value
listenToEvent eventName =
    BrowserApp.WindowEventListen { eventName = eventName, on = identity }


{-| An [`Interface`](BrowserApp#Interface) that listens changes to the inner window width and height.
-}
listenToResize : BrowserApp.Interface (Result Json.Decode.Error { width : Int, height : Int })
listenToResize =
    listenToEvent "resize"
        |> BrowserApp.on
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
