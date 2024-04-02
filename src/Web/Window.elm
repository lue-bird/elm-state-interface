module Web.Window exposing
    ( animationFrameListen, visibilityChangeListen
    , sizeRequest, resizeListen
    , preferredLanguagesRequest, preferredLanguagesChangeListen
    , documentListenTo, listenTo
    , titleReplaceBy, authorSet, keywordsSet, descriptionSet
    )

{-| Observe and alter the page's global environment as part of an [`Interface`](Web#Interface)

@docs animationFrameListen, visibilityChangeListen
@docs sizeRequest, resizeListen
@docs preferredLanguagesRequest, preferredLanguagesChangeListen
@docs documentListenTo, listenTo

When navigating to a new page on the same site,
you may want to change the document's context:

@docs titleReplaceBy, authorSet, keywordsSet, descriptionSet

-}

import Json.Decode
import Rope
import Time
import Web


{-| An [`Interface`](Web#Interface) for setting the document's title
-}
titleReplaceBy : String -> Web.Interface future_
titleReplaceBy titleReplacement =
    Web.DocumentTitleReplaceBy titleReplacement
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for adding or replacing the document's author metadata
-}
authorSet : String -> Web.Interface future_
authorSet authorName =
    Web.DocumentAuthorSet authorName
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for adding or replacing the document's keywords metadata
which should consist of words relevant to the page's content
-}
keywordsSet : List String -> Web.Interface future_
keywordsSet authorName =
    Web.DocumentKeywordsSet authorName
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for adding or replacing the document's description metadata
which should be a short and accurate summary of the content of the page.
Several browsers, like Firefox and Opera, use this as the default description of bookmarked pages.
-}
descriptionSet : String -> Web.Interface future_
descriptionSet authorName =
    Web.DocumentDescriptionSet authorName
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for detecting a specific [`document` event](https://developer.mozilla.org/en-US/docs/Web/API/Document#events)
that has no native [`Interface`](Web#Interface), like like scroll, scrollend, selectionchange or paste
-}
documentListenTo : String -> Web.Interface Json.Decode.Value
documentListenTo eventName =
    Web.DocumentEventListen { eventName = eventName, on = Json.Decode.value }
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for detecting a specific [`window` event](https://developer.mozilla.org/en-US/docs/Web/API/Window#events)
-}
listenTo : String -> Web.Interface Json.Decode.Value
listenTo eventName =
    Web.WindowEventListen { eventName = eventName, on = Json.Decode.value }
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for detecting changes to the [visibility to the user](Web#WindowVisibility)

You can use times where the page becomes hidden to for example pause a currently running game.
These times will also be the last reliable observation you can make before a user might close the page, so treat it as the likely end of the user's session

-}
visibilityChangeListen : Web.Interface Web.WindowVisibility
visibilityChangeListen =
    Web.WindowVisibilityChangeListen identity
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for getting the inner window width and height in pixels,
not including toolbars/scrollbars
-}
sizeRequest : Web.Interface { width : Int, height : Int }
sizeRequest =
    Web.WindowSizeRequest identity
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for detecting changes to the inner window width and height
-}
resizeListen : Web.Interface { width : Int, height : Int }
resizeListen =
    Web.WindowEventListen
        { eventName = "resize"
        , on =
            Json.Decode.field "target"
                (Json.Decode.map2 (\width height -> { width = width, height = height })
                    (Json.Decode.field "innerWidth" Json.Decode.int)
                    (Json.Decode.field "innerHeight" Json.Decode.int)
                )
        }
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for detecting when animation frames occur.
This will be about 60 times per second, though 75, 120, and 144 are also widely used.
To balance this out in your animation, the [current time](https://dark.elm.dmy.fr/packages/elm/time/latest/Time#Posix) is provided each frame.

To get a delta, you could use [`Web.Time.posixRequest`](Web-Time#posixRequest)
to get a start time and check with e.g. [`Duration.from`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Duration#from) how far you've progressed in the timeline.

Note: To improve performance and battery life, most browsers pause these notifications when the app is running in a background tab or a hidden `<iframe>`.

Replacement for [`Browser.Events.onAnimationFrame`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Events#onAnimationFrame)

Note: uses [`window.requestAnimationFrame`](https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame).

-}
animationFrameListen : Web.Interface Time.Posix
animationFrameListen =
    Web.WindowAnimationFrameListen identity
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for reading the languages the user prefers.
Each described using language tags according to [RFC 5646: Tags for Identifying Languages (also known as BCP 47)](https://datatracker.ietf.org/doc/html/rfc5646).
In the returned list they are ordered by preference with the most preferred language first.

Note: uses [`window.navigator.languages`](https://developer.mozilla.org/en-US/docs/Web/API/Navigator/languages)

-}
preferredLanguagesRequest : Web.Interface (List String)
preferredLanguagesRequest =
    Web.WindowPreferredLanguagesRequest identity
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for detecting changes to the languages the user prefers.
Each described using language tags according to [RFC 5646: Tags for Identifying Languages (also known as BCP 47)](https://datatracker.ietf.org/doc/html/rfc5646).
In the returned list they are ordered by preference with the most preferred language first.

Note: uses [`window.onlanguagechange`](https://developer.mozilla.org/en-US/docs/Web/API/Window/languagechange_event)

-}
preferredLanguagesChangeListen : Web.Interface (List String)
preferredLanguagesChangeListen =
    Web.WindowPreferredLanguagesChangeListen identity
        |> Rope.singleton
