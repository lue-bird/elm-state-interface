module Web.Navigation exposing
    ( urlRequest
    , pushUrl, replaceUrl
    , moveForward, moveBack, movementListen
    , load, reload
    )

{-| Helpers for `history` interaction as part of an [`Interface`](Web#Interface)

@docs urlRequest
@docs pushUrl, replaceUrl
@docs moveForward, moveBack, movementListen
@docs load, reload

-}

-- elm/browser on "How do I manage URL from a Browser.element?" https://github.com/elm/browser/blob/master/notes/navigation-in-elements.md

import AppUrl exposing (AppUrl)
import AppUrl.Local
import Json.Decode
import Rope
import Url exposing (Url)
import Web


{-| An [`Interface`](Web#Interface) for getting the current page's [app-specific URL](https://dark.elm.dmy.fr/packages/lydell/elm-app-url/latest/).
Is usually used while starting up the app.

Note: Uses [`window.location.href`](https://developer.mozilla.org/en-US/docs/Web/API/Window/location)

-}
urlRequest : Web.Interface AppUrl
urlRequest =
    Web.NavigationUrlRequest identity
        |> Web.Request
        |> Web.InterfaceWithFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for changing the [app-specific URL](https://dark.elm.dmy.fr/packages/lydell/elm-app-url/latest/),
without triggering a page load or adding a new entry to the browser history.

This can be useful if you have search box and you want the ?search=hats in the URL to match without adding a history entry for every single key stroke.
Imagine how annoying it would be to click back thirty times and still be on the same page!

Replacement for [`Browser.Navigation.replaceUrl`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#replaceUrl)

-}
replaceUrl : AppUrl -> Web.Interface future_
replaceUrl appUrl =
    Web.NavigationReplaceUrl appUrl
        |> Web.InterfaceWithoutFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for changing the [app-specific URL](https://dark.elm.dmy.fr/packages/lydell/elm-app-url/latest/)
and adding a new entry to the browser history
without triggering a page load.

Replacement for [`Browser.Navigation.pushUrl`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#pushUrl)

-}
pushUrl : AppUrl -> Web.Interface future_
pushUrl appUrl =
    Web.NavigationPushUrl appUrl
        |> Web.InterfaceWithoutFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for going forward a given number of pages.
If there are no more pages in the future, this will do nothing.

Note: You only manage the browser history that you created.

Replacement for [`Browser.Navigation.forward`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#forward)

-}
moveForward : Int -> Web.Interface future_
moveForward urlSteps =
    Web.NavigationGo urlSteps
        |> Web.InterfaceWithoutFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for going back a given number of pages.

Note: You only manage the browser history that you created.

Replacement for [`Browser.Navigation.back`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#back)

-}
moveBack : Int -> Web.Interface future_
moveBack urlSteps =
    Web.NavigationGo urlSteps
        |> Web.InterfaceWithoutFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for leaving the current page and loading the given [URL](https://dark.elm.dmy.fr/packages/elm/url/latest/).
This always results in a page load, even if the provided URL is the same as the current one.

    gotoElmWebsite : Web.Interface future_
    gotoElmWebsite =
        Web.Navigation.load
            -- https://elm-lang.org/
            { protocol = Url.Https
            , host = "elm-lang.org"
            , port_ = Nothing
            , path = "/"
            , query = Nothing
            , fragment = Nothing
            }

Replacement for [`Browser.Navigation.load`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#load)

-}
load : Url -> Web.Interface future_
load url =
    Web.NavigationLoad url
        |> Web.InterfaceWithoutFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for reloading the current page.
This always results in a page load!

Note: This may grab resources from the browser cache.

Replacement for [`Browser.Navigation.reload`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#reload)

-}
reload : Web.Interface future_
reload =
    Web.NavigationReload
        |> Web.InterfaceWithoutFuture
        |> Rope.singleton


{-| If you used [`pushUrl`](#pushUrl) to update the URL with new history entries,
when the user clicks ← or → buttons (or you call [`moveForward`](#moveForward) or [`moveBack`](#moveBack) yourself),
the URL will change but your UI will not.

[`movementListen`](#movementListen) is an [`Interface`](Web#Interface) for detecting those URL changes and making ui changes as needed.

When the app itself initiates a url change with [`pushUrl`](#pushUrl) or [`replaceUrl`](#replaceUrl), no such event is triggered.

Note: This event is called ["popstate"](https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event) in js

-}
movementListen : Web.Interface AppUrl
movementListen =
    Web.WindowEventListen
        { eventName = "popstate"
        , on =
            Json.Decode.field "state"
                (Json.Decode.oneOf
                    [ Json.Decode.field "appUrl" AppUrl.Local.jsonDecoder
                    , Json.Decode.null () |> Json.Decode.map (\() -> AppUrl.fromPath [])
                    ]
                )
        }
        |> Web.Listen
        |> Web.InterfaceWithFuture
        |> Rope.singleton
