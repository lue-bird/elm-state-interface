module BrowserApp.Navigation exposing
    ( urlRequest, byUserListen
    , forward, back, pushUrl, replaceUrl
    , load, reload
    )

{-| Helpers for `history` interaction as part of an [`Interface`](BrowserApp#Interface)

@docs urlRequest, byUserListen
@docs forward, back, pushUrl, replaceUrl
@docs load, reload

-}

-- elm/browser on "How do I manage URL from a Browser.element?" https://github.com/elm/browser/blob/master/notes/navigation-in-elements.md

import AppUrl exposing (AppUrl)
import AppUrl.Local
import BrowserApp
import Json.Decode
import Rope
import Url exposing (Url)


{-| An [`Interface`](BrowserApp#Interface) for getting the current page's url.
Is usually used while starting up the app.

Note: Uses [`window.location.href`](https://developer.mozilla.org/en-US/docs/Web/API/Window/location)

-}
urlRequest : BrowserApp.Interface AppUrl
urlRequest =
    BrowserApp.NavigationUrlRequest identity
        |> Rope.singleton


{-| An [`Interface`](BrowserApp#Interface) that changes the [aüü-specific URL](https://dark.elm.dmy.fr/packages/lydell/elm-app-url/latest/),
but neither triggers a page load nor adds a new entry to the browser history.

This can be useful if you have search box and you want the ?search=hats in the URL to match without adding a history entry for every single key stroke. Imagine how annoying it would be to click back thirty times and still be on the same page!

Replacement for [`Browser.Navigation.replaceUrl`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#replaceUrl)

-}
replaceUrl : AppUrl -> BrowserApp.Interface state_
replaceUrl appUrl =
    BrowserApp.NavigationReplaceUrl appUrl
        |> Rope.singleton


{-| An [`Interface`](BrowserApp#Interface) that changes the [app-specific URL](https://dark.elm.dmy.fr/packages/lydell/elm-app-url/latest/)
and adds a new entry to the browser history,
but does not trigger a page load.

Replacement for [`Browser.Navigation.pushUrl`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#pushUrl)

-}
pushUrl : AppUrl -> BrowserApp.Interface state_
pushUrl appUrl =
    BrowserApp.NavigationPushUrl appUrl
        |> Rope.singleton


{-| An [`Interface`](BrowserApp#Interface) that goes forward a given number of pages.
If there are no more pages in the future, this will do nothing.

Note: You only manage the browser history that you created.

Replacement for [`Browser.Navigation.forward`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#forward)

-}
forward : Int -> BrowserApp.Interface state_
forward urlSteps =
    BrowserApp.NavigationGo urlSteps
        |> Rope.singleton


{-| An [`Interface`](BrowserApp#Interface) that goes back a given number of pages.

Note: You only manage the browser history that you created.

Replacement for [`Browser.Navigation.back`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#back)

-}
back : Int -> BrowserApp.Interface state_
back urlSteps =
    BrowserApp.NavigationGo urlSteps
        |> Rope.singleton


{-| An [`Interface`](BrowserApp#Interface) that leaves the current page and loads the given [URL](https://dark.elm.dmy.fr/packages/elm/url/latest/).
This always results in a page load, even if the provided URL is the same as the current one.

    gotoElmWebsite : BrowserApp.Interface state_
    gotoElmWebsite =
        BrowserApp.Navigation.load "https://elm-lang.org"

Replacement for [`Browser.Navigation.load`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#load)

-}
load : Url -> BrowserApp.Interface state_
load url =
    BrowserApp.NavigationLoad url
        |> Rope.singleton


{-| An [`Interface`](BrowserApp#Interface) that reloads the current page.
This always results in a page load!

Note: This may grab resources from the browser cache.

Replacement for [`Browser.Navigation.reload`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#reload)

-}
reload : BrowserApp.Interface state_
reload =
    BrowserApp.NavigationReload
        |> Rope.singleton


{-| If you use [`pushUrl`](#pushUrl) to update the URL,
when the user clicks ← or → buttons, the URL will change but your UI will not.

Use [`byUserListen`](#byUserListen) to detect those URL changes and make ui changes as needed.

Note: When the app itself initiates a url change, like with [`pushUrl`](#pushUrl) or [`replaceUrl`](#replaceUrl)
, no such event is triggered

-}
byUserListen : BrowserApp.Interface (Result Json.Decode.Error AppUrl)
byUserListen =
    BrowserApp.WindowEventListen
        { eventName = "popstate"
        , on =
            Json.Decode.decodeValue
                (Json.Decode.field "appUrl" AppUrl.Local.jsonDecoder)
        }
        |> Rope.singleton
