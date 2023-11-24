module BrowserApp.Navigation exposing
    ( forward, back, pushUrl, replaceUrl
    , load, reload
    )

{-| Helpers for `document` interaction as part of an [`Interface`](BrowserApp#Interface)

@docs forward, back, pushUrl, replaceUrl
@docs load, reload

-}

import BrowserApp
import Json.Decode
import Json.Decode.Local


{-| An [`Interface`](BrowserApp#Interface) that changes the URL,
but neither triggers a page load nor adds a new entry to the browser history.

This can be useful if you have search box and you want the ?search=hats in the URL to match without adding a history entry for every single key stroke. Imagine how annoying it would be to click back thirty times and still be on the same page!

Replacement for [`Browser.Navigation.replaceUrl`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#replaceUrl)

-}
replaceUrl : String -> BrowserApp.Interface state_
replaceUrl url =
    BrowserApp.NavigationReplaceUrl url


{-| An [`Interface`](BrowserApp#Interface) that changes the URL and adds a new entry to the browser history,
but does not trigger a page load.

Replacement for [`Browser.Navigation.pushUrl`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#pushUrl)

-}
pushUrl : String -> BrowserApp.Interface state_
pushUrl url =
    BrowserApp.NavigationPushUrl url


{-| An [`Interface`](BrowserApp#Interface) that goes forward a given number of pages.
If there are no more pages in the future, this will do nothing.

Note: You only manage the browser history that you created.

Replacement for [`Browser.Navigation.forward`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#forward)

-}
forward : Int -> BrowserApp.Interface state_
forward urlSteps =
    BrowserApp.NavigationGo urlSteps


{-| An [`Interface`](BrowserApp#Interface) that goes back a given number of pages.

Note: You only manage the browser history that you created.

Replacement for [`Browser.Navigation.back`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#back)

-}
back : Int -> BrowserApp.Interface state_
back urlSteps =
    BrowserApp.NavigationGo urlSteps


{-| An [`Interface`](BrowserApp#Interface) that leaves the current page and loads the given URL.
This always results in a page load, even if the provided URL is the same as the current one.

    gotoElmWebsite : BrowserApp.Interface state_
    gotoElmWebsite =
        BrowserApp.Navigation.load "https://elm-lang.org"

Replacement for [`Browser.Navigation.load`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#load)

-}
load : String -> BrowserApp.Interface state_
load url =
    BrowserApp.NavigationLoad url


{-| An [`Interface`](BrowserApp#Interface) that reloads the current page.
This always results in a page load!

Note: This may grab resources from the browser cache.

Replacement for [`Browser.Navigation.reload`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Navigation#reload)

-}
reload : BrowserApp.Interface state_
reload =
    BrowserApp.NavigationReload
