module Web.LocalStorage exposing
    ( request, set, remove
    , setOnADifferentTabListen, removeOnADifferentTabListen
    )

{-| Saved data for the url origin (protocol, host name, port) across browser sessions.

The data doesn't expire and won't be cleared when the page is closed.
The only exception is "incognito mode", where all data is cleared once the last "private" tab is closed.

see [mdn on `Window.localStorage`](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage)

@docs request, set, remove
@docs setOnADifferentTabListen, removeOnADifferentTabListen

-}

import AppUrl exposing (AppUrl)
import Rope
import Web


{-| An [`Interface`](Web#Interface) for reading the value of the entry with the given key.
Comes back with `Nothing` if that key doesn't exist.

    import Json.Decode
    import Web

    projectFromLocalStorageRequest : Web.Interface (Result String Project)
    projectFromLocalStorageRequest =
        Web.LocalStorage.request "project"
            |> Web.interfaceFutureMap
                (\savedProject ->
                    case savedProject of
                        Nothing ->
                            "nothing had been saved" |> Err

                        Just savedProjectJsonString ->
                            savedProjectJsonString
                                |> Json.Decode.decodeString projectJsonDecoder
                                |> Result.mapError Json.Decode.errorToString
                )

-}
request : String -> Web.Interface (Maybe String)
request key =
    Web.LocalStorageRequest { key = key, on = identity }
        |> Web.InterfaceWithFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for replacing the value of the entry with the given key
or adding key and value as a new entry if the key doesn't exist.

    import Json.Decode
    import Web

    projectSaveToLocalStorage : Project -> Web.Interface future_
    projectSaveToLocalStorage =
        \project ->
            Web.LocalStorage.set "project"
                (project |> projectJsonEncode |> Json.Encode.encode 0)

Note: This will trigger an event for all other tabs of the same url origin
that can be listened to using [`setOnADifferentTabListen`](#setOnADifferentTabListen)

-}
set : String -> String -> Web.Interface future_
set key newOrReplacementValue =
    Web.LocalStorageSet { key = key, value = Just newOrReplacementValue }
        |> Web.InterfaceWithoutFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for deleting the entry with the given key if it exists.

Note: This will trigger an event for all other tabs of the same url origin
that can be listened to using [`removeOnADifferentTabListen`](#removeOnADifferentTabListen)

-}
remove : String -> Web.Interface future_
remove key =
    Web.LocalStorageSet { key = key, value = Nothing }
        |> Web.InterfaceWithoutFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for keeping an eye on
when the local storage on a different tab with the same url origin is removed.
-}
removeOnADifferentTabListen : String -> Web.Interface AppUrl
removeOnADifferentTabListen key =
    Web.LocalStorageRemoveOnADifferentTabListen { key = key, on = identity }
        |> Web.InterfaceWithFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for keeping an eye on
when the local storage on a different tab with the same url origin is set.

When the `oldValue` is `Nothing`, no entry with that key existed.

-}
setOnADifferentTabListen : String -> Web.Interface { appUrl : AppUrl, oldValue : Maybe String, newValue : String }
setOnADifferentTabListen key =
    Web.LocalStorageSetOnADifferentTabListen { key = key, on = identity }
        |> Web.InterfaceWithFuture
        |> Rope.singleton
