module BrowserApp.Http exposing
    ( expectJson, expectString, expectWhatever
    , bodyJson
    , get, post
    , request
    )

{-| Helpers for [HTTP types](BrowserApp#HttpRequest) as part of an [`Interface`](BrowserApp#Interface)

@docs expectJson, expectString, expectWhatever
@docs bodyJson
@docs get, post

@docs request

-}

import BrowserApp exposing (HttpBody, HttpError, HttpExpect, HttpHeader, HttpRequest)
import Json.Decode
import Json.Encode
import Rope


{-| Put some JSON value in the body of your request. This will automatically add the `Content-Type: application/json` header.
-}
bodyJson : Json.Encode.Value -> HttpBody
bodyJson value =
    BrowserApp.HttpBodyString { mimeType = "application/json", content = Json.Encode.encode 0 value }



-- Expect


{-| Expect the response body to be `JSON`, decode it using the supplied decoder.
-}
expectJson : HttpExpect (Result HttpError Json.Decode.Value)
expectJson =
    BrowserApp.HttpExpectJson identity


{-| Expect the response body to be a `String`.
-}
expectString : HttpExpect (Result HttpError String)
expectString =
    BrowserApp.HttpExpectString identity


{-| Discard the response body.
-}
expectWhatever : HttpExpect (Result HttpError ())
expectWhatever =
    BrowserApp.HttpExpectWhatever identity



-- request


{-| Create a `GET` [`HttpRequest`](BrowserApp#HttpRequest)
-}
get :
    { url : String
    , headers : List HttpHeader
    , expect : HttpExpect state
    , timeout : Maybe Int
    }
    -> HttpRequest state
get options =
    { url = options.url
    , method = "GET"
    , headers = options.headers
    , body = BrowserApp.HttpBodyEmpty
    , expect = options.expect
    , timeout = options.timeout
    }


{-| Create a `POST` [`HttpRequest`](BrowserApp#HttpRequest)
-}
post :
    { url : String
    , headers : List HttpHeader
    , body : HttpBody
    , expect : HttpExpect state
    , timeout : Maybe Int
    }
    -> HttpRequest state
post options =
    { url = options.url
    , method = "POST"
    , headers = options.headers
    , body = options.body
    , expect = options.expect
    , timeout = options.timeout
    }


{-| An [`Interface`](BrowserApp#Interface) for handling an [`HttpRequest`](BrowserApp#HttpRequest)
using the [fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
-}
request : HttpRequest state -> BrowserApp.Interface state
request =
    \httpRequest ->
        httpRequest |> BrowserApp.HttpRequest |> Rope.singleton
