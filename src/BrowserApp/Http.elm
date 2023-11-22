module BrowserApp.Http exposing (expectJson, expectString, expectWhatever, get, jsonBody, post)

{-| Helpers for [HttpBody](), [HttpExpect](..), [HttpHeader](), [HttpRequest](BrowserApp#HttpRequest)
-}

import BrowserApp exposing (HttpBody, HttpError, HttpExpect, HttpHeader, HttpRequest)
import Dict exposing (Dict)
import Json.Decode
import Json.Decode.Extra
import Json.Encode


{-| Put some JSON value in the body of your request. This will automatically add the `Content-Type: application/json` header.
-}
jsonBody : Json.Encode.Value -> HttpBody
jsonBody value =
    BrowserApp.HttpStringBody { mimeType = "application/json", content = Json.Encode.encode 0 value }



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



-- Send Request


{-| Send an Http `GET` request
-}
get :
    { url : String
    , headers : List HttpHeader
    , expect : HttpExpect state
    , timeout : Maybe Int
    }
    -> BrowserApp.Interface state
get options =
    BrowserApp.HttpRequest
        { url = options.url
        , method = "GET"
        , headers = options.headers
        , body = BrowserApp.HttpEmptyBody
        , expect = options.expect
        , timeout = options.timeout
        }


{-| Send an Http `POST` request
-}
post :
    { url : String
    , headers : List HttpHeader
    , body : HttpBody
    , expect : HttpExpect state
    , timeout : Maybe Int
    }
    -> BrowserApp.Interface state
post options =
    BrowserApp.HttpRequest
        { url = options.url
        , method = "POST"
        , headers = options.headers
        , body = options.body
        , expect = options.expect
        , timeout = options.timeout
        }
