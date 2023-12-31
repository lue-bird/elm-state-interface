module Web.Http exposing
    ( expectJson, expectString, expectWhatever
    , bodyJson
    , get, post
    , request
    )

{-| Helpers for [HTTP types](Web#HttpRequest) as part of an [`Interface`](Web#Interface)

@docs expectJson, expectString, expectWhatever
@docs bodyJson
@docs get, post

@docs request

-}

import Json.Decode
import Json.Encode
import Rope
import Web exposing (HttpBody, HttpError, HttpExpect, HttpHeader, HttpRequest)


{-| Put some JSON value in the body of your request. This will automatically add the `Content-Type: application/json` header.
-}
bodyJson : Json.Encode.Value -> HttpBody
bodyJson value =
    Web.HttpBodyString { mimeType = "application/json", content = Json.Encode.encode 0 value }



-- Expect


{-| Expect the response body to be `JSON`, decode it using the given decoder.
The result will either be

  - `Err` with an [`HttpError`](Web#HttpError) if it didn't succeed
  - `Ok` if there was a result with either
      - `Ok` with the decoded state
      - `Err` with a [`Json.Decode.Error`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Decode#Error) the actual text response

-}
expectJson : Json.Decode.Decoder state -> HttpExpect (Result HttpError (Result { actualBody : String, jsonError : Json.Decode.Error } state))
expectJson stateDecoder =
    Web.HttpExpectString
        (\result ->
            case result of
                Ok jsonString ->
                    case jsonString |> Json.Decode.decodeString stateDecoder of
                        Err jsonError ->
                            Ok (Err { actualBody = jsonString, jsonError = jsonError })

                        Ok json ->
                            Ok (Ok json)

                Err httpError ->
                    Err httpError
        )


{-| Expect the response body to be a `String`.
-}
expectString : HttpExpect (Result HttpError String)
expectString =
    Web.HttpExpectString identity


{-| Discard the response body.
-}
expectWhatever : HttpExpect (Result HttpError ())
expectWhatever =
    Web.HttpExpectWhatever identity



-- request


{-| Create a `GET` [`HttpRequest`](Web#HttpRequest)
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
    , body = Web.HttpBodyEmpty
    , expect = options.expect
    , timeout = options.timeout
    }


{-| Create a `POST` [`HttpRequest`](Web#HttpRequest)
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


{-| An [`Interface`](Web#Interface) for handling an [`HttpRequest`](Web#HttpRequest)
using the [fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
-}
request : HttpRequest state -> Web.Interface state
request =
    \httpRequest ->
        httpRequest
            |> Web.HttpRequest
            |> Web.InterfaceWithReceive
            |> Rope.singleton
