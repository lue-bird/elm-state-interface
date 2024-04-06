module Web.Http exposing
    ( request
    , get, post, addHeaders
    , expectString, expectJson, expectBytes, expectWhatever
    , bodyJson, bodyBytes
    )

{-| Helpers for [HTTP types](Web#HttpRequest) as part of an [`Interface`](Web#Interface)

@docs request
@docs get, post, addHeaders
@docs expectString, expectJson, expectBytes, expectWhatever
@docs bodyJson, bodyBytes

-}

import Bytes exposing (Bytes)
import Bytes.LocalExtra
import Json.Decode
import Json.Encode
import Rope
import Web exposing (HttpBody, HttpError, HttpExpect, HttpRequest)


{-| Put a given JSON value in the body of your request. This will automatically add the `Content-Type: application/json` header.
-}
bodyJson : Json.Encode.Value -> HttpBody
bodyJson content =
    Web.HttpBodyString { mimeType = "application/json", content = Json.Encode.encode 0 content }


{-| Put given [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/) in the body of your request.
The string argument should be a [MIME type](https://en.wikipedia.org/wiki/Media_type) to be used in the `Content-Type` header

    import Bytes exposing (Bytes)
    import Bytes.Encode
    import Time
    import Web
    import Zip
    import Zip.Entry

    exampleZipBody : Web.HttpBody
    exampleZipBody =
        Web.Http.bodyBytes "application/zip"
            (Zip.fromEntries
                [ Bytes.Encode.string "Hello, World!"
                    |> Bytes.Encode.encode
                    |> Zip.Entry.store
                        { path = "hello.txt"
                        , lastModified = ( Time.utc, Time.millisToPosix 0 )
                        , comment = Nothing
                        }
                ]
                |> Zip.toBytes
            )

  - 🧩 [`Zip` and `Zip.Entry` are from `agu-z/elm-zip`](https://dark.elm.dmy.fr/packages/agu-z/elm-zip/latest/)

-}
bodyBytes : String -> (Bytes -> HttpBody)
bodyBytes mimeType content =
    Web.HttpBodyUnsignedInt8s { mimeType = mimeType, content = content |> Bytes.LocalExtra.toUnsignedInt8List }



-- Expect


{-| Expect the response body to be `JSON`, decode it using the given decoder.
The result will either be

  - `Err` with an [`HttpError`](Web#HttpError) if it didn't succeed
  - `Ok` if there was a result with either
      - `Ok` with the decoded value
      - `Err` with a [`Json.Decode.Error`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Decode#Error)
        and the actual text response

-}
expectJson : Json.Decode.Decoder future -> HttpExpect (Result HttpError (Result { actualBody : String, jsonError : Json.Decode.Error } future))
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


{-| Expect the response body to be [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/).
The result will either be

  - `Err` with an [`HttpError`](Web#HttpError) if it didn't succeed
  - `Ok` with the `Bytes`

-}
expectBytes : HttpExpect (Result HttpError Bytes)
expectBytes =
    Web.HttpExpectBytes identity


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


{-| Create a `GET` [`HttpRequest`](Web#HttpRequest).

Use [`Web.Http.addHeaders`](Web-Http#addHeaders) to set custom headers as needed.
Use [`Web.Time.onceAt`](Web-Time#onceAt) to add a timeout of how long you are willing to wait before giving up.

-}
get :
    { url : String
    , expect : HttpExpect future
    }
    -> HttpRequest future
get options =
    { url = options.url
    , method = "GET"
    , headers = []
    , body = Web.HttpBodyEmpty
    , expect = options.expect
    }


{-| Add custom headers to the [`HttpRequest`](Web#HttpRequest).

    request
        |> Web.Http.addHeaders
            [ ( "X-Custom-Header", "ProcessThisImmediately" )
            ]

-}
addHeaders : List ( String, String ) -> (HttpRequest future -> HttpRequest future)
addHeaders headers =
    \httpRequest ->
        { httpRequest
            | headers =
                (headers |> List.map (\( name, value ) -> { name = name, value = value }))
                    ++ httpRequest.headers
        }


{-| Create a `POST` [`HttpRequest`](Web#HttpRequest).

Use [`Web.Http.addHeaders`](Web-Http#addHeaders) to set custom headers as needed.
Use [`Web.Time.onceAt`](Web-Time#onceAt) to add a timeout of how long you are willing to wait before giving up.

-}
post :
    { url : String
    , body : HttpBody
    , expect : HttpExpect future
    }
    -> HttpRequest future
post options =
    { url = options.url
    , method = "POST"
    , headers = []
    , body = options.body
    , expect = options.expect
    }


{-| An [`Interface`](Web#Interface) for handling an [`HttpRequest`](Web#HttpRequest)
using the [fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
-}
request : HttpRequest future -> Web.Interface future
request =
    \httpRequest ->
        httpRequest |> Web.HttpRequest |> Rope.singleton
