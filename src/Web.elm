module Web exposing
    ( ProgramConfig, program, Program
    , Interface, interfaceBatch, interfaceNone, interfaceFutureMap
    , DomNode(..), DomElement, DefaultActionHandling(..)
    , Audio, AudioSource, AudioSourceLoadError(..), AudioProcessing(..), AudioParameterTimeline, EditAudioDiff(..)
    , HttpRequest, HttpBody(..), HttpExpect(..), HttpError(..), HttpMetadata
    , SocketConnectionEvent(..), SocketId(..)
    , GeoLocation
    , WindowVisibility(..)
    , programInit, programUpdate, programSubscriptions
    , ProgramState(..), ProgramEvent(..)
    , InterfaceSingle(..), InterfaceSingleWithFuture(..), InterfaceSingleRequest(..), InterfaceSingleListen(..), InterfaceSingleWithoutFuture(..)
    , InterfaceDiff(..), InterfaceWithFutureDiff(..), InterfaceWithoutFutureDiff(..), EditDomDiff, ReplacementInEditDomDiff(..)
    , InterfaceSingleKeys, InterfaceSingleIdOrderTag
    , InterfaceSingleId(..), InterfaceSingleWithFutureId(..), InterfaceSingleRequestId(..), InterfaceSingleListenId(..), InterfaceSingleToIdTag, DomElementId, DomNodeId(..), HttpRequestId, HttpExpectId(..)
    )

{-| A state-interface program that can run in the browser

@docs ProgramConfig, program, Program

Tip: you can also [embed](#embed) a state-interface program as part of an existing app that uses The Elm Architecture


# interfaces

@docs Interface, interfaceBatch, interfaceNone, interfaceFutureMap


## DOM

Types used by [`Web.Dom`](Web-Dom)

@docs DomNode, DomElement, DefaultActionHandling


## Audio

Types used by [`Web.Audio`](Web-Audio)

@docs Audio, AudioSource, AudioSourceLoadError, AudioProcessing, AudioParameterTimeline, EditAudioDiff


## HTTP

Types used by [`Web.Http`](Web-Http)

@docs HttpRequest, HttpBody, HttpExpect, HttpError, HttpMetadata


## socket

Types used by [`Web.Socket`](Web-Socket)

@docs SocketConnectionEvent, SocketId


## geo location

Types used by [`Web.GeoLocation`](Web-GeoLocation)

@docs GeoLocation


## window

Types used by [`Web.Window`](Web-Window)

@docs WindowVisibility


## embed

If you just want to replace a part of your elm app with this architecture. Make sure to wire in all 3:

@docs programInit, programUpdate, programSubscriptions

Under the hood, [`Web.program`](Web#program) is then defined as just

    program config =
        Platform.worker
            { init = \() -> Web.programInit yourAppConfig
            , update = Web.programUpdate yourAppConfig
            , subscriptions = Web.programSubscriptions yourAppConfig
            }


## internals, safe to ignore for users

@docs ProgramState, ProgramEvent
@docs InterfaceSingle, InterfaceSingleWithFuture, InterfaceSingleRequest, InterfaceSingleListen, InterfaceSingleWithoutFuture
@docs InterfaceDiff, InterfaceWithFutureDiff, InterfaceWithoutFutureDiff, EditDomDiff, ReplacementInEditDomDiff
@docs InterfaceSingleKeys, InterfaceSingleIdOrderTag
@docs InterfaceSingleId, InterfaceSingleWithFutureId, InterfaceSingleRequestId, InterfaceSingleListenId, InterfaceSingleToIdTag, DomElementId, DomNodeId, HttpRequestId, HttpExpectId

-}

import AndOr exposing (AndOr)
import Angle exposing (Angle)
import AppUrl exposing (AppUrl)
import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.LocalExtra
import Dict exposing (Dict)
import Duration exposing (Duration)
import Emptiable exposing (Emptiable)
import Json.Codec exposing (JsonCodec)
import Json.Decode
import Json.Encode
import Keys exposing (Key, Keys)
import KeysSet exposing (KeysSet)
import Length exposing (Length)
import Map exposing (Mapping)
import N exposing (N1)
import Or
import Order exposing (Ordering)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Rope exposing (Rope)
import Set exposing (Set)
import Speed exposing (Speed)
import Time
import Typed
import Url exposing (Url)


{-| Ignore the specific fields, this is just exposed so can
for example simulate it more easily in tests, add a debugger etc.

A [`Web.program`](#program) would have this type

    main : Platform.Program () (Web.State YourState) (Web.Event YourState)
    main =
        Web.toProgram ...

In practice, please use [`Web.Program YourState`](#Program)

-}
type ProgramState appState
    = State
        { interface : Emptiable (KeysSet (InterfaceSingle appState) (InterfaceSingleKeys appState) N1) Possibly
        , appState : appState
        }


{-| Safe to ignore. Identification and order of an [`Interface`](#Interface)
-}
type alias InterfaceSingleKeys state =
    Key (InterfaceSingle state) (Order.By InterfaceSingleToIdTag InterfaceSingleIdOrderTag) InterfaceSingleId N1


{-| Safe to ignore. Tag for the ordering of an [`InterfaceSingleId`](#InterfaceSingleId)
-}
type InterfaceSingleIdOrderTag
    = InterfaceSingleIdOrderTag


{-| Safe to ignore. Tag for the identification mapping of an [`InterfaceSingle`](#InterfaceSingle) → [`InterfaceSingleId`](#InterfaceSingleId)
-}
type InterfaceSingleToIdTag
    = InterfaceSingleToIdTag


{-| What's needed to create a state-interface program.

  - the state is everything the program knows (what The Elm Architecture calls model)

  - The [`Interface`](#Interface) is the face to the outside world and can be created using the helpers in [`Web.Dom`](Web-Dom), [`Web.Time`](Web-Time), [`Web.Http`](Web-Http) etc.

  - connections to and from js

        port toJs : Json.Encode.Value -> Cmd event_

        port fromJs : (Json.Encode.Value -> event) -> Sub event

-}
type alias ProgramConfig state =
    RecordWithoutConstructorFunction
        { initialState : state
        , interface : state -> Interface state
        , ports :
            { toJs : Json.Encode.Value -> Cmd Never
            , fromJs : (Json.Encode.Value -> ProgramEvent state) -> Sub (ProgramEvent state)
            }
        }


{-| Incoming and outgoing effects.
To create one, use the helpers in [`Web.Time`](Web-Time), [`Web.Dom`](Web-Dom), [`Web.Http`](Web-Http) etc.

To combine multiple, use [`Web.interfaceBatch`](#interfaceBatch) and [`Web.interfaceNone`](#interfaceNone)

-}
type alias Interface future =
    Rope (InterfaceSingle future)


{-| A "non-batched" [`Interface`](#Interface).
To create one, use the helpers in `Web.Time`, `.Dom`, `.Http` etc.
-}
type InterfaceSingle future
    = InterfaceWithFuture (InterfaceSingleWithFuture future)
    | InterfaceWithoutFuture InterfaceSingleWithoutFuture


{-| An [`InterfaceSingle`](#InterfaceSingle) that will never notify elm
-}
type InterfaceSingleWithoutFuture
    = DocumentTitleReplaceBy String
    | DocumentAuthorSet String
    | DocumentKeywordsSet (List String)
    | DocumentDescriptionSet String
    | ConsoleLog String
    | ConsoleWarn String
    | ConsoleError String
    | NavigationReplaceUrl AppUrl
    | NavigationPushUrl AppUrl
    | NavigationGo Int
    | NavigationLoad Url
    | NavigationReload
    | FileDownloadUnsignedInt8s { mimeType : String, name : String, content : List Int }
    | ClipboardReplaceBy String
    | AudioPlay Audio
    | SocketMessage { id : SocketId, data : String }
    | SocketDisconnect SocketId
    | LocalStorageSet { key : String, value : Maybe String }


{-| These are possible errors we can get when loading an audio source file.

  - `AudioSourceLoadDecodeError`: This means we got the data but we couldn't decode it. One likely reason for this is that your url points to the wrong place and you're trying to decode a 404 page instead.
  - `AudioSourceLoadNetworkError`: We couldn't reach the url. Either it's some kind of CORS issue, the server is down, or you're disconnected from the internet.
  - `AudioSourceLoadUnknownError`: the audio source didn't load for a reason I'm not aware of. If this occurs in your app, [open an issue](https://github.com/lue-bird/elm-state-interface/issues/new) with the reason string so a new variant can be added for this

-}
type AudioSourceLoadError
    = AudioSourceLoadDecodeError
    | AudioSourceLoadNetworkError
    | AudioSourceLoadUnknownError String


{-| An [`InterfaceSingle`](#InterfaceSingle) that will notify elm some time in the future.
-}
type InterfaceSingleWithFuture future
    = DomNodeRender (DomNode future)
    | AudioSourceLoad { url : String, on : Result AudioSourceLoadError AudioSource -> future }
    | SocketConnect { address : String, on : SocketConnectionEvent -> future }
    | Request (InterfaceSingleRequest future)
    | Listen (InterfaceSingleListen future)


{-| An indication that connection has changed
after having initiated [`Web.Socket.connectTo`](Web-Socket#connectTo).

  - `SocketConnected`: connection has been opened. Gives access to a [`Web.SocketId`](#SocketId)
  - `SocketDisconnected`: connection has been closed with
      - the close `code` sent by the server
      - The `reason` indicating why the server closed the connection, specific to the particular server and sub-protocol

-}
type SocketConnectionEvent
    = SocketConnected SocketId
    | SocketDisconnected { code : Int, reason : String }


{-| An [`InterfaceSingleWithFuture`](#InterfaceSingleWithFuture) that will elm only once in the future.
-}
type InterfaceSingleRequest future
    = TimePosixRequest (Time.Posix -> future)
    | TimezoneOffsetRequest (Int -> future)
    | TimezoneNameRequest (Time.ZoneName -> future)
    | RandomUnsignedInt32sRequest { count : Int, on : List Int -> future }
    | WindowSizeRequest ({ width : Int, height : Int } -> future)
    | NavigationUrlRequest (AppUrl -> future)
    | HttpRequest (HttpRequest future)
    | ClipboardRequest (String -> future)
    | LocalStorageRequest { key : String, on : Maybe String -> future }
    | GeoLocationRequest (GeoLocation -> future)


{-| Position and (if available) altitude of the device on Earth, as well as the accuracy with which these properties are calculated.
The geographic position information is provided in terms of World Geodetic System coordinates (WGS84).

Device movement direction and speed might also be provided.

[`Length`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Length),
[`Angle`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Angle) and
[`Speed`](https://dark.elm.dmy.fr/packages/ianmackenzie/elm-units/latest/Speed)
are from `ianmackenzie/elm-units`

-}
type alias GeoLocation =
    RecordWithoutConstructorFunction
        { latitudeInDecimalDegrees : Float
        , longitudeInDecimalDegrees : Float
        , latitudeLongitudeAccuracy : Maybe Length
        , altitudeAboveNominalSeaLevel : Maybe Length
        , altitudeAccuracy : Maybe Length
        , headingWith0AsTrueNorthAndIncreasingClockwise : Maybe Angle
        , speed : Maybe Speed
        }


{-| An [`InterfaceSingleWithFuture`](#InterfaceSingleWithFuture) that will possibly notify elm multiple times in the future.
-}
type InterfaceSingleListen future
    = WindowEventListen { eventName : String, on : Json.Decode.Decoder future }
    | WindowVisibilityChangeListen (WindowVisibility -> future)
    | WindowAnimationFrameListen (Time.Posix -> future)
    | DocumentEventListen { eventName : String, on : Json.Decode.Decoder future }
    | TimePeriodicallyListen { intervalDurationMilliSeconds : Int, on : Time.Posix -> future }
    | SocketMessageListen { id : SocketId, on : String -> future }
    | LocalStorageRemoveOnADifferentTabListen { key : String, on : AppUrl -> future }
    | LocalStorageSetOnADifferentTabListen
        { key : String
        , on : { appUrl : AppUrl, oldValue : Maybe String, newValue : String } -> future
        }
    | GeoLocationListen (GeoLocation -> future)


{-| The visibility to the user

  - `WindowHidden`: the user has navigated to a new page, switched tabs, closed the tab, minimized or closed the browser, or, on mobile, switched from the browser to a different app
  - `WindowShown` otherwise

-}
type WindowVisibility
    = WindowShown
    | WindowHidden


{-| An HTTP request for use in an [`Interface`](#Interface).

You can set custom headers as needed.
The `timeout` can be set to a number of milliseconds you are willing to wait before giving up

-}
type alias HttpRequest future =
    RecordWithoutConstructorFunction
        { url : String
        , method : String
        , headers : List { name : String, value : String }
        , body : HttpBody
        , expect : HttpExpect future
        , timeout : Maybe Int
        }


{-| Describe what you expect to be returned in an http response body.
-}
type HttpExpect future
    = HttpExpectString (Result HttpError String -> future)
    | HttpExpectBytes (Result HttpError Bytes -> future)
    | HttpExpectWhatever (Result HttpError () -> future)


{-| Data send in your http request.

  - `HttpBodyEmpty`: Create an empty body for your request.
    This is useful for `GET` requests and `POST` requests where you are not sending any data.

  - `HttpBodyString`: Put a `String` in the body of your request. Defining `Web.Http.jsonBody` looks like this:

        import Json.Encode

        jsonBody : Json.Encode.Value -> Web.HttpBody
        jsonBody value =
            Web.HttpBodyString "application/json" (Json.Encode.encode 0 value)

    The first argument is a [MIME type](https://en.wikipedia.org/wiki/Media_type) of the body.

  - `HttpBodyUnsignedInt8s` is pretty much the same as `HttpBodyString` but for [`Bytes`](https://dark.elm.dmy.fr/packages/elm/bytes/latest/),
    see [`Web.Http.bodyBytes`](Web-Http#bodyBytes)

-}
type HttpBody
    = HttpBodyEmpty
    | HttpBodyString { mimeType : String, content : String }
    | HttpBodyUnsignedInt8s { mimeType : String, content : List Int }


{-| Safe to ignore. Identifier for an [`HttpRequest`](#HttpRequest)
-}
type alias HttpRequestId =
    RecordWithoutConstructorFunction
        { url : String
        , method : String
        , headers : List { name : String, value : String }
        , body : HttpBody
        , expect : HttpExpectId
        , timeout : Maybe Int
        }


{-| Safe to ignore. Identifier for an [`HttpExpect`](#HttpExpect)
-}
type HttpExpectId
    = IdHttpExpectString
    | IdHttpExpectBytes
    | IdHttpExpectWhatever


{-| Plain text or a [`DomElement`](#DomElement) for use in an [`Interface`](#Interface).
-}
type DomNode future
    = DomText String
    | DomElement (DomElement future)


{-| A tagged DOM node that can itself contain child [node](#DomNode)s
-}
type alias DomElement future =
    RecordWithoutConstructorFunction
        { namespace : Maybe String
        , tag : String
        , styles : Dict String String
        , attributes : Dict String String
        , attributesNamespaced : Dict ( String, String ) String
        , eventListens :
            Dict
                String
                { on : Json.Decode.Value -> future
                , defaultActionHandling : DefaultActionHandling
                }
        , subs : Array (DomNode future)
        }


{-| Setting for a listen [`Web.Dom.Modifier`](Web-Dom#Modifier) to keep or overwrite the browsers default action
-}
type DefaultActionHandling
    = DefaultActionPrevent
    | DefaultActionExecute


{-| Safe to ignore. Identifier for an [`Interface`](#Interface)
-}
type InterfaceSingleId
    = IdInterfaceWithFuture InterfaceSingleWithFutureId
    | IdInterfaceWithoutFuture InterfaceSingleWithoutFuture


{-| Safe to ignore. Identifier for an [`InterfaceSingleWithFuture`](#InterfaceSingleWithFuture)
-}
type InterfaceSingleWithFutureId
    = IdDomNodeRender
    | IdAudioSourceLoad String
    | IdSocketConnect { address : String }
    | IdRequest InterfaceSingleRequestId
    | IdListen InterfaceSingleListenId


{-| Safe to ignore. Possible identifier for an interface single that can send back values to elm
only once in the future
-}
type InterfaceSingleRequestId
    = IdTimePosixRequest
    | IdTimezoneOffsetRequest
    | IdTimezoneNameRequest
    | IdRandomUnsignedInt32sRequest Int
    | IdWindowSizeRequest
    | IdNavigationUrlRequest
    | IdHttpRequest HttpRequestId
    | IdClipboardRequest
    | IdLocalStorageRequest { key : String }
    | IdGeoLocationRequest


{-| Safe to ignore. Possible identifier for an interface single that can send back values to elm
at multiple times in the future
-}
type InterfaceSingleListenId
    = IdWindowEventListen String
    | IdWindowVisibilityChangeListen
    | IdWindowAnimationFrameListen
    | IdDocumentEventListen String
    | IdTimePeriodicallyListen { milliSeconds : Int }
    | IdSocketMessageListen SocketId
    | IdLocalStorageRemoveOnADifferentTabListen { key : String }
    | IdLocalStorageSetOnADifferentTabListen { key : String }
    | IdGeoLocationListen


{-| Safe to ignore. Identifier for a [`DomElement`](#DomElement)
-}
type alias DomElementId =
    RecordWithoutConstructorFunction
        { namespace : Maybe String
        , tag : String
        , styles : Dict String String
        , attributes : Dict String String
        , attributesNamespaced : Dict ( String, String ) String
        , eventListens : Dict String DefaultActionHandling
        , subs : Array DomNodeId
        }


{-| Safe to ignore. Identifier for a [`DomNode`](#DomNode)
-}
type DomNodeId
    = DomTextId String
    | DomElementId DomElementId


{-| Identifier for a [`Web.Socket`](Web-Socket) that can be used to [communicate](Web-Socket#communicate)
-}
type SocketId
    = SocketId Int


{-| Combine multiple [`Interface`](#Interface)s into one.
-}
interfaceBatch : List (Interface future) -> Interface future
interfaceBatch =
    \interfaces -> interfaces |> Rope.fromList |> Rope.concat


{-| Doing nothing as an [`Interface`](#Interface). These two examples are equivalent:

    Web.interfaceBatch [ a, Web.interfaceNone, b ]

and

    Web.interfaceBatch
        (List.filterMap identity
            [ a |> Just, Nothing, b |> Just ]
        )

-}
interfaceNone : Interface future_
interfaceNone =
    Rope.empty


{-| Take what the [`Interface`](#Interface) can come back with and return a different future value.

In practice, this is sometimes used like a kind of event-config pattern:

    Web.Time.posixRequest
        |> Web.interfaceFutureMap (\timeNow -> TimeReceived timeNow)

    button "show all entries"
        |> Web.Dom.render
        |> Web.interfaceFutureMap (\Pressed -> ShowAllEntriesButtonClicked)

sometimes as a way to deal with all events (equivalent to `update` in The Elm Architecture)

    ...
        |> Web.interfaceFutureMap
            (\event ->
                case event of
                    MouseMovedTo newMousePoint ->
                        { state | mousePoint = newMousePoint }

                    CounterDecreaseClicked ->
                        { state | counter = state.counter - 1 }

                    CounterIncreaseClicked ->
                        { state | counter = state.counter + 1 }
            )

and sometimes to nest events (like `Cmd.map/Task.map/Sub.map/...` in The Elm Architecture):

    type Event
        = DirectoryTreeViewEvent TreeUiEvent
        | SortButtonClicked

    type TreeUiEvent
        = Expanded TreePath
        | Collapsed TreePath

    interface : State -> Interface State
    interface state =
        ...
            [ treeUi ..
                |> Web.interfaceFutureMap DirectoryTreeViewEvent
            , ...
            ]
            |> Web.Dom.render

    treeUi : ... -> Web.DomNode TreeUiEvent

In all these examples, you end up converting the narrow future representation of part of the interface
to a broader representation for the parent interface

-}
interfaceFutureMap : (future -> mappedFuture) -> (Interface future -> Interface mappedFuture)
interfaceFutureMap futureChange =
    \interface ->
        interface
            |> Rope.toList
            |> List.map
                (\interfaceSingle ->
                    interfaceSingle |> interfaceSingleFutureMap futureChange
                )
            |> Rope.fromList


interfaceSingleFutureMap : (future -> mappedFuture) -> (InterfaceSingle future -> InterfaceSingle mappedFuture)
interfaceSingleFutureMap futureChange =
    \interface ->
        case interface of
            InterfaceWithoutFuture interfaceWithoutFuture ->
                interfaceWithoutFuture |> InterfaceWithoutFuture

            InterfaceWithFuture interfaceWithFuture ->
                interfaceWithFuture
                    |> interfaceWithFutureMap futureChange
                    |> InterfaceWithFuture


domNodeFutureMap : (future -> mappedFuture) -> (DomNode future -> DomNode mappedFuture)
domNodeFutureMap futureChange =
    \domElementToMap ->
        case domElementToMap of
            DomText text ->
                DomText text

            DomElement domElement ->
                domElement |> domElementMap futureChange |> DomElement


interfaceWithFutureMap : (future -> mappedFuture) -> (InterfaceSingleWithFuture future -> InterfaceSingleWithFuture mappedFuture)
interfaceWithFutureMap futureChange =
    \interface ->
        case interface of
            DomNodeRender domElementToRender ->
                domElementToRender |> domNodeFutureMap futureChange |> DomNodeRender

            AudioSourceLoad load ->
                { url = load.url, on = \event -> load.on event |> futureChange }
                    |> AudioSourceLoad

            SocketConnect connect ->
                { address = connect.address, on = \event -> event |> connect.on |> futureChange }
                    |> SocketConnect

            Request request ->
                request |> interfaceRequestFutureMap futureChange |> Request

            Listen listen ->
                listen |> interfaceListenFutureMap futureChange |> Listen


interfaceRequestFutureMap : (future -> mappedFuture) -> (InterfaceSingleRequest future -> InterfaceSingleRequest mappedFuture)
interfaceRequestFutureMap futureChange =
    \interfaceSingleRequest ->
        case interfaceSingleRequest of
            LocalStorageRequest request ->
                { key = request.key, on = \event -> event |> request.on |> futureChange }
                    |> LocalStorageRequest

            HttpRequest httpRequest ->
                httpRequest
                    |> httpRequestMap futureChange
                    |> HttpRequest

            WindowSizeRequest toFuture ->
                (\event -> toFuture event |> futureChange)
                    |> WindowSizeRequest

            NavigationUrlRequest toFuture ->
                (\event -> toFuture event |> futureChange) |> NavigationUrlRequest

            ClipboardRequest toFuture ->
                (\event -> toFuture event |> futureChange) |> ClipboardRequest

            TimePosixRequest requestTimeNow ->
                (\event -> requestTimeNow event |> futureChange)
                    |> TimePosixRequest

            TimezoneOffsetRequest requestTimezone ->
                (\event -> requestTimezone event |> futureChange)
                    |> TimezoneOffsetRequest

            TimezoneNameRequest requestTimezoneName ->
                (\event -> requestTimezoneName event |> futureChange)
                    |> TimezoneNameRequest

            RandomUnsignedInt32sRequest randomUnsignedInt32sRequest ->
                { count = randomUnsignedInt32sRequest.count
                , on = \ints -> randomUnsignedInt32sRequest.on ints |> futureChange
                }
                    |> RandomUnsignedInt32sRequest

            GeoLocationRequest toFuture ->
                (\event -> event |> toFuture |> futureChange) |> GeoLocationRequest


httpRequestMap : (future -> mappedFuture) -> (HttpRequest future -> HttpRequest mappedFuture)
httpRequestMap futureChange =
    \httpRequest ->
        { url = httpRequest.url
        , method = httpRequest.method
        , headers = httpRequest.headers
        , body = httpRequest.body
        , timeout = httpRequest.timeout
        , expect =
            case httpRequest.expect of
                HttpExpectWhatever expectWhatever ->
                    (\unit -> expectWhatever unit |> futureChange) |> HttpExpectWhatever

                HttpExpectString expectString ->
                    (\string -> expectString string |> futureChange) |> HttpExpectString

                HttpExpectBytes expectBytes ->
                    (\bytes -> expectBytes bytes |> futureChange) |> HttpExpectBytes
        }


interfaceListenFutureMap : (future -> mappedFuture) -> (InterfaceSingleListen future -> InterfaceSingleListen mappedFuture)
interfaceListenFutureMap futureChange =
    \interfaceSingleListen ->
        case interfaceSingleListen of
            WindowEventListen listen ->
                { eventName = listen.eventName, on = listen.on |> Json.Decode.map futureChange }
                    |> WindowEventListen

            WindowVisibilityChangeListen toFuture ->
                (\event -> toFuture event |> futureChange) |> WindowVisibilityChangeListen

            WindowAnimationFrameListen toFuture ->
                (\event -> toFuture event |> futureChange) |> WindowAnimationFrameListen

            TimePeriodicallyListen timePeriodicallyListen ->
                { intervalDurationMilliSeconds = timePeriodicallyListen.intervalDurationMilliSeconds
                , on = \posix -> timePeriodicallyListen.on posix |> futureChange
                }
                    |> TimePeriodicallyListen

            DocumentEventListen listen ->
                { eventName = listen.eventName, on = listen.on |> Json.Decode.map futureChange }
                    |> DocumentEventListen

            SocketMessageListen messageListen ->
                { id = messageListen.id, on = \event -> event |> messageListen.on |> futureChange }
                    |> SocketMessageListen

            LocalStorageRemoveOnADifferentTabListen listen ->
                { key = listen.key, on = \event -> event |> listen.on |> futureChange }
                    |> LocalStorageRemoveOnADifferentTabListen

            LocalStorageSetOnADifferentTabListen listen ->
                { key = listen.key, on = \event -> event |> listen.on |> futureChange }
                    |> LocalStorageSetOnADifferentTabListen

            GeoLocationListen toFuture ->
                (\event -> event |> toFuture |> futureChange) |> GeoLocationListen


domElementMap : (future -> mappedFuture) -> (DomElement future -> DomElement mappedFuture)
domElementMap futureChange =
    \domElementToMap ->
        { namespace = domElementToMap.namespace
        , tag = domElementToMap.tag
        , styles = domElementToMap.styles
        , attributes = domElementToMap.attributes
        , attributesNamespaced = domElementToMap.attributesNamespaced
        , eventListens =
            domElementToMap.eventListens
                |> Dict.map
                    (\_ listen ->
                        { on = \event -> listen.on event |> futureChange
                        , defaultActionHandling = listen.defaultActionHandling
                        }
                    )
        , subs =
            domElementToMap.subs |> Array.map (domNodeFutureMap futureChange)
        }


{-| Ignore the specific variants, this is just exposed so can
for example simulate it more easily in tests, add a debugger etc.

A [`Web.program`](#program) would have this type

    main : Platform.Program () (Web.State YourState) (Web.Event YourState)
    main =
        Web.toProgram ...

In practice, please use [`Web.Program YourState`](#Program)

-}
type ProgramEvent appState
    = InterfaceDiffFailedToDecode Json.Decode.Error
    | InterfaceEventDataFailedToDecode Json.Decode.Error
    | InterfaceEventIgnored
    | AppEventToNewAppState appState


domElementToId : DomElement future_ -> DomElementId
domElementToId =
    \domElement ->
        { namespace = domElement.namespace
        , tag = domElement.tag
        , styles = domElement.styles
        , attributes = domElement.attributes
        , attributesNamespaced = domElement.attributesNamespaced
        , eventListens =
            domElement.eventListens |> Dict.map (\_ listen -> listen.defaultActionHandling)
        , subs =
            domElement.subs |> Array.map domNodeToId
        }


domNodeDiff :
    List Int
    -> ( DomNode state, DomNode state )
    -> List EditDomDiff
domNodeDiff path =
    \( aNode, bNode ) ->
        case ( aNode, bNode ) of
            ( DomText _, DomElement bElement ) ->
                [ { path = path, replacement = bElement |> domElementToId |> DomElementId |> ReplacementDomNode } ]

            ( DomElement _, DomText bText ) ->
                [ { path = path, replacement = bText |> DomTextId |> ReplacementDomNode } ]

            ( DomText aText, DomText bText ) ->
                if aText == bText then
                    []

                else
                    [ { path = path, replacement = bText |> DomTextId |> ReplacementDomNode } ]

            ( DomElement aElement, DomElement bElement ) ->
                ( aElement, bElement ) |> domElementDiff path


domElementDiff :
    List Int
    -> ( DomElement state, DomElement state )
    -> List EditDomDiff
domElementDiff path =
    \( aElement, bElement ) ->
        if
            (aElement.tag == bElement.tag)
                && ((aElement.subs |> Array.length) == (bElement.subs |> Array.length))
        then
            let
                modifierDiffs : List ReplacementInEditDomDiff
                modifierDiffs =
                    [ if aElement.styles == bElement.styles then
                        Nothing

                      else
                        ReplacementDomElementStyles bElement.styles |> Just
                    , if aElement.attributes == bElement.attributes then
                        Nothing

                      else
                        ReplacementDomElementAttributes bElement.attributes |> Just
                    , if aElement.attributesNamespaced == bElement.attributesNamespaced then
                        Nothing

                      else
                        ReplacementDomElementAttributesNamespaced bElement.attributesNamespaced |> Just
                    , let
                        bElementEventListensId : Dict String DefaultActionHandling
                        bElementEventListensId =
                            bElement.eventListens |> Dict.map (\_ v -> v.defaultActionHandling)
                      in
                      if
                        (aElement.eventListens |> Dict.map (\_ v -> v.defaultActionHandling))
                            == bElementEventListensId
                      then
                        Nothing

                      else
                        ReplacementDomElementEventListens bElementEventListensId |> Just
                    ]
                        |> List.filterMap identity
            in
            (modifierDiffs
                |> List.map (\replacement -> { path = path, replacement = replacement })
            )
                ++ (List.map2 (\( subIndex, aSub ) bSub -> domNodeDiff (subIndex :: path) ( aSub, bSub ))
                        (aElement.subs |> Array.toIndexedList)
                        (bElement.subs |> Array.toList)
                        |> List.concat
                   )

        else
            [ { path = path, replacement = bElement |> domElementToId |> DomElementId |> ReplacementDomNode } ]


type Comparable
    = ComparableString String
    | ComparableList (List Comparable)


comparableOrder : ( Comparable, Comparable ) -> Order
comparableOrder =
    \( a, b ) ->
        case ( a, b ) of
            ( ComparableString aString, ComparableString bString ) ->
                compare aString bString

            ( ComparableString _, ComparableList _ ) ->
                LT

            ( ComparableList _, ComparableString _ ) ->
                GT

            ( ComparableList aList, ComparableList bList ) ->
                ( aList, bList ) |> comparableListOrder


comparableListOrder : ( List Comparable, List Comparable ) -> Order
comparableListOrder =
    \( a, b ) ->
        case ( a, b ) of
            ( [], [] ) ->
                EQ

            ( [], _ :: _ ) ->
                LT

            ( _ :: _, [] ) ->
                GT

            ( head0 :: tail0, head1 :: tail1 ) ->
                case ( head0, head1 ) |> comparableOrder of
                    LT ->
                        LT

                    GT ->
                        GT

                    EQ ->
                        comparableListOrder ( tail0, tail1 )


interfaceKeys : Keys (InterfaceSingle future) (InterfaceSingleKeys future) N1
interfaceKeys =
    Keys.oneBy interfaceToIdMapping interfaceIdOrder


interfaceToIdMapping : Mapping (InterfaceSingle future_) InterfaceSingleToIdTag InterfaceSingleId
interfaceToIdMapping =
    Map.tag InterfaceSingleToIdTag interfaceSingleToId


interfaceSingleToId : InterfaceSingle future_ -> InterfaceSingleId
interfaceSingleToId =
    \interface ->
        case interface of
            InterfaceWithoutFuture interfaceWithoutFuture ->
                interfaceWithoutFuture |> IdInterfaceWithoutFuture

            InterfaceWithFuture interfaceWithFuture ->
                interfaceWithFuture |> interfaceSingleWithFutureToId |> IdInterfaceWithFuture


interfaceSingleListenToId : InterfaceSingleListen future_ -> InterfaceSingleListenId
interfaceSingleListenToId =
    \interfaceSingleListen ->
        case interfaceSingleListen of
            TimePeriodicallyListen timePeriodicallyListen ->
                IdTimePeriodicallyListen
                    { milliSeconds = timePeriodicallyListen.intervalDurationMilliSeconds }

            SocketMessageListen messageListen ->
                IdSocketMessageListen messageListen.id

            LocalStorageRemoveOnADifferentTabListen listen ->
                IdLocalStorageRemoveOnADifferentTabListen { key = listen.key }

            LocalStorageSetOnADifferentTabListen listen ->
                IdLocalStorageSetOnADifferentTabListen { key = listen.key }

            WindowEventListen listen ->
                IdWindowEventListen listen.eventName

            WindowVisibilityChangeListen _ ->
                IdWindowVisibilityChangeListen

            WindowAnimationFrameListen _ ->
                IdWindowAnimationFrameListen

            DocumentEventListen listen ->
                IdDocumentEventListen listen.eventName

            GeoLocationListen _ ->
                IdGeoLocationListen


httpRequestToId : HttpRequest future_ -> HttpRequestId
httpRequestToId =
    \httpRequest ->
        { url = httpRequest.url
        , method = httpRequest.method |> String.toUpper
        , headers = httpRequest.headers
        , body = httpRequest.body
        , expect = httpRequest.expect |> httpExpectToId
        , timeout = httpRequest.timeout
        }


httpExpectToId : HttpExpect future_ -> HttpExpectId
httpExpectToId =
    \httpExpect ->
        case httpExpect of
            HttpExpectWhatever _ ->
                IdHttpExpectWhatever

            HttpExpectString _ ->
                IdHttpExpectString

            HttpExpectBytes _ ->
                IdHttpExpectBytes


interfaceSingleRequestToId : InterfaceSingleRequest future_ -> InterfaceSingleRequestId
interfaceSingleRequestToId =
    \interfaceSingleRequest ->
        case interfaceSingleRequest of
            TimePosixRequest _ ->
                IdTimePosixRequest

            TimezoneOffsetRequest _ ->
                IdTimezoneOffsetRequest

            TimezoneNameRequest _ ->
                IdTimezoneNameRequest

            RandomUnsignedInt32sRequest randomUnsignedInt32sRequest ->
                IdRandomUnsignedInt32sRequest randomUnsignedInt32sRequest.count

            HttpRequest httpRequest ->
                httpRequest |> httpRequestToId |> IdHttpRequest

            WindowSizeRequest _ ->
                IdWindowSizeRequest

            NavigationUrlRequest _ ->
                IdNavigationUrlRequest

            ClipboardRequest _ ->
                IdClipboardRequest

            LocalStorageRequest request ->
                IdLocalStorageRequest { key = request.key }

            GeoLocationRequest _ ->
                IdGeoLocationRequest


interfaceSingleWithFutureToId : InterfaceSingleWithFuture future_ -> InterfaceSingleWithFutureId
interfaceSingleWithFutureToId =
    \interfaceWithFuture ->
        case interfaceWithFuture of
            DomNodeRender _ ->
                IdDomNodeRender

            AudioSourceLoad load ->
                IdAudioSourceLoad load.url

            SocketConnect connect ->
                IdSocketConnect { address = connect.address }

            Request request ->
                request |> interfaceSingleRequestToId |> IdRequest

            Listen listen ->
                listen |> interfaceSingleListenToId |> IdListen


interfaceIdOrder : Ordering InterfaceSingleId InterfaceSingleIdOrderTag
interfaceIdOrder =
    Typed.tag InterfaceSingleIdOrderTag
        (\( a, b ) -> ( a |> interfaceSingleIdToComparable, b |> interfaceSingleIdToComparable ) |> comparableOrder)


interfaceSingleIdToComparable : InterfaceSingleId -> Comparable
interfaceSingleIdToComparable =
    \interfaceId ->
        case interfaceId of
            IdInterfaceWithoutFuture interfaceWithoutFuture ->
                interfaceWithoutFuture |> interfaceSingleWithoutFutureToComparable

            IdInterfaceWithFuture idInterfaceWithoutFuture ->
                idInterfaceWithoutFuture |> idInterfaceSingleWithFutureToComparable


idInterfaceSingleWithFutureToComparable : InterfaceSingleWithFutureId -> Comparable
idInterfaceSingleWithFutureToComparable =
    \idInterfaceWithoutFuture ->
        case idInterfaceWithoutFuture of
            IdDomNodeRender ->
                ComparableString "IdDomNodeRender"

            IdAudioSourceLoad url ->
                ComparableList
                    [ ComparableString "IdAudioSourceLoad"
                    , ComparableString url
                    ]

            IdSocketConnect connect ->
                ComparableList
                    [ ComparableString "IdSocketConnect"
                    , ComparableString connect.address
                    ]

            IdRequest request ->
                request |> interfaceSingleRequestIdToComparable

            IdListen listenId ->
                listenId |> listenIdToComparable


intToComparable : Int -> Comparable
intToComparable =
    \int -> int |> String.fromInt |> ComparableString


interfaceSingleRequestIdToComparable : InterfaceSingleRequestId -> Comparable
interfaceSingleRequestIdToComparable =
    \interfaceSingleRequestId ->
        case interfaceSingleRequestId of
            IdTimePosixRequest ->
                ComparableString "IdTimePosixRequest"

            IdTimezoneOffsetRequest ->
                ComparableString "IdTimezoneOffsetRequest"

            IdTimezoneNameRequest ->
                ComparableString "IdTimezoneNameRequest"

            IdRandomUnsignedInt32sRequest count ->
                ComparableList
                    [ ComparableString "IdRandomUnsignedInt32sRequest"
                    , count |> intToComparable
                    ]

            IdLocalStorageRequest request ->
                ComparableList
                    [ ComparableString "IdLocalStorageRequest"
                    , request.key |> ComparableString
                    ]

            IdHttpRequest request ->
                ComparableList
                    [ ComparableString "IdHttpRequest"
                    , request |> httpRequestIdToComparable
                    ]

            IdWindowSizeRequest ->
                ComparableString "IdWindowSizeRequest"

            IdNavigationUrlRequest ->
                ComparableString "IdNavigationUrlRequest"

            IdClipboardRequest ->
                ComparableString "IdClipboardRequest"

            IdGeoLocationRequest ->
                ComparableString "IdGeoLocationRequest"


maybeToComparable : (value -> Comparable) -> (Maybe value -> Comparable)
maybeToComparable valueToComparable =
    \maybe ->
        case maybe of
            Nothing ->
                "Nothing" |> ComparableString

            Just value ->
                ComparableList
                    [ "Just" |> ComparableString
                    , value |> valueToComparable
                    ]


httpRequestIdToComparable : HttpRequestId -> Comparable
httpRequestIdToComparable =
    \httpRequestId ->
        ComparableList
            [ httpRequestId.url |> ComparableString
            , httpRequestId.method |> ComparableString
            , httpRequestId.headers |> List.map httpHeaderToComparable |> ComparableList
            , httpRequestId.body |> httpBodyToComparable
            , httpRequestId.expect |> httpExpectIdToComparable
            , httpRequestId.timeout |> maybeToComparable intToComparable
            ]


httpHeaderToComparable : { name : String, value : String } -> Comparable
httpHeaderToComparable =
    \httpHeader ->
        ComparableList
            [ httpHeader.name |> ComparableString
            , httpHeader.value |> ComparableString
            ]


httpBodyToComparable : HttpBody -> Comparable
httpBodyToComparable =
    \httpBody ->
        case httpBody of
            HttpBodyEmpty ->
                "HttpBodyEmpty" |> ComparableString

            HttpBodyString stringBody ->
                ComparableList
                    [ "HttpBodyString" |> ComparableString
                    , stringBody |> httpStringBodyToComparable
                    ]

            HttpBodyUnsignedInt8s stringBody ->
                ComparableList
                    [ "HttpBodyBytes" |> ComparableString
                    , stringBody |> httpBytesBodyToComparable
                    ]


httpBytesBodyToComparable : { mimeType : String, content : List Int } -> Comparable
httpBytesBodyToComparable =
    \httpStringBody ->
        ComparableList
            [ httpStringBody.mimeType |> ComparableString
            , httpStringBody.content |> List.map intToComparable |> ComparableList
            ]


httpStringBodyToComparable : { mimeType : String, content : String } -> Comparable
httpStringBodyToComparable =
    \httpStringBody ->
        ComparableList
            [ httpStringBody.mimeType |> ComparableString
            , httpStringBody.content |> ComparableString
            ]


httpExpectIdToComparable : HttpExpectId -> Comparable
httpExpectIdToComparable =
    \httpExpectId ->
        case httpExpectId of
            IdHttpExpectString ->
                "IdHttpExpectString" |> ComparableString

            IdHttpExpectBytes ->
                "IdHttpExpectBytes" |> ComparableString

            IdHttpExpectWhatever ->
                "IdHttpExpectWhatever" |> ComparableString


socketIdToComparable : SocketId -> Comparable
socketIdToComparable =
    \(SocketId raw) -> raw |> intToComparable


listenIdToComparable : InterfaceSingleListenId -> Comparable
listenIdToComparable =
    \listenId ->
        case listenId of
            IdWindowEventListen eventName ->
                ComparableList
                    [ ComparableString "IdWindowEventListen"
                    , ComparableString eventName
                    ]

            IdWindowVisibilityChangeListen ->
                ComparableString "IdWindowVisibilityChangeListen"

            IdWindowAnimationFrameListen ->
                ComparableString "IdWindowAnimationFrameListen"

            IdDocumentEventListen eventName ->
                ComparableList
                    [ ComparableString "IdDocumentEventListen"
                    , ComparableString eventName
                    ]

            IdTimePeriodicallyListen intervalDuration ->
                ComparableList
                    [ ComparableString "IdTimePeriodicallyListen"
                    , intervalDuration.milliSeconds |> intToComparable
                    ]

            IdSocketMessageListen id ->
                ComparableList
                    [ ComparableString "IdSocketConnect"
                    , id |> socketIdToComparable
                    ]

            IdLocalStorageRemoveOnADifferentTabListen listen ->
                ComparableList
                    [ ComparableString "IdLocalStorageRemoveOnADifferentTabListen"
                    , listen.key |> ComparableString
                    ]

            IdLocalStorageSetOnADifferentTabListen listen ->
                ComparableList
                    [ ComparableString "IdLocalStorageSetOnADifferentTabListen"
                    , listen.key |> ComparableString
                    ]

            IdGeoLocationListen ->
                ComparableString "IdGeoLocationListen"


interfaceSingleWithoutFutureToComparable : InterfaceSingleWithoutFuture -> Comparable
interfaceSingleWithoutFutureToComparable =
    \interfaceWithoutFuture ->
        case interfaceWithoutFuture of
            DocumentTitleReplaceBy replacement ->
                ComparableList
                    [ ComparableString "DocumentTitleReplaceBy"
                    , ComparableString replacement
                    ]

            DocumentAuthorSet new ->
                ComparableList
                    [ ComparableString "DocumentAuthorSet"
                    , ComparableString new
                    ]

            DocumentKeywordsSet new ->
                ComparableList
                    [ ComparableString "DocumentKeywordsSet"
                    , new |> List.map ComparableString |> ComparableList
                    ]

            DocumentDescriptionSet new ->
                ComparableList
                    [ ComparableString "DocumentDescriptionSet"
                    , ComparableString new
                    ]

            ConsoleLog string ->
                ComparableList
                    [ ComparableString "ConsoleLog"
                    , ComparableString string
                    ]

            ConsoleWarn string ->
                ComparableList
                    [ ComparableString "ConsoleWarn"
                    , ComparableString string
                    ]

            ConsoleError string ->
                ComparableList
                    [ ComparableString "ConsoleError"
                    , ComparableString string
                    ]

            NavigationReplaceUrl url ->
                ComparableList
                    [ ComparableString "NavigationReplaceUrl"
                    , ComparableString (url |> AppUrl.toString)
                    ]

            NavigationPushUrl url ->
                ComparableList
                    [ ComparableString "NavigationPushUrl"
                    , ComparableString (url |> AppUrl.toString)
                    ]

            NavigationGo urlSteps ->
                ComparableList
                    [ ComparableString "NavigationGo"
                    , urlSteps |> intToComparable
                    ]

            NavigationLoad url ->
                ComparableList
                    [ ComparableString "NavigationLoad"
                    , ComparableString (url |> Url.toString)
                    ]

            NavigationReload ->
                ComparableString "NavigationReload"

            FileDownloadUnsignedInt8s config ->
                ComparableList
                    [ ComparableString "FileDownloadUnsignedInt8s"
                    , ComparableString config.name
                    , ComparableString config.mimeType
                    , config.content
                        |> List.map (\bit -> bit |> String.fromInt |> ComparableString)
                        |> ComparableList
                    ]

            ClipboardReplaceBy replacement ->
                ComparableList
                    [ ComparableString "ClipboardReplaceBy"
                    , ComparableString replacement
                    ]

            AudioPlay audio ->
                ComparableList
                    [ ComparableString "AudioPlay"
                    , audio.url |> ComparableString
                    , audio.startTime |> Time.posixToMillis |> String.fromInt |> ComparableString
                    ]

            SocketMessage message ->
                ComparableList
                    [ ComparableString "SocketMessage"
                    , message.id |> socketIdToComparable
                    , message.data |> ComparableString
                    ]

            SocketDisconnect id ->
                ComparableList
                    [ ComparableString "SocketDisconnect"
                    , id |> socketIdToComparable
                    ]

            LocalStorageSet set ->
                ComparableList
                    [ ComparableString "LocalStorageSet"
                    , set.key |> ComparableString
                    , set.value |> maybeToComparable ComparableString
                    ]


interfaceDiffs :
    { old : Emptiable (KeysSet (InterfaceSingle future) (InterfaceSingleKeys future) N1) Possibly
    , updated : Emptiable (KeysSet (InterfaceSingle future) (InterfaceSingleKeys future) N1) Possibly
    }
    -> List InterfaceDiff
interfaceDiffs =
    \interfaces ->
        ( { key = interfaceKeys, set = interfaces.old }
        , { key = interfaceKeys, set = interfaces.updated }
        )
            |> KeysSet.fold2From
                []
                (\interfaceAndOr soFar ->
                    interfaceOldAndOrUpdatedDiffs interfaceAndOr
                        ++ soFar
                )


domNodeToId : DomNode future_ -> DomNodeId
domNodeToId domNode =
    case domNode of
        DomText text ->
            DomTextId text

        DomElement element ->
            DomElementId (element |> domElementToId)


interfaceOldAndOrUpdatedDiffs : AndOr (InterfaceSingle future) (InterfaceSingle future) -> List InterfaceDiff
interfaceOldAndOrUpdatedDiffs =
    \interfaceAndOr ->
        case interfaceAndOr of
            AndOr.Both ( InterfaceWithFuture (DomNodeRender domElementPreviouslyRendered), InterfaceWithFuture (DomNodeRender domElementToRender) ) ->
                ( domElementPreviouslyRendered, domElementToRender )
                    |> domNodeDiff []
                    |> List.map (\diff -> diff |> EditDom |> InterfaceWithFutureDiff)

            AndOr.Both ( InterfaceWithoutFuture (AudioPlay previouslyPlayed), InterfaceWithoutFuture (AudioPlay toPlay) ) ->
                ( previouslyPlayed, toPlay )
                    |> audioDiff
                    |> List.map
                        (\diff ->
                            { url = toPlay.url, startTime = toPlay.startTime, replacement = diff }
                                |> EditAudio
                                |> InterfaceWithoutFutureDiff
                        )

            AndOr.Both _ ->
                []

            AndOr.Only (Or.First onlyOld) ->
                (case onlyOld of
                    InterfaceWithoutFuture removedInterfaceWithoutFuture ->
                        case removedInterfaceWithoutFuture of
                            AudioPlay audio ->
                                RemoveAudio { url = audio.url, startTime = audio.startTime } |> List.singleton

                            DocumentTitleReplaceBy _ ->
                                []

                            DocumentAuthorSet _ ->
                                []

                            DocumentKeywordsSet _ ->
                                []

                            DocumentDescriptionSet _ ->
                                []

                            ConsoleLog _ ->
                                []

                            ConsoleWarn _ ->
                                []

                            ConsoleError _ ->
                                []

                            NavigationReplaceUrl _ ->
                                []

                            NavigationPushUrl _ ->
                                []

                            NavigationGo _ ->
                                []

                            NavigationLoad _ ->
                                []

                            NavigationReload ->
                                []

                            FileDownloadUnsignedInt8s _ ->
                                []

                            ClipboardReplaceBy _ ->
                                []

                            SocketMessage _ ->
                                []

                            SocketDisconnect _ ->
                                []

                            LocalStorageSet _ ->
                                []

                    InterfaceWithFuture interfaceWithFuture ->
                        case interfaceWithFuture of
                            Request (TimePosixRequest _) ->
                                []

                            Request (TimezoneOffsetRequest _) ->
                                []

                            Request (TimezoneNameRequest _) ->
                                []

                            Request (RandomUnsignedInt32sRequest _) ->
                                []

                            Request (HttpRequest request) ->
                                RemoveHttpRequest (request |> httpRequestToId) |> List.singleton

                            DomNodeRender _ ->
                                RemoveDom |> List.singleton

                            Request (WindowSizeRequest _) ->
                                []

                            Request (NavigationUrlRequest _) ->
                                []

                            Request (ClipboardRequest _) ->
                                []

                            AudioSourceLoad _ ->
                                []

                            SocketConnect connect ->
                                RemoveSocketConnect { address = connect.address } |> List.singleton

                            Request (LocalStorageRequest _) ->
                                []

                            Request (GeoLocationRequest _) ->
                                []

                            Listen listen ->
                                listen |> interfaceSingleListenToId |> RemoveListen |> List.singleton
                )
                    |> List.map InterfaceWithoutFutureDiff

            AndOr.Only (Or.Second onlyUpdated) ->
                (case onlyUpdated of
                    InterfaceWithoutFuture interfaceWithoutFuture ->
                        Add interfaceWithoutFuture
                            |> InterfaceWithoutFutureDiff

                    InterfaceWithFuture interfaceWithFuture ->
                        (case interfaceWithFuture of
                            DomNodeRender domElementToRender ->
                                { path = []
                                , replacement = domElementToRender |> domNodeToId |> ReplacementDomNode
                                }
                                    |> EditDom

                            AudioSourceLoad load ->
                                AddAudioSourceLoad load.url

                            SocketConnect connect ->
                                AddSocketConnect { address = connect.address }

                            Request request ->
                                request |> interfaceSingleRequestToId |> AddRequest

                            Listen listen ->
                                listen |> interfaceSingleListenToId |> AddListen
                        )
                            |> InterfaceWithFutureDiff
                )
                    |> List.singleton


audioDiff : ( Audio, Audio ) -> List EditAudioDiff
audioDiff =
    \( previous, new ) ->
        [ if previous.volume == new.volume then
            Nothing

          else
            ReplacementAudioVolume new.volume |> Just
        , if previous.speed == new.speed then
            Nothing

          else
            ReplacementAudioSpeed new.speed |> Just
        , if previous.stereoPan == new.stereoPan then
            Nothing

          else
            ReplacementAudioStereoPan new.stereoPan |> Just
        , if previous.processingLastToFirst == new.processingLastToFirst then
            Nothing

          else
            new.processingLastToFirst |> List.reverse |> ReplacementAudioProcessing |> Just
        ]
            |> List.filterMap identity


socketIdJsonCodec : JsonCodec SocketId
socketIdJsonCodec =
    Json.Codec.map SocketId (\(SocketId index) -> index) Json.Codec.int


interfaceSingleListenIdJsonCodec : JsonCodec InterfaceSingleListenId
interfaceSingleListenIdJsonCodec =
    Json.Codec.choice
        (\idTimePeriodicallyListen idWindowEventListen idWindowVisibilityChangeListen idWindowAnimationFrameListen idDocumentEventListen idSocketMessageListen idLocalStorageRemoveOnADifferentTabListen idLocalStorageSetOnADifferentTabListen idGeoLocationListen interfaceSingleListenId ->
            case interfaceSingleListenId of
                IdTimePeriodicallyListen intervalDuration ->
                    idTimePeriodicallyListen intervalDuration

                IdWindowEventListen eventName ->
                    idWindowEventListen eventName

                IdWindowVisibilityChangeListen ->
                    idWindowVisibilityChangeListen ()

                IdWindowAnimationFrameListen ->
                    idWindowAnimationFrameListen ()

                IdDocumentEventListen eventName ->
                    idDocumentEventListen eventName

                IdSocketMessageListen id ->
                    idSocketMessageListen id

                IdLocalStorageRemoveOnADifferentTabListen listen ->
                    idLocalStorageRemoveOnADifferentTabListen listen

                IdLocalStorageSetOnADifferentTabListen listen ->
                    idLocalStorageSetOnADifferentTabListen listen

                IdGeoLocationListen ->
                    idGeoLocationListen ()
        )
        |> Json.Codec.variant ( IdTimePeriodicallyListen, "TimePeriodicallyListen" )
            (Json.Codec.record (\ms -> { milliSeconds = ms })
                |> Json.Codec.field ( .milliSeconds, "milliSeconds" ) Json.Codec.int
                |> Json.Codec.recordFinish
            )
        |> Json.Codec.variant ( IdWindowEventListen, "WindowEventListen" )
            Json.Codec.string
        |> Json.Codec.variant ( \() -> IdWindowVisibilityChangeListen, "WindowVisibilityChangeListen" )
            Json.Codec.unit
        |> Json.Codec.variant ( \() -> IdWindowAnimationFrameListen, "WindowAnimationFrameListen" )
            Json.Codec.unit
        |> Json.Codec.variant ( IdDocumentEventListen, "DocumentEventListen" )
            Json.Codec.string
        |> Json.Codec.variant ( IdSocketMessageListen, "SocketMessageListen" ) socketIdJsonCodec
        |> Json.Codec.variant ( IdLocalStorageRemoveOnADifferentTabListen, "LocalStorageRemoveOnADifferentTabListen" )
            (Json.Codec.record (\key -> { key = key })
                |> Json.Codec.field ( .key, "key" ) Json.Codec.string
                |> Json.Codec.recordFinish
            )
        |> Json.Codec.variant ( IdLocalStorageSetOnADifferentTabListen, "LocalStorageSetOnADifferentTabListen" )
            (Json.Codec.record (\key -> { key = key })
                |> Json.Codec.field ( .key, "key" ) Json.Codec.string
                |> Json.Codec.recordFinish
            )
        |> Json.Codec.variant ( \() -> IdGeoLocationListen, "GeoLocationListen" )
            Json.Codec.unit


interfaceWithFutureDiffJsonCodec : JsonCodec InterfaceWithFutureDiff
interfaceWithFutureDiffJsonCodec =
    Json.Codec.choice
        (\addEditDom addAudioSourceLoad addSocketConnect addListen addRequest interfaceWithFutureDiff ->
            case interfaceWithFutureDiff of
                EditDom editDomDiff ->
                    addEditDom editDomDiff

                AddAudioSourceLoad audioSourceLoad ->
                    addAudioSourceLoad audioSourceLoad

                AddSocketConnect connect ->
                    addSocketConnect connect

                AddListen listen ->
                    addListen listen

                AddRequest request ->
                    addRequest request
        )
        |> Json.Codec.variant ( EditDom, "EditDom" )
            (Json.Codec.record (\path replacement -> { path = path, replacement = replacement })
                |> Json.Codec.field ( .path, "path" ) (Json.Codec.list Json.Codec.int)
                |> Json.Codec.field ( .replacement, "replacement" )
                    replacementInEditDomDiffJsonCodec
                |> Json.Codec.recordFinish
            )
        |> Json.Codec.variant ( AddAudioSourceLoad, "AddAudioSourceLoad" )
            Json.Codec.string
        |> Json.Codec.variant ( AddSocketConnect, "AddSocketConnect" )
            (Json.Codec.record (\address -> { address = address })
                |> Json.Codec.field ( .address, "address" ) Json.Codec.string
                |> Json.Codec.recordFinish
            )
        |> Json.Codec.variant ( AddListen, "AddListen" ) interfaceSingleListenIdJsonCodec
        |> Json.Codec.variant ( AddRequest, "AddRequest" ) interfaceSingleRequestIdJsonCodec


listFirstJust : (node -> Maybe found) -> List node -> Maybe found
listFirstJust tryMapToFound list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case tryMapToFound head of
                Just b ->
                    Just b

                Nothing ->
                    listFirstJust tryMapToFound tail


httpRequestIdJsonCodec : JsonCodec HttpRequestId
httpRequestIdJsonCodec =
    { toJson =
        \httpRequestId ->
            Json.Encode.object
                [ ( "url", httpRequestId.url |> Json.Encode.string )
                , ( "method", httpRequestId.method |> Json.Encode.string )
                , ( "headers"
                  , httpRequestId.headers
                        |> addContentTypeForBody httpRequestId.body
                        |> (Json.Codec.list headerJsonCodec).toJson
                  )
                , ( "expect", httpRequestId.expect |> httpExpectIdJsonCodec.toJson )
                , ( "body", httpRequestId.body |> httpBodyToJson )
                , ( "timeout", httpRequestId.timeout |> httpTimeoutJsonCodec.toJson )
                ]
    , jsonDecoder =
        (Json.Codec.list headerJsonCodec).jsonDecoder
            |> Json.Decode.andThen
                (\headers ->
                    Json.Decode.map5
                        (\url method expect body timeout ->
                            { url = url
                            , method = method
                            , headers = headers
                            , expect = expect
                            , body = body
                            , timeout = timeout
                            }
                        )
                        (Json.Decode.field "url" Json.Decode.string)
                        (Json.Decode.field "method" Json.Decode.string)
                        (Json.Decode.field "expect" httpExpectIdJsonCodec.jsonDecoder)
                        (Json.Decode.field "body"
                            (let
                                maybeContentType : Maybe String
                                maybeContentType =
                                    headers
                                        |> listFirstJust
                                            (\header ->
                                                case header.name of
                                                    "Content-Type" ->
                                                        header.value |> Just

                                                    _ ->
                                                        Nothing
                                            )
                             in
                             case maybeContentType of
                                Just mimeType ->
                                    Json.Decode.map (\content -> HttpBodyString { mimeType = mimeType, content = content })
                                        Json.Decode.string

                                Nothing ->
                                    HttpBodyEmpty |> Json.Decode.null
                            )
                        )
                        (Json.Decode.field "timeout" httpTimeoutJsonCodec.jsonDecoder)
                )
    }


headerJsonCodec : JsonCodec { name : String, value : String }
headerJsonCodec =
    Json.Codec.record (\name value -> { name = name, value = value })
        |> Json.Codec.field ( .name, "name" ) Json.Codec.string
        |> Json.Codec.field ( .value, "value" ) Json.Codec.string
        |> Json.Codec.recordFinish


addContentTypeForBody : HttpBody -> (List { name : String, value : String } -> List { name : String, value : String })
addContentTypeForBody body headers =
    case body of
        HttpBodyEmpty ->
            headers

        HttpBodyString stringBodyInfo ->
            { name = "Content-Type", value = stringBodyInfo.mimeType } :: headers

        HttpBodyUnsignedInt8s bytesBodyInfo ->
            { name = "Content-Type", value = bytesBodyInfo.mimeType } :: headers


httpBodyToJson : HttpBody -> Json.Encode.Value
httpBodyToJson =
    \body ->
        case body of
            HttpBodyString stringBodyInfo ->
                Json.Encode.object
                    [ ( "tag", "string" |> Json.Encode.string )
                    , ( "value", stringBodyInfo.content |> Json.Encode.string )
                    ]

            HttpBodyUnsignedInt8s bytesBodyInfo ->
                Json.Encode.object
                    [ ( "tag", "uint8Array" |> Json.Encode.string )
                    , ( "value", bytesBodyInfo.content |> Json.Encode.list Json.Encode.int )
                    ]

            HttpBodyEmpty ->
                Json.Encode.object [ ( "tag", "empty" |> Json.Encode.string ) ]


httpTimeoutJsonCodec : JsonCodec (Maybe Int)
httpTimeoutJsonCodec =
    Json.Codec.nullable Json.Codec.int


httpExpectIdJsonCodec : JsonCodec HttpExpectId
httpExpectIdJsonCodec =
    Json.Codec.enum [ IdHttpExpectString, IdHttpExpectBytes, IdHttpExpectWhatever ]
        (\httpExpectId ->
            case httpExpectId of
                IdHttpExpectString ->
                    "string"

                IdHttpExpectBytes ->
                    "bytes"

                IdHttpExpectWhatever ->
                    "whatever"
        )


interfaceSingleRequestIdJsonCodec : JsonCodec InterfaceSingleRequestId
interfaceSingleRequestIdJsonCodec =
    Json.Codec.choice
        (\timePosixRequest timezoneOffsetRequest timezoneNameRequest randomUnsignedInt32sRequest httpRequest windowSizeRequest navigationUrlRequest clipboardRequest localStorageRequest geoLocationRequest interfaceSingleRequestId ->
            case interfaceSingleRequestId of
                IdTimePosixRequest ->
                    timePosixRequest ()

                IdTimezoneOffsetRequest ->
                    timezoneOffsetRequest ()

                IdTimezoneNameRequest ->
                    timezoneNameRequest ()

                IdRandomUnsignedInt32sRequest count ->
                    randomUnsignedInt32sRequest count

                IdHttpRequest httpRequestId ->
                    httpRequest httpRequestId

                IdWindowSizeRequest ->
                    windowSizeRequest ()

                IdNavigationUrlRequest ->
                    navigationUrlRequest ()

                IdClipboardRequest ->
                    clipboardRequest ()

                IdLocalStorageRequest request ->
                    localStorageRequest request

                IdGeoLocationRequest ->
                    geoLocationRequest ()
        )
        |> Json.Codec.variant ( \() -> IdTimePosixRequest, "TimePosixRequest" ) Json.Codec.unit
        |> Json.Codec.variant ( \() -> IdTimezoneOffsetRequest, "TimezoneOffsetRequest" ) Json.Codec.unit
        |> Json.Codec.variant ( \() -> IdTimezoneNameRequest, "TimezoneNameRequest" ) Json.Codec.unit
        |> Json.Codec.variant ( IdRandomUnsignedInt32sRequest, "RandomUnsignedInt32sRequest" )
            Json.Codec.int
        |> Json.Codec.variant ( IdHttpRequest, "HttpRequest" )
            httpRequestIdJsonCodec
        |> Json.Codec.variant ( \() -> IdWindowSizeRequest, "WindowSizeRequest" )
            Json.Codec.unit
        |> Json.Codec.variant ( \() -> IdNavigationUrlRequest, "NavigationUrlRequest" )
            Json.Codec.unit
        |> Json.Codec.variant ( \() -> IdClipboardRequest, "ClipboardRequest" ) Json.Codec.unit
        |> Json.Codec.variant ( IdLocalStorageRequest, "LocalStorageRequest" )
            (Json.Codec.record (\key -> { key = key })
                |> Json.Codec.field ( .key, "key" ) Json.Codec.string
                |> Json.Codec.recordFinish
            )
        |> Json.Codec.variant ( \() -> IdGeoLocationRequest, "GeoLocationRequest" ) Json.Codec.unit


domNodeIdJsonCodec : JsonCodec DomNodeId
domNodeIdJsonCodec =
    Json.Codec.choice
        (\domTextId domElementId domNodeId ->
            case domNodeId of
                DomTextId text ->
                    domTextId text

                DomElementId element ->
                    domElementId element
        )
        |> Json.Codec.variant ( DomTextId, "Text" ) Json.Codec.string
        |> Json.Codec.variant ( DomElementId, "Element" )
            (Json.Codec.lazy (\() -> domElementIdJsonCodec))


domElementAttributesNamespacedJsonCodec : JsonCodec (Dict ( String, String ) String)
domElementAttributesNamespacedJsonCodec =
    Json.Codec.dict
        (Json.Codec.map
            (\r -> { key = ( r.namespace, r.key ), value = r.value })
            (\entry ->
                let
                    ( namespace, key ) =
                        entry.key
                in
                { namespace = namespace, key = key, value = entry.value }
            )
            (Json.Codec.record
                (\namespace key value -> { namespace = namespace, key = key, value = value })
                |> Json.Codec.field ( .namespace, "namespace" ) Json.Codec.string
                |> Json.Codec.field ( .key, "key" ) Json.Codec.string
                |> Json.Codec.field ( .value, "value" ) Json.Codec.string
                |> Json.Codec.recordFinish
            )
        )


domElementAttributesJsonCodec : JsonCodec (Dict String String)
domElementAttributesJsonCodec =
    Json.Codec.dict
        (Json.Codec.record
            (\key value -> { key = key, value = value })
            |> Json.Codec.field ( .key, "key" ) Json.Codec.string
            |> Json.Codec.field ( .value, "value" ) Json.Codec.string
            |> Json.Codec.recordFinish
        )


domElementStylesJsonCodec : JsonCodec (Dict String String)
domElementStylesJsonCodec =
    Json.Codec.dict
        (Json.Codec.record
            (\key value -> { key = key, value = value })
            |> Json.Codec.field ( .key, "key" ) Json.Codec.string
            |> Json.Codec.field ( .value, "value" ) Json.Codec.string
            |> Json.Codec.recordFinish
        )


domElementEventListensJsonCodec : JsonCodec (Dict String DefaultActionHandling)
domElementEventListensJsonCodec =
    Json.Codec.dict
        (Json.Codec.map
            (\eventListen -> { key = eventListen.name, value = eventListen.defaultActionHandling })
            (\entry -> { name = entry.key, defaultActionHandling = entry.value })
            (Json.Codec.record
                (\name defaultActionHandling -> { name = name, defaultActionHandling = defaultActionHandling })
                |> Json.Codec.field ( .name, "name" ) Json.Codec.string
                |> Json.Codec.field ( .defaultActionHandling, "defaultActionHandling" ) defaultActionHandlingJsonCodec
                |> Json.Codec.recordFinish
            )
        )


defaultActionHandlingJsonCodec : JsonCodec DefaultActionHandling
defaultActionHandlingJsonCodec =
    Json.Codec.enum [ DefaultActionPrevent, DefaultActionExecute ]
        (\defaultActionHandling ->
            case defaultActionHandling of
                DefaultActionPrevent ->
                    "DefaultActionPrevent"

                DefaultActionExecute ->
                    "DefaultActionExecute"
        )


replacementInEditDomDiffJsonCodec : JsonCodec ReplacementInEditDomDiff
replacementInEditDomDiffJsonCodec =
    Json.Codec.choice
        (\replacementDomNode replacementDomElementStyles replacementDomElementAttributes replacementDomElementAttributesNamespaced replacementDomElementEventListens replacementInEditDomDiff ->
            case replacementInEditDomDiff of
                ReplacementDomNode id ->
                    replacementDomNode id

                ReplacementDomElementStyles styles ->
                    replacementDomElementStyles styles

                ReplacementDomElementAttributes attributes ->
                    replacementDomElementAttributes attributes

                ReplacementDomElementAttributesNamespaced attributesNamespaced ->
                    replacementDomElementAttributesNamespaced attributesNamespaced

                ReplacementDomElementEventListens listens ->
                    replacementDomElementEventListens listens
        )
        |> Json.Codec.variant ( ReplacementDomNode, "Node" ) domNodeIdJsonCodec
        |> Json.Codec.variant ( ReplacementDomElementStyles, "Styles" ) domElementStylesJsonCodec
        |> Json.Codec.variant ( ReplacementDomElementAttributes, "Attributes" ) domElementAttributesJsonCodec
        |> Json.Codec.variant ( ReplacementDomElementAttributesNamespaced, "AttributesNamespaced" ) domElementAttributesNamespacedJsonCodec
        |> Json.Codec.variant ( ReplacementDomElementEventListens, "EventListens" ) domElementEventListensJsonCodec


tagValueToJson : ( String, Json.Encode.Value ) -> Json.Encode.Value
tagValueToJson =
    \( tag, value ) ->
        Json.Encode.object
            [ ( "tag", tag |> Json.Encode.string )
            , ( "value", value )
            ]


interfaceDiffToJson : InterfaceDiff -> Json.Encode.Value
interfaceDiffToJson =
    \interfaceDiff ->
        tagValueToJson
            (case interfaceDiff of
                InterfaceWithFutureDiff withFutureDiff ->
                    ( "InterfaceWithFuture", withFutureDiff |> interfaceWithFutureDiffJsonCodec.toJson )

                InterfaceWithoutFutureDiff withoutFutureDiff ->
                    ( "InterfaceWithoutFuture", withoutFutureDiff |> interfaceWithoutFutureDiffToJson )
            )


audioParameterTimelineToJson : AudioParameterTimeline -> Json.Encode.Value
audioParameterTimelineToJson =
    \timeline ->
        Json.Encode.object
            [ ( "startValue", timeline.startValue |> Json.Encode.float )
            , ( "keyFrames"
              , timeline.keyFrames
                    |> List.sortBy (\keyFrame -> keyFrame.time |> Time.posixToMillis)
                    |> Json.Encode.list
                        (\keyFrame ->
                            Json.Encode.object
                                [ ( "time", keyFrame.time |> Time.posixToMillis |> Json.Encode.int )
                                , ( "value", keyFrame.value |> Json.Encode.float )
                                ]
                        )
              )
            ]


audioProcessingToJson : AudioProcessing -> Json.Encode.Value
audioProcessingToJson =
    \processing ->
        tagValueToJson
            (case processing of
                AudioLinearConvolution linearConvolution ->
                    ( "LinearConvolution"
                    , Json.Encode.object [ ( "sourceUrl", linearConvolution.sourceUrl |> Json.Encode.string ) ]
                    )

                AudioLowpass lowpass ->
                    ( "Lowpass"
                    , Json.Encode.object [ ( "cutoffFrequency", lowpass.cutoffFrequency |> audioParameterTimelineToJson ) ]
                    )

                AudioHighpass highpass ->
                    ( "highpasses"
                    , Json.Encode.object [ ( "cutoffFrequency", highpass.cutoffFrequency |> audioParameterTimelineToJson ) ]
                    )
            )


interfaceWithoutFutureDiffToJson : InterfaceWithoutFutureDiff -> Json.Encode.Value
interfaceWithoutFutureDiffToJson =
    \interfaceRemoveDiff ->
        tagValueToJson
            (case interfaceRemoveDiff of
                Add add ->
                    ( "Add"
                    , add |> interfaceSingleWithoutFutureToJson
                    )

                EditAudio audioEdit ->
                    ( "EditAudio"
                    , Json.Encode.object
                        [ ( "url", audioEdit.url |> Json.Encode.string )
                        , ( "startTime", audioEdit.startTime |> Time.posixToMillis |> Json.Encode.int )
                        , ( "replacement"
                          , tagValueToJson
                                (case audioEdit.replacement of
                                    ReplacementAudioSpeed new ->
                                        ( "Speed", new |> audioParameterTimelineToJson )

                                    ReplacementAudioVolume new ->
                                        ( "Volume", new |> audioParameterTimelineToJson )

                                    ReplacementAudioStereoPan new ->
                                        ( "StereoPan", new |> audioParameterTimelineToJson )

                                    ReplacementAudioProcessing new ->
                                        ( "Processing"
                                        , new |> Json.Encode.list audioProcessingToJson
                                        )
                                )
                          )
                        ]
                    )

                RemoveDom ->
                    ( "RemoveDom", Json.Encode.null )

                RemoveHttpRequest httpRequestId ->
                    ( "RemoveHttpRequest", httpRequestId |> httpRequestIdJsonCodec.toJson )

                RemoveAudio audioId ->
                    ( "RemoveAudio"
                    , Json.Encode.object
                        [ ( "url", audioId.url |> Json.Encode.string )
                        , ( "startTime", audioId.startTime |> Time.posixToMillis |> Json.Encode.int )
                        ]
                    )

                RemoveSocketConnect connect ->
                    ( "RemoveSocketConnect", Json.Encode.object [ ( "address", connect.address |> Json.Encode.string ) ] )

                RemoveListen listenId ->
                    ( "RemoveListen", listenId |> interfaceSingleListenIdJsonCodec.toJson )
            )


interfaceSingleWithoutFutureToJson : InterfaceSingleWithoutFuture -> Json.Encode.Value
interfaceSingleWithoutFutureToJson =
    \add ->
        tagValueToJson
            (case add of
                DocumentTitleReplaceBy replacement ->
                    ( "DocumentTitleReplaceBy", replacement |> Json.Encode.string )

                DocumentAuthorSet new ->
                    ( "DocumentAuthorSet", new |> Json.Encode.string )

                DocumentKeywordsSet new ->
                    ( "DocumentKeywordsSet", new |> String.join "," |> Json.Encode.string )

                DocumentDescriptionSet new ->
                    ( "DocumentDescriptionSet", new |> Json.Encode.string )

                ConsoleLog string ->
                    ( "ConsoleLog", string |> Json.Encode.string )

                ConsoleWarn string ->
                    ( "ConsoleWarn", string |> Json.Encode.string )

                ConsoleError string ->
                    ( "ConsoleError", string |> Json.Encode.string )

                NavigationPushUrl url ->
                    ( "NavigationPushUrl", url |> AppUrl.toString |> Json.Encode.string )

                NavigationReplaceUrl url ->
                    ( "NavigationReplaceUrl", url |> AppUrl.toString |> Json.Encode.string )

                NavigationGo urlSteps ->
                    ( "NavigationGo", urlSteps |> Json.Encode.int )

                NavigationLoad url ->
                    ( "NavigationLoad", url |> Url.toString |> Json.Encode.string )

                NavigationReload ->
                    ( "NavigationReload", Json.Encode.null )

                FileDownloadUnsignedInt8s config ->
                    ( "FileDownloadUnsignedInt8s"
                    , Json.Encode.object
                        [ ( "name", config.name |> Json.Encode.string )
                        , ( "mimeType", config.mimeType |> Json.Encode.string )
                        , ( "content"
                          , config.content |> Json.Encode.list Json.Encode.int
                          )
                        ]
                    )

                ClipboardReplaceBy replacement ->
                    ( "ClipboardReplaceBy"
                    , replacement |> Json.Encode.string
                    )

                AudioPlay audio ->
                    ( "Audio", audio |> audioToJson )

                SocketMessage message ->
                    ( "SocketMessage"
                    , Json.Encode.object
                        [ ( "id", message.id |> socketIdJsonCodec.toJson )
                        , ( "data", message.data |> Json.Encode.string )
                        ]
                    )

                SocketDisconnect id ->
                    ( "SocketDisconnect"
                    , id |> socketIdJsonCodec.toJson
                    )

                LocalStorageSet set ->
                    ( "LocalStorageSet"
                    , Json.Encode.object
                        [ ( "key", set.key |> Json.Encode.string )
                        , ( "value", set.value |> (Json.Codec.nullable Json.Codec.string).toJson )
                        ]
                    )
            )


audioToJson : Audio -> Json.Encode.Value
audioToJson audio =
    Json.Encode.object
        [ ( "url", audio.url |> Json.Encode.string )
        , ( "startTime", audio.startTime |> Time.posixToMillis |> Json.Encode.int )
        , ( "volume", audio.volume |> audioParameterTimelineToJson )
        , ( "speed", audio.speed |> audioParameterTimelineToJson )
        , ( "stereoPan", audio.stereoPan |> audioParameterTimelineToJson )
        , ( "processing"
          , audio.processingLastToFirst
                |> List.reverse
                |> Json.Encode.list audioProcessingToJson
          )
        ]


{-| The "init" part for an embedded program
-}
programInit : ProgramConfig state -> ( ProgramState state, Cmd (ProgramEvent state) )
programInit appConfig =
    let
        initialInterface : Emptiable (KeysSet (InterfaceSingle state) (InterfaceSingleKeys state) N1) Possibly
        initialInterface =
            appConfig.initialState
                |> appConfig.interface
                |> Rope.toList
                |> KeysSet.fromList interfaceKeys
    in
    ( State
        { interface = initialInterface
        , appState = appConfig.initialState
        }
    , { old = Emptiable.empty, updated = initialInterface }
        |> interfaceDiffs
        |> List.map (\diff -> appConfig.ports.toJs (diff |> interfaceDiffToJson))
        |> Cmd.batch
        |> Cmd.map never
    )


{-| The "subscriptions" part for an embedded program
-}
programSubscriptions : ProgramConfig state -> (ProgramState state -> Sub (ProgramEvent state))
programSubscriptions appConfig =
    \(State state) ->
        -- re-associate event based on current interface
        appConfig.ports.fromJs
            (\interfaceJson ->
                case interfaceJson |> Json.Decode.decodeValue (Json.Decode.field "diff" interfaceWithFutureDiffJsonCodec.jsonDecoder) of
                    Ok interfaceDiff ->
                        case
                            state.interface
                                |> KeysSet.toList interfaceKeys
                                |> listFirstJust
                                    (\stateInterface ->
                                        case stateInterface of
                                            InterfaceWithoutFuture _ ->
                                                Nothing

                                            InterfaceWithFuture withFuture ->
                                                interfaceFutureJsonDecoder interfaceDiff withFuture
                                    )
                        of
                            Just eventDataDecoderToConstructedEvent ->
                                case Json.Decode.decodeValue (Json.Decode.field "eventData" eventDataDecoderToConstructedEvent) interfaceJson of
                                    Ok appEvent ->
                                        appEvent |> AppEventToNewAppState

                                    Err eventDataJsonDecodeError ->
                                        eventDataJsonDecodeError |> InterfaceEventDataFailedToDecode

                            Nothing ->
                                InterfaceEventIgnored

                    Err interfaceDiffJsonDecodeError ->
                        interfaceDiffJsonDecodeError |> InterfaceDiffFailedToDecode
            )


interfaceFutureJsonDecoder : InterfaceWithFutureDiff -> InterfaceSingleWithFuture state -> Maybe (Json.Decode.Decoder state)
interfaceFutureJsonDecoder interfaceAddDiff interface =
    case interface of
        DomNodeRender domElementToRender ->
            case interfaceAddDiff of
                EditDom domEditDiff ->
                    (Json.Decode.map3 (\innerPath name event -> { innerPath = innerPath, name = name, event = event })
                        (Json.Decode.field "innerPath" (Json.Decode.list Json.Decode.int))
                        (Json.Decode.field "name" Json.Decode.string)
                        (Json.Decode.field "event" Json.Decode.value)
                        |> Json.Decode.andThen
                            (\specificEvent ->
                                case domElementToRender |> domElementAtReversePath ((specificEvent.innerPath ++ domEditDiff.path) |> List.reverse) of
                                    Nothing ->
                                        Json.Decode.fail "origin element of event not found"

                                    Just (DomText _) ->
                                        Json.Decode.fail "origin element of event leads to text, not element"

                                    Just (DomElement foundDomElement) ->
                                        case foundDomElement.eventListens |> Dict.get specificEvent.name of
                                            Nothing ->
                                                Json.Decode.fail "received event for element without listen"

                                            Just eventListen ->
                                                eventListen.on specificEvent.event |> Json.Decode.succeed
                            )
                    )
                        |> Just

                _ ->
                    Nothing

        AudioSourceLoad load ->
            case interfaceAddDiff of
                AddAudioSourceLoad loadedUrl ->
                    if loadedUrl == load.url then
                        Json.Decode.map load.on
                            (Json.Decode.oneOf
                                [ Json.Decode.map (\duration -> Ok { url = loadedUrl, duration = duration })
                                    (Json.Decode.field "ok"
                                        (Json.Decode.field "durationInSeconds"
                                            (Json.Decode.map Duration.seconds Json.Decode.float)
                                        )
                                    )
                                , Json.Decode.map
                                    (\errorMessage ->
                                        case errorMessage of
                                            "NetworkError" ->
                                                Err AudioSourceLoadNetworkError

                                            "MediaDecodeAudioDataUnknownContentType" ->
                                                Err AudioSourceLoadDecodeError

                                            "DOMException: The buffer passed to decodeAudioData contains an unknown content type." ->
                                                Err AudioSourceLoadDecodeError

                                            unknownMessage ->
                                                Err (AudioSourceLoadUnknownError unknownMessage)
                                    )
                                    (Json.Decode.field "err" Json.Decode.string)
                                ]
                            )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        SocketConnect connect ->
            case interfaceAddDiff of
                AddSocketConnect addConnect ->
                    if addConnect.address == connect.address then
                        socketConnectionEventJsonCodec.jsonDecoder
                            |> Json.Decode.map connect.on
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        Request request ->
            case interfaceAddDiff of
                AddRequest addRequestId ->
                    if (request |> interfaceSingleRequestToId) == addRequestId then
                        request |> requestFutureJsonDecoder |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        Listen listen ->
            case interfaceAddDiff of
                AddListen addListenId ->
                    if (listen |> interfaceSingleListenToId) == addListenId then
                        listen |> listenFutureJsonDecoder |> Just

                    else
                        Nothing

                _ ->
                    Nothing


socketConnectionEventJsonCodec : JsonCodec SocketConnectionEvent
socketConnectionEventJsonCodec =
    Json.Codec.choice
        (\socketConnected socketDisconnected socketConnectionEvent ->
            case socketConnectionEvent of
                SocketConnected id ->
                    socketConnected id

                SocketDisconnected disconnected ->
                    socketDisconnected disconnected
        )
        |> Json.Codec.variant ( SocketConnected, "SocketConnected" ) socketIdJsonCodec
        |> Json.Codec.variant ( SocketDisconnected, "SocketDisconnected" )
            (Json.Codec.record (\code reason -> { code = code, reason = reason })
                |> Json.Codec.field ( .code, "code" ) Json.Codec.int
                |> Json.Codec.field ( .reason, "reason" ) Json.Codec.string
                |> Json.Codec.recordFinish
            )


timePosixJsonCodec : JsonCodec Time.Posix
timePosixJsonCodec =
    Json.Codec.map Time.millisToPosix Time.posixToMillis Json.Codec.int


urlJsonDecoder : Json.Decode.Decoder Url
urlJsonDecoder =
    Json.Decode.andThen
        (\urlString ->
            case urlString |> Url.fromString of
                Nothing ->
                    "invalid URL" |> Json.Decode.fail

                Just url ->
                    url |> Json.Decode.succeed
        )
        Json.Decode.string


geoLocationJsonDecoder : Json.Decode.Decoder GeoLocation
geoLocationJsonDecoder =
    Json.Decode.map7
        (\latitudeInDecimalDegrees longitudeInDecimalDegrees latitudeLongitudeAccuracy altitudeAboveNominalSeaLevel altitudeAccuracy headingWith0AsTrueNorthAndIncreasingClockwise speed ->
            { latitudeInDecimalDegrees = latitudeInDecimalDegrees
            , longitudeInDecimalDegrees = longitudeInDecimalDegrees
            , latitudeLongitudeAccuracy = latitudeLongitudeAccuracy
            , altitudeAboveNominalSeaLevel = altitudeAboveNominalSeaLevel
            , altitudeAccuracy = altitudeAccuracy
            , headingWith0AsTrueNorthAndIncreasingClockwise = headingWith0AsTrueNorthAndIncreasingClockwise
            , speed = speed
            }
        )
        Json.Decode.float
        Json.Decode.float
        (Json.Decode.nullable (Json.Decode.map Length.meters Json.Decode.float))
        (Json.Decode.nullable (Json.Decode.map Length.meters Json.Decode.float))
        (Json.Decode.nullable (Json.Decode.map Length.meters Json.Decode.float))
        (Json.Decode.map
            (\maybeDegrees ->
                case maybeDegrees of
                    Nothing ->
                        Nothing

                    Just degrees ->
                        if degrees |> Basics.isNaN then
                            Nothing

                        else
                            Angle.degrees degrees |> Just
            )
            (Json.Decode.nullable Json.Decode.float)
        )
        (Json.Decode.nullable (Json.Decode.map Speed.metersPerSecond Json.Decode.float))


requestFutureJsonDecoder : InterfaceSingleRequest future -> Json.Decode.Decoder future
requestFutureJsonDecoder =
    \interfaceSingleRequest ->
        case interfaceSingleRequest of
            LocalStorageRequest request ->
                Json.Decode.nullable Json.Decode.string
                    |> Json.Decode.map request.on

            HttpRequest httpRequest ->
                Json.Decode.oneOf
                    [ Json.Decode.field "ok" (httpExpectJsonDecoder httpRequest.expect)
                    , Json.Decode.field "err" (httpErrorJsonDecoder httpRequest)
                        |> Json.Decode.map (httpExpectOnError httpRequest.expect)
                    ]

            WindowSizeRequest toFuture ->
                Json.Decode.map2 (\width height -> toFuture { width = width, height = height })
                    (Json.Decode.field "width" Json.Decode.int)
                    (Json.Decode.field "height" Json.Decode.int)

            NavigationUrlRequest toFuture ->
                urlJsonDecoder
                    |> Json.Decode.map (\url -> url |> AppUrl.fromUrl |> toFuture)

            ClipboardRequest toFuture ->
                Json.Decode.map toFuture Json.Decode.string

            TimePosixRequest requestTimeNow ->
                Json.Decode.map requestTimeNow timePosixJsonCodec.jsonDecoder

            TimezoneOffsetRequest requestTimezoneOffset ->
                Json.Decode.map requestTimezoneOffset Json.Decode.int

            TimezoneNameRequest requestTimezoneName ->
                Json.Decode.map requestTimezoneName
                    (Json.Decode.oneOf
                        [ Json.Decode.map Time.Offset Json.Decode.int
                        , Json.Decode.map Time.Name Json.Decode.string
                        ]
                    )

            RandomUnsignedInt32sRequest randomUnsignedInt32sRequest ->
                Json.Decode.map randomUnsignedInt32sRequest.on
                    (Json.Decode.list Json.Decode.int)

            GeoLocationRequest request ->
                Json.Decode.map request geoLocationJsonDecoder


httpExpectOnError : HttpExpect future -> (HttpError -> future)
httpExpectOnError =
    \httpExpect ->
        case httpExpect of
            HttpExpectString toFuture ->
                \e -> e |> Err |> toFuture

            HttpExpectBytes toFuture ->
                \e -> e |> Err |> toFuture

            HttpExpectWhatever toFuture ->
                \e -> e |> Err |> toFuture


httpExpectJsonDecoder : HttpExpect future -> Json.Decode.Decoder future
httpExpectJsonDecoder expect =
    httpMetadataJsonDecoder
        |> Json.Decode.andThen
            (\meta ->
                let
                    isOk : Bool
                    isOk =
                        meta.statusCode >= 200 && meta.statusCode < 300

                    badStatusJsonDecoder : Json.Decode.Decoder (Result HttpError value_)
                    badStatusJsonDecoder =
                        Json.Decode.map (\body -> Err (HttpBadStatus { metadata = meta, body = body })) Json.Decode.value
                in
                Json.Decode.field "body"
                    (case expect of
                        HttpExpectString toFuture ->
                            Json.Decode.map toFuture
                                (if isOk then
                                    Json.Decode.map Ok Json.Decode.string

                                 else
                                    badStatusJsonDecoder
                                )

                        HttpExpectBytes toFuture ->
                            Json.Decode.map toFuture
                                (if isOk then
                                    Json.Decode.map Ok
                                        (Json.Decode.map Bytes.LocalExtra.fromUnsignedInt8List
                                            (Json.Decode.list Json.Decode.int)
                                        )

                                 else
                                    badStatusJsonDecoder
                                )

                        HttpExpectWhatever toFuture ->
                            Json.Decode.map toFuture
                                (if isOk then
                                    Json.Decode.succeed (Ok ())

                                 else
                                    badStatusJsonDecoder
                                )
                    )
            )


httpMetadataJsonDecoder : Json.Decode.Decoder HttpMetadata
httpMetadataJsonDecoder =
    Json.Decode.map4
        (\url statusCode statusText headers ->
            { url = url
            , statusCode = statusCode
            , statusText = statusText
            , headers = headers
            }
        )
        (Json.Decode.field "url" Json.Decode.string)
        (Json.Decode.field "statusCode" Json.Decode.int)
        (Json.Decode.field "statusText" Json.Decode.string)
        (Json.Decode.field "headers" (Json.Decode.dict Json.Decode.string))


httpErrorJsonDecoder : HttpRequest future_ -> Json.Decode.Decoder HttpError
httpErrorJsonDecoder httpRequest =
    Json.Decode.field "cause" (Json.Decode.field "code" Json.Decode.string)
        |> Json.Decode.andThen
            (\code ->
                if httpNetworkErrorCodes |> Set.member code then
                    Json.Decode.succeed HttpNetworkError

                else
                    case code of
                        "BAD_URL" ->
                            Json.Decode.succeed (HttpBadUrl httpRequest.url)

                        _ ->
                            Json.Decode.field "name" Json.Decode.string
                                |> Json.Decode.andThen
                                    (\name ->
                                        case name of
                                            "AbortError" ->
                                                Json.Decode.succeed HttpTimeout

                                            _ ->
                                                Json.Decode.value
                                                    |> Json.Decode.andThen
                                                        (\errorValue ->
                                                            Json.Decode.fail
                                                                ([ "Unknown HTTP fetch error: "
                                                                 , errorValue |> Json.Encode.encode 0
                                                                 , ". consider submitting an issue for adding it as an explicit case to https://github.com/lue-bird/elm-state-interface/"
                                                                 ]
                                                                    |> String.concat
                                                                )
                                                        )
                                    )
            )


httpNetworkErrorCodes : Set String
httpNetworkErrorCodes =
    Set.fromList [ "EAGAIN", "ECONNRESET", "ECONNREFUSED", "ENOTFOUND", "UND_ERR", "UND_ERR_CONNECT_TIMEOUT", "UND_ERR_HEADERS_OVERFLOW", "UND_ERR_BODY_TIMEOUT", "UND_ERR_RESPONSE_STATUS_CODE", "UND_ERR_INVALID_ARG", "UND_ERR_INVALID_RETURN_VALUE", "UND_ERR_ABORTED", "UND_ERR_DESTROYED", "UND_ERR_CLOSED", "UND_ERR_SOCKET", "UND_ERR_NOT_SUPPORTED", "UND_ERR_REQ_CONTENT_LENGTH_MISMATCH", "UND_ERR_RES_CONTENT_LENGTH_MISMATCH", "UND_ERR_INFO", "UND_ERR_RES_EXCEEDED_MAX_SIZE" ]


listenFutureJsonDecoder : InterfaceSingleListen future -> Json.Decode.Decoder future
listenFutureJsonDecoder interfaceSingleListen =
    case interfaceSingleListen of
        WindowEventListen listen ->
            listen.on

        WindowVisibilityChangeListen toFuture ->
            windowVisibilityCodec.jsonDecoder
                |> Json.Decode.map toFuture

        WindowAnimationFrameListen toFuture ->
            timePosixJsonCodec.jsonDecoder
                |> Json.Decode.map toFuture

        DocumentEventListen listen ->
            listen.on

        TimePeriodicallyListen timePeriodicallyListen ->
            timePosixJsonCodec.jsonDecoder
                |> Json.Decode.map timePeriodicallyListen.on

        LocalStorageSetOnADifferentTabListen listen ->
            Json.Decode.map3
                (\appUrl oldValue newValue ->
                    { appUrl = appUrl, oldValue = oldValue, newValue = newValue }
                )
                (Json.Decode.field "url"
                    (urlJsonDecoder |> Json.Decode.map AppUrl.fromUrl)
                )
                (Json.Decode.field "oldValue" (Json.Decode.nullable Json.Decode.string))
                (Json.Decode.field "newValue" Json.Decode.string)
                |> Json.Decode.map listen.on

        LocalStorageRemoveOnADifferentTabListen listen ->
            urlJsonDecoder
                |> Json.Decode.map (\url -> url |> AppUrl.fromUrl |> listen.on)

        SocketMessageListen messageListen ->
            Json.Decode.string
                |> Json.Decode.map messageListen.on

        GeoLocationListen toFuture ->
            geoLocationJsonDecoder |> Json.Decode.map toFuture


windowVisibilityCodec : JsonCodec WindowVisibility
windowVisibilityCodec =
    Json.Codec.enum [ WindowShown, WindowHidden ]
        (\windowVisibility ->
            case windowVisibility of
                WindowShown ->
                    "visible"

                WindowHidden ->
                    "hidden"
        )


domElementAtReversePath : List Int -> (DomNode future -> Maybe (DomNode future))
domElementAtReversePath path domNode =
    case path of
        [] ->
            Just domNode

        subIndex :: parentsOfSub ->
            case domNode of
                DomText _ ->
                    Nothing

                DomElement domElement ->
                    case Array.get subIndex domElement.subs of
                        Nothing ->
                            Nothing

                        Just subNodeAtIndex ->
                            domElementAtReversePath parentsOfSub subNodeAtIndex


domElementIdJsonCodec : JsonCodec DomElementId
domElementIdJsonCodec =
    Json.Codec.record
        (\namespace tag styles attributes attributesNamespaced eventListens subs ->
            { namespace = namespace
            , tag = tag
            , styles = styles
            , attributes = attributes
            , attributesNamespaced = attributesNamespaced
            , eventListens = eventListens
            , subs = subs
            }
        )
        |> Json.Codec.field ( .namespace, "namespace" ) (Json.Codec.nullable Json.Codec.string)
        |> Json.Codec.field ( .tag, "tag" ) Json.Codec.string
        |> Json.Codec.field ( .styles, "styles" ) domElementStylesJsonCodec
        |> Json.Codec.field ( .attributes, "attributes" ) domElementAttributesJsonCodec
        |> Json.Codec.field ( .attributesNamespaced, "attributesNamespaced" ) domElementAttributesNamespacedJsonCodec
        |> Json.Codec.field ( .eventListens, "eventListens" )
            domElementEventListensJsonCodec
        |> Json.Codec.field ( .subs, "subs" ) (Json.Codec.array domNodeIdJsonCodec)
        |> Json.Codec.recordFinish


{-| The "update" part for an embedded program
-}
programUpdate : ProgramConfig state -> (ProgramEvent state -> ProgramState state -> ( ProgramState state, Cmd (ProgramEvent state) ))
programUpdate appConfig =
    \event ->
        case event of
            InterfaceEventIgnored ->
                \state ->
                    ( state, Cmd.none )

            InterfaceDiffFailedToDecode jsonError ->
                \state ->
                    ( state
                    , ("bug in lue-bird/elm-state-interface: interface diff failed to decode: "
                        ++ (jsonError |> Json.Decode.errorToString)
                      )
                        |> ConsoleError
                        |> Add
                        |> InterfaceWithoutFutureDiff
                        |> interfaceDiffToJson
                        |> appConfig.ports.toJs
                        |> Cmd.map never
                    )

            InterfaceEventDataFailedToDecode jsonError ->
                \state ->
                    ( state
                    , ("bug in lue-bird/elm-state-interface: interface event data failed to decode: "
                        ++ (jsonError |> Json.Decode.errorToString)
                      )
                        |> ConsoleError
                        |> Add
                        |> InterfaceWithoutFutureDiff
                        |> interfaceDiffToJson
                        |> appConfig.ports.toJs
                        |> Cmd.map never
                    )

            AppEventToNewAppState updatedAppState ->
                \(State oldState) ->
                    let
                        updatedInterface : Emptiable (KeysSet (InterfaceSingle state) (InterfaceSingleKeys state) N1) Possibly
                        updatedInterface =
                            updatedAppState
                                |> appConfig.interface
                                |> Rope.toList
                                |> KeysSet.fromList interfaceKeys
                    in
                    ( State { interface = updatedInterface, appState = updatedAppState }
                    , { old = oldState.interface, updated = updatedInterface }
                        |> interfaceDiffs
                        |> List.map (\diff -> appConfig.ports.toJs (diff |> interfaceDiffToJson))
                        |> Cmd.batch
                        |> Cmd.map never
                    )


{-| A Request can fail in a couple ways:

  - `BadUrl` means you did not provide a valid URL.
  - `Timeout` means it took too long to get a response.
  - `NetworkError` means the user turned off their wifi, went in a cave, etc.
  - `BadStatus` means you got a response back, but the status code indicates failure. Contains:
      - The response `Metadata`.
      - The raw response body as a `Json.Decode.Value`.
  - `BadBody` means you got a response back with a nice status code, but the body of the response was something unexpected. Contains:
      - The response `Metadata`.
      - The raw response body as a `Json.Decode.Value`.
      - The `Json.Decode.Error` that caused the error.

-}
type HttpError
    = HttpBadUrl String
    | HttpTimeout
    | HttpNetworkError
    | HttpBadStatus { metadata : HttpMetadata, body : Json.Decode.Value }


{-| Extra information about the response:

  - url of the server that actually responded (so you can detect redirects)
  - statusCode like 200 or 404
  - statusText describing what the statusCode means a little
  - headers like Content-Length and Expires

Note: It is possible for a response to have the same header multiple times.
In that case, all the values end up in a single entry in the headers dictionary.
The values are separated by commas, following the rules outlined [here](https://stackoverflow.com/questions/4371328/are-duplicate-http-response-headers-acceptable).

-}
type alias HttpMetadata =
    RecordWithoutConstructorFunction
        { url : String
        , statusCode : Int
        , statusText : String
        , headers : Dict String String
        }


{-| Safe to ignore. Individual messages to js. Also used to identify responses with the same part in the interface
-}
type InterfaceDiff
    = InterfaceWithFutureDiff InterfaceWithFutureDiff
    | InterfaceWithoutFutureDiff InterfaceWithoutFutureDiff


{-| Actions that will never notify elm again
-}
type InterfaceWithoutFutureDiff
    = Add InterfaceSingleWithoutFuture
    | EditAudio { url : String, startTime : Time.Posix, replacement : EditAudioDiff }
    | RemoveHttpRequest HttpRequestId
    | RemoveDom
    | RemoveSocketConnect { address : String }
    | RemoveAudio { url : String, startTime : Time.Posix }
    | RemoveListen InterfaceSingleListenId


{-| Actions that will notify elm some time in the future
-}
type InterfaceWithFutureDiff
    = EditDom EditDomDiff
    | AddSocketConnect { address : String }
    | AddAudioSourceLoad String
    | AddRequest InterfaceSingleRequestId
    | AddListen InterfaceSingleListenId


{-| What parts of an [`Audio`](#Audio) are replaced
-}
type EditAudioDiff
    = ReplacementAudioVolume AudioParameterTimeline
    | ReplacementAudioSpeed AudioParameterTimeline
    | ReplacementAudioStereoPan AudioParameterTimeline
    | ReplacementAudioProcessing (List AudioProcessing)


{-| Some kind of sound we want to play. To create `Audio`, start with [`Web.Audio.fromSource`](Web-Audio#fromSource)
-}
type alias Audio =
    RecordWithoutConstructorFunction
        { url : String
        , startTime : Time.Posix
        , volume : AudioParameterTimeline
        , speed : AudioParameterTimeline
        , stereoPan : AudioParameterTimeline
        , processingLastToFirst : List AudioProcessing
        }


{-| A single effect filter applied to an [`Audio`](#Audio)
-}
type AudioProcessing
    = AudioLinearConvolution { sourceUrl : String }
    | AudioLowpass { cutoffFrequency : AudioParameterTimeline }
    | AudioHighpass { cutoffFrequency : AudioParameterTimeline }


{-| Audio data we can use to play sounds.
Use [`Web.Audio.sourceLoad`](Web-Audio#sourceLoad) to fetch an [`AudioSource`](#AudioSource).

You can also use the contained source `duration`, for example to find fade-out times or to create a loop:

    audioLoop : AudioSource -> Time.Posix -> Time.Posix -> Audio
    audioLoop source initialTime lastTick =
        Web.Audio.fromSource source
            (Duration.addTo
                initialTime
                (source.duration
                    |> Quantity.multiplyBy
                        (((Duration.from initialTime lastTick |> Duration.inSeconds)
                            / (source.duration |> Duration.inSeconds)
                         )
                            |> floor
                            |> toFloat
                        )
                )
            )

-}
type alias AudioSource =
    RecordWithoutConstructorFunction
        { url : String
        , duration : Duration
        }


{-| defining how loud a sound should be at any point in time
-}
type alias AudioParameterTimeline =
    { startValue : Float
    , keyFrames : List { time : Time.Posix, value : Float }
    }


{-| Change the current node at a given path using a given [`ReplacementInEditDomDiff`](#ReplacementInEditDomDiff)
-}
type alias EditDomDiff =
    RecordWithoutConstructorFunction
        { path : List Int, replacement : ReplacementInEditDomDiff }


{-| What parts of a node are replaced. Either all modifiers of a certain kind or the whole node
-}
type ReplacementInEditDomDiff
    = ReplacementDomNode DomNodeId
    | ReplacementDomElementStyles (Dict String String)
    | ReplacementDomElementAttributes (Dict String String)
    | ReplacementDomElementAttributesNamespaced (Dict ( String, String ) String)
    | ReplacementDomElementEventListens (Dict String DefaultActionHandling)


{-| Create an elm [`Program`](https://dark.elm.dmy.fr/packages/elm/core/latest/Platform#Program)
with a given [`Web.ProgramConfig`](#ProgramConfig).
-}
program : ProgramConfig state -> Program state
program appConfig =
    Platform.worker
        { init = \() -> programInit appConfig
        , update = programUpdate appConfig
        , subscriptions = programSubscriptions appConfig
        }


{-| A [`Platform.Program`](https://dark.elm.dmy.fr/packages/elm/core/latest/Platform#Program)
that elm can run,
produced by [`Web.program`](#program)
-}
type alias Program state =
    Platform.Program () (ProgramState state) (ProgramEvent state)
