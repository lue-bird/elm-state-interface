module Web exposing
    ( program, Program
    , Interface, interfaceBatch, interfaceNone, interfaceFutureMap
    , DomElementHeader, DomElementVisibilityAlignment(..), DefaultActionHandling(..)
    , Audio, AudioSource, AudioSourceLoadError(..), AudioProcessing(..), AudioParameterTimeline
    , HttpRequest, HttpBody(..), HttpExpect(..), HttpError(..), HttpMetadata
    , SocketConnectionEvent(..), SocketId(..)
    , GeoLocation
    , Gamepad, GamepadButton(..)
    , NotificationClicked(..)
    , WindowVisibility(..)
    , ProgramConfig, programInit, programUpdate, programSubscriptions
    , ProgramState(..), ProgramEvent(..), InterfaceSingle(..), DomTextOrElementHeader(..)
    , interfaceSingleEdits, InterfaceSingleEdit(..), AudioEdit(..), DomEdit(..)
    )

{-| A state-interface program that can run in the browser

@docs program, Program

You can also [embed](#embed) a state-interface program as part of an existing app that uses The Elm Architecture.


# interface

@docs Interface, interfaceBatch, interfaceNone, interfaceFutureMap

That's it. Everything else below is types used in the other modules
like [`Web.Time`](Web-Time), [`Web.Dom`](Web-Dom), [`Web.Http`](Web-Http) etc.
Leave them be and look at those modules :)


## DOM

Types used by [`Web.Dom`](Web-Dom)

@docs DomElementHeader, DomElementVisibilityAlignment, DefaultActionHandling


## Audio

Types used by [`Web.Audio`](Web-Audio)

@docs Audio, AudioSource, AudioSourceLoadError, AudioProcessing, AudioParameterTimeline


## HTTP

Types used by [`Web.Http`](Web-Http)

@docs HttpRequest, HttpBody, HttpExpect, HttpError, HttpMetadata


## socket

Types used by [`Web.Socket`](Web-Socket)

@docs SocketConnectionEvent, SocketId


## geo location

Types used by [`Web.GeoLocation`](Web-GeoLocation)

@docs GeoLocation


## gamepads

Types used by [`Web.Gamepads`](Web-Gamepads)

@docs Gamepad, GamepadButton


## notification

Types used by [`Web.Notification`](Web-Notification)

@docs NotificationClicked


## window

Types used by [`Web.Window`](Web-Window)

@docs WindowVisibility


## embed

If you just want to replace a part of your elm app with this architecture. Make sure to wire in all 3:

@docs ProgramConfig, programInit, programUpdate, programSubscriptions

Under the hood, [`Web.program`](Web#program) is then defined as just

    program config =
        Platform.worker
            { init = \() -> Web.programInit yourAppConfig
            , update = Web.programUpdate yourAppConfig
            , subscriptions = Web.programSubscriptions yourAppConfig
            }


## internals, safe to ignore for users

Exposed so can for example simulate it more easily in tests, add a debugger etc.

@docs ProgramState, ProgramEvent, InterfaceSingle, DomTextOrElementHeader

To simulate diffing of interfaces, use

    FastDict.merge
        (\id toAdd -> ..add toAdd..)
        (\id old updated -> ..edit (interfaceSingleEdits { old, updated })..)
        (id toRemove -> ..remove toRemove..)
        updatedInterfaces
        oldInterfaces
        ..what you fold into..

see [`FastDict.merge`](https://dark.elm.dmy.fr/packages/miniBill/elm-fast-dict/latest/FastDict#merge)

@docs interfaceSingleEdits, InterfaceSingleEdit, AudioEdit, DomEdit

If you need more things like json encoders/decoders, [open an issue](https://github.com/lue-bird/elm-state-interface/issues/new)

-}

import Angle exposing (Angle)
import AppUrl exposing (AppUrl)
import AppUrl.LocalExtra
import Bytes exposing (Bytes)
import Bytes.LocalExtra
import Dict exposing (Dict)
import Duration exposing (Duration)
import FastDict
import Json.Decode
import Json.Decode.LocalExtra
import Json.Encode
import Json.Encode.LocalExtra
import Length exposing (Length)
import List.LocalExtra
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Result.LocalExtra
import Rope exposing (Rope)
import Set exposing (Set)
import Speed exposing (Speed)
import StructuredId exposing (StructuredId)
import Time
import Time.LocalExtra
import Url.LocalExtra



{- Hey! Here's how to add a new interface:


     - choose a name with roughly the shape `DomainSubjectVerb`

     - to `Web.InterfaceSingle`, add a variant `| [YourName] ..additional info (and future handles)..`

     - inside `Web.interfaceSingleFutureJsonDecoder`, specify what js values you expect to decode

     - inside `Web.interfaceSingleToStructuredId`, assign a unique identifier to your interface

       This helps recognize when interfaces have been added, have changed or have been deleted.
       Unless you want to allow your interface to be edited while it's running,
       it usually contains all the info from the `DomainSubjectVerb` variant (not including functions)

     - in `runner/index.ts` inside `interfaceAddImplementation`, add
       ```javascript
       case "[YourName]": return (yourInput) => {
           // perform your stuff

           // in case you want to send something to elm, use
           sendToElm({- your value -})

           // in case you want to do something once the interface gets removed,
           // either use the `abortSignal` directly or add
           abortSignal.addEventListener("abort", _event => {
               // remove your stuff
           })
       }
       ```

   Sometimes, removing + adding the new interface would not be the same as editing the existing one or would at least perform worse.
   For example, changing the volume of an audio should not require removing and and re-adding all audio nodes.

   If you also want to enable editing a running interface:

    - to `Web.InterfaceSingleEdit`, add a variant `| Edit[YourName] ..your diff info..`
    - inside `Web.interfaceSingleEdits`, add a case `( [YourName] old, [YourName] new ) -> Edit[YourName] ..the diff..`
    - in `runner/index.ts` inside `interfaceEditImplementation`, add a `case "Edit[YourName]" : return (yourInput) => { ... }`
-}


{-| The "model" in a [`Web.program`](#program)
-}
type ProgramState appState
    = State
        { interface : FastDict.Dict String (InterfaceSingle appState)
        , appState : appState
        }


{-| What's needed to create a state-interface [`program`](#program)
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

To combine multiple, use [`Web.interfaceBatch`](#interfaceBatch) and [`Web.interfaceNone`](#interfaceNone).
To change the value that comes back in the future, use [`Web.interfaceFutureMap`](Web#interfaceFutureMap)

-}
type alias Interface future =
    Rope (InterfaceSingle future)


{-| A "non-batched" [`Interface`](#Interface).
To create one, use the helpers in [`Web.Time`](Web-Time), [`Web.Dom`](Web-Dom), [`Web.Http`](Web-Http) etc.
-}
type InterfaceSingle future
    = DocumentTitleReplaceBy String
    | DocumentAuthorSet String
    | DocumentKeywordsSet (List String)
    | DocumentDescriptionSet String
    | DocumentEventListen { eventName : String, on : Json.Decode.Decoder future }
    | ConsoleLog String
    | ConsoleWarn String
    | ConsoleError String
    | NavigationReplaceUrl AppUrl
    | NavigationPushUrl AppUrl
    | NavigationGo Int
    | NavigationLoad String
    | NavigationReload
    | NavigationUrlRequest (AppUrl -> future)
    | FileDownloadUnsignedInt8s { mimeType : String, name : String, content : List Int }
    | ClipboardReplaceBy String
    | ClipboardRequest (String -> future)
    | AudioSourceLoad { url : String, on : Result AudioSourceLoadError AudioSource -> future }
    | AudioPlay Audio
    | DomNodeRender { path : List Int, node : DomTextOrElementHeader future }
    | NotificationAskForPermission
    | NotificationShow { id : String, message : String, details : String, on : NotificationClicked -> future }
    | HttpRequest (HttpRequest future)
    | TimePosixRequest (Time.Posix -> future)
    | TimezoneOffsetRequest (Int -> future)
    | TimeOnce { pointInTime : Time.Posix, on : Time.Posix -> future }
    | TimePeriodicallyListen { intervalDurationMilliSeconds : Int, on : Time.Posix -> future }
    | TimezoneNameRequest (String -> future)
    | RandomUnsignedInt32sRequest { count : Int, on : List Int -> future }
    | WindowSizeRequest ({ width : Int, height : Int } -> future)
    | WindowPreferredLanguagesRequest (List String -> future)
    | WindowEventListen { eventName : String, on : Json.Decode.Decoder future }
    | WindowVisibilityChangeListen (WindowVisibility -> future)
    | WindowAnimationFrameListen (Time.Posix -> future)
    | WindowPreferredLanguagesChangeListen (List String -> future)
    | SocketConnect { address : String, on : SocketConnectionEvent -> future }
    | SocketMessage { id : SocketId, data : String }
    | SocketMessageListen { id : SocketId, on : String -> future }
    | LocalStorageSet { key : String, value : Maybe String }
    | LocalStorageRequest { key : String, on : Maybe String -> future }
    | LocalStorageRemoveOnADifferentTabListen { key : String, on : AppUrl -> future }
    | LocalStorageSetOnADifferentTabListen
        { key : String
        , on : { appUrl : AppUrl, oldValue : Maybe String, newValue : String } -> future
        }
    | GeoLocationRequest (GeoLocation -> future)
    | GeoLocationChangeListen (GeoLocation -> future)
    | GamepadsRequest (Dict Int Gamepad -> future)
    | GamepadsChangeListen (Dict Int Gamepad -> future)


{-| These are possible errors we can get when loading an audio source file.

  - `AudioSourceLoadDecodeError`: This means we got the data but we couldn't decode it. One likely reason for this is that your url points to the wrong place and you're trying to decode a 404 page instead.
  - `AudioSourceLoadNetworkError`: We couldn't reach the url. Either it's some kind of CORS issue, the server is down, or you're disconnected from the internet.

-}
type AudioSourceLoadError
    = AudioSourceLoadDecodeError
    | AudioSourceLoadNetworkError


{-| The user clicked a displayed notification,
moving the focus to our page
-}
type NotificationClicked
    = NotificationClicked


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


{-| The visibility to the user

  - `WindowHidden`: the user has navigated to a new page, switched tabs, closed the tab, minimized or closed the browser, or, on mobile, switched from the browser to a different app
  - `WindowShown` otherwise

-}
type WindowVisibility
    = WindowShown
    | WindowHidden


{-| An HTTP request for use in an [`Interface`](#Interface).

Use [`Web.Http.addHeaders`](Web-Http#addHeaders) to set custom headers as needed.
Use [`Web.Time.onceAt`](Web-Time#onceAt) to add a timeout of how long you are willing to wait before giving up.

-}
type alias HttpRequest future =
    RecordWithoutConstructorFunction
        { url : String
        , method : String
        , headers : List { name : String, value : String }
        , body : HttpBody
        , expect : HttpExpect future
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


{-| Plain text or a [`DomElementHeader`](#DomElementHeader) for use in an [`Interface`](#Interface).
-}
type DomTextOrElementHeader future
    = DomText String
    | DomElementHeader (DomElementHeader future)


{-| Everything about a [tagged DOM element](Web-Dom#Element)
except potential sub-[node](Web-Dom#Node)s
-}
type alias DomElementHeader future =
    RecordWithoutConstructorFunction
        { namespace : Maybe String
        , tag : String
        , styles : Dict String String
        , attributes : Dict String String
        , attributesNamespaced : Dict ( String, String ) String
        , stringProperties : Dict String String
        , boolProperties : Dict String Bool
        , scrollToPosition : Maybe { fromLeft : Float, fromTop : Float }
        , scrollToShow : Maybe { x : DomElementVisibilityAlignment, y : DomElementVisibilityAlignment }
        , scrollPositionRequest : Maybe ({ fromLeft : Float, fromTop : Float } -> future)
        , eventListens :
            Dict
                String
                { on : Json.Decode.Value -> future
                , defaultActionHandling : DefaultActionHandling
                }
        }


{-| What part of the [`Web.Dom.Element`](Web-Dom#Element) should be visible

  - `DomElementStart`: mostly for text to read
  - `DomElementEnd`: mostly for text to write
  - `DomElementCenter`: mostly for images

-}
type DomElementVisibilityAlignment
    = DomElementStart
    | DomElementEnd
    | DomElementCenter


{-| Setting for a listen [`Web.Dom.Modifier`](Web-Dom#Modifier)
to keep or overwrite the browser's default action
-}
type DefaultActionHandling
    = DefaultActionPrevent
    | DefaultActionExecute


{-| Identifier for a [`Web.Socket`](Web-Socket) that can be used to [communicate](Web-Socket#communicate)
-}
type SocketId
    = SocketId Int


{-| Combine multiple [`Interface`](#Interface)s into one
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


interfaceSingleFutureMap : (future -> mappedFuture) -> (InterfaceSingle future -> InterfaceSingle mappedFuture)
interfaceSingleFutureMap futureChange =
    \interface ->
        case interface of
            DocumentTitleReplaceBy title ->
                DocumentTitleReplaceBy title

            DocumentAuthorSet author ->
                DocumentAuthorSet author

            DocumentKeywordsSet keywords ->
                DocumentKeywordsSet keywords

            DocumentDescriptionSet description ->
                DocumentDescriptionSet description

            ConsoleLog message ->
                ConsoleLog message

            ConsoleWarn message ->
                ConsoleWarn message

            ConsoleError message ->
                ConsoleError message

            NavigationReplaceUrl appUrl ->
                NavigationReplaceUrl appUrl

            NavigationPushUrl appUrl ->
                NavigationPushUrl appUrl

            NavigationGo urlStepCount ->
                NavigationGo urlStepCount

            NavigationLoad url ->
                NavigationLoad url

            NavigationReload ->
                NavigationReload

            FileDownloadUnsignedInt8s download ->
                FileDownloadUnsignedInt8s download

            ClipboardReplaceBy clipboard ->
                ClipboardReplaceBy clipboard

            AudioPlay audio ->
                AudioPlay audio

            SocketMessage socketMessage ->
                SocketMessage socketMessage

            LocalStorageSet localStorageItem ->
                LocalStorageSet localStorageItem

            NotificationAskForPermission ->
                NotificationAskForPermission

            DomNodeRender toRender ->
                { path = toRender.path, node = toRender.node |> domNodeFutureMap futureChange }
                    |> DomNodeRender

            AudioSourceLoad load ->
                { url = load.url, on = \event -> load.on event |> futureChange }
                    |> AudioSourceLoad

            SocketConnect connect ->
                { address = connect.address, on = \event -> event |> connect.on |> futureChange }
                    |> SocketConnect

            NotificationShow show ->
                { id = show.id
                , message = show.message
                , details = show.details
                , on = \future -> future |> show.on |> futureChange
                }
                    |> NotificationShow

            HttpRequest httpRequest ->
                httpRequest |> httpRequestFutureMap futureChange |> HttpRequest

            LocalStorageRequest request ->
                { key = request.key, on = \event -> event |> request.on |> futureChange }
                    |> LocalStorageRequest

            WindowSizeRequest toFuture ->
                (\event -> toFuture event |> futureChange) |> WindowSizeRequest

            WindowPreferredLanguagesRequest toFuture ->
                (\event -> toFuture event |> futureChange) |> WindowPreferredLanguagesRequest

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

            TimeOnce once ->
                { pointInTime = once.pointInTime
                , on = \event -> event |> once.on |> futureChange
                }
                    |> TimeOnce

            RandomUnsignedInt32sRequest randomUnsignedInt32sRequest ->
                { count = randomUnsignedInt32sRequest.count
                , on = \ints -> randomUnsignedInt32sRequest.on ints |> futureChange
                }
                    |> RandomUnsignedInt32sRequest

            GeoLocationRequest toFuture ->
                (\event -> event |> toFuture |> futureChange) |> GeoLocationRequest

            GamepadsRequest toFuture ->
                (\event -> event |> toFuture |> futureChange) |> GamepadsRequest

            WindowEventListen listen ->
                { eventName = listen.eventName, on = listen.on |> Json.Decode.map futureChange }
                    |> WindowEventListen

            WindowVisibilityChangeListen toFuture ->
                (\event -> toFuture event |> futureChange) |> WindowVisibilityChangeListen

            WindowAnimationFrameListen toFuture ->
                (\event -> toFuture event |> futureChange) |> WindowAnimationFrameListen

            WindowPreferredLanguagesChangeListen toFuture ->
                (\event -> toFuture event |> futureChange) |> WindowPreferredLanguagesChangeListen

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

            GeoLocationChangeListen toFuture ->
                (\event -> event |> toFuture |> futureChange) |> GeoLocationChangeListen

            GamepadsChangeListen toFuture ->
                (\event -> event |> toFuture |> futureChange) |> GamepadsChangeListen


domElementHeaderFutureMap : (future -> mappedFuture) -> (DomElementHeader future -> DomElementHeader mappedFuture)
domElementHeaderFutureMap futureChange =
    \domElementToMap ->
        { namespace = domElementToMap.namespace
        , tag = domElementToMap.tag
        , styles = domElementToMap.styles
        , attributes = domElementToMap.attributes
        , attributesNamespaced = domElementToMap.attributesNamespaced
        , stringProperties = domElementToMap.stringProperties
        , boolProperties = domElementToMap.boolProperties
        , scrollToPosition = domElementToMap.scrollToPosition
        , scrollToShow = domElementToMap.scrollToShow
        , scrollPositionRequest =
            domElementToMap.scrollPositionRequest
                |> Maybe.map (\request position -> position |> request |> futureChange)
        , eventListens =
            domElementToMap.eventListens
                |> Dict.map
                    (\_ listen ->
                        { on = \event -> listen.on event |> futureChange
                        , defaultActionHandling = listen.defaultActionHandling
                        }
                    )
        }


domNodeFutureMap : (future -> mappedFuture) -> (DomTextOrElementHeader future -> DomTextOrElementHeader mappedFuture)
domNodeFutureMap futureChange =
    \domElementToMap ->
        case domElementToMap of
            DomText text ->
                DomText text

            DomElementHeader domElement ->
                domElement |> domElementHeaderFutureMap futureChange |> DomElementHeader


httpRequestFutureMap : (future -> mappedFuture) -> (HttpRequest future -> HttpRequest mappedFuture)
httpRequestFutureMap futureChange =
    \httpRequest ->
        { url = httpRequest.url
        , method = httpRequest.method
        , headers = httpRequest.headers
        , body = httpRequest.body
        , expect =
            case httpRequest.expect of
                HttpExpectWhatever expectWhatever ->
                    (\unit -> expectWhatever unit |> futureChange) |> HttpExpectWhatever

                HttpExpectString expectString ->
                    (\string -> expectString string |> futureChange) |> HttpExpectString

                HttpExpectBytes expectBytes ->
                    (\bytes -> expectBytes bytes |> futureChange) |> HttpExpectBytes
        }


{-| Take what the [`Interface`](#Interface) can come back with and return a different future value.

In practice, this is sometimes used like a kind of event-config pattern:

    Web.Time.posixRequest
        |> Web.interfaceFutureMap (\timeNow -> TimeReceived timeNow)

    button "show all entries"
        |> Web.Dom.render
        |> Web.interfaceFutureMap (\Pressed -> ShowAllEntriesButtonClicked)

sometimes as a way to deal with all events (like `update` in The Elm Architecture)

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

    treeUi : ... -> Web.Dom.Node TreeUiEvent

In all these examples, you end up converting the narrow future representation of part of the interface
to a broader representation for the parent interface

-}
interfaceFutureMap : (future -> mappedFuture) -> (Interface future -> Interface mappedFuture)
interfaceFutureMap futureChange =
    \interface ->
        interface
            |> Rope.map
                (\interfaceSingle ->
                    interfaceSingle |> interfaceSingleFutureMap futureChange
                )


{-| The "msg" in a [`Web.program`](#program)
-}
type ProgramEvent appState
    = JsEventFailedToDecode Json.Decode.Error
    | JsEventEnabledConstructionOfNewAppState appState


{-| What [`InterfaceSingleEdit`](#InterfaceSingleEdit)s are needed to sync up
-}
interfaceSingleEdits :
    { old : InterfaceSingle future, updated : InterfaceSingle future }
    -> List InterfaceSingleEdit
interfaceSingleEdits =
    \interfaces ->
        case ( interfaces.old, interfaces.updated ) of
            ( DomNodeRender domElementPreviouslyRendered, DomNodeRender domElementToRender ) ->
                { old = domElementPreviouslyRendered.node, updated = domElementToRender.node }
                    |> domTextOrElementHeaderDiff
                    |> List.map
                        (\diff ->
                            { path = domElementPreviouslyRendered.path, replacement = diff }
                                |> EditDom
                        )

            ( AudioPlay previouslyPlayed, AudioPlay toPlay ) ->
                ( previouslyPlayed, toPlay )
                    |> audioDiff
                    |> List.map
                        (\diff ->
                            { url = toPlay.url, startTime = toPlay.startTime, replacement = diff }
                                |> EditAudio
                        )

            ( NotificationShow _, NotificationShow toShow ) ->
                { id = toShow.id, message = toShow.message, details = toShow.details }
                    |> EditNotification
                    |> List.singleton

            _ ->
                []


domTextOrElementHeaderDiff :
    { old : DomTextOrElementHeader state, updated : DomTextOrElementHeader state }
    -> List DomEdit
domTextOrElementHeaderDiff =
    \nodes ->
        case ( nodes.old, nodes.updated ) of
            ( DomText _, DomElementHeader bElement ) ->
                [ bElement |> domElementHeaderFutureMap (\_ -> ()) |> DomElementHeader |> ReplacementDomNode ]

            ( DomElementHeader _, DomText bText ) ->
                [ bText |> DomText |> ReplacementDomNode ]

            ( DomText aText, DomText bText ) ->
                if aText == bText then
                    []

                else
                    [ bText |> DomText |> ReplacementDomNode ]

            ( DomElementHeader aElement, DomElementHeader bElement ) ->
                ( aElement, bElement ) |> domElementHeaderDiff


domElementHeaderDiff : ( DomElementHeader future, DomElementHeader future ) -> List DomEdit
domElementHeaderDiff =
    \( aElement, bElement ) ->
        if aElement.tag == bElement.tag then
            [ ( aElement.styles, bElement.styles )
                |> dictEditAndRemoveDiff
                    { remove = identity, edit = \key value -> { key = key, value = value } }
                |> Maybe.map ReplacementDomElementStyles
            , ( aElement.attributes, bElement.attributes )
                |> dictEditAndRemoveDiff
                    { remove = identity, edit = \key value -> { key = key, value = value } }
                |> Maybe.map ReplacementDomElementAttributes
            , ( aElement.attributesNamespaced, bElement.attributesNamespaced )
                |> dictEditAndRemoveDiff
                    { remove = \( namespace, key ) -> { namespace = namespace, key = key }
                    , edit = \( namespace, key ) value -> { namespace = namespace, key = key, value = value }
                    }
                |> Maybe.map ReplacementDomElementAttributesNamespaced
            , ( aElement.stringProperties, bElement.stringProperties )
                |> dictEditAndRemoveDiff
                    { remove = identity, edit = \key value -> { key = key, value = value } }
                |> Maybe.map ReplacementDomElementStringProperties
            , ( aElement.boolProperties, bElement.boolProperties )
                |> dictEditAndRemoveDiff
                    { remove = identity, edit = \key value -> { key = key, value = value } }
                |> Maybe.map ReplacementDomElementBoolProperties
            , if aElement.scrollToPosition == bElement.scrollToPosition then
                Nothing

              else
                ReplacementDomElementScrollToPosition bElement.scrollToPosition |> Just
            , if aElement.scrollToShow == bElement.scrollToShow then
                Nothing

              else
                ReplacementDomElementScrollToShow bElement.scrollToShow |> Just
            , case ( aElement.scrollPositionRequest, bElement.scrollPositionRequest ) of
                ( Nothing, Nothing ) ->
                    Nothing

                ( Just _, Just _ ) ->
                    Nothing

                ( Just _, Nothing ) ->
                    Nothing

                ( Nothing, Just _ ) ->
                    ReplacementDomElementScrollPositionRequest |> Just
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
                |> List.LocalExtra.justs

        else
            [ bElement |> domElementHeaderFutureMap (\_ -> ()) |> DomElementHeader |> ReplacementDomNode ]


dictEditAndRemoveDiff :
    { remove : comparableKey -> removeSingle, edit : comparableKey -> value -> editSingle }
    ->
        (( Dict comparableKey value, Dict comparableKey value )
         -> Maybe { remove : List removeSingle, edit : List editSingle }
        )
dictEditAndRemoveDiff asDiffSingle =
    \( oldDict, updatedDict ) ->
        let
            diff : { remove : List removeSingle, edit : List editSingle }
            diff =
                Dict.merge
                    (\key _ soFar ->
                        { soFar | remove = soFar.remove |> (::) (asDiffSingle.remove key) }
                    )
                    (\key old updated soFar ->
                        if old == updated then
                            soFar

                        else
                            { soFar | edit = soFar.edit |> (::) (asDiffSingle.edit key updated) }
                    )
                    (\key updated soFar ->
                        { soFar | edit = soFar.edit |> (::) (asDiffSingle.edit key updated) }
                    )
                    oldDict
                    updatedDict
                    { remove = [], edit = [] }
        in
        case ( diff.remove, diff.edit ) of
            ( [], [] ) ->
                Nothing

            ( remove0 :: remove1Up, edit ) ->
                { remove = remove0 :: remove1Up, edit = edit } |> Just

            ( remove, edit0 :: edit0Up ) ->
                { remove = remove, edit = edit0 :: edit0Up } |> Just


audioDiff : ( Audio, Audio ) -> List AudioEdit
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
            |> List.LocalExtra.justs


{-| Determine which outgoing effects need to be executed based on the difference between old and updated interfaces

To for example determine the initial effects, use

    { old = FastDict.empty, updated = initialInterface }
        |> interfacesDiff

-}
interfacesDiff :
    { old : FastDict.Dict String (InterfaceSingle future)
    , updated : FastDict.Dict String (InterfaceSingle future)
    }
    -> List { id : String, diff : InterfaceSingleDiff }
interfacesDiff =
    \interfaces ->
        FastDict.merge
            (\id _ soFar ->
                { id = id, diff = Remove } :: soFar
            )
            (\id old updated soFar ->
                ({ old = old, updated = updated }
                    |> interfaceSingleEdits
                    |> List.map (\edit -> { id = id, diff = edit |> Edit })
                )
                    ++ soFar
            )
            (\id onlyNew soFar ->
                { id = id
                , diff = onlyNew |> interfaceSingleFutureMap (\_ -> ()) |> Add
                }
                    :: soFar
            )
            interfaces.old
            interfaces.updated
            []


toJsToJson : { id : String, diff : InterfaceSingleDiff } -> Json.Encode.Value
toJsToJson =
    \toJs ->
        Json.Encode.object
            [ ( "id", toJs.id |> Json.Encode.string )
            , ( "diff", toJs.diff |> interfaceSingleDiffToJson )
            ]


interfaceSingleDiffToJson : InterfaceSingleDiff -> Json.Encode.Value
interfaceSingleDiffToJson =
    \diff ->
        Json.Encode.LocalExtra.variant
            (case diff of
                Add interfaceSingleInfo ->
                    ( "Add", interfaceSingleInfo |> interfaceSingleToJson )

                Edit edit ->
                    ( "Edit", edit |> interfaceSingleEditToJson )

                Remove ->
                    ( "Remove", Json.Encode.null )
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
        Json.Encode.LocalExtra.variant
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


interfaceSingleEditToJson : InterfaceSingleEdit -> Json.Encode.Value
interfaceSingleEditToJson =
    \edit ->
        Json.Encode.LocalExtra.variant
            (case edit of
                EditDom editDomDiff ->
                    ( "EditDom"
                    , Json.Encode.object
                        [ ( "path", editDomDiff.path |> Json.Encode.list Json.Encode.int )
                        , ( "replacement", editDomDiff.replacement |> editDomDiffToJson )
                        ]
                    )

                EditAudio audioEdit ->
                    ( "EditAudio"
                    , Json.Encode.object
                        [ ( "url", audioEdit.url |> Json.Encode.string )
                        , ( "startTime", audioEdit.startTime |> Time.posixToMillis |> Json.Encode.int )
                        , ( "replacement"
                          , Json.Encode.LocalExtra.variant
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

                EditNotification editNotificationDiff ->
                    ( "EditNotification"
                    , Json.Encode.object
                        [ ( "id", editNotificationDiff.id |> Json.Encode.string )
                        , ( "message", editNotificationDiff.message |> Json.Encode.string )
                        , ( "details", editNotificationDiff.details |> Json.Encode.string )
                        ]
                    )
            )


domTextOrElementHeaderInfoToJson : DomTextOrElementHeader () -> Json.Encode.Value
domTextOrElementHeaderInfoToJson =
    \domNodeId ->
        Json.Encode.LocalExtra.variant
            (case domNodeId of
                DomText text ->
                    ( "Text", text |> Json.Encode.string )

                DomElementHeader element ->
                    ( "Element", element |> domElementHeaderInfoToJson )
            )


domElementEventListensInfoToJson : Dict String DefaultActionHandling -> Json.Encode.Value
domElementEventListensInfoToJson =
    \listens ->
        listens
            |> Dict.toList
            |> Json.Encode.list
                (\( name, defaultActionHandling ) ->
                    Json.Encode.object
                        [ ( "name", name |> Json.Encode.string )
                        , ( "defaultActionHandling", defaultActionHandling |> defaultActionHandlingToJson )
                        ]
                )


defaultActionHandlingToJson : DefaultActionHandling -> Json.Encode.Value
defaultActionHandlingToJson =
    \defaultActionHandling ->
        Json.Encode.string
            (case defaultActionHandling of
                DefaultActionPrevent ->
                    "DefaultActionPrevent"

                DefaultActionExecute ->
                    "DefaultActionExecute"
            )


domElementVisibilityAlignmentsToJson : { y : DomElementVisibilityAlignment, x : DomElementVisibilityAlignment } -> Json.Encode.Value
domElementVisibilityAlignmentsToJson =
    \alignments ->
        Json.Encode.object
            [ ( "x", alignments.x |> domElementVisibilityAlignmentToJson )
            , ( "y", alignments.y |> domElementVisibilityAlignmentToJson )
            ]


domElementVisibilityAlignmentToJson : DomElementVisibilityAlignment -> Json.Encode.Value
domElementVisibilityAlignmentToJson =
    \alignment ->
        Json.Encode.string
            (case alignment of
                DomElementStart ->
                    "start"

                DomElementEnd ->
                    "end"

                DomElementCenter ->
                    "center"
            )


domElementScrollPositionToJson : { fromLeft : Float, fromTop : Float } -> Json.Encode.Value
domElementScrollPositionToJson =
    \position ->
        Json.Encode.object
            [ ( "fromLeft", position.fromLeft |> Json.Encode.float )
            , ( "fromTop", position.fromTop |> Json.Encode.float )
            ]


domElementHeaderInfoToJson : DomElementHeader () -> Json.Encode.Value
domElementHeaderInfoToJson =
    \header ->
        Json.Encode.object
            [ ( "namespace", header.namespace |> Json.Encode.LocalExtra.nullable Json.Encode.string )
            , ( "tag", header.tag |> Json.Encode.string )
            , ( "styles", header.styles |> domElementStylesToJson )
            , ( "attributes", header.attributes |> domElementAttributesToJson )
            , ( "attributesNamespaced", header.attributesNamespaced |> domElementAttributesNamespacedToJson )
            , ( "stringProperties", header.stringProperties |> domElementStringPropertiesToJson )
            , ( "boolProperties", header.boolProperties |> domElementBoolPropertiesToJson )
            , ( "scrollToPosition"
              , header.scrollToPosition |> Json.Encode.LocalExtra.nullable domElementScrollPositionToJson
              )
            , ( "scrollToShow"
              , header.scrollToShow |> Json.Encode.LocalExtra.nullable domElementVisibilityAlignmentsToJson
              )
            , ( "scrollPositionRequest"
              , case header.scrollPositionRequest of
                    Nothing ->
                        Json.Encode.bool False

                    Just _ ->
                        Json.Encode.bool True
              )
            , ( "eventListens"
              , header.eventListens
                    |> Dict.map (\_ listen -> listen.defaultActionHandling)
                    |> domElementEventListensInfoToJson
              )
            ]


domElementAttributesNamespacedToJson : Dict ( String, String ) String -> Json.Encode.Value
domElementAttributesNamespacedToJson =
    \attributes ->
        attributes
            |> Dict.toList
            |> Json.Encode.list
                (\( ( namespace, key ), value ) ->
                    Json.Encode.object
                        [ ( "namespace", namespace |> Json.Encode.string )
                        , ( "key", key |> Json.Encode.string )
                        , ( "value", value |> Json.Encode.string )
                        ]
                )


domElementAttributesToJson : Dict String String -> Json.Encode.Value
domElementAttributesToJson =
    \attributes ->
        attributes
            |> Dict.toList
            |> Json.Encode.list
                (\( key, value ) ->
                    Json.Encode.object
                        [ ( "key", key |> Json.Encode.string )
                        , ( "value", value |> Json.Encode.string )
                        ]
                )


domElementStylesToJson : Dict String String -> Json.Encode.Value
domElementStylesToJson =
    \styles ->
        styles
            |> Dict.toList
            |> Json.Encode.list
                (\( key, value ) ->
                    Json.Encode.object
                        [ ( "key", key |> Json.Encode.string )
                        , ( "value", value |> Json.Encode.string )
                        ]
                )


domElementBoolPropertiesToJson : Dict String Bool -> Json.Encode.Value
domElementBoolPropertiesToJson =
    \boolProperties ->
        boolProperties
            |> Dict.toList
            |> Json.Encode.list
                (\( key, value ) ->
                    Json.Encode.object
                        [ ( "key", key |> Json.Encode.string )
                        , ( "value", value |> Json.Encode.bool )
                        ]
                )


domElementStringPropertiesToJson : Dict String String -> Json.Encode.Value
domElementStringPropertiesToJson =
    \stringProperties ->
        stringProperties
            |> Dict.toList
            |> Json.Encode.list
                (\( key, value ) ->
                    Json.Encode.object
                        [ ( "key", key |> Json.Encode.string )
                        , ( "value", value |> Json.Encode.string )
                        ]
                )


editDomDiffToJson : DomEdit -> Json.Encode.Value
editDomDiffToJson =
    \replacementInEditDomDiff ->
        Json.Encode.LocalExtra.variant
            (case replacementInEditDomDiff of
                ReplacementDomNode node ->
                    ( "Node", node |> domTextOrElementHeaderInfoToJson )

                ReplacementDomElementStyles styles ->
                    ( "Styles"
                    , Json.Encode.object
                        [ ( "remove", styles.remove |> Json.Encode.list Json.Encode.string )
                        , ( "edit"
                          , styles.edit
                                |> Json.Encode.list
                                    (\editSingle ->
                                        Json.Encode.object
                                            [ ( "key", editSingle.key |> Json.Encode.string )
                                            , ( "value", editSingle.value |> Json.Encode.string )
                                            ]
                                    )
                          )
                        ]
                    )

                ReplacementDomElementAttributes attributes ->
                    ( "Attributes"
                    , Json.Encode.object
                        [ ( "remove", attributes.remove |> Json.Encode.list Json.Encode.string )
                        , ( "edit"
                          , attributes.edit
                                |> Json.Encode.list
                                    (\editSingle ->
                                        Json.Encode.object
                                            [ ( "key", editSingle.key |> Json.Encode.string )
                                            , ( "value", editSingle.value |> Json.Encode.string )
                                            ]
                                    )
                          )
                        ]
                    )

                ReplacementDomElementAttributesNamespaced attributesNamespaced ->
                    ( "AttributesNamespaced"
                    , Json.Encode.object
                        [ ( "remove"
                          , attributesNamespaced.remove
                                |> Json.Encode.list
                                    (\removeSingle ->
                                        Json.Encode.object
                                            [ ( "namespace", removeSingle.namespace |> Json.Encode.string )
                                            , ( "key", removeSingle.key |> Json.Encode.string )
                                            ]
                                    )
                          )
                        , ( "edit"
                          , attributesNamespaced.edit
                                |> Json.Encode.list
                                    (\editSingle ->
                                        Json.Encode.object
                                            [ ( "namespace", editSingle.namespace |> Json.Encode.string )
                                            , ( "key", editSingle.key |> Json.Encode.string )
                                            , ( "value", editSingle.value |> Json.Encode.string )
                                            ]
                                    )
                          )
                        ]
                    )

                ReplacementDomElementStringProperties stringProperties ->
                    ( "StringProperties"
                    , Json.Encode.object
                        [ ( "remove", stringProperties.remove |> Json.Encode.list Json.Encode.string )
                        , ( "edit"
                          , stringProperties.edit
                                |> Json.Encode.list
                                    (\editSingle ->
                                        Json.Encode.object
                                            [ ( "key", editSingle.key |> Json.Encode.string )
                                            , ( "value", editSingle.value |> Json.Encode.string )
                                            ]
                                    )
                          )
                        ]
                    )

                ReplacementDomElementBoolProperties boolProperties ->
                    ( "StringProperties"
                    , Json.Encode.object
                        [ ( "remove", boolProperties.remove |> Json.Encode.list Json.Encode.string )
                        , ( "edit"
                          , boolProperties.edit
                                |> Json.Encode.list
                                    (\editSingle ->
                                        Json.Encode.object
                                            [ ( "key", editSingle.key |> Json.Encode.string )
                                            , ( "value", editSingle.value |> Json.Encode.bool )
                                            ]
                                    )
                          )
                        ]
                    )

                ReplacementDomElementScrollToPosition maybePosition ->
                    ( "ScrollToPosition"
                    , maybePosition |> Json.Encode.LocalExtra.nullable domElementScrollPositionToJson
                    )

                ReplacementDomElementScrollToShow alignment ->
                    ( "ScrollToShow"
                    , alignment |> Json.Encode.LocalExtra.nullable domElementVisibilityAlignmentsToJson
                    )

                ReplacementDomElementScrollPositionRequest ->
                    ( "ScrollPositionRequest", Json.Encode.null )

                ReplacementDomElementEventListens listens ->
                    ( "EventListens", listens |> domElementEventListensInfoToJson )
            )


interfaceSingleToJson : InterfaceSingle () -> Json.Encode.Value
interfaceSingleToJson =
    \add ->
        Json.Encode.LocalExtra.variant
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
                    ( "NavigationLoad", url |> Json.Encode.string )

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
                    ( "AudioPlay", audio |> audioToJson )

                SocketMessage message ->
                    ( "SocketMessage"
                    , Json.Encode.object
                        [ ( "id", message.id |> socketIdToJson )
                        , ( "data", message.data |> Json.Encode.string )
                        ]
                    )

                LocalStorageSet set ->
                    ( "LocalStorageSet"
                    , Json.Encode.object
                        [ ( "key", set.key |> Json.Encode.string )
                        , ( "value", set.value |> Json.Encode.LocalExtra.nullable Json.Encode.string )
                        ]
                    )

                NotificationAskForPermission ->
                    ( "NotificationAskForPermission", Json.Encode.null )

                DomNodeRender render ->
                    ( "DomNodeRender"
                    , Json.Encode.object
                        [ ( "path", render.path |> Json.Encode.list Json.Encode.int )
                        , ( "node", render.node |> domTextOrElementHeaderInfoToJson )
                        ]
                    )

                AudioSourceLoad audioSourceLoad ->
                    ( "AudioSourceLoad", audioSourceLoad.url |> Json.Encode.string )

                SocketConnect connect ->
                    ( "SocketConnect"
                    , Json.Encode.object [ ( "address", connect.address |> Json.Encode.string ) ]
                    )

                NotificationShow show ->
                    ( "NotificationShow"
                    , Json.Encode.object
                        [ ( "id", show.id |> Json.Encode.string )
                        , ( "message", show.message |> Json.Encode.string )
                        , ( "details", show.details |> Json.Encode.string )
                        ]
                    )

                HttpRequest httpRequestInfo ->
                    ( "HttpRequest", httpRequestInfo |> httpRequestInfoToJson )

                TimePosixRequest _ ->
                    ( "TimePosixRequest", Json.Encode.null )

                TimezoneOffsetRequest _ ->
                    ( "TimezoneOffsetRequest", Json.Encode.null )

                TimezoneNameRequest _ ->
                    ( "TimezoneNameRequest", Json.Encode.null )

                TimeOnce once ->
                    ( "TimeOnce"
                    , Json.Encode.object
                        [ ( "pointInTime", once.pointInTime |> Time.posixToMillis |> Json.Encode.int ) ]
                    )

                RandomUnsignedInt32sRequest request ->
                    ( "RandomUnsignedInt32sRequest", request.count |> Json.Encode.int )

                WindowSizeRequest _ ->
                    ( "WindowSizeRequest", Json.Encode.null )

                WindowPreferredLanguagesRequest _ ->
                    ( "WindowPreferredLanguagesRequest", Json.Encode.null )

                NavigationUrlRequest _ ->
                    ( "NavigationUrlRequest", Json.Encode.null )

                ClipboardRequest _ ->
                    ( "ClipboardRequest", Json.Encode.null )

                LocalStorageRequest request ->
                    ( "LocalStorageRequest"
                    , Json.Encode.object [ ( "key", request.key |> Json.Encode.string ) ]
                    )

                GeoLocationRequest _ ->
                    ( "GeoLocationRequest", Json.Encode.null )

                GamepadsRequest _ ->
                    ( "GamepadsRequest", Json.Encode.null )

                TimePeriodicallyListen intervalDuration ->
                    ( "TimePeriodicallyListen"
                    , Json.Encode.object
                        [ ( "milliSeconds", intervalDuration.intervalDurationMilliSeconds |> Json.Encode.int ) ]
                    )

                WindowEventListen listen ->
                    ( "WindowEventListen", listen.eventName |> Json.Encode.string )

                WindowVisibilityChangeListen _ ->
                    ( "WindowVisibilityChangeListen", Json.Encode.null )

                WindowAnimationFrameListen _ ->
                    ( "WindowAnimationFrameListen", Json.Encode.null )

                WindowPreferredLanguagesChangeListen _ ->
                    ( "WindowPreferredLanguagesChangeListen", Json.Encode.null )

                DocumentEventListen listen ->
                    ( "DocumentEventListen", listen.eventName |> Json.Encode.string )

                SocketMessageListen listen ->
                    ( "SocketMessageListen", listen.id |> socketIdToJson )

                LocalStorageRemoveOnADifferentTabListen listen ->
                    ( "LocalStorageRemoveOnADifferentTabListen"
                    , Json.Encode.object
                        [ ( "key", listen.key |> Json.Encode.string ) ]
                    )

                LocalStorageSetOnADifferentTabListen listen ->
                    ( "LocalStorageSetOnADifferentTabListen"
                    , Json.Encode.object
                        [ ( "key", listen.key |> Json.Encode.string ) ]
                    )

                GeoLocationChangeListen _ ->
                    ( "GeoLocationChangeListen", Json.Encode.null )

                GamepadsChangeListen _ ->
                    ( "GamepadsChangeListen", Json.Encode.null )
            )


socketIdToJson : SocketId -> Json.Encode.Value
socketIdToJson =
    \(SocketId index) -> index |> Json.Encode.int


httpRequestInfoToJson : HttpRequest () -> Json.Encode.Value
httpRequestInfoToJson =
    \httpRequestId ->
        Json.Encode.object
            [ ( "url", httpRequestId.url |> Json.Encode.string )
            , ( "method", httpRequestId.method |> Json.Encode.string )
            , ( "headers"
              , httpRequestId.headers
                    |> addContentTypeForBody httpRequestId.body
                    |> Json.Encode.list headerToJson
              )
            , ( "expect", httpRequestId.expect |> httpExpectInfoToJson )
            , ( "body", httpRequestId.body |> httpBodyToJson )
            ]


headerToJson : { name : String, value : String } -> Json.Encode.Value
headerToJson =
    \header ->
        Json.Encode.object
            [ ( "name", header.name |> Json.Encode.string )
            , ( "value", header.value |> Json.Encode.string )
            ]


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
        Json.Encode.LocalExtra.variant
            (case body of
                HttpBodyString stringBodyInfo ->
                    ( "String"
                    , stringBodyInfo.content |> Json.Encode.string
                    )

                HttpBodyUnsignedInt8s bytesBodyInfo ->
                    ( "Uint8Array", bytesBodyInfo.content |> Json.Encode.list Json.Encode.int )

                HttpBodyEmpty ->
                    ( "Empty", Json.Encode.null )
            )


httpExpectInfoToJson : HttpExpect () -> Json.Encode.Value
httpExpectInfoToJson =
    \httpExpectId ->
        Json.Encode.string
            (case httpExpectId of
                HttpExpectString _ ->
                    "String"

                HttpExpectBytes _ ->
                    "Bytes"

                HttpExpectWhatever _ ->
                    "Whatever"
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


interfacesFromRope : Rope (InterfaceSingle future) -> FastDict.Dict String (InterfaceSingle future)
interfacesFromRope =
    \rope ->
        rope
            |> Rope.foldl
                (\interfaceSingle soFar ->
                    soFar |> insertInterface interfaceSingle
                )
                FastDict.empty


interfaceSingleToStructuredId : InterfaceSingle future_ -> StructuredId
interfaceSingleToStructuredId =
    \interfaceSingle ->
        StructuredId.ofVariant
            (case interfaceSingle of
                DocumentTitleReplaceBy title ->
                    ( "DocumentTitleReplaceBy", [ title |> StructuredId.ofString ] )

                DocumentAuthorSet author ->
                    ( "DocumentAuthorSet", [ author |> StructuredId.ofString ] )

                DocumentKeywordsSet keywords ->
                    ( "DocumentKeywordsSet", [ keywords |> StructuredId.ofList StructuredId.ofString ] )

                DocumentDescriptionSet description ->
                    ( "DocumentDescriptionSet", [ description |> StructuredId.ofString ] )

                ConsoleLog message ->
                    ( "ConsoleLog", [ message |> StructuredId.ofString ] )

                ConsoleWarn message ->
                    ( "ConsoleWarn", [ message |> StructuredId.ofString ] )

                ConsoleError message ->
                    ( "ConsoleError", [ message |> StructuredId.ofString ] )

                NavigationReplaceUrl appUrl ->
                    ( "NavigationReplaceUrl", [ appUrl |> AppUrl.LocalExtra.toStructuredId ] )

                NavigationPushUrl appUrl ->
                    ( "NavigationPushUrl", [ appUrl |> AppUrl.LocalExtra.toStructuredId ] )

                NavigationGo urlSteps ->
                    ( "NavigationGo", [ urlSteps |> StructuredId.ofInt ] )

                NavigationLoad url ->
                    ( "NavigationLoad", [ url |> StructuredId.ofString ] )

                NavigationReload ->
                    ( "NavigationReload", [] )

                FileDownloadUnsignedInt8s config ->
                    ( "FileDownloadUnsignedInt8s"
                    , [ config.name |> StructuredId.ofString
                      , config.mimeType |> StructuredId.ofString
                      , config.content |> StructuredId.ofList StructuredId.ofInt
                      ]
                    )

                ClipboardReplaceBy replacement ->
                    ( "ClipboardReplaceBy", [ replacement |> StructuredId.ofString ] )

                AudioPlay audio ->
                    ( "AudioPlay"
                    , [ audio.url |> StructuredId.ofString
                      , audio.startTime |> Time.LocalExtra.posixToStructureId
                      ]
                    )

                SocketMessage message ->
                    ( "SocketMessage"
                    , [ message.id |> socketIdToStructuredId
                      , message.data |> StructuredId.ofString
                      ]
                    )

                LocalStorageSet set ->
                    ( "LocalStorageSet"
                    , [ set.key |> StructuredId.ofString
                      , set.value |> StructuredId.ofMaybe StructuredId.ofString
                      ]
                    )

                NotificationAskForPermission ->
                    ( "NotificationAskForPermission", [] )

                DomNodeRender path ->
                    ( "DomNodeRender"
                    , [ path.path |> StructuredId.ofList StructuredId.ofInt ]
                    )

                AudioSourceLoad load ->
                    ( "AudioSourceLoad"
                    , [ load.url |> StructuredId.ofString
                      ]
                    )

                SocketConnect connect ->
                    ( "SocketConnect"
                    , [ StructuredId.ofString connect.address
                      ]
                    )

                NotificationShow show ->
                    ( "NotificationShow"
                    , [ show.id |> StructuredId.ofString
                      ]
                    )

                HttpRequest request ->
                    ( "HttpRequest"
                    , [ request.url |> StructuredId.ofString
                      ]
                    )

                TimePosixRequest _ ->
                    ( "TimePosixRequest", [] )

                TimezoneOffsetRequest _ ->
                    ( "TimezoneOffsetRequest", [] )

                TimezoneNameRequest _ ->
                    ( "TimezoneNameRequest", [] )

                TimeOnce once ->
                    ( "TimeOnce", [ once.pointInTime |> Time.LocalExtra.posixToStructureId ] )

                RandomUnsignedInt32sRequest request ->
                    ( "RandomUnsignedInt32sRequest"
                    , [ request.count |> StructuredId.ofInt
                      ]
                    )

                LocalStorageRequest request ->
                    ( "LocalStorageRequest"
                    , [ request.key |> StructuredId.ofString
                      ]
                    )

                WindowSizeRequest _ ->
                    ( "WindowSizeRequest", [] )

                WindowPreferredLanguagesRequest _ ->
                    ( "WindowPreferredLanguagesRequest", [] )

                NavigationUrlRequest _ ->
                    ( "NavigationUrlRequest", [] )

                ClipboardRequest _ ->
                    ( "ClipboardRequest", [] )

                GeoLocationRequest _ ->
                    ( "GeoLocationRequest", [] )

                GamepadsRequest _ ->
                    ( "GamepadsRequest", [] )

                WindowEventListen listen ->
                    ( "WindowEventListen"
                    , [ listen.eventName |> StructuredId.ofString
                      ]
                    )

                WindowVisibilityChangeListen _ ->
                    ( "WindowVisibilityChangeListen", [] )

                WindowAnimationFrameListen _ ->
                    ( "WindowAnimationFrameListen", [] )

                WindowPreferredLanguagesChangeListen _ ->
                    ( "WindowPreferredLanguagesChangeListen", [] )

                DocumentEventListen listen ->
                    ( "DocumentEventListen"
                    , [ listen.eventName |> StructuredId.ofString
                      ]
                    )

                TimePeriodicallyListen listen ->
                    ( "TimePeriodicallyListen"
                    , [ listen.intervalDurationMilliSeconds |> StructuredId.ofInt
                      ]
                    )

                SocketMessageListen listen ->
                    ( "SocketMessageListen"
                    , [ listen.id |> socketIdToStructuredId
                      ]
                    )

                LocalStorageRemoveOnADifferentTabListen listen ->
                    ( "LocalStorageRemoveOnADifferentTabListen"
                    , [ listen.key |> StructuredId.ofString ]
                    )

                LocalStorageSetOnADifferentTabListen listen ->
                    ( "LocalStorageSetOnADifferentTabListen"
                    , [ listen.key |> StructuredId.ofString ]
                    )

                GeoLocationChangeListen _ ->
                    ( "GeoLocationChangeListen", [] )

                GamepadsChangeListen _ ->
                    ( "GamepadsChangeListen", [] )
            )


socketIdToStructuredId : SocketId -> StructuredId
socketIdToStructuredId =
    \(SocketId raw) -> raw |> StructuredId.ofInt


insertInterface :
    InterfaceSingle future
    -> (FastDict.Dict String (InterfaceSingle future) -> FastDict.Dict String (InterfaceSingle future))
insertInterface element =
    \dict ->
        dict
            |> FastDict.insert
                (element |> interfaceSingleToStructuredId |> StructuredId.toString)
                element


{-| The "init" part for an embedded program
-}
programInit : ProgramConfig state -> ( ProgramState state, Cmd (ProgramEvent state) )
programInit appConfig =
    let
        initialInterface : FastDict.Dict String (InterfaceSingle state)
        initialInterface =
            appConfig.initialState
                |> appConfig.interface
                |> interfacesFromRope
    in
    ( State
        { interface = initialInterface
        , appState = appConfig.initialState
        }
    , { old = FastDict.empty, updated = initialInterface }
        |> interfacesDiff
        |> List.map (\diff -> appConfig.ports.toJs (diff |> toJsToJson))
        |> Cmd.batch
        |> Cmd.map never
    )


{-| The "subscriptions" part for an embedded program
-}
programSubscriptions : ProgramConfig state -> (ProgramState state -> Sub (ProgramEvent state))
programSubscriptions appConfig =
    \(State state) ->
        appConfig.ports.fromJs
            (\interfaceJson ->
                interfaceJson
                    |> Json.Decode.decodeValue
                        (Json.Decode.field "id" Json.Decode.string
                            |> Json.Decode.andThen
                                (\originalInterfaceId ->
                                    case state.interface |> FastDict.get originalInterfaceId of
                                        Just interfaceSingleAcceptingFuture ->
                                            case interfaceSingleAcceptingFuture |> interfaceSingleFutureJsonDecoder of
                                                Just eventDataDecoder ->
                                                    Json.Decode.field "eventData" eventDataDecoder

                                                Nothing ->
                                                    "interface did not expect any events" |> Json.Decode.fail

                                        Nothing ->
                                            "no associated interface found" |> Json.Decode.fail
                                )
                            |> Json.Decode.map JsEventEnabledConstructionOfNewAppState
                        )
                    |> Result.LocalExtra.valueOrOnError JsEventFailedToDecode
            )


{-| [json `Decoder`](https://dark.elm.dmy.fr/packages/elm/json/latest/Json-Decode#Decoder)
for the transformed event data coming back
-}
interfaceSingleFutureJsonDecoder : InterfaceSingle future -> Maybe (Json.Decode.Decoder future)
interfaceSingleFutureJsonDecoder =
    \interface ->
        case interface of
            DocumentTitleReplaceBy _ ->
                Nothing

            DocumentAuthorSet _ ->
                Nothing

            DocumentKeywordsSet _ ->
                Nothing

            DocumentDescriptionSet _ ->
                Nothing

            ConsoleLog _ ->
                Nothing

            ConsoleWarn _ ->
                Nothing

            ConsoleError _ ->
                Nothing

            DocumentEventListen listen ->
                listen.on |> Just

            NavigationReplaceUrl _ ->
                Nothing

            NavigationPushUrl _ ->
                Nothing

            NavigationGo _ ->
                Nothing

            NavigationLoad _ ->
                Nothing

            NavigationReload ->
                Nothing

            NavigationUrlRequest toFuture ->
                Url.LocalExtra.jsonDecoder
                    |> Json.Decode.map AppUrl.fromUrl
                    |> Json.Decode.map toFuture
                    |> Just

            FileDownloadUnsignedInt8s _ ->
                Nothing

            ClipboardReplaceBy _ ->
                Nothing

            ClipboardRequest toFuture ->
                Json.Decode.string |> Json.Decode.map toFuture |> Just

            AudioSourceLoad load ->
                Json.Decode.oneOf
                    [ Json.Decode.map (\duration -> Ok { url = load.url, duration = duration })
                        (Json.Decode.LocalExtra.variant "Success"
                            (Json.Decode.field "durationInSeconds"
                                (Json.Decode.map Duration.seconds Json.Decode.float)
                            )
                        )
                    , Json.Decode.LocalExtra.variant "Error"
                        (Json.Decode.map Err audioSourceLoadErrorJsonDecoder)
                    ]
                    |> Json.Decode.map load.on
                    |> Just

            AudioPlay _ ->
                Nothing

            DomNodeRender toRender ->
                case toRender.node of
                    DomText _ ->
                        Nothing

                    DomElementHeader domElement ->
                        Json.Decode.oneOf
                            ([ Json.Decode.LocalExtra.variant "EventListen"
                                (Json.Decode.map2 (\eventListen event -> eventListen.on event)
                                    (Json.Decode.field "name"
                                        (Json.Decode.string
                                            |> Json.Decode.andThen
                                                (\specificEventName ->
                                                    case domElement.eventListens |> Dict.get specificEventName of
                                                        Nothing ->
                                                            Json.Decode.fail "received event of a kind that isn't listened for"

                                                        Just eventListen ->
                                                            eventListen |> Json.Decode.succeed
                                                )
                                        )
                                    )
                                    (Json.Decode.field "event" Json.Decode.value)
                                )
                                |> Just
                             , case domElement.scrollPositionRequest of
                                Nothing ->
                                    Nothing

                                Just request ->
                                    Json.Decode.LocalExtra.variant "ScrollPositionRequest"
                                        domElementScrollPositionJsonDecoder
                                        |> Json.Decode.map request
                                        |> Just
                             ]
                                |> List.LocalExtra.justs
                            )
                            |> Just

            NotificationAskForPermission ->
                Nothing

            NotificationShow show ->
                notificationResponseJsonDecoder
                    |> Json.Decode.map show.on
                    |> Just

            HttpRequest httpRequest ->
                Json.Decode.oneOf
                    [ Json.Decode.LocalExtra.variant "Success" (httpSuccessResponseJsonDecoder httpRequest.expect)
                    , Json.Decode.LocalExtra.variant "Error" httpErrorJsonDecoder
                        |> Json.Decode.map (httpExpectOnError httpRequest.expect)
                    ]
                    |> Just

            TimePosixRequest toFuture ->
                Time.LocalExtra.posixJsonDecoder |> Json.Decode.map toFuture |> Just

            TimezoneOffsetRequest toFuture ->
                Json.Decode.int |> Json.Decode.map toFuture |> Just

            TimePeriodicallyListen timePeriodicallyListen ->
                Time.LocalExtra.posixJsonDecoder
                    |> Json.Decode.map timePeriodicallyListen.on
                    |> Just

            TimeOnce once ->
                Time.LocalExtra.posixJsonDecoder
                    |> Json.Decode.map once.on
                    |> Just

            TimezoneNameRequest toFuture ->
                Json.Decode.string |> Json.Decode.map toFuture |> Just

            RandomUnsignedInt32sRequest randomUnsignedInt32sRequest ->
                Json.Decode.list Json.Decode.int
                    |> Json.Decode.map randomUnsignedInt32sRequest.on
                    |> Just

            WindowSizeRequest toFuture ->
                Json.Decode.map2 (\width height -> { width = width, height = height })
                    (Json.Decode.field "width" Json.Decode.int)
                    (Json.Decode.field "height" Json.Decode.int)
                    |> Json.Decode.map toFuture
                    |> Just

            WindowPreferredLanguagesRequest toFuture ->
                Json.Decode.list Json.Decode.string
                    |> Json.Decode.map toFuture
                    |> Just

            WindowEventListen listen ->
                listen.on |> Just

            WindowVisibilityChangeListen toFuture ->
                windowVisibilityJsonDecoder |> Json.Decode.map toFuture |> Just

            WindowAnimationFrameListen toFuture ->
                Time.LocalExtra.posixJsonDecoder |> Json.Decode.map toFuture |> Just

            WindowPreferredLanguagesChangeListen toFuture ->
                Json.Decode.list Json.Decode.string
                    |> Json.Decode.map toFuture
                    |> Just

            SocketConnect connect ->
                socketConnectionEventJsonDecoder
                    |> Json.Decode.map connect.on
                    |> Just

            SocketMessage _ ->
                Nothing

            SocketMessageListen messageListen ->
                Json.Decode.string |> Json.Decode.map messageListen.on |> Just

            LocalStorageSet _ ->
                Nothing

            LocalStorageRequest request ->
                Json.Decode.nullable Json.Decode.string
                    |> Json.Decode.map request.on
                    |> Just

            LocalStorageRemoveOnADifferentTabListen listen ->
                Url.LocalExtra.jsonDecoder
                    |> Json.Decode.map AppUrl.fromUrl
                    |> Json.Decode.map listen.on
                    |> Just

            LocalStorageSetOnADifferentTabListen listen ->
                Json.Decode.map3
                    (\appUrl oldValue newValue ->
                        { appUrl = appUrl, oldValue = oldValue, newValue = newValue }
                    )
                    (Json.Decode.field "url"
                        (Url.LocalExtra.jsonDecoder |> Json.Decode.map AppUrl.fromUrl)
                    )
                    (Json.Decode.field "oldValue" (Json.Decode.nullable Json.Decode.string))
                    (Json.Decode.field "newValue" Json.Decode.string)
                    |> Json.Decode.map listen.on
                    |> Just

            GeoLocationRequest toFuture ->
                geoLocationJsonDecoder |> Json.Decode.map toFuture |> Just

            GeoLocationChangeListen toFuture ->
                geoLocationJsonDecoder |> Json.Decode.map toFuture |> Just

            GamepadsRequest toFuture ->
                gamepadsJsonDecoder |> Json.Decode.map toFuture |> Just

            GamepadsChangeListen toFuture ->
                gamepadsJsonDecoder |> Json.Decode.map toFuture |> Just


audioSourceLoadErrorJsonDecoder : Json.Decode.Decoder AudioSourceLoadError
audioSourceLoadErrorJsonDecoder =
    Json.Decode.string
        |> Json.Decode.map
            (\errorMessage ->
                case errorMessage of
                    "NetworkError" ->
                        AudioSourceLoadNetworkError

                    "MediaDecodeAudioDataUnknownContentType" ->
                        AudioSourceLoadDecodeError

                    "DOMException: The buffer passed to decodeAudioData contains an unknown content type." ->
                        AudioSourceLoadDecodeError

                    _ ->
                        AudioSourceLoadNetworkError
            )


domElementScrollPositionJsonDecoder : Json.Decode.Decoder { fromLeft : Float, fromTop : Float }
domElementScrollPositionJsonDecoder =
    Json.Decode.map2 (\fromLeft fromTop -> { fromLeft = fromLeft, fromTop = fromTop })
        (Json.Decode.field "fromLeft" Json.Decode.float)
        (Json.Decode.field "fromTop" Json.Decode.float)


notificationResponseJsonDecoder : Json.Decode.Decoder NotificationClicked
notificationResponseJsonDecoder =
    Json.Decode.map (\() -> NotificationClicked) (Json.Decode.LocalExtra.onlyString "Clicked")


socketConnectionEventJsonDecoder : Json.Decode.Decoder SocketConnectionEvent
socketConnectionEventJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map SocketConnected (Json.Decode.LocalExtra.variant "SocketConnected" socketIdJsonDecoder)
        , Json.Decode.map SocketDisconnected
            (Json.Decode.LocalExtra.variant "SocketDisconnected"
                (Json.Decode.map2 (\code reason -> { code = code, reason = reason })
                    (Json.Decode.field "code" Json.Decode.int)
                    (Json.Decode.field "reason" Json.Decode.string)
                )
            )
        ]


socketIdJsonDecoder : Json.Decode.Decoder SocketId
socketIdJsonDecoder =
    Json.Decode.map SocketId Json.Decode.int


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


gamepadsJsonDecoder : Json.Decode.Decoder (Dict Int Gamepad)
gamepadsJsonDecoder =
    Json.Decode.map
        (\maybeGamepads ->
            maybeGamepads
                |> List.LocalExtra.foldUpIndexedFrom Dict.empty
                    (\index maybeGamepad soFar ->
                        case maybeGamepad of
                            Nothing ->
                                soFar

                            Just gamepad ->
                                soFar |> Dict.insert index gamepad
                    )
        )
        (Json.Decode.list maybeGamepadJsonDecoder)


{-| <https://www.w3.org/TR/gamepad/#remapping>
-}
gamepadStandardButtonMap : GamepadButtonMap
gamepadStandardButtonMap =
    { primary = Just 0
    , secondary = Just 1
    , tertiary = Just 2
    , quaternary = Just 3
    , leftBumper = Just 4
    , rightBumper = Just 5
    , leftTrigger = Just 6
    , rightTrigger = Just 7
    , select = Just 8
    , start = Just 9
    , leftThumbstickButton = Just 10
    , rightThumbstickButton = Just 11
    , arrowUp = Just 12
    , arrowDown = Just 13
    , arrowLeft = Just 14
    , arrowRight = Just 15
    , homeButton = Just 16
    , touchpad = Just 17
    }


maybeGamepadJsonDecoder : Json.Decode.Decoder (Maybe Gamepad)
maybeGamepadJsonDecoder =
    Json.Decode.nullable
        (Json.Decode.field "connected" Json.Decode.bool
            |> Json.Decode.andThen
                (\connected ->
                    if not connected then
                        Nothing |> Json.Decode.succeed

                    else
                        Json.Decode.map3
                            (\kindId buttons thumbsticks ->
                                let
                                    buttonMap : GamepadButtonMap
                                    buttonMap =
                                        buttonMapping |> FastDict.get kindId |> Maybe.withDefault gamepadStandardButtonMap

                                    at : (GamepadButtonMap -> Maybe Int) -> GamepadButton
                                    at indexField =
                                        case buttonMap |> indexField of
                                            Nothing ->
                                                gamepadButtonUnknown

                                            Just index ->
                                                buttons |> listAtIndex index |> Maybe.withDefault gamepadButtonUnknown
                                in
                                { primaryButton = at .primary
                                , secondaryButton = at .secondary
                                , tertiaryButton = at .tertiary
                                , quaternaryButton = at .quaternary
                                , leftBumperButton = at .leftBumper
                                , rightBumperButton = at .rightBumper
                                , leftTriggerButton = at .leftTrigger
                                , rightTriggerButton = at .rightTrigger
                                , selectButton = at .select
                                , startButton = at .start
                                , leftThumbstickButton = at .leftThumbstickButton
                                , rightThumbstickButton = at .rightThumbstickButton
                                , upButton = at .arrowUp
                                , downButton = at .arrowDown
                                , leftButton = at .arrowLeft
                                , rightButton = at .arrowRight
                                , homeButton = at .homeButton
                                , touchpadButton = at .touchpad
                                , additionalButtons =
                                    let
                                        mappedButtonIndexes : Set Int
                                        mappedButtonIndexes =
                                            [ .primary, .secondary, .tertiary, .quaternary, .leftBumper, .rightBumper, .leftTrigger, .rightTrigger, .select, .start, .leftThumbstickButton, .rightThumbstickButton, .arrowUp, .arrowDown, .arrowLeft, .arrowRight, .homeButton, .touchpad ]
                                                |> List.filterMap (\field -> buttonMap |> field)
                                                |> Set.fromList
                                    in
                                    buttons
                                        |> List.LocalExtra.justsMapIndexed
                                            (\index button ->
                                                if mappedButtonIndexes |> Set.member index then
                                                    Nothing

                                                else
                                                    button |> Just
                                            )
                                , kindId = kindId
                                , thumbstickLeft = thumbsticks.left
                                , thumbstickRight = thumbsticks.right
                                , thumbsticksAdditional = thumbsticks.additional
                                }
                                    |> Just
                            )
                            (Json.Decode.field "id" Json.Decode.string)
                            (Json.Decode.field "buttons" (Json.Decode.list gamepadButtonJsonDecoder))
                            (Json.Decode.list Json.Decode.float
                                |> Json.Decode.field "axes"
                                |> Json.Decode.map
                                    (\axes ->
                                        case (axes |> gamepadThumbsticksFromAxes) |> listPadToAtLeast 2 gamepadThumbstickUnknown of
                                            left :: right :: additional ->
                                                { left = left
                                                , right = right
                                                , additional = additional
                                                }

                                            -- can't happen
                                            _ ->
                                                { left = gamepadThumbstickUnknown, right = gamepadThumbstickUnknown, additional = [] }
                                    )
                            )
                )
        )
        |> Json.Decode.map (Maybe.andThen identity)


gamepadButtonUnknown : GamepadButton
gamepadButtonUnknown =
    GamepadButtonReleased { isTouched = False }


gamepadThumbstickUnknown : { x : Float, y : Float }
gamepadThumbstickUnknown =
    { x = 0, y = 0 }


gamepadThumbsticksFromAxes : List Float -> List { x : Float, y : Float }
gamepadThumbsticksFromAxes =
    \axes ->
        case axes of
            x :: y :: rest ->
                { x = x, y = y } :: gamepadThumbsticksFromAxes rest

            _ ->
                []


gamepadButtonJsonDecoder : Json.Decode.Decoder GamepadButton
gamepadButtonJsonDecoder =
    Json.Decode.map3
        (\isPressed isTouched value ->
            if isPressed then
                GamepadButtonPressed { firmnessPercentage = value }

            else
                GamepadButtonReleased { isTouched = isTouched }
        )
        (Json.Decode.field "pressed" Json.Decode.bool)
        (Json.Decode.field "touched" Json.Decode.bool)
        (Json.Decode.field "value" Json.Decode.float)


listPadToAtLeast : Int -> a -> (List a -> List a)
listPadToAtLeast newMinimumSize paddingElement =
    \list ->
        list
            ++ List.repeat (newMinimumSize - (list |> List.length)) paddingElement


{-| using

  - <https://dark.elm.dmy.fr/packages/noordstar/elm-gamepad/latest>
  - <https://github.com/bluebitgame/GamepadServer/blob/6124d3d9a6d83f2efca21e53eb944c554b5755b5/web/gamepad.js#L9>
  - <https://github.com/bmsuseluda/emuze/blob/ecc0c35a3b7399142f550d06eba89e88e6ca3f01/app/hooks/useGamepads/tests/gamepadTypeMapping.test.ts#L36>
  - <https://github.com/beetrootpaul/beetpx/blob/2a5c38a0f5fbcd76efcd0d97e42566594c79eae9/src/game_input/gamepad_mapping/GamepadMapping8BitDo.ts>
  - <https://github.com/philschatz/puzzlescript/tree/5f1444f426691fd8e87a817bf5821b6c71742381/packages/puzzlescript-web/src/browser/controller/configs>
  - <https://github.com/bomblebees/bomblebees/tree/417156f942b7bd6bbee48f1f97b3611bf4db70d8/Assets/Plugins/InControl/Source/Unity/DeviceProfiles>
  - <https://github.com/ericlathrop/html5-gamepad/blob/master/mappings>

to consider adding

  - <https://github.com/electrovir/gamepad-type>
  - <https://github.com/STREGAsGate/GateEngine/blob/07bbc90668fafe211830b535128eee75a98aef3f/Sources/GateEngine/System/HID/GamePad/GamePadInterpreter/Interpreters/WASIGamePadInterpreter.swift#L58>
  - <https://github.com/mdqinc/SDL_GameControllerDB>

On many other models I didn't find good examples so I don't feel confident enough giving a mapping

-}
buttonMapping : FastDict.Dict String GamepadButtonMap
buttonMapping =
    FastDict.empty
        |> fastDictInsertSameValueFor
            [ "xinput", "Wireless Controller (STANDARD GAMEPAD)" ]
            gamepadStandardButtonMap
        |> fastDictInsertSameValueFor
            [ "Xbox 360 Controller (XInput STANDARD GAMEPAD)"
            , "Wireless Controller (STANDARD GAMEPAD Vendor: 054c Product: 05c4)"
            , "Xbox Wireless Controller (STANDARD GAMEPAD Vendor: 045e Product: 02fd)"
            , "HID-compliant game controller (STANDARD GAMEPAD Vendor: 045e Product: 02fd)"
            ]
            { gamepadStandardButtonMap | touchpad = Nothing }
        |> fastDictInsertSameValueFor
            [ "Xbox Wireless Controller Extended Gamepad"
            , "Unknown Gamepad (Vendor: beef Product: 046d)"
            ]
            gamepadStandardButtonMap
        |> fastDictInsertSameValueFor
            [ "054c-05c4-Wireless Controller", "Wireless Controller (STANDARD GAMEPAD Vendor: 054c Product: 05c4)" ]
            gamepadStandardButtonMap
        |> fastDictInsertSameValueFor
            [ "45e-28e-Xbox 360 Wired Controller" ]
            { primary = Just 11
            , secondary = Just 12
            , tertiary = Just 13
            , quaternary = Just 14
            , leftBumper = Just 8
            , rightBumper = Just 9
            , touchpad = Nothing
            , select = Just 5
            , start = Just 4
            , arrowUp = Just 0
            , arrowDown = Just 1
            , arrowLeft = Just 2
            , arrowRight = Just 3
            , leftThumbstickButton = Just 6
            , rightThumbstickButton = Just 7
            , homeButton = Just 10

            -- these are only analog
            , leftTrigger = Nothing
            , rightTrigger = Nothing
            }
        |> fastDictInsertSameValueFor
            [ "0810-0001- USB Gamepad          ", " USB Gamepad           (Vendor: 0810 Product: 0001)" ]
            { gamepadStandardButtonMap
                | primary = Just 2
                , tertiary = Just 3
                , quaternary = Just 0
                , touchpad = Nothing
            }
        |> fastDictInsertSameValueFor
            [ "0810-e501-usb gamepad           ", "usb gamepad            (Vendor: 0810 Product: e501)" ]
            { gamepadStandardButtonMap
                | primary = Just 1
                , secondary = Just 2
                , tertiary = Just 0
                , leftTrigger = Nothing
                , rightTrigger = Nothing
                , leftThumbstickButton = Nothing
                , rightThumbstickButton = Nothing
                , arrowUp = Nothing
                , arrowLeft = Nothing
                , arrowRight = Nothing
                , arrowDown = Nothing
                , homeButton = Nothing
            }
        -- nintendo style
        |> fastDictInsertSameValueFor
            [ "057e-2009-Pro Controller"
            , "Pro Controller (STANDARD GAMEPAD Vendor: 057e Product: 2009)"
            , "057e-2009-Pro Wireless Gamepad"
            , "Wireless Gamepad (STANDARD GAMEPAD Vendor: 057e Product: 2009)"
            ]
            { gamepadStandardButtonMap
                | primary = Just 1
                , secondary = Just 0
                , quaternary = Just 2
            }
        |> fastDictInsertSameValueFor
            [ "Pro Controller Extended Gamepad" ]
            { gamepadStandardButtonMap
                | primary = Just 1
                , secondary = Just 0
                , quaternary = Just 2
            }
        -- dualsense style
        -- too unclear to me how arrows and dpad are interpreted.
        -- Like, some different button indexes belong to the same button output.
        -- And some button values seem to be mixed up in the axis values
        --     [ "54c-ce6-DualSense Wireless Controller"
        --     , "DualSense Wireless Controller (STANDARD GAMEPAD Vendor: 054c Product: 0ce6)"
        --     , "054c-0ce6-Wireless Controller"
        --     , "Wireless Controller (STANDARD GAMEPAD Vendor: 054c Product: 0ce6)"
        --     ]
        -- 8BitDo style
        -- too unclear to me how arrows and dpad are interpreted. Some button values seem to be mixed up in the axis values
        --    [ "2dc8-5112-8BitDo Lite 2"
        --    , "8BitDo Lite 2 (Vendor: 2dc8 Product: 5112)"
        --    , "2dc8-5112-Bluetooth Wireless Controller   "
        --    , "Bluetooth Wireless Controller    (Vendor: 2dc8 Product: 5112)"
        --    , "8BitDo Lite 2 Extended Gamepad"
        --    ]
        --
        -- playstation 3 style
        |> fastDictInsertSameValueFor
            [ "54c-268-PLAYSTATION(R)3 Controller"
            , "PS3 GamePad (Vendor: 054c Product: 0268)"
            ]
            { primary = Just 14
            , secondary = Just 13
            , tertiary = Just 15
            , quaternary = Just 12
            , leftBumper = Just 10
            , rightBumper = Just 11
            , leftTrigger = Just 8
            , rightTrigger = Just 9
            , select = Just 0
            , start = Just 3
            , leftThumbstickButton = Just 1
            , rightThumbstickButton = Just 2
            , arrowUp = Just 4
            , arrowDown = Just 6
            , arrowLeft = Just 7
            , arrowRight = Just 5
            , homeButton = Nothing
            , touchpad = Nothing
            }
        -- playstation 4 style
        |> fastDictInsertSameValueFor
            [ "54c-9cc-Wireless Controller" ]
            { primary = Just 1
            , secondary = Just 2
            , tertiary = Just 0
            , quaternary = Just 3
            , leftBumper = Just 4
            , rightBumper = Just 5
            , leftTrigger = Just 6
            , rightTrigger = Just 7
            , select = Just 8
            , start = Just 9
            , leftThumbstickButton = Just 10
            , rightThumbstickButton = Just 11
            , arrowUp = Just 14
            , arrowDown = Just 15
            , arrowLeft = Just 16
            , arrowRight = Just 17
            , homeButton = Just 12
            , touchpad = Just 13
            }
        -- logi style
        |> fastDictInsertSameValueFor
            [ "046d- c216-Logitech Dual Action"
            , "Logitech Dual Action (STANDARD GAMEPAD Vendor: 046d Product: c216)"
            ]
            { primary = Just 0
            , secondary = Just 1
            , tertiary = Just 2
            , quaternary = Just 3
            , leftBumper = Just 4
            , rightBumper = Just 5
            , leftTrigger = Just 6
            , rightTrigger = Just 7
            , select = Just 8
            , start = Just 9
            , leftThumbstickButton = Just 10
            , rightThumbstickButton = Just 11
            , arrowUp = Just 12
            , arrowDown = Just 13
            , arrowLeft = Just 14
            , arrowRight = Just 15
            , homeButton = Nothing
            , touchpad = Nothing
            }


fastDictInsertSameValueFor : List comparableKey -> value -> (FastDict.Dict comparableKey value -> FastDict.Dict comparableKey value)
fastDictInsertSameValueFor keyList value =
    \dict ->
        keyList |> List.foldl (\key dictSoFar -> dictSoFar |> FastDict.insert key value) dict


listAtIndex : Int -> (List a -> Maybe a)
listAtIndex index sticks =
    case sticks of
        [] ->
            Nothing

        head :: tail ->
            if index <= 0 then
                head |> Just

            else
                listAtIndex (index - 1) tail


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


httpSuccessResponseJsonDecoder : HttpExpect future -> Json.Decode.Decoder future
httpSuccessResponseJsonDecoder expect =
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


httpErrorJsonDecoder : Json.Decode.Decoder HttpError
httpErrorJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map (\() -> HttpBadUrl)
            (Json.Decode.field "cause"
                (Json.Decode.field "code" (Json.Decode.LocalExtra.onlyString "BAD_URL"))
            )
        , Json.Decode.succeed HttpNetworkError
        ]


windowVisibilityJsonDecoder : Json.Decode.Decoder WindowVisibility
windowVisibilityJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map (\() -> WindowShown) (Json.Decode.LocalExtra.onlyString "visible")
        , Json.Decode.map (\() -> WindowHidden) (Json.Decode.LocalExtra.onlyString "hidden")
        ]


{-| Controller information on button presses, thumbstick positions etc.

  - `primaryButton`: The most common action like "enter"/"confirm" or jump

  - `secondaryButton`: Usually "cancel"/"skip" or a common ability like duck/sneak, drop or heal

  - `tertiaryButton`: common-ish ability like consume, interact, reload or an ultimate power

  - `quaternaryButton`: less common action like quick menu or mode switch

  - `leftBumperButton`, `rightBumperButton`: The top row of smaller buttons on the side opposite of you, sometimes called shoulder buttons.
    Often used for alternative menu actions like slot switching or quick map

  - `leftTriggerButton`, `rightTriggerButton`: The bottom row of larger buttons on the side opposite of you.
    Often used for holdable abilities like sliding or dashing

  - `selectButton`: Usually opens an in-world menu with for example inventory, lore, ability overview or a map

  - `startButton`: Usually pauses the game and opens a menu with options like settings and quit

  - `leftThumbstickButton`, `rightThumbstickButton`: Not all gamepads have these, so they often either double existing actions like "confirm"
    or perform actions that are only very rarely helpful, like hiding ui elements or making a screenshot

  - `upButton`, `downBottom`, `leftButton`, `rightButton`: exactly one step in a direction, usually in a (quick) menu/inventory

  - `homeButton`: Usually turns the gamepad on/off, or changes the state of the game

  - `touchpadButton`: Not present on most gamepads. While the touchpad is often used for controlling the mouse, it can also be used as a simple button

  - `thumbstickLeft`, `thumbstickRight`: Those wiggly that can be moved in any direction by any amount.
    They are provided as `x, y` signed percentages

  - `kindId`: some information about the gamepad, usually containing the USB vendor, product id of the gamepad
    and the name of the gamepad as provided by the driver. See [mdn](https://developer.mozilla.org/en-US/docs/Web/API/Gamepad/id)

    You can use this information to for example determine how to show controls

  - `buttonsAdditional`, `thumbsticksAdditional`: Maybe you have a weird gamepad with 3 thumbsticks? These might help 🤷

Implementation note:
As you know, gamepad layouts differ between models.
For most of them, we're able to map them to the buttons and thumbsticks above.
If you experience issues with some model, [open an issue]()

-}
type alias Gamepad =
    RecordWithoutConstructorFunction
        { primaryButton : GamepadButton
        , secondaryButton : GamepadButton
        , tertiaryButton : GamepadButton
        , quaternaryButton : GamepadButton
        , leftBumperButton : GamepadButton
        , rightBumperButton : GamepadButton
        , leftTriggerButton : GamepadButton
        , rightTriggerButton : GamepadButton
        , selectButton : GamepadButton
        , startButton : GamepadButton
        , leftThumbstickButton : GamepadButton
        , rightThumbstickButton : GamepadButton
        , upButton : GamepadButton
        , downButton : GamepadButton
        , leftButton : GamepadButton
        , rightButton : GamepadButton
        , homeButton : GamepadButton
        , touchpadButton : GamepadButton
        , additionalButtons : List GamepadButton
        , kindId : String
        , thumbstickLeft : { x : Float, y : Float }
        , thumbstickRight : { x : Float, y : Float }
        , thumbsticksAdditional : List { x : Float, y : Float }
        }


{-| Buttons are either held down with an optional value between 0 and 1 to measure how hard,
or they are released with an optional detection of touch which defaults to false.
-}
type GamepadButton
    = GamepadButtonPressed { firmnessPercentage : Float }
    | GamepadButtonReleased { isTouched : Bool }


type alias GamepadButtonMap =
    RecordWithoutConstructorFunction
        { primary : Maybe Int
        , secondary : Maybe Int
        , tertiary : Maybe Int
        , quaternary : Maybe Int
        , start : Maybe Int
        , select : Maybe Int
        , leftThumbstickButton : Maybe Int
        , rightThumbstickButton : Maybe Int
        , arrowUp : Maybe Int
        , arrowLeft : Maybe Int
        , arrowRight : Maybe Int
        , arrowDown : Maybe Int
        , leftTrigger : Maybe Int
        , leftBumper : Maybe Int
        , rightTrigger : Maybe Int
        , rightBumper : Maybe Int
        , homeButton : Maybe Int
        , touchpad : Maybe Int
        }


{-| The "update" part for an embedded program
-}
programUpdate : ProgramConfig state -> (ProgramEvent state -> ProgramState state -> ( ProgramState state, Cmd (ProgramEvent state) ))
programUpdate appConfig =
    \event ->
        case event of
            JsEventFailedToDecode jsonError ->
                \state ->
                    ( state
                    , let
                        notifyOfBugInterface : InterfaceSingle ()
                        notifyOfBugInterface =
                            ([ "bug: js event failed to decode: "
                             , jsonError |> Json.Decode.errorToString
                             , ". Please open an issue on github.com/lue-bird/elm-state-interface"
                             ]
                                |> String.concat
                            )
                                |> ConsoleError
                      in
                      { id = notifyOfBugInterface |> interfaceSingleToStructuredId |> StructuredId.toString
                      , diff = notifyOfBugInterface |> Add
                      }
                        |> toJsToJson
                        |> appConfig.ports.toJs
                        |> Cmd.map never
                    )

            JsEventEnabledConstructionOfNewAppState updatedAppState ->
                \(State oldState) ->
                    let
                        updatedInterface : FastDict.Dict String (InterfaceSingle state)
                        updatedInterface =
                            updatedAppState |> appConfig.interface |> interfacesFromRope
                    in
                    ( State { interface = updatedInterface, appState = updatedAppState }
                    , { old = oldState.interface, updated = updatedInterface }
                        |> interfacesDiff
                        |> List.map (\diff -> appConfig.ports.toJs (diff |> toJsToJson))
                        |> Cmd.batch
                        |> Cmd.map never
                    )


{-| A Request can fail in a couple ways:

  - `BadUrl` means you did not provide a valid URL
  - `NetworkError` means the user turned off their wifi, went in a cave, etc.
    or the server CORS is misconfigured.
    Note: A 404 for example does not constitute a network error
  - `BadStatus` means you got a response back, but the status code indicates failure. Contains:
      - The response `Metadata`.
      - The raw response body as a `Json.Decode.Value`.

-}
type HttpError
    = HttpBadUrl
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


{-| Individual message to js to sync up with the latest interface type
-}
type InterfaceSingleDiff
    = Add (InterfaceSingle ())
    | Edit InterfaceSingleEdit
    | Remove


{-| Individual message to js to sync up with the latest interface type,
describing changes to an existing interface with the same identity
-}
type InterfaceSingleEdit
    = EditDom { path : List Int, replacement : DomEdit }
    | EditAudio { url : String, startTime : Time.Posix, replacement : AudioEdit }
    | EditNotification { id : String, message : String, details : String }


{-| What parts of an [`Audio`](#Audio) are replaced
-}
type AudioEdit
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


{-| What parts of a node are replaced. Either all modifiers of a certain kind or the whole node
-}
type DomEdit
    = ReplacementDomNode (DomTextOrElementHeader ())
    | ReplacementDomElementStyles { edit : List { key : String, value : String }, remove : List String }
    | ReplacementDomElementAttributes { edit : List { key : String, value : String }, remove : List String }
    | ReplacementDomElementAttributesNamespaced { edit : List { namespace : String, key : String, value : String }, remove : List { namespace : String, key : String } }
    | ReplacementDomElementStringProperties { edit : List { key : String, value : String }, remove : List String }
    | ReplacementDomElementBoolProperties { edit : List { key : String, value : Bool }, remove : List String }
    | ReplacementDomElementScrollToPosition (Maybe { fromLeft : Float, fromTop : Float })
    | ReplacementDomElementScrollToShow (Maybe { x : DomElementVisibilityAlignment, y : DomElementVisibilityAlignment })
    | ReplacementDomElementScrollPositionRequest
    | ReplacementDomElementEventListens (Dict String DefaultActionHandling)


{-| Create a [`Program`](#Program):

  - The state is everything the program knows (what The Elm Architecture calls model).
    And it always starts with a given `initialState`

  - The [`Interface`](#Interface) is the face to the outside world
    and can be created using the helpers in [`Web.Dom`](Web-Dom), [`Web.Time`](Web-Time), [`Web.Http`](Web-Http) etc.
    The given `interface` function constructs these interfaces based on the current state

  - Connections to and from js

        port toJs : Json.Encode.Value -> Cmd event_

        port fromJs : (Json.Encode.Value -> event) -> Sub event

-}
program :
    { initialState : state
    , interface : state -> Interface state
    , ports :
        { toJs : Json.Encode.Value -> Cmd Never
        , fromJs : (Json.Encode.Value -> ProgramEvent state) -> Sub (ProgramEvent state)
        }
    }
    -> Program state
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
