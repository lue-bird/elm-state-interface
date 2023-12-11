module Web exposing
    ( ProgramConfig
    , program, ProgramState(..), ProgramEvent(..)
    , programInit, programUpdate, programSubscriptions
    , Interface, interfaceBatch, interfaceNone, interfaceMap
    , DomNode(..), DomElement, DefaultActionHandling(..)
    , Audio, AudioSource, AudioSourceLoadError(..), AudioVolumeTimeline, EditAudioDiff(..)
    , HttpRequest, HttpHeader, HttpBody(..), HttpExpect(..), HttpError(..), HttpMetadata
    , InterfaceSingle(..), InterfaceSingleWithReceive(..), InterfaceSingleWithoutReceive(..)
    , InterfaceDiff(..), InterfaceWithReceiveDiff(..), InterfaceWithoutReceiveDiff(..), EditDomDiff, ReplacementInEditDomDiff(..)
    , InterfaceSingleKeys, InterfaceSingleIdOrderTag
    , InterfaceSingleId(..), InterfaceSingleWithReceiveId(..), InterfaceSingleToIdTag, DomElementId, DomNodeId(..), HttpRequestId, HttpExpectId(..)
    )

{-| A state-interface program running in the browser

@docs ProgramConfig


## as elm/browser Program

@docs program, ProgramState, ProgramEvent


## embed

If you just want to replace a part of your elm app with this architecture. Make sure to wire in all 3:

@docs programInit, programUpdate, programSubscriptions


# interface types

@docs Interface, interfaceBatch, interfaceNone, interfaceMap


## DOM

Types used by [`Web.Dom`](Web-Dom)

@docs DomNode, DomElement, DefaultActionHandling


## Audio

@docs Audio, AudioSource, AudioSourceLoadError, AudioVolumeTimeline, EditAudioDiff


## HTTP

Types used by [`Web.Http`](Web-Http)

@docs HttpRequest, HttpHeader, HttpBody, HttpExpect, HttpError, HttpMetadata


## internals, safe to ignore

@docs InterfaceSingle, InterfaceSingleWithReceive, InterfaceSingleWithoutReceive
@docs InterfaceDiff, InterfaceWithReceiveDiff, InterfaceWithoutReceiveDiff, EditDomDiff, ReplacementInEditDomDiff
@docs InterfaceSingleKeys, InterfaceSingleIdOrderTag
@docs InterfaceSingleId, InterfaceSingleWithReceiveId, InterfaceSingleToIdTag, DomElementId, DomNodeId, HttpRequestId, HttpExpectId

-}

import AndOr exposing (AndOr)
import AppUrl exposing (AppUrl)
import Array exposing (Array)
import Dict exposing (Dict)
import Duration exposing (Duration)
import Emptiable exposing (Emptiable)
import Json.Decode
import Json.Decode.Local
import Json.Encode
import Keys exposing (Key, Keys)
import KeysSet exposing (KeysSet)
import Map exposing (Mapping)
import N exposing (N1)
import Or
import Order exposing (Ordering)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Rope exposing (Rope)
import Set exposing (Set)
import Time
import Typed
import Url exposing (Url)


{-| Ignore the specific fields, this is just exposed so can annotate a program state like in

    main : Program () (Web.State YourState) (Web.Event YourState)
    main =
        Web.toProgram ...

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

  - `state` is what elm calls the model
  - An [`Interface`](#Interface) can be created using the helpers in `Web.Time`, `Web.Dom`, `Web.Http` etc.

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
To create one, use the helpers in `Web.Time`, `.Dom`, `.Http` etc.

To combine multiple, use [`Web.interfaceBatch`](#interfaceBatch) and [`Web.interfaceNone`](#interfaceNone)

-}
type alias Interface state =
    Rope (InterfaceSingle state)


{-| An "non-batched" [`Interface`](#Interface).
To create one, use the helpers in `Web.Time`, `.Dom`, `.Http` etc.
-}
type InterfaceSingle state
    = InterfaceWithReceive (InterfaceSingleWithReceive state)
    | InterfaceWithoutReceive InterfaceSingleWithoutReceive


{-| An [`InterfaceSingle`](#InterfaceSingle) that will never notify elm
-}
type InterfaceSingleWithoutReceive
    = ConsoleLog String
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


{-| These are possible errors we can get when loading an audio source file.

  - `AudioSourceLoadDecodeError`: This means we got the data but we couldn't decode it. One likely reason for this is that your url points to the wrong place and you're trying to decode a 404 page instead.
  - `AudioSourceLoadNetworkError`: We couldn't reach the url. Either it's some kind of CORS issue, the server is down, or you're disconnected from the internet.
  - `AudioSourceLoadUnknownError`: the audio source didn't for some other reason!

-}
type AudioSourceLoadError
    = AudioSourceLoadDecodeError
    | AudioSourceLoadNetworkError
    | AudioSourceLoadUnknownError String


{-| An [`InterfaceSingle`](#InterfaceSingle) that will notify elm some time in the future.
-}
type InterfaceSingleWithReceive state
    = TimePosixRequest (Time.Posix -> state)
    | TimezoneOffsetRequest (Int -> state)
    | TimezoneNameRequest (Time.ZoneName -> state)
    | TimePeriodicallyListen { intervalDurationMilliSeconds : Int, on : Time.Posix -> state }
    | RandomUnsignedInt32sRequest { count : Int, on : List Int -> state }
    | DocumentEventListen { eventName : String, on : Json.Decode.Decoder state }
    | DomNodeRender (DomNode state)
    | HttpRequest (HttpRequest state)
    | WindowSizeRequest ({ width : Int, height : Int } -> state)
    | WindowEventListen { eventName : String, on : Json.Decode.Decoder state }
    | WindowAnimationFrameListen (Time.Posix -> state)
    | NavigationUrlRequest (AppUrl -> state)
    | ClipboardRequest (String -> state)
    | AudioSourceLoad { url : String, on : Result AudioSourceLoadError AudioSource -> state }


{-| An HTTP request for use in an [`Interface`](#Interface).

You can set custom headers as needed.
The `timeout` can be set to a number of milliseconds you are willing to wait before giving up

-}
type alias HttpRequest state =
    RecordWithoutConstructorFunction
        { url : String
        , method : String
        , headers : List HttpHeader
        , body : HttpBody
        , expect : HttpExpect state
        , timeout : Maybe Int
        }


{-| An Http header for configuring a request.
-}
type alias HttpHeader =
    ( String, String )


{-| Describe what you expect to be returned in an http response body.
-}
type HttpExpect state
    = HttpExpectString (Result HttpError String -> state)
    | HttpExpectWhatever (Result HttpError () -> state)


{-| Data send in your http request.

  - `HttpBodyEmpty`: Create an empty body for your request.
    This is useful for `GET` requests and `POST` requests where you are not sending any data.

  - `HttpBodyString`: Put a `String` in the body of your request. Defining `Web.Http.jsonBody` looks like this:

        import Json.Encode

        jsonBody : Json.Encode.Value -> Web.HttpBody
        jsonBody value =
            Web.HttpBodyString "application/json" (Json.Encode.encode 0 value)

    The first argument is a [MIME type](https://en.wikipedia.org/wiki/Media_type) of the body.

-}
type HttpBody
    = HttpBodyEmpty
    | HttpBodyString { mimeType : String, content : String }


{-| Safe to ignore. Identifier for an [`HttpRequest`](#HttpRequest)
-}
type alias HttpRequestId =
    RecordWithoutConstructorFunction
        { url : String
        , method : String
        , headers : List HttpHeader
        , body : HttpBody
        , expect : HttpExpectId
        , timeout : Maybe Int
        }


{-| Safe to ignore. Identifier for an [`HttpExpect`](#HttpExpect)
-}
type HttpExpectId
    = IdHttpExpectString
    | IdHttpExpectWhatever


{-| Plain text or a [`DomElement`](#DomElement) for use in an [`Interface`](#Interface).
-}
type DomNode state
    = DomText String
    | DomElement (DomElement state)


{-| A tagged DOM node that can itself contain child [node](#DomNode)s
-}
type alias DomElement state =
    RecordWithoutConstructorFunction
        { namespace : Maybe String
        , tag : String
        , styles : Dict String String
        , attributes : Dict String String
        , attributesNamespaced : Dict ( String, String ) String
        , eventListens :
            Dict
                String
                { on : Json.Decode.Value -> state
                , defaultActionHandling : DefaultActionHandling
                }
        , subs : Array (DomNode state)
        }


{-| Setting for a listen [`Web.Dom.Modifier`](Web-Dom#Modifier) to keep or overwrite the browsers default action.
-}
type DefaultActionHandling
    = DefaultActionPrevent
    | DefaultActionExecute


{-| Safe to ignore. Identifier for an [`Interface`](#Interface)
-}
type InterfaceSingleId
    = IdInterfaceWithReceive InterfaceSingleWithReceiveId
    | IdInterfaceWithoutReceive InterfaceSingleWithoutReceive


{-| Safe to ignore. Identifier for an [`InterfaceSingleWithReceive`](#InterfaceSingleWithReceive)
-}
type InterfaceSingleWithReceiveId
    = IdTimePosixRequest
    | IdTimezoneOffsetRequest
    | IdTimezoneNameRequest
    | IdTimePeriodicallyListen { milliSeconds : Int }
    | IdRandomUnsignedInt32sRequest Int
    | IdDomNodeRender
    | IdHttpRequest HttpRequestId
    | IdWindowSizeRequest
    | IdWindowEventListen String
    | IdWindowAnimationFrameListen
    | IdNavigationUrlRequest
    | IdDocumentEventListen String
    | IdClipboardRequest
    | IdAudioSourceLoad String


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


{-| Combine multiple [`Interface`](#Interface)s into one.
-}
interfaceBatch : List (Interface state) -> Interface state
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
interfaceNone : Interface state_
interfaceNone =
    Rope.empty


{-| Map the state constructed by the [`Interface`](#Interface).

In practice, this is sometimes used like a kind of event-config pattern:

    Web.Time.posixRequest
        |> Web.interfaceMap (\timeNow -> TimeReceived timeNow)

sometimes like elm's `update`

    ...
        |> Web.interfaceMap
            (\event ->
                case event of
                    MouseMovedTo newMousePoint ->
                        { state | mousePoint = newMousePoint }

                    CounterDecreaseClicked ->
                        { state | counter = state.counter - 1 }

                    CounterIncreaseClicked ->
                        { state | counter = state.counter + 1 }
            )

and sometimes like elm's `Cmd.map/Task.map/Sub.map/...`:

    type State
        = MenuState Menu.State
        | PlayingState Playing.State

    interface : State -> Interface State
    interface state =
        case state of
            MenuState menuState ->
                Web.interfaceMap MenuState (Menu.interface menuState)

            PlayingState playingState ->
                Web.interfaceMap PlayingState (Playing.interface playingState)

In all these examples, you end up converting the narrow state representation of part of the interface to a broader representation for
the parent interface

-}
interfaceMap : (state -> mappedState) -> (Interface state -> Interface mappedState)
interfaceMap stateChange =
    \interface ->
        interface
            |> Rope.toList
            |> List.map
                (\interfaceSingle ->
                    interfaceSingle |> interfaceSingleMap stateChange
                )
            |> Rope.fromList


interfaceSingleMap : (state -> mappedState) -> (InterfaceSingle state -> InterfaceSingle mappedState)
interfaceSingleMap stateChange =
    \interface ->
        case interface of
            InterfaceWithoutReceive interfaceWithoutReceive ->
                interfaceWithoutReceive |> InterfaceWithoutReceive

            InterfaceWithReceive interfaceWithReceive ->
                interfaceWithReceive
                    |> interfaceWithReceiveMap stateChange
                    |> InterfaceWithReceive


domNodeMap : (state -> mappedState) -> (DomNode state -> DomNode mappedState)
domNodeMap stateChange =
    \domElementToMap ->
        case domElementToMap of
            DomText text ->
                DomText text

            DomElement domElement ->
                domElement |> domElementMap stateChange |> DomElement


interfaceWithReceiveMap : (state -> mappedState) -> (InterfaceSingleWithReceive state -> InterfaceSingleWithReceive mappedState)
interfaceWithReceiveMap stateChange =
    \interface ->
        case interface of
            TimePosixRequest requestTimeNow ->
                (\event -> requestTimeNow event |> stateChange)
                    |> TimePosixRequest

            TimezoneOffsetRequest requestTimezone ->
                (\event -> requestTimezone event |> stateChange)
                    |> TimezoneOffsetRequest

            TimezoneNameRequest requestTimezoneName ->
                (\event -> requestTimezoneName event |> stateChange)
                    |> TimezoneNameRequest

            TimePeriodicallyListen timePeriodicallyListen ->
                { intervalDurationMilliSeconds = timePeriodicallyListen.intervalDurationMilliSeconds
                , on = \posix -> timePeriodicallyListen.on posix |> stateChange
                }
                    |> TimePeriodicallyListen

            RandomUnsignedInt32sRequest randomUnsignedInt32sRequest ->
                { count = randomUnsignedInt32sRequest.count
                , on = \ints -> randomUnsignedInt32sRequest.on ints |> stateChange
                }
                    |> RandomUnsignedInt32sRequest

            DomNodeRender domElementToRender ->
                domElementToRender |> domNodeMap stateChange |> DomNodeRender

            HttpRequest httpRequest ->
                httpRequest
                    |> httpRequestMap stateChange
                    |> HttpRequest

            WindowSizeRequest toState ->
                (\event -> toState event |> stateChange)
                    |> WindowSizeRequest

            WindowEventListen listen ->
                { eventName = listen.eventName, on = listen.on |> Json.Decode.map stateChange }
                    |> WindowEventListen

            WindowAnimationFrameListen toState ->
                (\event -> toState event |> stateChange) |> WindowAnimationFrameListen

            NavigationUrlRequest toState ->
                (\event -> toState event |> stateChange) |> NavigationUrlRequest

            DocumentEventListen listen ->
                { eventName = listen.eventName, on = listen.on |> Json.Decode.map stateChange }
                    |> DocumentEventListen

            ClipboardRequest toState ->
                (\event -> toState event |> stateChange) |> ClipboardRequest

            AudioSourceLoad load ->
                { url = load.url, on = \event -> load.on event |> stateChange } |> AudioSourceLoad


httpRequestMap : (state -> mappedState) -> (HttpRequest state -> HttpRequest mappedState)
httpRequestMap stateChange =
    \httpRequest ->
        { url = httpRequest.url
        , method = httpRequest.method
        , headers = httpRequest.headers
        , body = httpRequest.body
        , timeout = httpRequest.timeout
        , expect =
            case httpRequest.expect of
                HttpExpectWhatever expectWhatever ->
                    (\unit -> expectWhatever unit |> stateChange) |> HttpExpectWhatever

                HttpExpectString expectString ->
                    (\string -> expectString string |> stateChange) |> HttpExpectString
        }


domElementMap : (state -> mappedState) -> (DomElement state -> DomElement mappedState)
domElementMap stateChange =
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
                        { on = \event -> listen.on event |> stateChange
                        , defaultActionHandling = listen.defaultActionHandling
                        }
                    )
        , subs =
            domElementToMap.subs |> Array.map (domNodeMap stateChange)
        }


{-| Ignore the specific variants, this is just exposed so can annotate a program event like in

    main : Program () (Web.State YourState) (Web.Event YourState)
    main =
        Web.toProgram ...

-}
type ProgramEvent appState
    = InterfaceDiffFailedToDecode Json.Decode.Error
    | InterfaceEventDataFailedToDecode Json.Decode.Error
    | InterfaceEventIgnored
    | AppEventToNewAppState appState


domElementToId : DomElement state_ -> DomElementId
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


interfaceKeys : Keys (InterfaceSingle state) (InterfaceSingleKeys state) N1
interfaceKeys =
    Keys.oneBy interfaceToIdMapping interfaceIdOrder


interfaceToIdMapping : Mapping (InterfaceSingle state_) InterfaceSingleToIdTag InterfaceSingleId
interfaceToIdMapping =
    Map.tag InterfaceSingleToIdTag interfaceSingleToId


interfaceSingleToId : InterfaceSingle state_ -> InterfaceSingleId
interfaceSingleToId =
    \interface ->
        case interface of
            InterfaceWithoutReceive interfaceWithoutReceive ->
                interfaceWithoutReceive |> IdInterfaceWithoutReceive

            InterfaceWithReceive interfaceWithReceive ->
                interfaceWithReceive |> interfaceWithReceiveToId |> IdInterfaceWithReceive


httpRequestToId : HttpRequest state_ -> HttpRequestId
httpRequestToId =
    \httpRequest ->
        { url = httpRequest.url
        , method = httpRequest.method |> String.toUpper
        , headers = httpRequest.headers
        , body = httpRequest.body
        , expect = httpRequest.expect |> httpExpectToId
        , timeout = httpRequest.timeout
        }


httpExpectToId : HttpExpect state_ -> HttpExpectId
httpExpectToId =
    \httpExpect ->
        case httpExpect of
            HttpExpectWhatever _ ->
                IdHttpExpectWhatever

            HttpExpectString _ ->
                IdHttpExpectString


interfaceWithReceiveToId : InterfaceSingleWithReceive state_ -> InterfaceSingleWithReceiveId
interfaceWithReceiveToId =
    \interfaceWithReceive ->
        case interfaceWithReceive of
            TimePosixRequest _ ->
                IdTimePosixRequest

            TimezoneOffsetRequest _ ->
                IdTimezoneOffsetRequest

            TimezoneNameRequest _ ->
                IdTimezoneNameRequest

            TimePeriodicallyListen timePeriodicallyListen ->
                IdTimePeriodicallyListen
                    { milliSeconds = timePeriodicallyListen.intervalDurationMilliSeconds }

            RandomUnsignedInt32sRequest randomUnsignedInt32sRequest ->
                IdRandomUnsignedInt32sRequest randomUnsignedInt32sRequest.count

            DomNodeRender _ ->
                IdDomNodeRender

            HttpRequest httpRequest ->
                httpRequest |> httpRequestToId |> IdHttpRequest

            WindowSizeRequest _ ->
                IdWindowSizeRequest

            WindowEventListen listen ->
                IdWindowEventListen listen.eventName

            WindowAnimationFrameListen _ ->
                IdWindowAnimationFrameListen

            NavigationUrlRequest _ ->
                IdNavigationUrlRequest

            DocumentEventListen listen ->
                IdDocumentEventListen listen.eventName

            ClipboardRequest _ ->
                IdClipboardRequest

            AudioSourceLoad load ->
                IdAudioSourceLoad load.url


interfaceIdOrder : Ordering InterfaceSingleId InterfaceSingleIdOrderTag
interfaceIdOrder =
    Typed.tag InterfaceSingleIdOrderTag
        (\( a, b ) -> ( a |> interfaceSingleIdToComparable, b |> interfaceSingleIdToComparable ) |> comparableOrder)


interfaceSingleIdToComparable : InterfaceSingleId -> Comparable
interfaceSingleIdToComparable =
    \interfaceId ->
        case interfaceId of
            IdInterfaceWithoutReceive interfaceWithoutReceive ->
                interfaceWithoutReceive |> interfaceWithoutReceiveToComparable

            IdInterfaceWithReceive idInterfaceWithoutReceive ->
                idInterfaceWithoutReceive |> idInterfaceWithoutReceiveToComparable


intToComparable : Int -> Comparable
intToComparable =
    \int -> int |> String.fromInt |> ComparableString


idInterfaceWithoutReceiveToComparable : InterfaceSingleWithReceiveId -> Comparable
idInterfaceWithoutReceiveToComparable =
    \idInterfaceWithoutReceive ->
        case idInterfaceWithoutReceive of
            IdTimePosixRequest ->
                ComparableString "IdTimePosixRequest"

            IdTimezoneOffsetRequest ->
                ComparableString "IdTimezoneOffsetRequest"

            IdTimezoneNameRequest ->
                ComparableString "IdTimezoneNameRequest"

            IdTimePeriodicallyListen intervalDuration ->
                ComparableList
                    [ ComparableString "IdTimePeriodicallyListen"
                    , intervalDuration.milliSeconds |> intToComparable
                    ]

            IdRandomUnsignedInt32sRequest count ->
                ComparableList
                    [ ComparableString "IdRandomUnsignedInt32sRequest"
                    , count |> intToComparable
                    ]

            IdDomNodeRender ->
                ComparableString "IdDomNodeRender"

            IdHttpRequest request ->
                ComparableList
                    [ ComparableString "IdHttpRequest"
                    , request |> httpRequestIdToComparable
                    ]

            IdWindowSizeRequest ->
                ComparableString "IdWindowSizeRequest"

            IdWindowEventListen eventName ->
                ComparableList
                    [ ComparableString "IdWindowEventListen"
                    , ComparableString eventName
                    ]

            IdWindowAnimationFrameListen ->
                ComparableString "IdWindowAnimationFrameListen"

            IdNavigationUrlRequest ->
                ComparableString "IdNavigationUrlRequest"

            IdDocumentEventListen eventName ->
                ComparableList
                    [ ComparableString "IdDocumentEventListen"
                    , ComparableString eventName
                    ]

            IdClipboardRequest ->
                ComparableString "IdClipboardRequest"

            IdAudioSourceLoad url ->
                ComparableList
                    [ ComparableString "IdAudioSourceLoad"
                    , ComparableString url
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


httpHeaderToComparable : HttpHeader -> Comparable
httpHeaderToComparable =
    \( httpHeaderName, httpHeaderValue ) ->
        ComparableList
            [ httpHeaderName |> ComparableString
            , httpHeaderValue |> ComparableString
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

            IdHttpExpectWhatever ->
                "IdHttpExpectWhatever" |> ComparableString


interfaceWithoutReceiveToComparable : InterfaceSingleWithoutReceive -> Comparable
interfaceWithoutReceiveToComparable =
    \interfaceWithoutReceive ->
        case interfaceWithoutReceive of
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


interfaceDiffs :
    { old : Emptiable (KeysSet (InterfaceSingle state) (InterfaceSingleKeys state) N1) Possibly
    , updated : Emptiable (KeysSet (InterfaceSingle state) (InterfaceSingleKeys state) N1) Possibly
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


domNodeToId : DomNode state_ -> DomNodeId
domNodeToId domNode =
    case domNode of
        DomText text ->
            DomTextId text

        DomElement element ->
            DomElementId (element |> domElementToId)


interfaceOldAndOrUpdatedDiffs : AndOr (InterfaceSingle state) (InterfaceSingle state) -> List InterfaceDiff
interfaceOldAndOrUpdatedDiffs =
    \interfaceAndOr ->
        case interfaceAndOr of
            AndOr.Both ( InterfaceWithReceive (DomNodeRender domElementPreviouslyRendered), InterfaceWithReceive (DomNodeRender domElementToRender) ) ->
                ( domElementPreviouslyRendered, domElementToRender )
                    |> domNodeDiff []
                    |> List.map (\diff -> diff |> AddEditDom |> InterfaceWithReceiveDiff)

            AndOr.Both ( InterfaceWithoutReceive (AudioPlay previouslyPlayed), InterfaceWithoutReceive (AudioPlay toPlay) ) ->
                ( previouslyPlayed, toPlay )
                    |> audioDiff
                    |> List.map
                        (\diff ->
                            { url = toPlay.url, startTime = toPlay.startTime, replacement = diff }
                                |> AddEditAudio
                                |> InterfaceWithoutReceiveDiff
                        )

            AndOr.Both _ ->
                []

            AndOr.Only (Or.First onlyOld) ->
                (case onlyOld of
                    InterfaceWithoutReceive _ ->
                        []

                    InterfaceWithReceive interfaceWithReceive ->
                        case interfaceWithReceive of
                            TimePosixRequest _ ->
                                []

                            TimezoneOffsetRequest _ ->
                                []

                            TimezoneNameRequest _ ->
                                []

                            TimePeriodicallyListen timePeriodicallyListen ->
                                RemoveTimePeriodicallyListen
                                    { milliSeconds = timePeriodicallyListen.intervalDurationMilliSeconds }
                                    |> List.singleton

                            RandomUnsignedInt32sRequest _ ->
                                []

                            HttpRequest request ->
                                RemoveHttpRequest (request |> httpRequestToId) |> List.singleton

                            DomNodeRender _ ->
                                RemoveDom |> List.singleton

                            WindowSizeRequest _ ->
                                []

                            WindowEventListen listen ->
                                RemoveWindowEventListen listen.eventName |> List.singleton

                            WindowAnimationFrameListen _ ->
                                RemoveWindowAnimationFrameListen |> List.singleton

                            NavigationUrlRequest _ ->
                                []

                            DocumentEventListen listen ->
                                RemoveDocumentEventListen listen.eventName |> List.singleton

                            ClipboardRequest _ ->
                                []

                            AudioSourceLoad _ ->
                                []
                )
                    |> List.map InterfaceWithoutReceiveDiff

            AndOr.Only (Or.Second onlyUpdated) ->
                (case onlyUpdated of
                    InterfaceWithoutReceive interfaceWithoutReceive ->
                        (case interfaceWithoutReceive of
                            ConsoleLog string ->
                                AddConsoleLog string

                            ConsoleWarn string ->
                                AddConsoleWarn string

                            ConsoleError string ->
                                AddConsoleError string

                            NavigationReplaceUrl url ->
                                AddNavigationReplaceUrl url

                            NavigationPushUrl url ->
                                AddNavigationPushUrl url

                            NavigationGo urlSteps ->
                                AddNavigationGo urlSteps

                            NavigationLoad url ->
                                url |> AddNavigationLoad

                            NavigationReload ->
                                AddNavigationReload

                            FileDownloadUnsignedInt8s config ->
                                AddFileDownloadUnsignedInt8s config

                            ClipboardReplaceBy replacement ->
                                AddClipboardReplaceBy replacement

                            AudioPlay audio ->
                                AddAudio audio
                        )
                            |> InterfaceWithoutReceiveDiff

                    InterfaceWithReceive interfaceWithReceive ->
                        (case interfaceWithReceive of
                            TimePosixRequest _ ->
                                AddTimePosixRequest

                            TimezoneOffsetRequest _ ->
                                AddTimezoneOffsetRequest

                            TimezoneNameRequest _ ->
                                AddTimezoneNameRequest

                            TimePeriodicallyListen timePeriodicallyListen ->
                                AddTimePeriodicallyListen
                                    { milliSeconds = timePeriodicallyListen.intervalDurationMilliSeconds }

                            RandomUnsignedInt32sRequest randomUnsignedInt32sRequest ->
                                AddRandomUnsignedInt32sRequest randomUnsignedInt32sRequest.count

                            DomNodeRender domElementToRender ->
                                { path = []
                                , replacement = domElementToRender |> domNodeToId |> ReplacementDomNode
                                }
                                    |> AddEditDom

                            HttpRequest httpRequest ->
                                AddHttpRequest (httpRequest |> httpRequestToId)

                            WindowSizeRequest _ ->
                                AddWindowSizeRequest

                            WindowEventListen listen ->
                                AddWindowEventListen listen.eventName

                            WindowAnimationFrameListen _ ->
                                AddWindowAnimationFrameListen

                            NavigationUrlRequest _ ->
                                AddNavigationUrlRequest

                            DocumentEventListen listen ->
                                AddDocumentEventListen listen.eventName

                            ClipboardRequest _ ->
                                AddClipboardRequest

                            AudioSourceLoad load ->
                                AddAudioSourceLoad load.url
                        )
                            |> InterfaceWithReceiveDiff
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
        , if previous.volumeTimelines == new.volumeTimelines then
            Nothing

          else
            ReplacementAudioVolumeTimelines new.volumeTimelines |> Just
        ]
            |> List.filterMap identity


interfaceDiffToJson : InterfaceDiff -> Json.Encode.Value
interfaceDiffToJson =
    \interfaceDiff ->
        case interfaceDiff of
            InterfaceWithReceiveDiff addOrReplaceDiff ->
                addOrReplaceDiff |> interfaceWithReceiveDiffToJson

            InterfaceWithoutReceiveDiff removeDiff ->
                removeDiff |> interfaceWithoutReceiveDiffToJson


domNodeIdToJson : DomNodeId -> Json.Encode.Value
domNodeIdToJson =
    \domElementSubId ->
        Json.Encode.object
            [ case domElementSubId of
                DomTextId text ->
                    ( "text", text |> Json.Encode.string )

                DomElementId element ->
                    ( "element"
                    , element |> domElementIdToJson
                    )
            ]


httpRequestIdToJson : HttpRequestId -> Json.Encode.Value
httpRequestIdToJson =
    \httpRequestId ->
        Json.Encode.object
            [ ( "url", httpRequestId.url |> Json.Encode.string )
            , ( "method", httpRequestId.method |> Json.Encode.string )
            , ( "headers", httpRequestId.headers |> headersToJson httpRequestId.body )
            , ( "expect", httpRequestId.expect |> httpExpectIdToJson )
            , ( "body", httpRequestId.body |> httpBodyToJson )
            , ( "timeout", httpRequestId.timeout |> httpTimeoutToJson )
            ]


httpTimeoutToJson : Maybe Int -> Json.Encode.Value
httpTimeoutToJson =
    \maybeTimeout ->
        case maybeTimeout of
            Nothing ->
                Json.Encode.null

            Just timeout ->
                timeout |> Json.Encode.int


httpExpectIdToJson : HttpExpectId -> Json.Encode.Value
httpExpectIdToJson =
    \httpExpectId ->
        case httpExpectId of
            IdHttpExpectString ->
                Json.Encode.string "STRING"

            IdHttpExpectWhatever ->
                Json.Encode.string "WHATEVER"


headersToJson : HttpBody -> List HttpHeader -> Json.Encode.Value
headersToJson body headers =
    headers
        |> addContentTypeForBody body
        |> Json.Encode.list headerToJson


addContentTypeForBody : HttpBody -> List HttpHeader -> List HttpHeader
addContentTypeForBody body headers =
    case body of
        HttpBodyEmpty ->
            headers

        HttpBodyString stringBodyInfo ->
            ( "Content-Type", stringBodyInfo.mimeType ) :: headers


headerToJson : HttpHeader -> Json.Encode.Value
headerToJson ( name, value ) =
    Json.Encode.list identity
        [ Json.Encode.string name
        , Json.Encode.string value
        ]


httpBodyToJson : HttpBody -> Json.Encode.Value
httpBodyToJson =
    \body ->
        case body of
            HttpBodyString stringBodyInfo ->
                stringBodyInfo.content |> Json.Encode.string

            HttpBodyEmpty ->
                Json.Encode.null


domElementAttributesNamespacedToJson : Dict ( String, String ) String -> Json.Encode.Value
domElementAttributesNamespacedToJson =
    \attributesNamespaced ->
        attributesNamespaced
            |> Dict.toList
            |> Json.Encode.list
                (\( ( namespace, key ), value ) ->
                    Json.Encode.object
                        [ ( "namespace", namespace |> Json.Encode.string )
                        , ( "key", key |> Json.Encode.string )
                        , ( "value", value |> Json.Encode.string )
                        ]
                )


defaultActionHandlingToJson : DefaultActionHandling -> Json.Encode.Value
defaultActionHandlingToJson =
    \defaultActionHandling ->
        case defaultActionHandling of
            DefaultActionPrevent ->
                "DefaultActionPrevent" |> Json.Encode.string

            DefaultActionExecute ->
                "DefaultActionExecute" |> Json.Encode.string


interfaceWithReceiveDiffToJson : InterfaceWithReceiveDiff -> Json.Encode.Value
interfaceWithReceiveDiffToJson =
    \interfaceAddOrReplaceDiff ->
        Json.Encode.object
            [ case interfaceAddOrReplaceDiff of
                AddTimePosixRequest ->
                    ( "addTimePosixRequest", Json.Encode.null )

                AddTimezoneOffsetRequest ->
                    ( "addTimezoneOffsetRequest", Json.Encode.null )

                AddTimezoneNameRequest ->
                    ( "addTimezoneNameRequest", Json.Encode.null )

                AddTimePeriodicallyListen intervalDuration ->
                    ( "addTimePeriodicallyListen"
                    , Json.Encode.object [ ( "milliSeconds", intervalDuration.milliSeconds |> Json.Encode.int ) ]
                    )

                AddRandomUnsignedInt32sRequest count ->
                    ( "addRandomUnsignedInt32sRequest", count |> Json.Encode.int )

                AddEditDom editDomDiff ->
                    ( "addEditDom"
                    , Json.Encode.object
                        [ ( "path", editDomDiff.path |> Json.Encode.list Json.Encode.int )
                        , ( "replacement"
                          , Json.Encode.object
                                [ case editDomDiff.replacement of
                                    ReplacementDomNode domElementReplacement ->
                                        ( "node"
                                        , domElementReplacement |> domNodeIdToJson
                                        )

                                    ReplacementDomElementStyles styles ->
                                        ( "styles", styles |> Json.Encode.dict identity Json.Encode.string )

                                    ReplacementDomElementAttributes attributes ->
                                        ( "attributes", attributes |> Json.Encode.dict identity Json.Encode.string )

                                    ReplacementDomElementAttributesNamespaced attributesNamespaced ->
                                        ( "attributesNamespaced", attributesNamespaced |> domElementAttributesNamespacedToJson )

                                    ReplacementDomElementEventListens eventListens ->
                                        ( "eventListens", eventListens |> Json.Encode.dict identity defaultActionHandlingToJson )
                                ]
                          )
                        ]
                    )

                AddHttpRequest httpRequestId ->
                    ( "addHttpRequest", httpRequestId |> httpRequestIdToJson )

                AddWindowSizeRequest ->
                    ( "addWindowSizeRequest", Json.Encode.null )

                AddWindowEventListen eventName ->
                    ( "addWindowEventListen", eventName |> Json.Encode.string )

                AddWindowAnimationFrameListen ->
                    ( "addWindowAnimationFrameListen", Json.Encode.null )

                AddNavigationUrlRequest ->
                    ( "addNavigationUrlRequest", Json.Encode.null )

                AddDocumentEventListen eventName ->
                    ( "addDocumentEventListen", eventName |> Json.Encode.string )

                AddClipboardRequest ->
                    ( "addClipboardRequest", Json.Encode.null )

                AddAudioSourceLoad audioSourceLoad ->
                    ( "addAudioSourceLoad", audioSourceLoad |> Json.Encode.string )
            ]


audioVolumeTimelineToJson : AudioVolumeTimeline -> Json.Encode.Value
audioVolumeTimelineToJson =
    \timeline ->
        timeline
            |> Dict.toList
            |> Json.Encode.list
                (\( time, volume ) ->
                    Json.Encode.object
                        [ ( "time", time |> Json.Encode.int )
                        , ( "volume", volume |> Json.Encode.float )
                        ]
                )


interfaceWithoutReceiveDiffToJson : InterfaceWithoutReceiveDiff -> Json.Encode.Value
interfaceWithoutReceiveDiffToJson =
    \interfaceRemoveDiff ->
        Json.Encode.object
            [ case interfaceRemoveDiff of
                AddConsoleLog string ->
                    ( "addConsoleLog", string |> Json.Encode.string )

                AddConsoleWarn string ->
                    ( "addConsoleWarn", string |> Json.Encode.string )

                AddConsoleError string ->
                    ( "addConsoleError", string |> Json.Encode.string )

                AddNavigationPushUrl url ->
                    ( "addNavigationPushUrl", url |> AppUrl.toString |> Json.Encode.string )

                AddNavigationReplaceUrl url ->
                    ( "addNavigationReplaceUrl", url |> AppUrl.toString |> Json.Encode.string )

                AddNavigationGo urlSteps ->
                    ( "addNavigationGo", urlSteps |> Json.Encode.int )

                AddNavigationLoad url ->
                    ( "addNavigationLoad", url |> Url.toString |> Json.Encode.string )

                AddNavigationReload ->
                    ( "addNavigationReload", Json.Encode.null )

                AddFileDownloadUnsignedInt8s config ->
                    ( "addFileDownloadUnsignedInt8s"
                    , Json.Encode.object
                        [ ( "name", config.name |> Json.Encode.string )
                        , ( "mimeType", config.mimeType |> Json.Encode.string )
                        , ( "content"
                          , config.content |> Json.Encode.list Json.Encode.int
                          )
                        ]
                    )

                AddClipboardReplaceBy replacement ->
                    ( "addClipboardReplaceBy"
                    , replacement |> Json.Encode.string
                    )

                AddAudio audio ->
                    ( "addAudio", audio |> audioToJson )

                AddEditAudio audioEdit ->
                    ( "addEditAudio"
                    , Json.Encode.object
                        [ ( "url", audioEdit.url |> Json.Encode.string )
                        , ( "startTime", audioEdit.startTime |> Time.posixToMillis |> Json.Encode.int )
                        , ( "replacement"
                          , Json.Encode.object
                                [ case audioEdit.replacement of
                                    ReplacementAudioSpeed new ->
                                        ( "speed", new |> Json.Encode.float )

                                    ReplacementAudioVolume new ->
                                        ( "volume", new |> Json.Encode.float )

                                    ReplacementAudioPan new ->
                                        ( "pan", new |> Json.Encode.float )

                                    ReplacementAudioVolumeTimelines new ->
                                        ( "volumeTimelines", new |> Json.Encode.list audioVolumeTimelineToJson )
                                ]
                          )
                        ]
                    )

                RemoveTimePeriodicallyListen intervalDuration ->
                    ( "removeTimePeriodicallyListen"
                    , Json.Encode.object [ ( "milliSeconds", intervalDuration.milliSeconds |> Json.Encode.int ) ]
                    )

                RemoveDom ->
                    ( "removeDom", Json.Encode.null )

                RemoveHttpRequest httpRequestId ->
                    ( "removeHttpRequest", httpRequestId |> httpRequestIdToJson )

                RemoveWindowEventListen eventName ->
                    ( "removeWindowEventListen", eventName |> Json.Encode.string )

                RemoveWindowAnimationFrameListen ->
                    ( "removeWindowAnimationFrameListen", Json.Encode.null )

                RemoveDocumentEventListen eventName ->
                    ( "removeDocumentEventListen", eventName |> Json.Encode.string )

                RemoveAudio audioId ->
                    ( "removeAudio"
                    , Json.Encode.object
                        [ ( "url", audioId.url |> Json.Encode.string )
                        , ( "startTime", audioId.startTime |> Time.posixToMillis |> Json.Encode.int )
                        ]
                    )
            ]


audioToJson : Audio -> Json.Encode.Value
audioToJson audio =
    Json.Encode.object
        [ ( "url", audio.url |> Json.Encode.string )
        , ( "startTime", audio.startTime |> Time.posixToMillis |> Json.Encode.int )
        , ( "volume", audio.volume |> Json.Encode.float )
        , ( "volumeTimelines", audio.volumeTimelines |> Json.Encode.list audioVolumeTimelineToJson )
        , ( "speed", audio.speed |> Json.Encode.float )
        , ( "pan", audio.pan |> Json.Encode.float )
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


{-| The "subscriptions" part for an embedded program
-}
programSubscriptions : ProgramConfig state -> (ProgramState state -> Sub (ProgramEvent state))
programSubscriptions appConfig =
    \(State state) ->
        -- re-associate event based on current interface
        appConfig.ports.fromJs
            (\interfaceJson ->
                case interfaceJson |> Json.Decode.decodeValue (Json.Decode.field "diff" interfaceDiffWithReceiveJsonDecoder) of
                    Ok interfaceDiff ->
                        case
                            state.interface
                                |> KeysSet.toList interfaceKeys
                                |> listFirstJust
                                    (\stateInterface ->
                                        case stateInterface of
                                            InterfaceWithoutReceive _ ->
                                                Nothing

                                            InterfaceWithReceive withReceive ->
                                                eventDataAndConstructStateJsonDecoder interfaceDiff withReceive
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


domNodeIdJsonDecoder : Json.Decode.Decoder DomNodeId
domNodeIdJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map DomTextId (Json.Decode.field "text" Json.Decode.string)
        , Json.Decode.map DomElementId
            (Json.Decode.field "element"
                (Json.Decode.lazy (\() -> domElementIdJsonDecoder))
            )
        ]


domElementAttributesNamespacedJsonDecoder : Json.Decode.Decoder (Dict ( String, String ) String)
domElementAttributesNamespacedJsonDecoder =
    Json.Decode.map Dict.fromList
        (Json.Decode.list
            (Json.Decode.succeed (\namespace key value -> ( ( namespace, key ), value ))
                |> Json.Decode.Local.andMap (Json.Decode.field "namespace" Json.Decode.string)
                |> Json.Decode.Local.andMap (Json.Decode.field "key" Json.Decode.string)
                |> Json.Decode.Local.andMap (Json.Decode.field "value" Json.Decode.string)
            )
        )


defaultActionHandlingJsonDecoder : Json.Decode.Decoder DefaultActionHandling
defaultActionHandlingJsonDecoder =
    Json.Decode.andThen
        (\string ->
            case string of
                "DefaultActionPrevent" ->
                    DefaultActionPrevent |> Json.Decode.succeed

                "DefaultActionExecute" ->
                    DefaultActionExecute |> Json.Decode.succeed

                _ ->
                    "needs to be either DefaultActionPrevent or DefaultActionExecute" |> Json.Decode.fail
        )
        Json.Decode.string


interfaceDiffWithReceiveJsonDecoder : Json.Decode.Decoder InterfaceWithReceiveDiff
interfaceDiffWithReceiveJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map (\() -> AddTimePosixRequest)
            (Json.Decode.field "addTimePosixRequest" (Json.Decode.null ()))
        , Json.Decode.map (\() -> AddTimezoneOffsetRequest)
            (Json.Decode.field "addTimezoneOffsetRequest" (Json.Decode.null ()))
        , Json.Decode.map (\() -> AddTimezoneNameRequest)
            (Json.Decode.field "addTimezoneNameRequest" (Json.Decode.null ()))
        , Json.Decode.map AddTimePeriodicallyListen
            (Json.Decode.field "addTimePeriodicallyListen"
                (Json.Decode.map (\ms -> { milliSeconds = ms })
                    (Json.Decode.field "milliSeconds" Json.Decode.int)
                )
            )
        , Json.Decode.map AddRandomUnsignedInt32sRequest
            (Json.Decode.field "addRandomUnsignedInt32sRequest" Json.Decode.int)
        , Json.Decode.map AddEditDom
            (Json.Decode.field "addEditDom"
                (Json.Decode.succeed (\path replacement -> { path = path, replacement = replacement })
                    |> Json.Decode.Local.andMap (Json.Decode.field "path" (Json.Decode.list Json.Decode.int))
                    |> Json.Decode.Local.andMap
                        (Json.Decode.field "replacement"
                            (Json.Decode.oneOf
                                [ Json.Decode.map ReplacementDomNode
                                    (Json.Decode.field "node" domNodeIdJsonDecoder)
                                , Json.Decode.map ReplacementDomElementStyles
                                    (Json.Decode.field "styles" (Json.Decode.dict Json.Decode.string))
                                , Json.Decode.map ReplacementDomElementAttributes
                                    (Json.Decode.field "attributes" (Json.Decode.dict Json.Decode.string))
                                , Json.Decode.map ReplacementDomElementAttributesNamespaced
                                    (Json.Decode.field "attributesNamespaced" domElementAttributesNamespacedJsonDecoder)
                                , Json.Decode.map ReplacementDomElementEventListens
                                    (Json.Decode.field "eventListens" (Json.Decode.dict defaultActionHandlingJsonDecoder))
                                ]
                            )
                        )
                )
            )
        , Json.Decode.map AddHttpRequest
            (Json.Decode.field "addHttpRequest" httpRequestIdJsonDecoder)
        , Json.Decode.map (\() -> AddWindowSizeRequest)
            (Json.Decode.field "addWindowSizeRequest" (Json.Decode.null ()))
        , Json.Decode.map AddWindowEventListen
            (Json.Decode.field "addWindowEventListen" Json.Decode.string)
        , Json.Decode.map (\() -> AddWindowAnimationFrameListen)
            (Json.Decode.field "addWindowAnimationFrameListen" (Json.Decode.null ()))
        , Json.Decode.map (\() -> AddNavigationUrlRequest)
            (Json.Decode.field "addNavigationUrlRequest" (Json.Decode.null ()))
        , Json.Decode.map AddDocumentEventListen
            (Json.Decode.field "addDocumentEventListen" Json.Decode.string)
        , Json.Decode.map AddAudioSourceLoad
            (Json.Decode.field "addAudioSourceLoad" Json.Decode.string)
        ]


httpRequestIdJsonDecoder : Json.Decode.Decoder HttpRequestId
httpRequestIdJsonDecoder =
    headersJsonDecoder
        |> Json.Decode.andThen
            (\headers ->
                Json.Decode.succeed
                    (\url method expect body timeout ->
                        { url = url
                        , method = method
                        , headers = headers
                        , expect = expect
                        , body = body
                        , timeout = timeout
                        }
                    )
                    |> Json.Decode.Local.andMap (Json.Decode.field "url" Json.Decode.string)
                    |> Json.Decode.Local.andMap (Json.Decode.field "method" Json.Decode.string)
                    |> Json.Decode.Local.andMap (Json.Decode.field "expect" httpExpectIdJsonDecoder)
                    |> Json.Decode.Local.andMap
                        (Json.Decode.field "body"
                            (headers
                                |> listFirstJust
                                    (\( name, value ) ->
                                        case name of
                                            "Content-Type" ->
                                                value |> Just

                                            _ ->
                                                Nothing
                                    )
                                |> Maybe.map
                                    (\mimeType ->
                                        Json.Decode.succeed (\content -> HttpBodyString { mimeType = mimeType, content = content })
                                            |> Json.Decode.Local.andMap Json.Decode.string
                                    )
                                |> Maybe.withDefault (HttpBodyEmpty |> Json.Decode.null)
                            )
                        )
                    |> Json.Decode.Local.andMap (Json.Decode.field "timeout" httpTimeoutJsonDecoder)
            )


httpTimeoutJsonDecoder : Json.Decode.Decoder (Maybe Int)
httpTimeoutJsonDecoder =
    Json.Decode.nullable Json.Decode.int


httpExpectIdJsonDecoder : Json.Decode.Decoder HttpExpectId
httpExpectIdJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map (\() -> IdHttpExpectString) (jsonDecodeStringOnly "STRING")
        , Json.Decode.map (\() -> IdHttpExpectWhatever) (jsonDecodeStringOnly "WHATEVER")
        ]


jsonDecodeStringOnly : String -> Json.Decode.Decoder ()
jsonDecodeStringOnly constant =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                if string == constant then
                    () |> Json.Decode.succeed

                else
                    Json.Decode.fail ("not " ++ constant)
            )


headersJsonDecoder : Json.Decode.Decoder (List HttpHeader)
headersJsonDecoder =
    Json.Decode.list headerJsonDecoder


headerJsonDecoder : Json.Decode.Decoder HttpHeader
headerJsonDecoder =
    Json.Decode.succeed (\name value -> ( name, value ))
        |> Json.Decode.Local.andMap (Json.Decode.index 0 Json.Decode.string)
        |> Json.Decode.Local.andMap (Json.Decode.index 1 Json.Decode.string)


eventDataAndConstructStateJsonDecoder : InterfaceWithReceiveDiff -> InterfaceSingleWithReceive state -> Maybe (Json.Decode.Decoder state)
eventDataAndConstructStateJsonDecoder interfaceAddDiff interface =
    case interface of
        TimePosixRequest requestTimeNow ->
            case interfaceAddDiff of
                AddTimePosixRequest ->
                    Json.Decode.succeed requestTimeNow
                        |> Json.Decode.Local.andMap (Json.Decode.map Time.millisToPosix Json.Decode.int)
                        |> Just

                _ ->
                    Nothing

        TimezoneOffsetRequest requestTimezoneOffset ->
            case interfaceAddDiff of
                AddTimezoneOffsetRequest ->
                    Json.Decode.succeed requestTimezoneOffset
                        |> Json.Decode.Local.andMap Json.Decode.int
                        |> Just

                _ ->
                    Nothing

        TimezoneNameRequest requestTimezoneName ->
            case interfaceAddDiff of
                AddTimezoneNameRequest ->
                    Json.Decode.succeed requestTimezoneName
                        |> Json.Decode.Local.andMap
                            (Json.Decode.oneOf
                                [ Json.Decode.map Time.Offset Json.Decode.int
                                , Json.Decode.map Time.Name Json.Decode.string
                                ]
                            )
                        |> Just

                _ ->
                    Nothing

        TimePeriodicallyListen timePeriodicallyListen ->
            case interfaceAddDiff of
                AddTimePeriodicallyListen diffIntervalDuration ->
                    if
                        timePeriodicallyListen.intervalDurationMilliSeconds
                            == diffIntervalDuration.milliSeconds
                    then
                        Json.Decode.succeed timePeriodicallyListen.on
                            |> Json.Decode.Local.andMap
                                (Json.Decode.map Time.millisToPosix Json.Decode.int)
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        RandomUnsignedInt32sRequest randomUnsignedInt32sRequest ->
            case interfaceAddDiff of
                AddRandomUnsignedInt32sRequest diffCount ->
                    if randomUnsignedInt32sRequest.count == diffCount then
                        Json.Decode.succeed randomUnsignedInt32sRequest.on
                            |> Json.Decode.Local.andMap
                                (Json.Decode.list Json.Decode.int)
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        DomNodeRender domElementToRender ->
            case interfaceAddDiff of
                AddEditDom domEditDiff ->
                    (Json.Decode.succeed (\innerPath name event -> { innerPath = innerPath, name = name, event = event })
                        |> Json.Decode.Local.andMap (Json.Decode.field "innerPath" (Json.Decode.list Json.Decode.int))
                        |> Json.Decode.Local.andMap (Json.Decode.field "name" Json.Decode.string)
                        |> Json.Decode.Local.andMap
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

        HttpRequest httpRequest ->
            case interfaceAddDiff of
                AddHttpRequest addedHttpRequestId ->
                    if (httpRequest |> httpRequestToId) == addedHttpRequestId then
                        Json.Decode.oneOf
                            [ Json.Decode.field "ok" (httpExpectJsonDecoder httpRequest.expect)
                            , Json.Decode.field "err" (httpErrorJsonDecoder httpRequest)
                                |> Json.Decode.map (httpExpectOnError httpRequest.expect)
                            ]
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        WindowSizeRequest toState ->
            case interfaceAddDiff of
                AddWindowSizeRequest ->
                    Json.Decode.succeed (\width height -> toState { width = width, height = height })
                        |> Json.Decode.Local.andMap (Json.Decode.field "width" Json.Decode.int)
                        |> Json.Decode.Local.andMap (Json.Decode.field "height" Json.Decode.int)
                        |> Just

                _ ->
                    Nothing

        WindowEventListen listen ->
            case interfaceAddDiff of
                AddWindowEventListen addedEventName ->
                    if addedEventName == listen.eventName then
                        listen.on |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        WindowAnimationFrameListen toState ->
            case interfaceAddDiff of
                AddWindowAnimationFrameListen ->
                    Json.Decode.map Time.millisToPosix Json.Decode.int
                        |> Json.Decode.map toState
                        |> Just

                _ ->
                    Nothing

        NavigationUrlRequest toState ->
            case interfaceAddDiff of
                AddNavigationUrlRequest ->
                    Json.Decode.andThen
                        (\urlString ->
                            case urlString |> Url.fromString of
                                Nothing ->
                                    "invalid URL" |> Json.Decode.fail

                                Just url ->
                                    url |> AppUrl.fromUrl |> toState |> Json.Decode.succeed
                        )
                        Json.Decode.string
                        |> Just

                _ ->
                    Nothing

        DocumentEventListen listen ->
            case interfaceAddDiff of
                AddDocumentEventListen addedEventName ->
                    if addedEventName == listen.eventName then
                        listen.on |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        ClipboardRequest toState ->
            case interfaceAddDiff of
                AddClipboardRequest ->
                    Json.Decode.map toState Json.Decode.string |> Just

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


domElementAtReversePath : List Int -> (DomNode state -> Maybe (DomNode state))
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


httpExpectOnError : HttpExpect state -> (HttpError -> state)
httpExpectOnError =
    \httpExpect ->
        case httpExpect of
            HttpExpectString toState ->
                \e -> e |> Err |> toState

            HttpExpectWhatever toState ->
                \e -> e |> Err |> toState


httpExpectJsonDecoder : HttpExpect state -> Json.Decode.Decoder state
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
                        HttpExpectString toState ->
                            Json.Decode.map toState
                                (if isOk then
                                    Json.Decode.map Ok Json.Decode.string

                                 else
                                    badStatusJsonDecoder
                                )

                        HttpExpectWhatever toState ->
                            Json.Decode.map toState
                                (if isOk then
                                    Json.Decode.succeed (Ok ())

                                 else
                                    badStatusJsonDecoder
                                )
                    )
            )


httpMetadataJsonDecoder : Json.Decode.Decoder HttpMetadata
httpMetadataJsonDecoder =
    Json.Decode.succeed
        (\url statusCode statusText headers ->
            { url = url
            , statusCode = statusCode
            , statusText = statusText
            , headers = headers
            }
        )
        |> Json.Decode.Local.andMap (Json.Decode.field "url" Json.Decode.string)
        |> Json.Decode.Local.andMap (Json.Decode.field "statusCode" Json.Decode.int)
        |> Json.Decode.Local.andMap (Json.Decode.field "statusText" Json.Decode.string)
        |> Json.Decode.Local.andMap (Json.Decode.field "headers" (Json.Decode.dict Json.Decode.string))


httpErrorJsonDecoder : HttpRequest state_ -> Json.Decode.Decoder HttpError
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


domElementIdJsonDecoder : Json.Decode.Decoder DomElementId
domElementIdJsonDecoder =
    Json.Decode.succeed
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
        |> Json.Decode.Local.andMap (Json.Decode.field "namespace" (Json.Decode.nullable Json.Decode.string))
        |> Json.Decode.Local.andMap (Json.Decode.field "tag" Json.Decode.string)
        |> Json.Decode.Local.andMap (Json.Decode.field "styles" (Json.Decode.dict Json.Decode.string))
        |> Json.Decode.Local.andMap (Json.Decode.field "attributes" (Json.Decode.dict Json.Decode.string))
        |> Json.Decode.Local.andMap
            (Json.Decode.field "attributesNamespaced" domElementAttributesNamespacedJsonDecoder)
        |> Json.Decode.Local.andMap
            (Json.Decode.field "eventListens"
                (Json.Decode.dict defaultActionHandlingJsonDecoder)
            )
        |> Json.Decode.Local.andMap
            (Json.Decode.field "subs" (Json.Decode.array domNodeIdJsonDecoder))


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
                        |> AddConsoleError
                        |> InterfaceWithoutReceiveDiff
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
                        |> AddConsoleError
                        |> InterfaceWithoutReceiveDiff
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


domElementIdToJson : DomElementId -> Json.Encode.Value
domElementIdToJson =
    \domElementId ->
        Json.Encode.object
            [ ( "namespace"
              , case domElementId.namespace of
                    Nothing ->
                        Json.Encode.null

                    Just namespace ->
                        namespace |> Json.Encode.string
              )
            , ( "tag", domElementId.tag |> Json.Encode.string )
            , ( "styles", domElementId.styles |> Json.Encode.dict identity Json.Encode.string )
            , ( "attributes", domElementId.attributes |> Json.Encode.dict identity Json.Encode.string )
            , ( "attributesNamespaced"
              , domElementId.attributesNamespaced |> domElementAttributesNamespacedToJson
              )
            , ( "eventListens", domElementId.eventListens |> Json.Encode.dict identity defaultActionHandlingToJson )
            , ( "subs", domElementId.subs |> Json.Encode.array domNodeIdToJson )
            ]


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
    = InterfaceWithReceiveDiff InterfaceWithReceiveDiff
    | InterfaceWithoutReceiveDiff InterfaceWithoutReceiveDiff


{-| Actions that will never notify elm again
-}
type InterfaceWithoutReceiveDiff
    = AddConsoleLog String
    | AddConsoleWarn String
    | AddConsoleError String
    | AddNavigationReplaceUrl AppUrl
    | AddNavigationPushUrl AppUrl
    | AddNavigationGo Int
    | AddNavigationLoad Url
    | AddNavigationReload
    | AddFileDownloadUnsignedInt8s { mimeType : String, name : String, content : List Int }
    | AddClipboardReplaceBy String
    | AddAudio Audio
    | AddEditAudio { url : String, startTime : Time.Posix, replacement : EditAudioDiff }
    | RemoveTimePeriodicallyListen { milliSeconds : Int }
    | RemoveHttpRequest HttpRequestId
    | RemoveDom
    | RemoveWindowEventListen String
    | RemoveWindowAnimationFrameListen
    | RemoveDocumentEventListen String
    | RemoveAudio { url : String, startTime : Time.Posix }


{-| Actions that will notify elm some time in the future
-}
type InterfaceWithReceiveDiff
    = AddTimePosixRequest
    | AddTimezoneOffsetRequest
    | AddTimezoneNameRequest
    | AddTimePeriodicallyListen { milliSeconds : Int }
    | AddRandomUnsignedInt32sRequest Int
    | AddDocumentEventListen String
    | AddEditDom EditDomDiff
    | AddHttpRequest HttpRequestId
    | AddWindowSizeRequest
    | AddWindowEventListen String
    | AddWindowAnimationFrameListen
    | AddNavigationUrlRequest
    | AddClipboardRequest
    | AddAudioSourceLoad String


{-| What parts of an [`Audio`](#Audio) are replaced
-}
type EditAudioDiff
    = ReplacementAudioVolume Float
    | ReplacementAudioSpeed Float
    | ReplacementAudioPan Float
    | ReplacementAudioVolumeTimelines (List AudioVolumeTimeline)


{-| Some kind of sound we want to play. To create `Audio` start with [`Web.Audio.fromSource`](Web-Audio#fromSource)
-}
type alias Audio =
    RecordWithoutConstructorFunction
        { url : String
        , startTime : Time.Posix
        , volume : Float
        , volumeTimelines : List AudioVolumeTimeline
        , speed : Float
        , pan : Float
        }


{-| Audio data we can use to play sounds.
Use [`Web.Audio.sourceLoad`](Web-Audio#sourceLoad) to fetch an [`AudioSource`](#AudioSource).

You can for example use the contained source `duration` to loop:

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
type alias AudioVolumeTimeline =
    Dict
        -- in milliseconds
        Int
        Float


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
with a given [`Web.ProgramConfig`](#ProgramConfig). Short for

    Platform.worker
        { init = \() -> Web.programInit yourAppConfig
        , update = Web.programUpdate yourAppConfig
        , subscriptions = Web.programSubscriptions yourAppConfig
        }

-}
program : ProgramConfig state -> Program () (ProgramState state) (ProgramEvent state)
program appConfig =
    Platform.worker
        { init = \() -> programInit appConfig
        , update = programUpdate appConfig
        , subscriptions = programSubscriptions appConfig
        }
