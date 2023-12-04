module Web exposing
    ( ProgramConfig
    , program, ProgramState(..), ProgramEvent(..)
    , programInit, programUpdate, programSubscriptions
    , Interface, InterfaceSingle(..), interfaceBatch, interfaceNone, interfaceMap
    , DomNode(..), DomElement
    , HttpRequest, HttpHeader, HttpBody(..), HttpExpect(..), HttpError(..), HttpMetadata
    , InterfaceDiff(..), InterfaceWithReceiveDiff(..), InterfaceWithoutReceiveDiff(..)
    , InterfaceSingleKeys, InterfaceSingleIdOrderTag
    , InterfaceSingleId(..), InterfaceSingleToIdTag, DomElementId, DomNodeId(..), HttpRequestId, HttpExpectId(..)
    )

{-| A state-interface program running in the browser

@docs ProgramConfig


## as elm Program

@docs program, ProgramState, ProgramEvent


## embed

If you just want to replace a part of your elm app with this architecture. Make sure to wire in all 3:

@docs programInit, programUpdate, programSubscriptions


# interface types

@docs Interface, InterfaceSingle, interfaceBatch, interfaceNone, interfaceMap


## DOM

Types used by [`Web.Dom`](Web-Dom)

@docs DomNode, DomElement


## HTTP

Types used by [`Web.Http`](Web-Http)

@docs HttpRequest, HttpHeader, HttpBody, HttpExpect, HttpError, HttpMetadata


## internals, safe to ignore

@docs InterfaceDiff, InterfaceWithReceiveDiff, InterfaceWithoutReceiveDiff
@docs InterfaceSingleKeys, InterfaceSingleIdOrderTag
@docs InterfaceSingleId, InterfaceSingleToIdTag, DomElementId, DomNodeId, HttpRequestId, HttpExpectId

-}

import AndOr
import AppUrl exposing (AppUrl)
import AppUrl.Local
import Array exposing (Array)
import Dict exposing (Dict)
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
    = TimePosixRequest (Time.Posix -> state)
    | TimezoneOffsetRequest (Int -> state)
    | TimezoneNameRequest (Time.ZoneName -> state)
    | TimePeriodicallyListen { intervalDurationMilliSeconds : Int, on : Time.Posix -> state }
    | RandomUnsignedIntsRequest { count : Int, on : List Int -> state }
    | ConsoleLog String
    | DomNodeRender (DomNode state)
    | HttpRequest (HttpRequest state)
    | WindowSizeRequest ({ width : Int, height : Int } -> state)
    | WindowEventListen { eventName : String, on : Json.Decode.Decoder state }
    | WindowAnimationFrameListen (Time.Posix -> state)
    | NavigationUrlRequest (AppUrl -> state)
    | DocumentEventListen { eventName : String, on : Json.Decode.Decoder state }
    | NavigationReplaceUrl AppUrl
    | NavigationPushUrl AppUrl
    | NavigationGo Int
    | NavigationLoad Url
    | NavigationReload
    | FileDownloadUnsignedInt8List { mimeType : String, name : String, content : List Int }


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
    = HttpExpectJson (Result HttpError Json.Decode.Value -> state)
    | HttpExpectString (Result HttpError String -> state)
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
    = IdHttpExpectJson
    | IdHttpExpectString
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
        , eventListens : Dict String (Json.Decode.Value -> state)
        , subs : Array (DomNode state)
        }


{-| Safe to ignore. Identifier for an [`Interface`](#Interface)
-}
type InterfaceSingleId
    = IdTimePosixRequest
    | IdTimezoneOffsetRequest
    | IdTimezoneNameRequest
    | IdTimePeriodicallyListen { milliSeconds : Int }
    | IdRandomUnsignedIntsRequest Int
    | IdConsoleLog String
    | IdDomNodeRender
    | IdHttpRequest HttpRequestId
    | IdWindowSizeRequest
    | IdWindowEventListen String
    | IdWindowAnimationFrameListen
    | IdNavigationUrlRequest
    | IdDocumentEventListen String
    | IdNavigationReplaceUrl AppUrl
    | IdNavigationPushUrl AppUrl
    | IdNavigationGo Int
    | IdNavigationLoad Url
    | IdNavigationReload
    | IdFileDownloadUnsignedInt8List { mimeType : String, name : String, content : List Int }


{-| Safe to ignore. Identifier for a [`DomElement`](#DomElement)
-}
type alias DomElementId =
    RecordWithoutConstructorFunction
        { namespace : Maybe String
        , tag : String
        , styles : Dict String String
        , attributes : Dict String String
        , attributesNamespaced : Dict ( String, String ) String
        , eventListens : Set String
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


domNodeMap : (state -> mappedState) -> (DomNode state -> DomNode mappedState)
domNodeMap stateChange =
    \domElementToMap ->
        case domElementToMap of
            DomText text ->
                DomText text

            DomElement domElement ->
                domElement |> domElementMap stateChange |> DomElement


interfaceSingleMap : (state -> mappedState) -> (InterfaceSingle state -> InterfaceSingle mappedState)
interfaceSingleMap stateChange =
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

            RandomUnsignedIntsRequest randomUnsignedIntsRequest ->
                { count = randomUnsignedIntsRequest.count
                , on = \ints -> randomUnsignedIntsRequest.on ints |> stateChange
                }
                    |> RandomUnsignedIntsRequest

            ConsoleLog string ->
                ConsoleLog string

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

            NavigationReplaceUrl url ->
                url |> NavigationReplaceUrl

            NavigationPushUrl url ->
                url |> NavigationPushUrl

            NavigationGo urlSteps ->
                urlSteps |> NavigationGo

            NavigationLoad url ->
                url |> NavigationLoad

            NavigationReload ->
                NavigationReload

            FileDownloadUnsignedInt8List config ->
                FileDownloadUnsignedInt8List config


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

                HttpExpectJson expectJson ->
                    (\json -> expectJson json |> stateChange) |> HttpExpectJson
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
                |> Dict.map (\_ listen -> \event -> listen event |> stateChange)
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


domNodeDiff :
    List Int
    -> ( DomNode state, DomNode state )
    -> List { path : List Int, replacementDomNode : DomNode state }
domNodeDiff path =
    \( aNode, bNode ) ->
        case ( aNode, bNode ) of
            ( DomText _, DomElement _ ) ->
                []

            ( DomElement _, DomText _ ) ->
                []

            ( DomText aText, DomText bText ) ->
                if aText == bText then
                    []

                else
                    [ { path = path, replacementDomNode = bText |> DomText } ]

            ( DomElement aElement, DomElement bElement ) ->
                ( aElement, bElement ) |> domElementDiff path


domElementDiff :
    List Int
    -> ( DomElement state, DomElement state )
    -> List { path : List Int, replacementDomNode : DomNode state }
domElementDiff path =
    \( aElement, bElement ) ->
        if
            (aElement.tag == bElement.tag)
                && (aElement.styles == bElement.styles)
                && (aElement.attributes == bElement.attributes)
                && ((aElement.eventListens |> Dict.keys) == (bElement.eventListens |> Dict.keys))
                && ((aElement.subs |> Array.length) == (bElement.subs |> Array.length))
        then
            List.map2 (\( subIndex, aSub ) bSub -> domNodeDiff (subIndex :: path) ( aSub, bSub ))
                (aElement.subs |> Array.toIndexedList)
                (bElement.subs |> Array.toList)
                |> List.concat

        else
            [ { path = path, replacementDomNode = bElement |> DomElement } ]


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

            HttpExpectJson _ ->
                IdHttpExpectJson


interfaceSingleToId : InterfaceSingle state_ -> InterfaceSingleId
interfaceSingleToId =
    \interface ->
        case interface of
            TimePosixRequest _ ->
                IdTimePosixRequest

            TimezoneOffsetRequest _ ->
                IdTimezoneOffsetRequest

            TimezoneNameRequest _ ->
                IdTimezoneNameRequest

            TimePeriodicallyListen timePeriodicallyListen ->
                IdTimePeriodicallyListen
                    { milliSeconds = timePeriodicallyListen.intervalDurationMilliSeconds }

            RandomUnsignedIntsRequest randomUnsignedIntsRequest ->
                IdRandomUnsignedIntsRequest randomUnsignedIntsRequest.count

            ConsoleLog string ->
                IdConsoleLog string

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

            NavigationReplaceUrl url ->
                url |> IdNavigationReplaceUrl

            NavigationPushUrl url ->
                url |> IdNavigationPushUrl

            NavigationGo urlSteps ->
                urlSteps |> IdNavigationGo

            NavigationLoad url ->
                url |> IdNavigationLoad

            NavigationReload ->
                IdNavigationReload

            FileDownloadUnsignedInt8List config ->
                IdFileDownloadUnsignedInt8List config


interfaceIdOrder : Ordering InterfaceSingleId InterfaceSingleIdOrderTag
interfaceIdOrder =
    Typed.tag InterfaceSingleIdOrderTag
        (\( a, b ) -> ( a |> interfaceSingleIdToComparable, b |> interfaceSingleIdToComparable ) |> comparableOrder)


intToComparable : Int -> Comparable
intToComparable =
    \int -> int |> String.fromInt |> ComparableString


interfaceSingleIdToComparable : InterfaceSingleId -> Comparable
interfaceSingleIdToComparable =
    \interfaceId ->
        case interfaceId of
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

            IdRandomUnsignedIntsRequest count ->
                ComparableList
                    [ ComparableString "IdRandomUnsignedIntsRequest"
                    , count |> intToComparable
                    ]

            IdConsoleLog string ->
                ComparableList
                    [ ComparableString "IdConsoleLog"
                    , ComparableString string
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

            IdNavigationReplaceUrl url ->
                ComparableList
                    [ ComparableString "IdNavigationReplaceUrl"
                    , ComparableString (url |> AppUrl.toString)
                    ]

            IdNavigationPushUrl url ->
                ComparableList
                    [ ComparableString "IdNavigationPushUrl"
                    , ComparableString (url |> AppUrl.toString)
                    ]

            IdNavigationGo urlSteps ->
                ComparableList
                    [ ComparableString "IdNavigationGo"
                    , urlSteps |> intToComparable
                    ]

            IdNavigationLoad url ->
                ComparableList
                    [ ComparableString "IdNavigationLoad"
                    , ComparableString (url |> Url.toString)
                    ]

            IdNavigationReload ->
                ComparableString "IdNavigationReload"

            IdFileDownloadUnsignedInt8List config ->
                ComparableList
                    [ ComparableString config.name
                    , ComparableString config.mimeType
                    , config.content
                        |> List.map (\bit -> bit |> String.fromInt |> ComparableString)
                        |> ComparableList
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
            IdHttpExpectJson ->
                "IdHttpExpectJson" |> ComparableString

            IdHttpExpectString ->
                "IdHttpExpectString" |> ComparableString

            IdHttpExpectWhatever ->
                "IdHttpExpectWhatever" |> ComparableString


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


interfaceOldAndOrUpdatedDiffs : AndOr.AndOr (InterfaceSingle state) (InterfaceSingle state) -> List InterfaceDiff
interfaceOldAndOrUpdatedDiffs =
    \interfaceAndOr ->
        case interfaceAndOr of
            AndOr.Both ( DomNodeRender domElementPreviouslyRendered, DomNodeRender domElementToRender ) ->
                ( domElementPreviouslyRendered, domElementToRender )
                    |> domNodeDiff []
                    |> List.map
                        (\subDiff ->
                            ReplaceDomNode
                                { path = subDiff.path
                                , domNode = subDiff.replacementDomNode |> domNodeToId
                                }
                        )
                    |> List.map InterfaceWithReceiveDiff

            AndOr.Both _ ->
                []

            AndOr.Only (Or.First onlyOld) ->
                (case onlyOld of
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

                    RandomUnsignedIntsRequest _ ->
                        []

                    ConsoleLog _ ->
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

                    FileDownloadUnsignedInt8List _ ->
                        []
                )
                    |> List.map InterfaceWithoutReceiveDiff

            AndOr.Only (Or.Second onlyUpdated) ->
                (case onlyUpdated of
                    TimePosixRequest _ ->
                        AddTimePosixRequest |> InterfaceWithReceiveDiff

                    TimezoneOffsetRequest _ ->
                        AddTimezoneOffsetRequest |> InterfaceWithReceiveDiff

                    TimezoneNameRequest _ ->
                        AddTimezoneNameRequest |> InterfaceWithReceiveDiff

                    TimePeriodicallyListen timePeriodicallyListen ->
                        AddTimePeriodicallyListen
                            { milliSeconds = timePeriodicallyListen.intervalDurationMilliSeconds }
                            |> InterfaceWithReceiveDiff

                    RandomUnsignedIntsRequest randomUnsignedIntsRequest ->
                        AddRandomUnsignedIntsRequest randomUnsignedIntsRequest.count |> InterfaceWithReceiveDiff

                    ConsoleLog string ->
                        AddConsoleLog string |> InterfaceWithoutReceiveDiff

                    DomNodeRender domElementToRender ->
                        ReplaceDomNode
                            { path = []
                            , domNode = domElementToRender |> domNodeToId
                            }
                            |> InterfaceWithReceiveDiff

                    HttpRequest httpRequest ->
                        AddHttpRequest (httpRequest |> httpRequestToId) |> InterfaceWithReceiveDiff

                    WindowSizeRequest _ ->
                        AddWindowSizeRequest |> InterfaceWithReceiveDiff

                    WindowEventListen listen ->
                        AddWindowEventListen listen.eventName |> InterfaceWithReceiveDiff

                    WindowAnimationFrameListen _ ->
                        AddWindowAnimationFrameListen |> InterfaceWithReceiveDiff

                    NavigationUrlRequest _ ->
                        AddNavigationUrlRequest |> InterfaceWithReceiveDiff

                    DocumentEventListen listen ->
                        AddDocumentEventListen listen.eventName |> InterfaceWithReceiveDiff

                    NavigationReplaceUrl url ->
                        AddNavigationReplaceUrl url |> InterfaceWithoutReceiveDiff

                    NavigationPushUrl url ->
                        AddNavigationPushUrl url |> InterfaceWithoutReceiveDiff

                    NavigationGo urlSteps ->
                        AddNavigationGo urlSteps |> InterfaceWithoutReceiveDiff

                    NavigationLoad url ->
                        url |> AddNavigationLoad |> InterfaceWithoutReceiveDiff

                    NavigationReload ->
                        AddNavigationReload |> InterfaceWithoutReceiveDiff

                    FileDownloadUnsignedInt8List config ->
                        AddFileDownloadUnsignedInt8List config |> InterfaceWithoutReceiveDiff
                )
                    |> List.singleton


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

            IdHttpExpectJson ->
                Json.Encode.string "JSON"

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

                AddRandomUnsignedIntsRequest count ->
                    ( "addRandomUnsignedIntsRequest", count |> Json.Encode.int )

                ReplaceDomNode domElementToAdd ->
                    ( "replaceDomNode"
                    , Json.Encode.object
                        [ ( "path", domElementToAdd.path |> Json.Encode.list Json.Encode.int )
                        , ( "domNode", domElementToAdd.domNode |> domNodeIdToJson )
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
            ]


interfaceWithoutReceiveDiffToJson : InterfaceWithoutReceiveDiff -> Json.Encode.Value
interfaceWithoutReceiveDiffToJson =
    \interfaceRemoveDiff ->
        Json.Encode.object
            [ case interfaceRemoveDiff of
                AddConsoleLog string ->
                    ( "addConsoleLog", string |> Json.Encode.string )

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

                AddFileDownloadUnsignedInt8List config ->
                    ( "addFileDownloadUnsignedInt8List"
                    , Json.Encode.object
                        [ ( "name", config.name |> Json.Encode.string )
                        , ( "mimeType", config.mimeType |> Json.Encode.string )
                        , ( "content"
                          , config.content |> Json.Encode.list Json.Encode.int
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
                                        eventDataAndConstructStateJsonDecoder interfaceDiff stateInterface
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
        , Json.Decode.map AddRandomUnsignedIntsRequest
            (Json.Decode.field "addRandomUnsignedIntsRequest" Json.Decode.int)
        , Json.Decode.map ReplaceDomNode
            (Json.Decode.field "replaceDomNode"
                (Json.Decode.succeed (\path domNode -> { path = path, domNode = domNode })
                    |> Json.Decode.Local.andMap (Json.Decode.field "path" (Json.Decode.list Json.Decode.int))
                    |> Json.Decode.Local.andMap (Json.Decode.field "domNode" domNodeIdJsonDecoder)
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
        , Json.Decode.map (\() -> IdHttpExpectJson) (jsonDecodeStringOnly "JSON")
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


eventDataAndConstructStateJsonDecoder : InterfaceWithReceiveDiff -> InterfaceSingle state -> Maybe (Json.Decode.Decoder state)
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

        RandomUnsignedIntsRequest randomUnsignedIntsRequest ->
            case interfaceAddDiff of
                AddRandomUnsignedIntsRequest diffCount ->
                    if randomUnsignedIntsRequest.count == diffCount then
                        Json.Decode.succeed randomUnsignedIntsRequest.on
                            |> Json.Decode.Local.andMap
                                (Json.Decode.list Json.Decode.int)
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        ConsoleLog _ ->
            Nothing

        DomNodeRender domElementToRender ->
            case interfaceAddDiff of
                ReplaceDomNode domNodeReplacement ->
                    (Json.Decode.succeed (\innerPath name event -> { innerPath = innerPath, name = name, event = event })
                        |> Json.Decode.Local.andMap (Json.Decode.field "innerPath" (Json.Decode.list Json.Decode.int))
                        |> Json.Decode.Local.andMap (Json.Decode.field "name" Json.Decode.string)
                        |> Json.Decode.Local.andMap
                            (Json.Decode.field "event" Json.Decode.value)
                        |> Json.Decode.andThen
                            (\specificEvent ->
                                case domElementToRender |> domElementAtReversePath ((specificEvent.innerPath ++ domNodeReplacement.path) |> List.reverse) of
                                    Nothing ->
                                        Json.Decode.fail "origin element of event not found"

                                    Just (DomText _) ->
                                        Json.Decode.fail "origin element of event leads to text, not element"

                                    Just (DomElement foundDomElement) ->
                                        case foundDomElement.eventListens |> Dict.get specificEvent.name of
                                            Nothing ->
                                                Json.Decode.fail "received event for element without listen"

                                            Just eventListen ->
                                                eventListen specificEvent.event |> Json.Decode.succeed
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
                            [ httpExpectJsonDecoder httpRequest.expect
                            , httpErrorJsonDecoder httpRequest |> Json.Decode.map (httpExpectOnError httpRequest.expect)
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

        NavigationPushUrl _ ->
            Nothing

        NavigationReplaceUrl _ ->
            Nothing

        NavigationGo _ ->
            Nothing

        NavigationLoad _ ->
            Nothing

        NavigationReload ->
            Nothing

        FileDownloadUnsignedInt8List _ ->
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
            HttpExpectJson toState ->
                \e -> e |> Err |> toState

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
                in
                Json.Decode.field "body"
                    (case expect of
                        HttpExpectJson toState ->
                            Json.Decode.map toState
                                (if isOk then
                                    Json.Decode.map Ok Json.Decode.value

                                 else
                                    Json.Decode.map (\body -> Err (HttpBadStatus { metadata = meta, body = body })) Json.Decode.value
                                )

                        HttpExpectString toState ->
                            Json.Decode.map toState
                                (if isOk then
                                    Json.Decode.map Ok Json.Decode.string

                                 else
                                    Json.Decode.map (\body -> Err (HttpBadStatus { metadata = meta, body = body })) Json.Decode.value
                                )

                        HttpExpectWhatever toState ->
                            Json.Decode.map toState
                                (if isOk then
                                    Json.Decode.succeed (Ok ())

                                 else
                                    Json.Decode.map (\body -> Err (HttpBadStatus { metadata = meta, body = body })) Json.Decode.value
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
    Json.Decode.field "reason" Json.Decode.string
        |> Json.Decode.andThen
            (\code ->
                case code of
                    "TIMEOUT" ->
                        Json.Decode.succeed HttpTimeout

                    "NETWORK_ERROR" ->
                        Json.Decode.succeed HttpNetworkError

                    "BAD_URL" ->
                        Json.Decode.succeed (HttpBadUrl httpRequest.url)

                    otherCode ->
                        Json.Decode.fail ("Unknown error code: " ++ otherCode)
            )


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
            (Json.Decode.field "attributesNamespaced"
                (Json.Decode.map Dict.fromList
                    (Json.Decode.list
                        (Json.Decode.succeed (\namespace key value -> ( ( namespace, key ), value ))
                            |> Json.Decode.Local.andMap (Json.Decode.field "namespace" Json.Decode.string)
                            |> Json.Decode.Local.andMap (Json.Decode.field "key" Json.Decode.string)
                            |> Json.Decode.Local.andMap (Json.Decode.field "value" Json.Decode.string)
                        )
                    )
                )
            )
        |> Json.Decode.Local.andMap
            (Json.Decode.field "eventListens"
                (Json.Decode.map Set.fromList (Json.Decode.list Json.Decode.string))
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
                        |> AddConsoleLog
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
                        |> AddConsoleLog
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


domElementToId : DomElement state_ -> DomElementId
domElementToId =
    \domElement ->
        { namespace = domElement.namespace
        , tag = domElement.tag
        , styles = domElement.styles
        , attributes = domElement.attributes
        , attributesNamespaced = domElement.attributesNamespaced
        , eventListens =
            domElement.eventListens |> Dict.foldl (\k _ -> Set.insert k) Set.empty
        , subs =
            domElement.subs |> Array.map domNodeToId
        }


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
              , domElementId.attributesNamespaced
                    |> Dict.toList
                    |> Json.Encode.list
                        (\( ( namespace, key ), value ) ->
                            Json.Encode.object
                                [ ( "namespace", namespace |> Json.Encode.string )
                                , ( "key", key |> Json.Encode.string )
                                , ( "value", value |> Json.Encode.string )
                                ]
                        )
              )
            , ( "eventListens", domElementId.eventListens |> Json.Encode.set Json.Encode.string )
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
    | AddNavigationReplaceUrl AppUrl
    | AddNavigationPushUrl AppUrl
    | AddNavigationGo Int
    | AddNavigationLoad Url
    | AddNavigationReload
    | AddFileDownloadUnsignedInt8List { mimeType : String, name : String, content : List Int }
    | RemoveTimePeriodicallyListen { milliSeconds : Int }
    | RemoveHttpRequest HttpRequestId
    | RemoveDom
    | RemoveWindowEventListen String
    | RemoveWindowAnimationFrameListen
    | RemoveDocumentEventListen String


{-| Actions that will notify elm some time in the future
-}
type InterfaceWithReceiveDiff
    = AddTimePosixRequest
    | AddTimezoneOffsetRequest
    | AddTimezoneNameRequest
    | AddTimePeriodicallyListen { milliSeconds : Int }
    | AddRandomUnsignedIntsRequest Int
    | AddDocumentEventListen String
    | ReplaceDomNode { path : List Int, domNode : DomNodeId }
    | AddHttpRequest HttpRequestId
    | AddWindowSizeRequest
    | AddWindowEventListen String
    | AddWindowAnimationFrameListen
    | AddNavigationUrlRequest


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
