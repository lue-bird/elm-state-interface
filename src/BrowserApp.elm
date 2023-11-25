module BrowserApp exposing
    ( Config
    , toProgram, State(..), Event(..)
    , init, subscriptions, update
    , Interface, InterfaceSingle(..), batch, none, map
    , DomNode(..), DomElement
    , HttpRequest, HttpHeader, HttpBody(..), HttpExpect(..), HttpError(..), HttpMetadata
    , InterfaceDiff(..)
    , InterfaceSingleKeys, InterfaceSingleIdOrderTag
    , InterfaceSingleId(..), InterfaceSingleToIdTag, DomElementId, DomNodeId(..), HttpRequestId, HttpExpectId(..)
    )

{-| A state-interface program running in the browser

@docs Config


## Program

@docs toProgram, State, Event


## embed

If you just want to replace a part of your elm app with this architecture. Make sure to wire in all 3:

@docs init, subscriptions, update


# interface types

@docs Interface, InterfaceSingle, batch, none, map


## DOM

Types used by [`BrowserApp.Dom`](BrowserApp-Dom)

@docs DomNode, DomElement


## HTTP

Types used by [`BrowserApp.Http`](BrowserApp-Http)

@docs HttpRequest, HttpHeader, HttpBody, HttpExpect, HttpError, HttpMetadata


## internals, safe to ignore

@docs InterfaceDiff
@docs InterfaceSingleKeys, InterfaceSingleIdOrderTag
@docs InterfaceSingleId, InterfaceSingleToIdTag, DomElementId, DomNodeId, HttpRequestId, HttpExpectId

-}

import Array exposing (Array)
import Char.Order
import Dict exposing (Dict)
import Emptiable exposing (Emptiable)
import Int.Order
import Json.Decode
import Json.Decode.Local
import Json.Encode
import Keys exposing (Key, Keys)
import KeysSet exposing (KeysSet)
import List.Order
import Map exposing (Mapping)
import N exposing (N1)
import Order exposing (Ordering)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Rope exposing (Rope)
import Set exposing (Set)
import String.Order
import Time
import Typed


{-| Ignore the specific fields, this is just exposed so can annotate a program state like in

    main : Program () (BrowserApp.State YourState) (BrowserApp.Event YourState)
    main =
        BrowserApp.toProgram ...

-}
type State appState
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
  - An [`Interface`](#Interface) can be created using the helpers in `BrowserApp.Time`, `BrowserApp.Dom`, `BrowserApp.Http` etc.

-}
type alias Config state =
    RecordWithoutConstructorFunction
        { initialState : state
        , interface : state -> Interface state
        , ports :
            { toJs : Json.Encode.Value -> Cmd Never
            , fromJs : (Json.Encode.Value -> Event state) -> Sub (Event state)
            }
        }


{-| Incoming and outgoing effects.
To create one, use the helpers in `BrowserApp.Time`, `.Dom`, `.Http` etc.

To combine multiple, use [`BrowserApp.batch`](#batch) and [`BrowserApp.none`](#none)

-}
type alias Interface state =
    Rope (InterfaceSingle state)


{-| An "non-batched" [`Interface`](#Interface).
To create one, use the helpers in `BrowserApp.Time`, `.Dom`, `.Http` etc.
-}
type InterfaceSingle state
    = TimePosixRequest (Time.Posix -> state)
    | TimezoneRequest (Time.Zone -> state)
    | TimezoneNameRequest (Time.ZoneName -> state)
    | ConsoleLog String
    | DomNodeRender (DomNode state)
    | HttpRequest (HttpRequest state)
    | WindowEventListen { eventName : String, on : Json.Decode.Value -> state }
    | WindowAnimationFrameListen (Time.Posix -> state)
    | DocumentEventListen { eventName : String, on : Json.Decode.Value -> state }
    | NavigationReplaceUrl String
    | NavigationPushUrl String
    | NavigationGo Int
    | NavigationLoad String
    | NavigationReload


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

  - `HttpBodyString`: Put a `String` in the body of your request. Defining `BrowserApp.Http.jsonBody` looks like this:

        import Json.Encode

        jsonBody : Json.Encode.Value -> BrowserApp.HttpBody
        jsonBody value =
            BrowserApp.HttpBodyString "application/json" (Json.Encode.encode 0 value)

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
        { tag : String
        , styles : Dict String String
        , attributes : Dict String String
        , eventListens : Dict String (Json.Decode.Value -> state)
        , subs : Array (DomNode state)
        }


{-| Safe to ignore. Identifier for an [`Interface`](#Interface)
-}
type InterfaceSingleId
    = IdTimePosixRequest
    | IdRequestTimezone
    | IdRequestTimezoneName
    | IdConsoleLog String
    | IdRenderDomNode
    | IdHttpRequest HttpRequestId
    | IdWindowEventListen String
    | IdWindowAnimationFrameListen
    | IdDocumentEventListen String
    | IdNavigationReplaceUrl String
    | IdNavigationPushUrl String
    | IdNavigationGo Int
    | IdNavigationLoad String
    | IdNavigationReload


{-| Safe to ignore. Identifier for a [`DomElement`](#DomElement)
-}
type alias DomElementId =
    RecordWithoutConstructorFunction
        { tag : String
        , styles : Dict String String
        , attributes : Dict String String
        , eventListens : Set String
        , subs : Array DomNodeId
        }


{-| Safe to ignore. Identifier for a [`DomNode`](#DomNode)
-}
type DomNodeId
    = DomTextId String
    | DomElementId DomElementId


type DomElementIdOrder
    = DomElementIdOrder


type DomNodeIdOrder
    = DomNodeIdOrder


domElementIdOrder : Ordering DomElementId DomElementIdOrder
domElementIdOrder =
    Typed.mapTo DomElementIdOrder
        identity
        (Order.by (Map.tag { tag = () } .tag) (String.Order.earlier Char.Order.unicode)
            |> Order.onTie
                (Order.by (Map.tag () .styles)
                    (dictOrderEarlier (String.Order.earlier Char.Order.unicode))
                )
            |> Order.onTie
                (Order.by (Map.tag () .attributes)
                    (dictOrderEarlier (String.Order.earlier Char.Order.unicode))
                )
            |> Order.onTie
                (Order.by (Map.tag () .eventListens)
                    (setOrderEarlier (String.Order.earlier Char.Order.unicode))
                )
            |> Order.onTie
                (Order.by (Map.tag () .subs)
                    (Typed.tag ()
                        (\( a, b ) -> Order.with (List.Order.earlier domNodeIdOrder) (a |> Array.toList) (b |> Array.toList))
                    )
                )
        )


dictOrderEarlier : Ordering key nodeTag -> Ordering (Dict key value_) (Order.By DictKeys (List.Order.Earlier nodeTag))
dictOrderEarlier keyOrder =
    Order.by (Map.tag DictKeys Dict.keys)
        (List.Order.earlier keyOrder)


setOrderEarlier : Ordering node nodeTag -> Ordering (Set node) (Order.By SetToList (List.Order.Earlier nodeTag))
setOrderEarlier keyOrder =
    Order.by (Map.tag SetToList Set.toList)
        (List.Order.earlier keyOrder)


type DictKeys
    = DictKeys


type SetToList
    = SetToList


domNodeIdOrder : Ordering DomNodeId DomNodeIdOrder
domNodeIdOrder =
    Typed.mapTo DomNodeIdOrder
        identity
        (Order.on (Map.tag () domNodeIdToText) (String.Order.earlier Char.Order.unicode)
            |> Order.onTie
                (Order.on (Map.tag () domNodeIdToElement)
                    (Typed.tag ()
                        (\( a, b ) -> Order.with domElementIdOrder a b)
                    )
                )
        )


domNodeIdToText : DomNodeId -> Maybe String
domNodeIdToText =
    \domElementSub ->
        case domElementSub of
            DomTextId string ->
                string |> Just

            DomElementId _ ->
                Nothing


domNodeIdToElement : DomNodeId -> Maybe DomElementId
domNodeIdToElement =
    \domElementSub ->
        case domElementSub of
            DomElementId domElementId ->
                domElementId |> Just

            DomTextId _ ->
                Nothing


{-| Combine multiple [`Interface`](#Interface)s into one.
-}
batch : List (Interface state) -> Interface state
batch =
    \interfaces -> interfaces |> Rope.fromList |> Rope.concat


{-| Doing nothing as an [`Interface`](#Interface). These two examples are equivalent:

    BrowserApp.batch [ a, BrowserApp.none, b ]

and

    BrowserApp.batch
        (List.filterMap identity
            [ a |> Just, Nothing, b |> Just ]
        )

-}
none : Interface state_
none =
    Rope.empty


{-| Map the state constructed by the [`Interface`](#Interface).

In practice, this is sometimes used like a kind of event-config pattern:

    BrowserApp.Time.posixRequest
        |> BrowserApp.map (\timeNow -> TimeReceived timeNow)

sometimes like elm's `update`

    ...
        |> BrowserApp.map
            (\event ->
                case event of
                    MouseMovedTo newMousePoint ->
                        { state | mousePoint = newMousePoint }

                    CounterDecreaseClicked ->
                        { state | counter = state.counter - 1 }

                    CounterIncreaseClicked ->
                        { state | counter = state.counter + 1 }
            )

and sometimes like elm's `Cmd.map/Html.map/...`:

    type State
        = MenuState Menu.State
        | PlayingState Playing.State

    interface : State -> Interface State
    interface state =
        case state of
            MenuState menuState ->
                BrowserApp.map MenuState (Menu.interface menuState)

            PlayingState playingState ->
                BrowserApp.map PlayingState (Playing.interface playingState)

In all these examples, you end up converting the narrow state representation of part of the interface to a broader representation for
the parent interface

-}
map : (state -> mappedState) -> (Interface state -> Interface mappedState)
map stateChange =
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

            TimezoneRequest requestTimezone ->
                (\event -> requestTimezone event |> stateChange)
                    |> TimezoneRequest

            TimezoneNameRequest requestTimezoneName ->
                (\event -> requestTimezoneName event |> stateChange)
                    |> TimezoneNameRequest

            ConsoleLog string ->
                ConsoleLog string

            DomNodeRender domElementToRender ->
                domElementToRender |> domNodeMap stateChange |> DomNodeRender

            HttpRequest httpRequest ->
                httpRequest
                    |> httpRequestMap stateChange
                    |> HttpRequest

            WindowEventListen listen ->
                { eventName = listen.eventName, on = \value -> listen.on value |> stateChange }
                    |> WindowEventListen

            WindowAnimationFrameListen toState ->
                (\event -> toState event |> stateChange) |> WindowAnimationFrameListen

            DocumentEventListen listen ->
                { eventName = listen.eventName, on = \value -> listen.on value |> stateChange }
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
        { tag = domElementToMap.tag
        , styles = domElementToMap.styles
        , attributes = domElementToMap.attributes
        , eventListens =
            domElementToMap.eventListens
                |> Dict.map (\_ listen -> \event -> listen event |> stateChange)
        , subs =
            domElementToMap.subs |> Array.map (domNodeMap stateChange)
        }


{-| Ignore the specific variants, this is just exposed so can annotate a program event like in

    main : Program () (BrowserApp.State YourState) (BrowserApp.Event YourState)
    main =
        BrowserApp.toProgram ...

-}
type Event appState
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

            TimezoneRequest _ ->
                IdRequestTimezone

            TimezoneNameRequest _ ->
                IdRequestTimezoneName

            ConsoleLog string ->
                IdConsoleLog string

            DomNodeRender _ ->
                IdRenderDomNode

            HttpRequest httpRequest ->
                httpRequest |> httpRequestToId |> IdHttpRequest

            WindowEventListen listen ->
                IdWindowEventListen listen.eventName

            WindowAnimationFrameListen _ ->
                IdWindowAnimationFrameListen

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


interfaceIdOrder : Ordering InterfaceSingleId InterfaceSingleIdOrderTag
interfaceIdOrder =
    Typed.tag InterfaceSingleIdOrderTag
        (\( a, b ) ->
            case a of
                IdTimePosixRequest ->
                    case b of
                        IdTimePosixRequest ->
                            EQ

                        _ ->
                            GT

                IdRequestTimezone ->
                    case b of
                        IdRequestTimezone ->
                            EQ

                        _ ->
                            GT

                IdRequestTimezoneName ->
                    case b of
                        IdRequestTimezoneName ->
                            EQ

                        _ ->
                            GT

                IdConsoleLog aString ->
                    case b of
                        IdConsoleLog bString ->
                            Order.with (String.Order.earlier Char.Order.unicode) aString bString

                        _ ->
                            GT

                IdRenderDomNode ->
                    case b of
                        IdRenderDomNode ->
                            EQ

                        _ ->
                            GT

                IdHttpRequest aRequest ->
                    case b of
                        IdHttpRequest bRequest ->
                            Order.with httpRequestOrder aRequest bRequest

                        _ ->
                            GT

                IdWindowEventListen aEventName ->
                    case b of
                        IdWindowEventListen bEventName ->
                            Order.with (String.Order.earlier Char.Order.unicode) aEventName bEventName

                        _ ->
                            GT

                IdWindowAnimationFrameListen ->
                    case b of
                        IdWindowAnimationFrameListen ->
                            EQ

                        _ ->
                            GT

                IdDocumentEventListen aEventName ->
                    case b of
                        IdDocumentEventListen bEventName ->
                            Order.with (String.Order.earlier Char.Order.unicode) aEventName bEventName

                        _ ->
                            GT

                IdNavigationReplaceUrl aUrl ->
                    case b of
                        IdNavigationReplaceUrl bUrl ->
                            Order.with (String.Order.earlier Char.Order.unicode) aUrl bUrl

                        _ ->
                            GT

                IdNavigationPushUrl aUrl ->
                    case b of
                        IdNavigationPushUrl bUrl ->
                            Order.with (String.Order.earlier Char.Order.unicode) aUrl bUrl

                        _ ->
                            GT

                IdNavigationGo aUrlSteps ->
                    case b of
                        IdNavigationGo bUrlSteps ->
                            Order.with Int.Order.up aUrlSteps bUrlSteps

                        _ ->
                            GT

                IdNavigationLoad aUrl ->
                    case b of
                        IdNavigationLoad bUrl ->
                            Order.with (String.Order.earlier Char.Order.unicode) aUrl bUrl

                        _ ->
                            GT

                IdNavigationReload ->
                    case b of
                        IdNavigationReload ->
                            EQ

                        _ ->
                            GT
        )


httpRequestOrder : Ordering HttpRequestId ()
httpRequestOrder =
    Typed.tag ()
        (Order.by (Map.tag () .url) (String.Order.earlier Char.Order.unicode)
            |> Order.onTie (Order.by (Map.tag () .method) (String.Order.earlier Char.Order.unicode))
            |> Order.onTie (Order.by (Map.tag () .headers) (List.Order.earlier httpHeaderOrder))
            |> Order.onTie (Order.by (Map.tag () .body) httpBodyOrder)
            |> Order.onTie (Order.by (Map.tag () .expect) httpExpectIdOrder)
            |> Order.onTie (Order.by (Map.tag () .timeout) (Order.on (Map.tag () identity) Int.Order.up))
            |> Typed.untag
        )


httpHeaderOrder : Ordering HttpHeader ()
httpHeaderOrder =
    Order.by (Map.tag () Tuple.first) (String.Order.earlier Char.Order.unicode)
        |> Order.onTie (Order.by (Map.tag () Tuple.second) (String.Order.earlier Char.Order.unicode))
        |> Typed.untag
        |> Typed.tag ()


httpBodyOrder : Ordering HttpBody ()
httpBodyOrder =
    Order.on (Map.tag () httpBodyToEmpty) Order.tie
        |> Order.onTie (Order.on (Map.tag () httpBodyToString) stringBodyOrder)
        |> Typed.untag
        |> Typed.tag ()


httpBodyToEmpty : HttpBody -> Maybe ()
httpBodyToEmpty =
    \httpBody ->
        case httpBody of
            HttpBodyEmpty ->
                () |> Just

            _ ->
                Nothing


httpBodyToString : HttpBody -> Maybe { mimeType : String, content : String }
httpBodyToString =
    \httpBody ->
        case httpBody of
            HttpBodyString stringBody ->
                stringBody |> Just

            _ ->
                Nothing


stringBodyOrder : Ordering { mimeType : String, content : String } ()
stringBodyOrder =
    Order.by (Map.tag () .mimeType) (String.Order.earlier Char.Order.unicode)
        |> Order.onTie (Order.by (Map.tag () .content) (String.Order.earlier Char.Order.unicode))
        |> Typed.untag
        |> Typed.tag ()


httpExpectIdOrder : Ordering HttpExpectId ()
httpExpectIdOrder =
    Order.on (Map.tag () httpExpectIdToExpectJson) Order.tie
        |> Order.onTie (Order.on (Map.tag () httpExpectIdToExpectString) Order.tie)
        |> Order.onTie (Order.on (Map.tag () httpExpectIdToExpectWhatever) Order.tie)
        |> Typed.untag
        |> Typed.tag ()


httpExpectIdToExpectJson : HttpExpectId -> Maybe ()
httpExpectIdToExpectJson =
    \httpBody ->
        case httpBody of
            IdHttpExpectJson ->
                () |> Just

            _ ->
                Nothing


httpExpectIdToExpectString : HttpExpectId -> Maybe ()
httpExpectIdToExpectString =
    \httpBody ->
        case httpBody of
            IdHttpExpectString ->
                () |> Just

            _ ->
                Nothing


httpExpectIdToExpectWhatever : HttpExpectId -> Maybe ()
httpExpectIdToExpectWhatever =
    \httpBody ->
        case httpBody of
            IdHttpExpectWhatever ->
                () |> Just

            _ ->
                Nothing


domNodeToId : DomNode state_ -> DomNodeId
domNodeToId domNode =
    case domNode of
        DomText text ->
            DomTextId text

        DomElement element ->
            DomElementId (element |> domElementToId)


interfaceDiffToCmds :
    { old : Emptiable (KeysSet (InterfaceSingle state) (InterfaceSingleKeys state) N1) Possibly
    , updated : Emptiable (KeysSet (InterfaceSingle state) (InterfaceSingleKeys state) N1) Possibly
    }
    -> List InterfaceDiff
interfaceDiffToCmds =
    \interfaces ->
        [ interfaces.old
            |> KeysSet.except interfaceKeys
                (interfaces.updated |> KeysSet.toKeys interfaceKeys)
            |> KeysSet.remove interfaceKeys IdRenderDomNode
            |> KeysSet.toList interfaceKeys
            |> List.filterMap
                (\interface ->
                    case interface of
                        TimePosixRequest _ ->
                            Nothing

                        TimezoneRequest _ ->
                            Nothing

                        TimezoneNameRequest _ ->
                            Nothing

                        ConsoleLog _ ->
                            Nothing

                        HttpRequest request ->
                            RemoveHttpRequest (request |> httpRequestToId) |> Just

                        DomNodeRender _ ->
                            RemoveDom |> Just

                        WindowEventListen listen ->
                            RemoveWindowEventListen listen.eventName |> Just

                        WindowAnimationFrameListen _ ->
                            RemoveWindowAnimationFrameListen |> Just

                        DocumentEventListen listen ->
                            RemoveDocumentEventListen listen.eventName |> Just

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
                )
        , interfaces.updated
            |> KeysSet.except interfaceKeys
                (interfaces.old |> KeysSet.toKeys interfaceKeys)
            |> KeysSet.remove interfaceKeys IdRenderDomNode
            |> KeysSet.toList interfaceKeys
            |> List.filterMap
                (\interface ->
                    case interface of
                        TimePosixRequest _ ->
                            AddTimePosixRequest |> Just

                        TimezoneRequest _ ->
                            AddTimezoneRequest |> Just

                        TimezoneNameRequest _ ->
                            AddTimezoneNameRequest |> Just

                        ConsoleLog string ->
                            AddConsoleLog string |> Just

                        DomNodeRender _ ->
                            Nothing

                        HttpRequest httpRequest ->
                            AddHttpRequest (httpRequest |> httpRequestToId) |> Just

                        WindowEventListen listen ->
                            AddWindowEventListen listen.eventName |> Just

                        WindowAnimationFrameListen _ ->
                            AddWindowAnimationFrameListen |> Just

                        DocumentEventListen listen ->
                            AddDocumentEventListen listen.eventName |> Just

                        NavigationReplaceUrl url ->
                            AddNavigationReplaceUrl url |> Just

                        NavigationPushUrl url ->
                            AddNavigationPushUrl url |> Just

                        NavigationGo urlSteps ->
                            AddNavigationGo urlSteps |> Just

                        NavigationLoad url ->
                            url |> AddNavigationLoad |> Just

                        NavigationReload ->
                            AddNavigationReload |> Just
                )
        , case ( interfaces.old |> KeysSet.element interfaceKeys IdRenderDomNode, interfaces.updated |> KeysSet.element interfaceKeys IdRenderDomNode ) of
            ( Emptiable.Filled (DomNodeRender domElementPreviouslyRendered), Emptiable.Filled (DomNodeRender domElementToRender) ) ->
                ( domElementPreviouslyRendered, domElementToRender )
                    |> domNodeDiff []
                    |> List.map
                        (\subDiff ->
                            ReplaceDomNode
                                { path = subDiff.path
                                , domNode = subDiff.replacementDomNode |> domNodeToId
                                }
                        )

            ( Emptiable.Empty _, Emptiable.Filled (DomNodeRender replacementDomNode) ) ->
                [ ReplaceDomNode
                    { path = []
                    , domNode = replacementDomNode |> domNodeToId
                    }
                ]

            ( Emptiable.Filled (DomNodeRender _), _ ) ->
                -- already handles earlier
                []

            _ ->
                []
        ]
            |> List.concat


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


interfaceDiffToJson : InterfaceDiff -> Json.Encode.Value
interfaceDiffToJson =
    \interfaceDiff ->
        Json.Encode.object
            [ case interfaceDiff of
                AddTimePosixRequest ->
                    ( "addRequestTimeNow", Json.Encode.null )

                AddTimezoneRequest ->
                    ( "addRequestTimezoneOffset", Json.Encode.null )

                AddTimezoneNameRequest ->
                    ( "addRequestTimezoneName", Json.Encode.null )

                AddConsoleLog string ->
                    ( "addConsoleLog", string |> Json.Encode.string )

                ReplaceDomNode domElementToAdd ->
                    ( "replaceDomNode"
                    , Json.Encode.object
                        [ ( "path", domElementToAdd.path |> Json.Encode.list Json.Encode.int )
                        , ( "domNode", domElementToAdd.domNode |> domNodeIdToJson )
                        ]
                    )

                RemoveDom ->
                    ( "removeDom", Json.Encode.null )

                AddHttpRequest httpRequestId ->
                    ( "addHttpRequest", httpRequestId |> httpRequestIdToJson )

                RemoveHttpRequest httpRequestId ->
                    ( "removeHttpRequest", httpRequestId |> httpRequestIdToJson )

                AddWindowEventListen eventName ->
                    ( "addWindowEventListen", eventName |> Json.Encode.string )

                RemoveWindowEventListen eventName ->
                    ( "removeWindowEventListen", eventName |> Json.Encode.string )

                AddWindowAnimationFrameListen ->
                    ( "addWindowAnimationFrameListen", Json.Encode.null )

                RemoveWindowAnimationFrameListen ->
                    ( "removeWindowAnimationFrameListen", Json.Encode.null )

                AddDocumentEventListen eventName ->
                    ( "addDocumentEventListen", eventName |> Json.Encode.string )

                RemoveDocumentEventListen eventName ->
                    ( "removeDocumentEventListen", eventName |> Json.Encode.string )

                AddNavigationPushUrl url ->
                    ( "addNavigationPushUrl", url |> Json.Encode.string )

                AddNavigationReplaceUrl url ->
                    ( "addNavigationReplaceUrl", url |> Json.Encode.string )

                AddNavigationGo urlSteps ->
                    ( "addNavigationGo", urlSteps |> Json.Encode.int )

                AddNavigationLoad url ->
                    ( "addNavigationLoad", url |> Json.Encode.string )

                AddNavigationReload ->
                    ( "addNavigationReload", Json.Encode.null )
            ]


httpRequestIdToJson : HttpRequestId -> Json.Encode.Value
httpRequestIdToJson =
    \httpRequestId ->
        Json.Encode.object
            [ ( "url", httpRequestId.url |> Json.Encode.string )
            , ( "method", httpRequestId.method |> Json.Encode.string )
            , ( "headers", encodeHeaders httpRequestId.body httpRequestId.headers )
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


encodeHeaders : HttpBody -> List HttpHeader -> Json.Encode.Value
encodeHeaders body headers =
    headers
        |> addContentTypeForBody body
        |> Json.Encode.list encodeHeader


addContentTypeForBody : HttpBody -> List HttpHeader -> List HttpHeader
addContentTypeForBody body headers =
    case body of
        HttpBodyEmpty ->
            headers

        HttpBodyString stringBodyInfo ->
            ( "Content-Type", stringBodyInfo.mimeType ) :: headers


encodeHeader : HttpHeader -> Json.Encode.Value
encodeHeader ( name, value ) =
    Json.Encode.list identity
        [ Json.Encode.string name
        , Json.Encode.string value
        ]


httpBodyToJson : HttpBody -> Json.Encode.Value
httpBodyToJson body =
    case body of
        HttpBodyString stringBodyInfo ->
            stringBodyInfo.content |> Json.Encode.string

        HttpBodyEmpty ->
            Json.Encode.null


{-| The "init" part for an embedded program
-}
init : Config state -> ( State state, Cmd (Event state) )
init appConfig =
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
        |> interfaceDiffToCmds
        |> List.map (\diff -> appConfig.ports.toJs (diff |> interfaceDiffToJson))
        |> Cmd.batch
        |> Cmd.map never
    )


{-| The "subscriptions" part for an embedded program
-}
subscriptions : Config state -> (State state -> Sub (Event state))
subscriptions appConfig =
    \(State state) ->
        -- re-associate event based on current interface
        appConfig.ports.fromJs
            (\interfaceJson ->
                case interfaceJson |> Json.Decode.decodeValue (Json.Decode.field "diff" interfaceDiffJsonDecoder) of
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


interfaceDiffJsonDecoder : Json.Decode.Decoder InterfaceDiff
interfaceDiffJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map (\() -> AddTimePosixRequest)
            (Json.Decode.field "requestTimeNow" (Json.Decode.null ()))
        , Json.Decode.map ReplaceDomNode
            (Json.Decode.field "replaceDomNode"
                (Json.Decode.succeed (\path domNode -> { path = path, domNode = domNode })
                    |> Json.Decode.Local.andMap (Json.Decode.field "path" (Json.Decode.list Json.Decode.int))
                    |> Json.Decode.Local.andMap (Json.Decode.field "domNode" domNodeIdJsonDecoder)
                )
            )
        , Json.Decode.map (\() -> RemoveDom)
            (Json.Decode.field "removeDom" (Json.Decode.null ()))
        ]


eventDataAndConstructStateJsonDecoder : InterfaceDiff -> InterfaceSingle state -> Maybe (Json.Decode.Decoder state)
eventDataAndConstructStateJsonDecoder interfaceDiff interface =
    case interface of
        TimePosixRequest requestTimeNow ->
            case interfaceDiff of
                AddTimePosixRequest ->
                    Json.Decode.succeed requestTimeNow
                        |> Json.Decode.Local.andMap (Json.Decode.map Time.millisToPosix Json.Decode.int)
                        |> Just

                _ ->
                    Nothing

        TimezoneRequest requestTimezone ->
            case interfaceDiff of
                AddTimezoneRequest ->
                    Json.Decode.succeed requestTimezone
                        |> Json.Decode.Local.andMap
                            (Json.Decode.map (\offset -> Time.customZone offset []) Json.Decode.int)
                        |> Just

                _ ->
                    Nothing

        TimezoneNameRequest requestTimezoneName ->
            case interfaceDiff of
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

        ConsoleLog _ ->
            Nothing

        DomNodeRender domElementToRender ->
            case interfaceDiff of
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
            case interfaceDiff of
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

        WindowEventListen listen ->
            case interfaceDiff of
                AddWindowEventListen addedEventName ->
                    if addedEventName == listen.eventName then
                        Json.Decode.value |> Json.Decode.map listen.on |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        WindowAnimationFrameListen toState ->
            case interfaceDiff of
                AddWindowAnimationFrameListen ->
                    Json.Decode.map Time.millisToPosix Json.Decode.int
                        |> Json.Decode.map toState
                        |> Just

                _ ->
                    Nothing

        DocumentEventListen listen ->
            case interfaceDiff of
                AddDocumentEventListen addedEventName ->
                    if addedEventName == listen.eventName then
                        Json.Decode.value |> Json.Decode.map listen.on |> Just

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


domElementIdJsonDecoder : Json.Decode.Decoder DomElementId
domElementIdJsonDecoder =
    Json.Decode.succeed
        (\tag styles attributes eventListens subs ->
            { tag = tag
            , styles = styles
            , attributes = attributes
            , eventListens = eventListens
            , subs = subs
            }
        )
        |> Json.Decode.Local.andMap (Json.Decode.field "tag" Json.Decode.string)
        |> Json.Decode.Local.andMap (Json.Decode.field "styles" (Json.Decode.dict Json.Decode.string))
        |> Json.Decode.Local.andMap (Json.Decode.field "attributes" (Json.Decode.dict Json.Decode.string))
        |> Json.Decode.Local.andMap
            (Json.Decode.field "eventListens"
                (Json.Decode.map Set.fromList (Json.Decode.list Json.Decode.string))
            )
        |> Json.Decode.Local.andMap
            (Json.Decode.field "subs" (Json.Decode.array domNodeIdJsonDecoder))


{-| The "update" part for an embedded program
-}
update : Config state -> (Event state -> State state -> ( State state, Cmd (Event state) ))
update appConfig =
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
                        |> interfaceDiffToCmds
                        |> List.map (\diff -> appConfig.ports.toJs (diff |> interfaceDiffToJson))
                        |> Cmd.batch
                        |> Cmd.map never
                    )


domElementToId : DomElement state_ -> DomElementId
domElementToId =
    \domElement ->
        { tag = domElement.tag
        , styles = domElement.styles
        , attributes = domElement.attributes
        , eventListens =
            domElement.eventListens |> Dict.foldl (\k _ -> Set.insert k) Set.empty
        , subs =
            domElement.subs |> Array.map domNodeToId
        }


domElementIdToJson : DomElementId -> Json.Encode.Value
domElementIdToJson =
    \domElementId ->
        Json.Encode.object
            [ ( "tag", domElementId.tag |> Json.Encode.string )
            , ( "styles", domElementId.styles |> Json.Encode.dict identity Json.Encode.string )
            , ( "attributes", domElementId.attributes |> Json.Encode.dict identity Json.Encode.string )
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

**Note:**

It is possible for a response to have the same header multiple times.
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
    = AddTimePosixRequest
    | AddTimezoneRequest
    | AddTimezoneNameRequest
    | AddConsoleLog String
    | ReplaceDomNode { path : List Int, domNode : DomNodeId }
    | AddHttpRequest HttpRequestId
    | RemoveHttpRequest HttpRequestId
    | RemoveDom
    | AddWindowEventListen String
    | RemoveWindowEventListen String
    | AddWindowAnimationFrameListen
    | RemoveWindowAnimationFrameListen
    | AddDocumentEventListen String
    | RemoveDocumentEventListen String
    | AddNavigationReplaceUrl String
    | AddNavigationPushUrl String
    | AddNavigationGo Int
    | AddNavigationLoad String
    | AddNavigationReload


{-| Create an elm [`Program`](https://dark.elm.dmy.fr/packages/elm/core/latest/Platform#Program)
with a given [`BrowserApp.Config`](#Config). Short for

    Platform.worker
        { init = \() -> BrowserApp.init yourAppConfig
        , update = BrowserApp.update yourAppConfig
        , subscriptions = BrowserApp.subscriptions yourAppConfig
        }

-}
toProgram : Config state -> Program () (State state) (Event state)
toProgram appConfig =
    Platform.worker
        { init = \() -> init appConfig
        , update = update appConfig
        , subscriptions = subscriptions appConfig
        }
