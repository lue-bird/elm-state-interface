module BrowserApp exposing
    ( Config
    , toProgram, Event(..), State
    , init, subscriptions, update
    , Interface(..), on
    , DomNode(..), DomElement
    , HttpBody(..), HttpExpect(..), HttpHeader, HttpRequest, HttpError(..), HttpMetadata
    , InterfaceKeys, InterfaceIdOrder
    , InterfaceId(..), InterfaceToIdTag, InterfaceIdToRenderDomElement, InterfaceIdToRequestTimeNow, DomElementId, DomNodeId(..)
    , InterfaceDiff(..)
    )

{-| A state-interface program running in the browser as the platform

@docs Config


## Program

@docs toProgram, Event, State


## embed

Replace just one part of your elm app with this architecture. Make sure to wire in all 3:

@docs init, subscriptions, update


# interface types

@docs Interface, on


## DOM

@docs DomNode, DomElement


## HTTP

@docs HttpBody, HttpExpect, HttpHeader, HttpRequest, HttpError, HttpMetadata


## internals, safe to ignore

@docs InterfaceKeys, InterfaceIdOrder
@docs InterfaceId, InterfaceToIdTag, InterfaceIdToRenderDomElement, InterfaceIdToRequestTimeNow, DomElementId, DomNodeId
@docs InterfaceDiff

-}

import Array exposing (Array)
import Char.Order
import Dict exposing (Dict)
import Emptiable exposing (Emptiable)
import Json.Decode
import Json.Decode.Extra
import Json.Encode
import Keys exposing (Key, Keys)
import KeysSet exposing (KeysSet)
import List.Order
import Map exposing (Mapping)
import N exposing (N1)
import Order exposing (Ordering)
import Possibly exposing (Possibly)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Set exposing (Set)
import String.Order
import Time
import Typed


{-| To annotate a program state. E.g.

    main : Program () (BrowserApp.State YourState) (BrowserApp.Event YourState)
    main =
        BrowserApp.toProgram ...

-}
type alias State appState =
    { interface : Emptiable (KeysSet (Interface appState) (InterfaceKeys appState) N1) Possibly
    , appState : appState
    }


{-| Safely ignore. Identification and order of an [`Interface`](#Interface)
-}
type alias InterfaceKeys state =
    Key (Interface state) (Order.By InterfaceToIdTag InterfaceIdOrder) InterfaceId N1


{-| Safely ignore. Tag for the ordering of an [`Interface`](#Interface)
-}
type alias InterfaceIdOrder =
    Order.OnTieNext
        (Order.On InterfaceIdToRenderDomElement Order.Tie)
        (Order.On InterfaceIdToRequestTimeNow Order.Tie)


{-| Safely ignore. Tag for the identification mapping of an [`Interface`](#Interface) → [`InterfaceId`](#InterfaceId)
-}
type InterfaceToIdTag
    = InterfaceToIdTag


{-| What's needed to create a program
-}
type alias Config state =
    { initialState : state
    , interface : state -> List (Interface state)
    , ports :
        { toJs : Json.Encode.Value -> Cmd Never
        , fromJs : (Json.Encode.Value -> Event state) -> Sub (Event state)
        }
    }


{-| Incoming and outgoing effects.
To create one, use the helpers in `BrowserApp.Time`, `.Dom`, `.Http` etc.

  - `HttpRequest`: Send an [Http request](#HttpRequest) - similar to `elm/http`'s [`Http.Task`](https://package.elm-lang.org/packages/elm/http/latest/Http#task)

-}
type Interface state
    = TimeCurrentRequest (Time.Posix -> state)
    | TimezoneRequest (Time.Zone -> state)
    | TimezoneNameRequest (Time.ZoneName -> state)
    | ConsoleLog String
    | DomNodeRender (DomNode state)
    | HttpRequest (HttpRequest state)
    | WindowEventListen { eventName : String, on : Json.Decode.Value -> state }
    | DocumentEventListen { eventName : String, on : Json.Decode.Value -> state }
    | NavigationReplaceUrl String
    | NavigationPushUrl String
    | NavigationGo Int
    | NavigationLoad String
    | NavigationReload


type alias HttpRequest state =
    RecordWithoutConstructorFunction
        { url : String
        , method : String
        , headers : List HttpHeader
        , body : HttpBody
        , expect : HttpExpect state
        , timeout : Maybe Int
        }


{-| An Http header for configuring a request. TODO switch to named
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

  - `HttpEmptyBody`: Create an empty body for your request.
    This is useful for `GET` requests and `POST` requests where you are not sending any data.

  - `HttpStringBody`: Put a `String` in the body of your request. Defining `BrowserApp.Http.jsonBody` looks like this:

        import Json.Encode

        jsonBody : Json.Encode.Value -> BrowserApp.HttpBody
        jsonBody value =
            BrowserApp.HttpStringBody "application/json" (Json.Encode.encode 0 value)

    The first argument is a [MIME type](https://en.wikipedia.org/wiki/Media_type) of the body.

-}
type HttpBody
    = HttpEmptyBody
    | HttpStringBody { mimeType : String, content : String }


type alias HttpRequestId =
    RecordWithoutConstructorFunction
        { url : String
        , method : String
        , headers : List HttpHeader
        , body : HttpBody
        , expect : HttpExpectId
        , timeout : Maybe Int
        }


type HttpExpectId
    = IdHttpExpectJson
    | IdHttpExpectString
    | IdHttpExpectWhatever


{-| Plain text or a [`DomElement`](#DomElement)
-}
type DomNode state
    = DomText String
    | DomElement (DomElement state)


{-| A primitive for svg/html.
Compare with [`elm/virtual-dom`](https://dark.elm.dmy.fr/packages/elm/virtual-dom/latest/)
-}
type alias DomElement state =
    RecordWithoutConstructorFunction
        { tag : String
        , styles : Dict String String
        , attributes : Dict String String
        , eventListeners : Dict String (Json.Decode.Value -> state)
        , subs : Array (DomNode state)
        }


{-| Identifier for an [`Interface`](#Interface)
-}
type InterfaceId
    = IdRequestTimeNow
    | IdRequestTimezone
    | IdRequestTimezoneName
    | IdConsoleLog String
    | IdRenderDomNode
    | IdHttpRequest HttpRequestId
    | IdWindowEventListen String
    | IdDocumentEventListen String
    | IdNavigationReplaceUrl String
    | IdNavigationPushUrl String
    | IdNavigationGo Int
    | IdNavigationLoad String
    | IdNavigationReload


{-| Identifier for a [`DomElement`](#DomElement)
-}
type alias DomElementId =
    RecordWithoutConstructorFunction
        { tag : String
        , styles : Dict String String
        , attributes : Dict String String
        , eventListeners : Set String
        , subs : Array DomNodeId
        }


{-| Identifier for an [`DomNode`](#DomNode)
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
                (Order.by (Map.tag () .eventListeners)
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


type DictKeys
    = DictKeys


type SetToList
    = SetToList


domNodeOn : (state -> mappedState) -> (DomNode state -> DomNode mappedState)
domNodeOn stateChange =
    \domElementToMap ->
        case domElementToMap of
            DomText text ->
                DomText text

            DomElement domElement_ ->
                domElement_ |> domElementOn stateChange |> DomElement


{-| Map the state constructed by the [`Interface`](#Interface)
-}
on : (state -> mappedState) -> (Interface state -> Interface mappedState)
on stateChange =
    \interface ->
        case interface of
            TimeCurrentRequest requestTimeNow ->
                (\event -> requestTimeNow event |> stateChange)
                    |> TimeCurrentRequest

            TimezoneRequest requestTimezone ->
                (\event -> requestTimezone event |> stateChange)
                    |> TimezoneRequest

            TimezoneNameRequest requestTimezoneName ->
                (\event -> requestTimezoneName event |> stateChange)
                    |> TimezoneNameRequest

            ConsoleLog string ->
                ConsoleLog string

            DomNodeRender domElementToRender ->
                domElementToRender |> domNodeOn stateChange |> DomNodeRender

            HttpRequest httpRequest ->
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
                    |> HttpRequest

            WindowEventListen listener ->
                { eventName = listener.eventName, on = \value -> listener.on value |> stateChange }
                    |> WindowEventListen

            DocumentEventListen listener ->
                { eventName = listener.eventName, on = \value -> listener.on value |> stateChange }
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


domElementOn : (state -> mappedState) -> (DomElement state -> DomElement mappedState)
domElementOn stateChange =
    \domElementToMap ->
        { tag = domElementToMap.tag
        , styles = domElementToMap.styles
        , attributes = domElementToMap.attributes
        , eventListeners =
            domElementToMap.eventListeners
                |> Dict.map (\_ listener -> \event -> listener event |> stateChange)
        , subs =
            domElementToMap.subs |> Array.map (domNodeOn stateChange)
        }


{-| To annotate a program event. E.g.

    main : Program () (BrowserApp.State YourState) (BrowserApp.Event YourState)
    main =
        BrowserApp.toProgram ...

-}
type Event appState
    = InterfaceDiffFailedToDecode Json.Decode.Error
    | InterfaceEventDataFailedToDecode Json.Decode.Error
    | InterfaceEventIgnored InterfaceDiff
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
                && ((aElement.eventListeners |> Dict.keys) == (bElement.eventListeners |> Dict.keys))
                && ((aElement.subs |> Array.length) == (bElement.subs |> Array.length))
        then
            List.map2 (\( subIndex, aSub ) bSub -> domNodeDiff (subIndex :: path) ( aSub, bSub ))
                (aElement.subs |> Array.toIndexedList)
                (bElement.subs |> Array.toList)
                |> List.concat

        else
            [ { path = path, replacementDomNode = bElement |> DomElement } ]


interfaceKeys : Keys (Interface state) (InterfaceKeys state) N1
interfaceKeys =
    Keys.oneBy interfaceToIdMapping interfaceIdOrder


interfaceToIdMapping : Mapping (Interface state_) InterfaceToIdTag InterfaceId
interfaceToIdMapping =
    Map.tag InterfaceToIdTag interfaceToId


interfaceToId : Interface state_ -> InterfaceId
interfaceToId =
    \interface ->
        case interface of
            TimeCurrentRequest _ ->
                IdRequestTimeNow

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

            WindowEventListen listener ->
                IdWindowEventListen listener.eventName

            DocumentEventListen listener ->
                IdDocumentEventListen listener.eventName

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


interfaceIdOrder : Ordering InterfaceId InterfaceIdOrder
interfaceIdOrder =
    Order.on idToRenderDomElementMapping Order.tie
        |> Order.onTie (Order.on idToRequestTimeNowMapping Order.tie)


idToRenderDomElementMapping : Mapping InterfaceId InterfaceIdToRenderDomElement (Maybe ())
idToRenderDomElementMapping =
    Map.tag InterfaceIdToRenderDomElement
        (\interfaceId ->
            case interfaceId of
                IdRenderDomNode ->
                    () |> Just

                _ ->
                    Nothing
        )


idToRequestTimeNowMapping : Mapping InterfaceId InterfaceIdToRequestTimeNow (Maybe ())
idToRequestTimeNowMapping =
    Map.tag InterfaceIdToRequestTimeNow
        (\interfaceId ->
            case interfaceId of
                IdRequestTimeNow ->
                    () |> Just

                _ ->
                    Nothing
        )


domNodeToId : DomNode state_ -> DomNodeId
domNodeToId domNode =
    case domNode of
        DomText text ->
            DomTextId text

        DomElement element ->
            DomElementId (element |> domElementToId)


interfaceDiffToCmds :
    { old : Emptiable (KeysSet (Interface state) (InterfaceKeys state) N1) Possibly
    , updated : Emptiable (KeysSet (Interface state) (InterfaceKeys state) N1) Possibly
    }
    -> List InterfaceDiff
interfaceDiffToCmds =
    \interfaces ->
        (interfaces.old
            |> KeysSet.except interfaceKeys
                (interfaces.updated |> KeysSet.toKeys interfaceKeys)
            |> KeysSet.remove interfaceKeys IdRenderDomNode
            |> KeysSet.toList interfaceKeys
            |> List.filterMap
                (\interface ->
                    case interface of
                        TimeCurrentRequest _ ->
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

                        WindowEventListen listener ->
                            RemoveWindowEventListener listener.eventName |> Just

                        DocumentEventListen listener ->
                            RemoveDocumentEventListener listener.eventName |> Just

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
        )
            ++ (interfaces.updated
                    |> KeysSet.except interfaceKeys
                        (interfaces.old |> KeysSet.toKeys interfaceKeys)
                    |> KeysSet.remove interfaceKeys IdRenderDomNode
                    |> KeysSet.toList interfaceKeys
                    |> List.filterMap
                        (\interface ->
                            case interface of
                                TimeCurrentRequest _ ->
                                    AddTimeCurrentRequest |> Just

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

                                WindowEventListen listener ->
                                    AddWindowEventListener listener.eventName |> Just

                                DocumentEventListen listener ->
                                    AddDocumentEventListener listener.eventName |> Just

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
               )
            ++ (case ( interfaces.old |> KeysSet.element interfaceKeys IdRenderDomNode, interfaces.updated |> KeysSet.element interfaceKeys IdRenderDomNode ) of
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
               )


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
                AddTimeCurrentRequest ->
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

                AddWindowEventListener eventName ->
                    ( "addWindowEventListener", eventName |> Json.Encode.string )

                RemoveWindowEventListener eventName ->
                    ( "removeWindowEventListener", eventName |> Json.Encode.string )

                AddDocumentEventListener eventName ->
                    ( "addDocumentEventListener", eventName |> Json.Encode.string )

                RemoveDocumentEventListener eventName ->
                    ( "removeDocumentEventListener", eventName |> Json.Encode.string )

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


{-| The "init" part for an embedded program
-}
init : Config state -> ( State state, Cmd (Event state) )
init appConfig =
    let
        initialInterface : Emptiable (KeysSet (Interface state) (InterfaceKeys state) N1) Possibly
        initialInterface =
            appConfig.initialState
                |> appConfig.interface
                |> KeysSet.fromList interfaceKeys
    in
    ( { interface = initialInterface
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
    \state ->
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
                                InterfaceEventIgnored interfaceDiff

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
        [ Json.Decode.map (\() -> AddTimeCurrentRequest)
            (Json.Decode.field "requestTimeNow" (Json.Decode.null ()))
        , Json.Decode.map ReplaceDomNode
            (Json.Decode.field "replaceDomNode"
                (Json.Decode.succeed (\path domNode -> { path = path, domNode = domNode })
                    |> Json.Decode.Extra.andMap (Json.Decode.field "path" (Json.Decode.list Json.Decode.int))
                    |> Json.Decode.Extra.andMap (Json.Decode.field "domNode" domNodeIdJsonDecoder)
                )
            )
        , Json.Decode.map (\() -> RemoveDom)
            (Json.Decode.field "removeDom" (Json.Decode.null ()))
        ]


eventDataAndConstructStateJsonDecoder : InterfaceDiff -> Interface state -> Maybe (Json.Decode.Decoder state)
eventDataAndConstructStateJsonDecoder interfaceDiff interface =
    case interface of
        TimeCurrentRequest requestTimeNow ->
            case interfaceDiff of
                AddTimeCurrentRequest ->
                    Json.Decode.succeed requestTimeNow
                        |> Json.Decode.Extra.andMap (Json.Decode.map Time.millisToPosix Json.Decode.int)
                        |> Just

                _ ->
                    Nothing

        TimezoneRequest requestTimezone ->
            case interfaceDiff of
                AddTimezoneRequest ->
                    Json.Decode.succeed requestTimezone
                        |> Json.Decode.Extra.andMap
                            (Json.Decode.map (\offset -> Time.customZone offset []) Json.Decode.int)
                        |> Just

                _ ->
                    Nothing

        TimezoneNameRequest requestTimezoneName ->
            case interfaceDiff of
                AddTimezoneNameRequest ->
                    Json.Decode.succeed requestTimezoneName
                        |> Json.Decode.Extra.andMap
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
                        |> Json.Decode.Extra.andMap (Json.Decode.field "innerPath" (Json.Decode.list Json.Decode.int))
                        |> Json.Decode.Extra.andMap (Json.Decode.field "name" Json.Decode.string)
                        |> Json.Decode.Extra.andMap
                            (Json.Decode.field "event" Json.Decode.value)
                        |> Json.Decode.andThen
                            (\specificEvent ->
                                case domElementToRender |> domElementAtReversePath ((specificEvent.innerPath ++ domNodeReplacement.path) |> List.reverse) of
                                    Nothing ->
                                        Json.Decode.fail "origin element of event not found"

                                    Just (DomText _) ->
                                        Json.Decode.fail "origin element of event leads to text, not element"

                                    Just (DomElement foundDomElement) ->
                                        case foundDomElement.eventListeners |> Dict.get specificEvent.name of
                                            Nothing ->
                                                Json.Decode.fail "received event for element without listener"

                                            Just listenToEvent ->
                                                listenToEvent specificEvent.event |> Json.Decode.succeed
                            )
                    )
                        |> Just

                _ ->
                    Nothing

        HttpRequest httpRequest ->
            case interfaceDiff of
                AddHttpRequest addedHttpRequestId ->
                    if (httpRequest |> httpRequestToId) == addedHttpRequestId then
                        httpExpectJsonDecoder httpRequest.expect |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        WindowEventListen listener ->
            case interfaceDiff of
                AddWindowEventListener addedEventName ->
                    if addedEventName == listener.eventName then
                        Json.Decode.value |> Json.Decode.map listener.on |> Just

                    else
                        Nothing

                _ ->
                    Nothing

        DocumentEventListen listener ->
            case interfaceDiff of
                AddDocumentEventListener addedEventName ->
                    if addedEventName == listener.eventName then
                        Json.Decode.value |> Json.Decode.map listener.on |> Just

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
domElementAtReversePath path =
    \domNode ->
        case path of
            [] ->
                domNode |> Just

            subIndex :: parentsOfSub ->
                case domNode of
                    DomText _ ->
                        Nothing

                    DomElement domElement_ ->
                        case domElement_.subs |> Array.get subIndex of
                            Nothing ->
                                Nothing

                            Just subNodeAtIndex ->
                                subNodeAtIndex |> domElementAtReversePath parentsOfSub


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
        (\tag styles attributes eventListeners subs ->
            { tag = tag
            , styles = styles
            , attributes = attributes
            , eventListeners = eventListeners
            , subs = subs
            }
        )
        |> Json.Decode.Extra.andMap (Json.Decode.field "tag" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "styles" (Json.Decode.dict Json.Decode.string))
        |> Json.Decode.Extra.andMap (Json.Decode.field "attributes" (Json.Decode.dict Json.Decode.string))
        |> Json.Decode.Extra.andMap
            (Json.Decode.field "eventListeners"
                (Json.Decode.map Set.fromList (Json.Decode.list Json.Decode.string))
            )
        |> Json.Decode.Extra.andMap
            (Json.Decode.field "subs" (Json.Decode.array domNodeIdJsonDecoder))


{-| The "update" part for an embedded program
-}
update : Config state -> (Event state -> State state -> ( State state, Cmd (Event state) ))
update appConfig =
    \event ->
        case event of
            InterfaceEventIgnored interfaceDiff ->
                \state ->
                    let
                        _ =
                            Debug.log "info: ignored event from this interface" interfaceDiff
                    in
                    ( state, Cmd.none )

            InterfaceDiffFailedToDecode jsonError ->
                \state ->
                    let
                        _ =
                            Debug.log "interface diff failed to decode" (jsonError |> Json.Decode.errorToString)
                    in
                    ( state, Cmd.none )

            InterfaceEventDataFailedToDecode jsonError ->
                \state ->
                    let
                        _ =
                            Debug.log "InterfaceEventDataFailedToDecode" (jsonError |> Json.Decode.errorToString)
                    in
                    ( state, Cmd.none )

            AppEventToNewAppState updatedAppState ->
                \oldState ->
                    let
                        updatedInterface : Emptiable (KeysSet (Interface state) (InterfaceKeys state) N1) Possibly
                        updatedInterface =
                            updatedAppState
                                |> appConfig.interface
                                |> KeysSet.fromList interfaceKeys
                    in
                    ( { interface = updatedInterface, appState = updatedAppState }
                    , { old = oldState.interface, updated = updatedInterface }
                        |> interfaceDiffToCmds
                        |> List.map (\diff -> appConfig.ports.toJs (diff |> interfaceDiffToJson))
                        |> Cmd.batch
                        |> Cmd.map never
                    )


domElementToId : DomElement state_ -> DomElementId
domElementToId =
    \domElement_ ->
        { tag = domElement_.tag
        , styles = domElement_.styles
        , attributes = domElement_.attributes
        , eventListeners =
            domElement_.eventListeners |> Dict.foldl (\k _ -> Set.insert k) Set.empty
        , subs =
            domElement_.subs |> Array.map domNodeToId
        }


domElementIdToJson : DomElementId -> Json.Encode.Value
domElementIdToJson =
    \domElementId ->
        Json.Encode.object
            [ ( "tag", domElementId.tag |> Json.Encode.string )
            , ( "styles", domElementId.styles |> Json.Encode.dict identity Json.Encode.string )
            , ( "attributes", domElementId.attributes |> Json.Encode.dict identity Json.Encode.string )
            , ( "eventListeners", domElementId.eventListeners |> Json.Encode.set Json.Encode.string )
            , ( "subs", domElementId.subs |> Json.Encode.array domNodeIdToJson )
            ]


httpRequestToId : HttpRequest state -> HttpRequestId
httpRequestToId =
    \httpRequest ->
        { url = httpRequest.url
        , method = httpRequest.method |> String.toUpper
        , headers = httpRequest.headers
        , body = httpRequest.body
        , expect = httpRequest.expect |> httpExpectToId
        , timeout = httpRequest.timeout
        }


httpExpectToId : HttpExpect state -> HttpExpectId
httpExpectToId =
    \httpExpect ->
        case httpExpect of
            HttpExpectWhatever _ ->
                IdHttpExpectWhatever

            HttpExpectString _ ->
                IdHttpExpectString

            HttpExpectJson _ ->
                IdHttpExpectJson


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


httpErrorJsonDecoder : HttpRequest state_ -> Json.Decode.Decoder HttpError
httpErrorJsonDecoder r =
    Json.Decode.field "reason" Json.Decode.string
        |> Json.Decode.andThen
            (\code ->
                case code of
                    "TIMEOUT" ->
                        Json.Decode.succeed HttpTimeout

                    "NETWORK_ERROR" ->
                        Json.Decode.succeed HttpNetworkError

                    "BAD_URL" ->
                        Json.Decode.succeed (HttpBadUrl r.url)

                    _ ->
                        Json.Decode.fail ("Unknown error code: " ++ code)
            )


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
                                    Json.Decode.map Ok (Json.Decode.succeed ())

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
        |> Json.Decode.Extra.andMap (Json.Decode.field "url" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "statusCode" Json.Decode.int)
        |> Json.Decode.Extra.andMap (Json.Decode.field "statusText" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "headers" (Json.Decode.dict Json.Decode.string))


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
        HttpEmptyBody ->
            headers

        HttpStringBody stringBodyInfo ->
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
        HttpStringBody stringBodyInfo ->
            stringBodyInfo.content |> Json.Encode.string

        HttpEmptyBody ->
            Json.Encode.null


{-| Individual messages to js. Also used to identify responses with the same part in the interface
-}
type InterfaceDiff
    = AddTimeCurrentRequest
    | AddTimezoneRequest
    | AddTimezoneNameRequest
    | AddConsoleLog String
    | ReplaceDomNode { path : List Int, domNode : DomNodeId }
    | AddHttpRequest HttpRequestId
    | RemoveHttpRequest HttpRequestId
    | RemoveDom
    | AddWindowEventListener String
    | RemoveWindowEventListener String
    | AddDocumentEventListener String
    | RemoveDocumentEventListener String
    | AddNavigationReplaceUrl String
    | AddNavigationPushUrl String
    | AddNavigationGo Int
    | AddNavigationLoad String
    | AddNavigationReload


{-| Create an elm [`Program`](https://dark.elm.dmy.fr/packages/elm/core/latest/Platform#Program). Short for

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


{-| Safely ignore. Tag for the mapping [`Interface` → `RenderDomNode`](#Interface)
-}
type InterfaceIdToRenderDomElement
    = InterfaceIdToRenderDomElement


{-| Safely ignore. Tag for the mapping [`Interface` → `RequestTimeNow`](#Interface)
-}
type InterfaceIdToRequestTimeNow
    = InterfaceIdToRequestTimeNow
