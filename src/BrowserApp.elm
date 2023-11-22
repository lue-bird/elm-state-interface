module BrowserApp exposing
    ( Config, Interface(..)
    , on
    , DomNodeId(..), DomElementId
    , DomNode(..), DomElement, domElement, domElementAddSubs, domOnEvent
    , toProgram, Event(..), State
    , init, subscriptions, update
    , InterfaceKeys, InterfaceIdOrder, InterfaceId(..), InterfaceToIdTag, InterfaceIdToRenderDomElement, InterfaceIdToRequestTimeNow, InterfaceDiff(..)
    )

{-| A state-interface program running in the browser as the platform

@docs Config, Interface
@docs on


## DOM

@docs DomNodeId, DomElementId
@docs DomNode, DomElement, domElement, domElementAddSubs, domOnEvent


## Program

@docs toProgram, Event, State


## embed

Replace just one part of your elm app with this architecture. Make sure to wire in all 3:

@docs init, subscriptions, update


## internals, safe to ignore

@docs InterfaceKeys, InterfaceIdOrder, InterfaceId, InterfaceToIdTag, InterfaceIdToRenderDomElement, InterfaceIdToRequestTimeNow, InterfaceDiff

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


{-| Incoming and outgoing effects
-}
type Interface state
    = RequestTimeNow { on : Time.Posix -> state }
    | RequestTimezone { on : Time.Zone -> state }
    | RequestTimezoneName { on : Time.ZoneName -> state }
    | ConsoleLog String
    | RenderDomNode (DomNode state)


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
    | ConsoleLogId String
    | IdRenderDomNode


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
            RequestTimeNow requestTimeNow ->
                { on = \event -> requestTimeNow.on event |> stateChange }
                    |> RequestTimeNow

            RequestTimezone requestTimezone ->
                { on = \event -> requestTimezone.on event |> stateChange }
                    |> RequestTimezone

            RequestTimezoneName requestTimezoneName ->
                { on = \event -> requestTimezoneName.on event |> stateChange }
                    |> RequestTimezoneName

            ConsoleLog string ->
                ConsoleLog string

            RenderDomNode domElementToRender ->
                domElementToRender |> domNodeOn stateChange |> RenderDomNode


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
            RequestTimeNow _ ->
                IdRequestTimeNow

            RequestTimezone _ ->
                IdRequestTimezone

            RequestTimezoneName _ ->
                IdRequestTimezoneName

            ConsoleLog string ->
                ConsoleLogId string

            RenderDomNode _ ->
                IdRenderDomNode


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
                        RequestTimeNow _ ->
                            Nothing

                        RequestTimezone _ ->
                            Nothing

                        RequestTimezoneName _ ->
                            Nothing

                        ConsoleLog _ ->
                            Nothing

                        RenderDomNode _ ->
                            RemoveDom |> Just
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
                                RequestTimeNow _ ->
                                    AddRequestTimeNow |> Just

                                RequestTimezone _ ->
                                    AddRequestTimezone |> Just

                                RequestTimezoneName _ ->
                                    AddRequestTimezoneName |> Just

                                ConsoleLog string ->
                                    AddConsoleLog string |> Just

                                RenderDomNode _ ->
                                    Nothing
                        )
               )
            ++ (case ( interfaces.old |> KeysSet.element interfaceKeys IdRenderDomNode, interfaces.updated |> KeysSet.element interfaceKeys IdRenderDomNode ) of
                    ( Emptiable.Filled (RenderDomNode domElementPreviouslyRendered), Emptiable.Filled (RenderDomNode domElementToRender) ) ->
                        ( domElementPreviouslyRendered, domElementToRender )
                            |> domNodeDiff []
                            |> List.map
                                (\subDiff ->
                                    ReplaceDomNode
                                        { path = subDiff.path
                                        , domNode = subDiff.replacementDomNode |> domNodeToId
                                        }
                                )

                    ( Emptiable.Empty _, Emptiable.Filled (RenderDomNode replacementDomNode) ) ->
                        [ ReplaceDomNode
                            { path = []
                            , domNode = replacementDomNode |> domNodeToId
                            }
                        ]

                    ( Emptiable.Filled (RenderDomNode _), _ ) ->
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
        case interfaceDiff of
            AddRequestTimeNow ->
                Json.Encode.object [ ( "addRequestTimeNow", Json.Encode.null ) ]

            AddRequestTimezone ->
                Json.Encode.object [ ( "addRequestTimezoneOffset", Json.Encode.null ) ]

            AddRequestTimezoneName ->
                Json.Encode.object [ ( "addRequestTimezoneName", Json.Encode.null ) ]

            AddConsoleLog string ->
                Json.Encode.object [ ( "addConsoleLog", string |> Json.Encode.string ) ]

            ReplaceDomNode domElementToAdd ->
                Json.Encode.object
                    [ ( "replaceDomNode"
                      , Json.Encode.object
                            [ ( "path", domElementToAdd.path |> Json.Encode.list Json.Encode.int )
                            , ( "domNode", domElementToAdd.domNode |> domNodeIdToJson )
                            ]
                      )
                    ]

            RemoveDom ->
                Json.Encode.object
                    [ ( "removeDom", Json.Encode.null ) ]


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
        [ Json.Decode.map (\() -> AddRequestTimeNow)
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
        RequestTimeNow requestTimeNow ->
            case interfaceDiff of
                AddRequestTimeNow ->
                    Json.Decode.succeed requestTimeNow.on
                        |> Json.Decode.Extra.andMap (Json.Decode.map Time.millisToPosix Json.Decode.int)
                        |> Just

                _ ->
                    Nothing

        RequestTimezone requestTimezone ->
            case interfaceDiff of
                AddRequestTimezone ->
                    Json.Decode.succeed requestTimezone.on
                        |> Json.Decode.Extra.andMap
                            (Json.Decode.map (\offset -> Time.customZone offset []) Json.Decode.int)
                        |> Just

                _ ->
                    Nothing

        RequestTimezoneName requestTimezoneName ->
            case interfaceDiff of
                AddRequestTimezoneName ->
                    Json.Decode.succeed requestTimezoneName.on
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

        RenderDomNode domElementToRender ->
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


{-| Individual messages to js. Also used to identify responses with the same part in the interface
-}
type InterfaceDiff
    = AddRequestTimeNow
    | AddRequestTimezone
    | AddRequestTimezoneName
    | AddConsoleLog String
    | ReplaceDomNode { path : List Int, domNode : DomNodeId }
    | RemoveDom


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


{-| Create a DOM element with a given tag.
For example for <p>text</p>

    BrowserApp.domElement "p"
        |> BrowserApp.domElementAddSubs [ BrowserApp.Text "text" ]

-}
domElement : String -> DomElement state_
domElement tag =
    { tag = tag
    , eventListeners = Dict.empty
    , styles = Dict.empty
    , attributes = Dict.empty
    , subs = Array.empty
    }


{-| Append child-[`DomNode`](#DomNode)s
-}
domElementAddSubs : List (DomNode state) -> (DomElement state -> DomElement state)
domElementAddSubs subs =
    \domElement_ ->
        { domElement_ | subs = Array.append domElement_.subs (Array.fromList subs) }


{-| Listen for a specific html event.
-}
domOnEvent : String -> (Json.Decode.Value -> state) -> (DomElement state -> DomElement state)
domOnEvent eventName eventToState =
    \domElement_ ->
        { domElement_
            | eventListeners =
                domElement_.eventListeners |> Dict.insert eventName eventToState
        }
