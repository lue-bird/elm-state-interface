module Web.Dom exposing
    ( text
    , element, elementNamespaced
    , Modifier, ModifierSingle(..), attribute, attributeNamespaced, style
    , listenTo, listenToPreventingDefaultAction
    , scrollToShow, scrollPositionRequest, scrollToPosition
    , modifierFutureMap, modifierBatch, modifierNone
    , futureMap, render
    , Node(..), Element
    )

{-| Helpers for [DOM nodes](#Node) as part of an [`Interface`](Web#Interface).

These are primitives used for svg and html.
Compare with [`elm/virtual-dom`](https://dark.elm.dmy.fr/packages/elm/virtual-dom/latest/)

@docs text
@docs element, elementNamespaced
@docs Modifier, ModifierSingle, attribute, attributeNamespaced, style
@docs listenTo, listenToPreventingDefaultAction
@docs scrollToShow, scrollPositionRequest, scrollToPosition
@docs modifierFutureMap, modifierBatch, modifierNone
@docs futureMap, render
@docs Node, Element

-}

import Dict
import Json.Decode
import List.LocalExtra
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Rope exposing (Rope)
import Web


{-| An [`Interface`](Web#Interface) for displaying a given [`Web.Dom.Node`](Web-Dom#Node).
-}
render : Node future -> Web.Interface future
render =
    \domNode ->
        domNode
            |> nodeFlatten
            |> List.map (\nodeAndPath -> nodeAndPath |> Web.DomNodeRender |> Web.InterfaceWithFuture)
            |> Rope.fromList


nodeFlatten : Node future -> List { path : List Int, node : Web.DomTextOrElementHeader future }
nodeFlatten =
    \node -> node |> nodeFlattenToRope |> Rope.toList


nodeFlattenToRope : Node future -> Rope { path : List Int, node : Web.DomTextOrElementHeader future }
nodeFlattenToRope =
    \node ->
        case node of
            Text string ->
                { path = [], node = Web.DomText string } |> Rope.singleton

            Element element_ ->
                Rope.prepend
                    { path = [], node = Web.DomElementHeader element_.header }
                    (List.foldl
                        (\sub soFar ->
                            { subIndex = soFar.subIndex + 1
                            , rope =
                                Rope.appendTo
                                    soFar.rope
                                    (Rope.map (\layerPart -> { layerPart | path = soFar.subIndex :: layerPart.path })
                                        (nodeFlattenToRope sub)
                                    )
                            }
                        )
                        { subIndex = 0, rope = Rope.empty }
                        element_.subs
                    ).rope


{-| Wire events from this [`Web.Dom.Node`](Web-Dom#Node) to a specific event, for example

    buttonUi "start"
        |> Web.Dom.futureMap (\Clicked -> StartButtonClicked)

with

    buttonUi : String -> Web.Dom.Node ButtonEvent
    buttonUi label =
        Web.Dom.element "button"
            [ Web.Dom.listenTo "click"
                |> Web.Dom.modifierFutureMap (\_ -> Clicked)
            ]
            [ Web.Dom.text label ]

    type ButtonEvent
        = Clicked

-}
futureMap : (future -> mappedFuture) -> (Node future -> Node mappedFuture)
futureMap futureChange =
    \domElementToMap ->
        case domElementToMap of
            Text string ->
                Text string

            Element domElement ->
                domElement |> elementFutureMap futureChange |> Element


elementFutureMap : (future -> mappedFuture) -> (Element future -> Element mappedFuture)
elementFutureMap futureChange =
    \domElementToMap ->
        { header = domElementToMap.header |> domElementHeaderFutureMap futureChange
        , subs =
            domElementToMap.subs |> List.map (\node -> node |> futureMap futureChange)
        }


domElementHeaderFutureMap : (future -> mappedFuture) -> (Web.DomElementHeader future -> Web.DomElementHeader mappedFuture)
domElementHeaderFutureMap futureChange =
    \domElementToMap ->
        { namespace = domElementToMap.namespace
        , tag = domElementToMap.tag
        , styles = domElementToMap.styles
        , attributes = domElementToMap.attributes
        , attributesNamespaced = domElementToMap.attributesNamespaced
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


{-| Plain text [DOM `Node`](#Node)
-}
text : String -> Node future_
text =
    Text


elementWithMaybeNamespace : Maybe String -> String -> List (Modifier future) -> List (Node future) -> Node future
elementWithMaybeNamespace maybeNamespace tag modifiers subs =
    let
        modifierList : List (ModifierSingle future)
        modifierList =
            modifiers |> modifierBatch |> Rope.toList
    in
    { header =
        { namespace = maybeNamespace
        , tag = tag
        , scrollToPosition =
            modifierList
                |> List.LocalExtra.firstJustMap
                    (\modifier ->
                        case modifier of
                            ScrollToPosition position ->
                                position |> Just

                            _ ->
                                Nothing
                    )
        , scrollToShow =
            modifierList
                |> List.LocalExtra.firstJustMap
                    (\modifier ->
                        case modifier of
                            ScrollToShow alignment ->
                                alignment |> Just

                            _ ->
                                Nothing
                    )
        , scrollPositionRequest =
            modifierList
                |> List.LocalExtra.firstJustMap
                    (\modifier ->
                        case modifier of
                            ScrollPositionRequest positionRequest ->
                                positionRequest |> Just

                            _ ->
                                Nothing
                    )
        , eventListens =
            modifierList
                |> List.filterMap
                    (\modifier ->
                        case modifier of
                            Listen listen ->
                                ( listen.eventName
                                , { on = listen.on
                                  , defaultActionHandling = listen.defaultActionHandling
                                  }
                                )
                                    |> Just

                            _ ->
                                Nothing
                    )
                |> Dict.fromList
        , styles =
            modifierList
                |> List.filterMap
                    (\modifier ->
                        case modifier of
                            Style keyValue ->
                                ( keyValue.key, keyValue.value ) |> Just

                            _ ->
                                Nothing
                    )
                |> Dict.fromList
        , attributes =
            modifierList
                |> List.filterMap
                    (\modifier ->
                        case modifier of
                            Attribute keyValue ->
                                case keyValue.namespace of
                                    Just _ ->
                                        Nothing

                                    Nothing ->
                                        ( keyValue.key, keyValue.value ) |> Just

                            _ ->
                                Nothing
                    )
                |> Dict.fromList
        , attributesNamespaced =
            modifierList
                |> List.filterMap
                    (\modifier ->
                        case modifier of
                            Attribute keyValue ->
                                case keyValue.namespace of
                                    Just namespace ->
                                        ( ( namespace, keyValue.key ), keyValue.value ) |> Just

                                    Nothing ->
                                        Nothing

                            _ ->
                                Nothing
                    )
                |> Dict.fromList
        }
    , subs = subs
    }
        |> Element


{-| Create a DOM element with a given tag, [`Modifier`](#Modifier)s and sub-[node](Web-Dom#Node)s.
For example to get `<p>flying</p>`

    Web.Dom.element "p"
        []
        [ Web.Dom.text "flying" ]

To create SVG elements, use [`Web.Svg.element`](Web-Svg#element)

-}
element : String -> List (Modifier future) -> List (Node future) -> Node future
element tag modifiers subs =
    elementWithMaybeNamespace Nothing tag modifiers subs


{-| Create a DOM element with a given namespace, tag, [`Modifier`](#Modifier)s and sub-[node](Web-Dom#Node)s.
For example, [`Web.Svg`](Web-Svg) defines its elements using

    element : String -> List (Modifier future) -> List (DomNode future) -> DomNode future
    element tag modifiers subs =
        Web.Dom.elementNamespaced "http://www.w3.org/2000/svg" tag modifiers subs

-}
elementNamespaced : String -> String -> List (Modifier future) -> List (Node future) -> Node future
elementNamespaced namespace tag modifiers subs =
    elementWithMaybeNamespace (namespace |> Just) tag modifiers subs


{-| Set the behavior of a [`Web.Dom.element`](Web-Dom#element).
To create one, use [`attribute`](#attribute), [`style`](#style), [`listenTo`](#listenTo).
To combine multiple, use [`Web.Dom.modifierBatch`](#modifierBatch) and [`Web.Dom.modifierNone`](#modifierNone)

For example to get `<a href="https://elm-lang.org">elm</a>`

    Web.Dom.element "a"
        [ Web.Dom.attribute "href" "https://elm-lang.org" ]
        [ Web.Dom.text "elm" ]

Btw: If you can think of a nicer name for this like "customization", "characteristic" or "aspect",
please [open an issue](https://github.com/lue-bird/elm-state-interface/issues/new)!

-}
type alias Modifier future =
    Rope (ModifierSingle future)


{-| Combine multiple [`Modifier`](#Modifier)s into one.
-}
modifierBatch : List (Modifier future) -> Modifier future
modifierBatch =
    \modifiers -> modifiers |> Rope.fromList |> Rope.concat


{-| Doing nothing as a [`Modifier`](#Modifier). These two examples are equivalent:

    Web.Dom.modifierBatch
        [ a, Web.Dom.modifierNone, b ]

and

    Web.Dom.modifierBatch
        (List.filterMap identity
            [ a |> Just, Nothing, b |> Just ]
        )

-}
modifierNone : Modifier future_
modifierNone =
    Rope.empty


{-| An individual [`Modifier`](#Modifier).
Create using [`attribute`](#attribute), [`style`](#style), [`listenTo`](#listenTo).
-}
type ModifierSingle future
    = Attribute { namespace : Maybe String, key : String, value : String }
    | Style { key : String, value : String }
    | ScrollToPosition { fromLeft : Float, fromTop : Float }
    | ScrollToShow { x : Web.DomElementVisibilityAlignment, y : Web.DomElementVisibilityAlignment }
    | ScrollPositionRequest ({ fromLeft : Float, fromTop : Float } -> future)
    | Listen
        { eventName : String
        , on : Json.Decode.Value -> future
        , defaultActionHandling : Web.DefaultActionHandling
        }


{-| A key-value attribute [`Modifier`](#Modifier)
-}
attribute : String -> String -> Modifier future_
attribute key value =
    { namespace = Nothing, key = key, value = value } |> Attribute |> Rope.singleton


{-| A namespaced key-value attribute [`Modifier`](#Modifier).
For example, you could define an SVG xlink href attribute as

    attributeXlinkHref : String -> Modifier msg
    attributeXlinkHref value =
        Web.Dom.attributeNamespaced "http://www.w3.org/1999/xlink" "xlink:href" value

-}
attributeNamespaced : String -> String -> String -> Modifier future_
attributeNamespaced namespace key value =
    { namespace = namespace |> Just, key = key, value = value } |> Attribute |> Rope.singleton


{-| A key-value style [`Modifier`](#Modifier)
-}
style : String -> String -> Modifier future_
style key value =
    { key = key, value = value } |> Style |> Rope.singleton


{-| Listen for a specific DOM event on the [`Web.Dom.Element`](Web-Dom#Element).
Use [`modifierFutureMap`](#modifierFutureMap) to wire this to a specific event.

If you want to override the browser's default behavior for that event,
use [`listenToPreventingDefaultAction`](#listenToPreventingDefaultAction)

-}
listenTo : String -> Modifier Json.Decode.Value
listenTo eventName =
    { eventName = eventName, on = identity, defaultActionHandling = Web.DefaultActionExecute }
        |> Listen
        |> Rope.singleton


{-| Like [`listenTo`](#listenTo) but [preventing the browser's default action](https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault).

That's for example how elm's [`Browser.Events.onSubmit`](https://dark.elm.dmy.fr/packages/elm/html/latest/Html-Events#onSubmit)
prevents the form from changing the page’s location:

    submitListen : Web.Dom.Modifier ()
    submitListen =
        Web.Dom.listenToPreventingDefaultAction "submit"
            |> Web.Dom.modifierFutureMap (\_ -> ())

-}
listenToPreventingDefaultAction : String -> Modifier Json.Decode.Value
listenToPreventingDefaultAction eventName =
    { eventName = eventName, on = identity, defaultActionHandling = Web.DefaultActionPrevent }
        |> Listen
        |> Rope.singleton


{-| Wire events from this [`Modifier`](#Modifier) to a specific event.

    Web.Dom.listen "click" |> Web.Dom.modifierFutureMap (\_ -> ButtonClicked)

-}
modifierFutureMap : (future -> mappedFuture) -> (Modifier future -> Modifier mappedFuture)
modifierFutureMap futureChange =
    \modifier ->
        modifier |> Rope.map (\modifierSingle -> modifierSingle |> modifierSingleMap futureChange)


modifierSingleMap : (future -> mappedFuture) -> (ModifierSingle future -> ModifierSingle mappedFuture)
modifierSingleMap futureChange =
    \modifier ->
        case modifier of
            Attribute keyValue ->
                keyValue |> Attribute

            Style keyValue ->
                keyValue |> Style

            ScrollToPosition position ->
                position |> ScrollToPosition

            ScrollToShow alignment ->
                alignment |> ScrollToShow

            ScrollPositionRequest request ->
                (\future -> future |> request |> futureChange) |> ScrollPositionRequest

            Listen listen ->
                { eventName = listen.eventName
                , on = \json -> listen.on json |> futureChange
                , defaultActionHandling = listen.defaultActionHandling
                }
                    |> Listen


{-| Getting the current scroll position from the left and top.

Use in combination with [`scrollToPosition`](#scrollToPosition)
to implement saving and restoring scroll position even when users had navigated off a URL.

-}
scrollPositionRequest : Modifier { fromLeft : Float, fromTop : Float }
scrollPositionRequest =
    ScrollPositionRequest identity |> Rope.singleton


{-| Ensure a given initial scroll position in both directions.
To move to the edge in a direction, use [`scrollToShow`](#scrollToShow) instead.

Unlike [`style`](#style)s,
this is just an initial configuration
which can be changed by user actions.
So adding e.g. `scrollToPosition ...`
will scroll once the next render happens
but will not prevent users from scrolling away.

-}
scrollToPosition :
    { fromLeft : Float, fromTop : Float }
    -> Modifier future_
scrollToPosition position =
    ScrollToPosition position |> Rope.singleton


{-| Ensure a given initial [`DomElementVisibilityAlignment`](Web#DomElementVisibilityAlignment)
in both directions.

Unlike [`style`](#style)s,
this is just an initial configuration
which can be changed by user actions.
So adding e.g. `scrollToShow { y = Web.DomElementStart, x = Web.DomElementStart }`
will scroll to the top left once the next render happens
but will not prevent users from scrolling away.

Note: Uses [`Element.scrollIntoView`](https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollIntoView)

-}
scrollToShow :
    { x : Web.DomElementVisibilityAlignment, y : Web.DomElementVisibilityAlignment }
    -> Modifier future_
scrollToShow preferredAlignment =
    ScrollToShow preferredAlignment |> Rope.singleton


{-| Plain text or an [`Element`](#Element). Create using [`text`](#text) and [`element`](#element)
-}
type Node future
    = Text String
    | Element (Element future)


{-| A tagged DOM node that can itself contain child [node](#Node)s
-}
type alias Element future =
    RecordWithoutConstructorFunction
        { header : Web.DomElementHeader future
        , subs : List (Node future)
        }
