module Web.Dom exposing
    ( documentEventListen
    , text
    , element, elementNamespaced
    , Modifier, ModifierSingle(..), attribute, attributeNamespaced, style
    , listenTo, listenToPreventingDefaultAction
    , modifierFutureMap, modifierBatch, modifierNone
    , futureMap, render
    )

{-| Helpers for [DOM node types](Web#DomNode) as part of an [`Interface`](Web#Interface).

These are primitives used for svg and html.
Compare with [`elm/virtual-dom`](https://dark.elm.dmy.fr/packages/elm/virtual-dom/latest/)

@docs documentEventListen
@docs text
@docs element, elementNamespaced
@docs Modifier, ModifierSingle, attribute, attributeNamespaced, style
@docs listenTo, listenToPreventingDefaultAction
@docs modifierFutureMap, modifierBatch, modifierNone
@docs futureMap, render

-}

import Array
import Dict
import Json.Decode
import Rope exposing (Rope)
import Web exposing (DomElement, DomNode)


{-| An [`Interface`](Web#Interface) for detecting a specific [`document`](https://developer.mozilla.org/en-US/docs/Web/API/Document) event
that has no native [`Interface`](Web#Interface) like like scroll, scrollend, selectionchange or paste
-}
documentEventListen : String -> Web.Interface Json.Decode.Value
documentEventListen eventName =
    Web.DocumentEventListen { eventName = eventName, on = Json.Decode.value }
        |> Web.Listen
        |> Web.InterfaceWithFuture
        |> Rope.singleton


{-| An [`Interface`](Web#Interface) for displaying a given [`DomNode`](Web#DomNode).
-}
render : DomNode future -> Web.Interface future
render =
    \domNode ->
        domNode
            |> Web.DomNodeRender
            |> Web.InterfaceWithFuture
            |> Rope.singleton


{-| Wire events from this [`DomNode`](Web#DomNode) to a specific event.

    buttonUi "start"
        |> Web.Dom.futureMap (\Clicked -> StartButtonClicked)

with e.g.

    buttonUi : List (Web.DomNode ()) -> Web.DomNode ButtonEvent
    buttonUi subs =
        Web.Dom.element "button"
            [ Web.Dom.listenTo "click"
                |> Web.Dom.modifierFutureMap (\_ -> Clicked)
            ]
            [ Web.Dom.text label ]

    type ButtonEvent
        = Clicked

-}
futureMap : (future -> mappedFuture) -> (DomNode future -> DomNode mappedFuture)
futureMap futureChange =
    \domElementToMap ->
        case domElementToMap of
            Web.DomText string ->
                Web.DomText string

            Web.DomElement domElement ->
                domElement |> elementMap futureChange |> Web.DomElement


elementMap : (future -> mappedFuture) -> (DomElement future -> DomElement mappedFuture)
elementMap futureChange =
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
            domElementToMap.subs |> Array.map (futureMap futureChange)
        }


{-| Plain text [`DomNode`](Web#DomNode)
-}
text : String -> DomNode future_
text =
    Web.DomText


elementWithMaybeNamespace : Maybe String -> String -> List (Modifier future) -> List (DomNode future) -> DomNode future
elementWithMaybeNamespace maybeNamespace tag modifiers subs =
    let
        modifierList : List (ModifierSingle future)
        modifierList =
            modifiers |> modifierBatch |> Rope.toList
    in
    { namespace = maybeNamespace
    , tag = tag
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
    , subs = subs |> Array.fromList
    }
        |> Web.DomElement


{-| Create a DOM element with a given tag, [`Modifier`](#Modifier)s and sub-[node](Web#DomNode)s.
For example to get `<p>flying</p>`

    Web.Dom.element "p"
        []
        [ Web.Dom.text "flying" ]

-}
element : String -> List (Modifier future) -> List (DomNode future) -> DomNode future
element tag modifiers subs =
    elementWithMaybeNamespace Nothing tag modifiers subs


{-| Create a DOM element with a given namespace, tag, [`Modifier`](#Modifier)s and sub-[node](Web#DomNode)s.
For example, [`Web.Svg`](Web-Svg) defines its elements using

    element : String -> List (Modifier future) -> List (DomNode future) -> DomNode future
    element tag modifiers subs =
        Web.Dom.elementNamespaced "http://www.w3.org/2000/svg" tag modifiers subs

-}
elementNamespaced : String -> String -> List (Modifier future) -> List (DomNode future) -> DomNode future
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


{-| Listen for a specific DOM event on the [`DomElement`](Web#DomElement).
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

            Listen listen ->
                { eventName = listen.eventName
                , on = \json -> listen.on json |> futureChange
                , defaultActionHandling = listen.defaultActionHandling
                }
                    |> Listen
