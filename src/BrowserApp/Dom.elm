module BrowserApp.Dom exposing
    ( documentEventListen
    , text
    , element, Modifier, ModifierSingle(..), attribute, style, listenTo, modifierMap, modifierBatch, modifierNone
    , render
    )

{-| Helpers for [DOM node types](BrowserApp#DomNode) as part of an [`Interface`](BrowserApp#Interface).

These are primitives used for svg and html.
Compare with [`elm/virtual-dom`](https://dark.elm.dmy.fr/packages/elm/virtual-dom/latest/)

@docs documentEventListen
@docs text
@docs element, Modifier, ModifierSingle, attribute, style, listenTo, modifierMap, modifierBatch, modifierNone
@docs render

-}

import Array
import BrowserApp exposing (DomNode)
import Dict
import Json.Decode
import Rope exposing (Rope)


{-| An [`Interface`](BrowserApp#Interface) that listens for a specific `document` event
like like keypress, keydown, keyup, click, mousemove, mousedown, mouseup
-}
documentEventListen : String -> BrowserApp.Interface Json.Decode.Value
documentEventListen eventName =
    BrowserApp.DocumentEventListen { eventName = eventName, on = identity }
        |> Rope.singleton


{-| An [`Interface`](BrowserApp#Interface) for displaying a given [`DomNode`](BrowserApp#DomNode).
-}
render : DomNode state -> BrowserApp.Interface state
render =
    \domNode ->
        domNode
            |> BrowserApp.DomNodeRender
            |> Rope.singleton


{-| Plain text [`DomNode`](BrowserApp#DomNode)
-}
text : String -> DomNode state_
text =
    BrowserApp.DomText


{-| Create a DOM element with a given tag, [`Modifier`](#Modifier)s and sub-[node](BrowserApp#DomNode)s.
For example to get `<p>flying</p>`

    BrowserApp.Dom.element "p"
        []
        [ BrowserApp.Dom.text "flying" ]

-}
element : String -> List (Modifier state) -> List (DomNode state) -> DomNode state
element tag modifiers subs =
    let
        modifierList : List (ModifierSingle state)
        modifierList =
            modifiers |> modifierBatch |> Rope.toList
    in
    { tag = tag
    , eventListens =
        modifierList
            |> List.foldl
                (\modifier ->
                    case modifier of
                        Listen listen ->
                            Dict.insert listen.eventName listen.on

                        _ ->
                            identity
                )
                Dict.empty
    , styles = Dict.empty
    , attributes = Dict.empty
    , subs = subs |> Array.fromList
    }
        |> BrowserApp.DomElement


{-| Set the behavior of a [`BrowserApp.Dom.element`](BrowserApp-Dom#element).
To create one, use [`attribute`](#attribute), [`style`](#style), [`listenTo`](#listenTo).
To combine multiple, use [`BrowserApp.Dom.modifierBatch`](#modifierBatch) and [`BrowserApp.Dom.modifierNone`](#modifierNone)

For example to get `<a href="https://elm-lang.org">elm</a>`

    BrowserApp.Dom.element "a"
        [ BrowserApp.Dom.attribute "href" "https://elm-lang.org" ]
        [ BrowserApp.Dom.text "elm" ]

-}
type alias Modifier state =
    Rope (ModifierSingle state)


{-| Combine multiple [`Modifier`](#Modifier)s into one.
-}
modifierBatch : List (Modifier state) -> Modifier state
modifierBatch =
    \modifiers -> modifiers |> Rope.fromList |> Rope.concat


{-| Doing nothing as a [`Modifier`](#Modifier). These two examples are equivalent:

    BrowserApp.Dom.modifierBatch
        [ a, BrowserApp.Dom.modifierNone, b ]

and

    BrowserApp.Dom.modifierBatch
        (List.filterMap identity
            [ a |> Just, Nothing, b |> Just ]
        )

-}
modifierNone : Modifier state_
modifierNone =
    Rope.empty


{-| An individual [`Modifier`](#Modifier).
Create using [`attribute`](#attribute), [`style`](#style), [`listenTo`](#listenTo).
-}
type ModifierSingle state
    = Attribute { key : String, value : String }
    | Style { key : String, value : String }
    | Listen { eventName : String, on : Json.Decode.Value -> state }


{-| A key-value attribute [`Modifier`](#Modifier)
-}
attribute : String -> String -> Modifier state_
attribute key value =
    { key = key, value = value } |> Attribute |> Rope.singleton


{-| A key-value style [`Modifier`](#Modifier)
-}
style : String -> String -> Modifier state_
style key value =
    { key = key, value = value } |> Attribute |> Rope.singleton


{-| Listen for a specific DOM event on the [`DomElement`](BrowserApp#DomElement).
Use [`modifierMap`](#modifierMap) to wire this to a specific event.
-}
listenTo : String -> Modifier Json.Decode.Value
listenTo eventName =
    Listen { eventName = eventName, on = identity } |> Rope.singleton


{-| Wire events from this [`Modifier`](#Modifier) to a specific event.

    BrowserApp.Dom.listen "click" |> BrowserApp.Dom.modifierMap (\_ -> ButtonClicked)

-}
modifierMap : (state -> mappedState) -> (Modifier state -> Modifier mappedState)
modifierMap stateChange =
    \modifier ->
        modifier |> Rope.map (\modifierSingle -> modifierSingle |> modifierSingleMap stateChange)


modifierSingleMap : (state -> mappedState) -> (ModifierSingle state -> ModifierSingle mappedState)
modifierSingleMap stateChange =
    \modifier ->
        case modifier of
            Attribute keyValue ->
                keyValue |> Attribute

            Style keyValue ->
                keyValue |> Style

            Listen listen ->
                { eventName = listen.eventName
                , on = \json -> listen.on json |> stateChange
                }
                    |> Listen
