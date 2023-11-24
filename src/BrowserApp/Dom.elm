module BrowserApp.Dom exposing
    ( text
    , element, elementAddAttributes, elementAddStyles, elementOnEvent, elementAddSubs, elementToNode
    , render
    )

{-| Helpers for [DOM node types](BrowserApp#DOM) as part of an [`Interface`](BrowserApp#Interface).

These are primitives used for svg and html.
Compare with [`elm/virtual-dom`](https://dark.elm.dmy.fr/packages/elm/virtual-dom/latest/)

@docs text
@docs element, elementAddAttributes, elementAddStyles, elementOnEvent, elementAddSubs, elementToNode
@docs render

-}

import Array
import BrowserApp exposing (DomElement, DomNode)
import Dict
import Json.Decode


{-| An [`Interface`](BrowserApp#Interface) for displaying a given [`DomNode`](BrowserApp#DomNode).
Don't forget to call [`elementToNode`](#elementToNode) if necessary
-}
render : DomNode state -> BrowserApp.Interface state
render =
    \domNode ->
        domNode
            |> BrowserApp.DomNodeRender
            |> BrowserApp.InterfaceSingle


{-| Plain text [`DomNode`](BrowserApp#DomNode)
-}
text : String -> DomNode state_
text =
    BrowserApp.DomText


{-| Create a [`DomElement`](BrowserApp#DomElement) with a given tag.
For example for <p>text</p>

    BrowserApp.Dom.element "p"
        |> BrowserApp.Dom.elementAddSubs [ BrowserApp.Dom.text "text" ]

-}
element : String -> DomElement state_
element tag =
    { tag = tag
    , eventListeners = Dict.empty
    , styles = Dict.empty
    , attributes = Dict.empty
    , subs = Array.empty
    }


{-| Append child-[`DomNode`](BrowserApp#DomNode)s
-}
elementAddSubs : List (DomNode state) -> (DomElement state -> DomElement state)
elementAddSubs subs =
    \domElement ->
        { domElement | subs = Array.append domElement.subs (Array.fromList subs) }


{-| Listen for a specific DOM event on the [`DomElement`](BrowserApp#DomElement)
-}
elementOnEvent : String -> (Json.Decode.Value -> state) -> (DomElement state -> DomElement state)
elementOnEvent eventName eventToState =
    \domElement ->
        { domElement
            | eventListeners =
                domElement.eventListeners |> Dict.insert eventName eventToState
        }


{-| Add attributes as `( key, value )` tuples to the [`DomElement`](BrowserApp#DomElement).

For example to get `<a href="https://elm-lang.org">Elm</a>`

    BrowserApp.Dom.element "a"
        |> BrowserApp.Dom.elementAddAttributes
            [ ( "href", "https://elm-lang.org" ) ]
        |> BrowserApp.Dom.elementAddSubs
            [ BrowserApp.Dom.text "Elm" ]
        |> BrowserApp.Dom.elementToNode

-}
elementAddAttributes : List ( String, String ) -> (DomElement state -> DomElement state)
elementAddAttributes attributes =
    \domElement ->
        { domElement
            | attributes =
                Dict.union (attributes |> Dict.fromList) domElement.attributes
        }


{-| Add styles as `( key, value )` tuples to the [`DomElement`](BrowserApp#DomElement)
-}
elementAddStyles : List ( String, String ) -> (DomElement state -> DomElement state)
elementAddStyles styles =
    \domElement ->
        { domElement
            | styles =
                Dict.union (styles |> Dict.fromList) domElement.styles
        }


{-| Convert a [`DomElement`](BrowserApp#DomElement) to a more general [`DomNode`](BrowserApp#DomNode)
-}
elementToNode : DomElement state -> DomNode state
elementToNode =
    BrowserApp.DomElement
