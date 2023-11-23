module BrowserApp.Dom exposing
    ( text
    , element, elementAddSubs, elementOnEvent, elementToNode
    , render
    )

{-| Helpers for [DOM](BrowserApp#DOM) nodes as part of an [`Interface`](BrowserApp#Interface)

@docs text
@docs element, elementAddSubs, elementOnEvent, elementToNode
@docs render

-}

import Array
import BrowserApp exposing (DomElement, DomNode)
import Dict
import Json.Decode


{-| An [`Interface`](BrowserApp#Interface) for displaying a given [`DomNode`](BrowserApp#DomNode)
-}
render : DomNode state -> BrowserApp.Interface state
render =
    BrowserApp.DomNodeRender


{-| Plain text [`DomNode`](BrowserApp#DomNode)
-}
text : String -> DomNode state_
text =
    BrowserApp.DomText


{-| Create a DOM element with a given tag.
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
    \domElement_ ->
        { domElement_ | subs = Array.append domElement_.subs (Array.fromList subs) }


{-| Listen for a specific html event
-}
elementOnEvent : String -> (Json.Decode.Value -> state) -> (DomElement state -> DomElement state)
elementOnEvent eventName eventToState =
    \domElement_ ->
        { domElement_
            | eventListeners =
                domElement_.eventListeners |> Dict.insert eventName eventToState
        }


{-| Convert a [`DomElement`](BrowserApp#DomElement) to a more general [`DomNode`](BrowserApp#DomNode)
-}
elementToNode : DomElement state -> DomNode state
elementToNode =
    BrowserApp.DomElement
