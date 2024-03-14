module Web.Svg exposing (element)

{-| Helpers for svg [DOM node types](Web#DomNode)

@docs element

for text, attributes etc use the helpers in [`Web.Dom`](Web-Dom)

-}

import Web exposing (DomNode)
import Web.Dom exposing (Modifier)


{-| Create an SVG element [`DomNode`](Web#DomNode).
with a given tag, [`Modifier`](Web-Dom#Modifier)s and sub-nodes.
-}
element : String -> List (Modifier future) -> List (DomNode future) -> DomNode future
element tag modifiers subs =
    Web.Dom.elementNamespaced "http://www.w3.org/2000/svg" tag modifiers subs
