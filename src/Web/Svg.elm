module Web.Svg exposing (element)

{-| Helpers for svg [DOM nodes](Web-Dom#Node)

@docs element

for text, attributes etc use the helpers in [`Web.Dom`](Web-Dom)

-}

import Web.Dom


{-| Create an SVG element [`Web.Dom.Node`](Web-Dom#Node).
with a given tag, [`Modifier`](Web-Dom#Modifier)s and sub-nodes.
-}
element : String -> List (Web.Dom.Modifier future) -> List (Web.Dom.Node future) -> Web.Dom.Node future
element tag modifiers subs =
    Web.Dom.elementNamespaced "http://www.w3.org/2000/svg" tag modifiers subs
