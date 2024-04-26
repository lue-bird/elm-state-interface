module Svg.LocalExtra exposing (circle, ellipse, fillUniform, line, polygon, polyline, rotated, strokeUniform, strokeWidth)

import Angle
import Color exposing (Color)
import Web.Dom
import Web.Svg


line :
    { start : { x : Float, y : Float }, end : { x : Float, y : Float } }
    -> List (Web.Dom.Modifier future)
    -> Web.Dom.Node future
line lineGeometry additionalModifiers =
    Web.Svg.element "line"
        ([ Web.Dom.attribute "x1" (lineGeometry.start.x |> String.fromFloat)
         , Web.Dom.attribute "y1" (lineGeometry.start.y |> String.fromFloat)
         , Web.Dom.attribute "x2" (lineGeometry.end.x |> String.fromFloat)
         , Web.Dom.attribute "y2" (lineGeometry.end.y |> String.fromFloat)
         ]
            ++ additionalModifiers
        )
        []


circle : { position : { x : Float, y : Float }, radius : Float } -> List (Web.Dom.Modifier future) -> Web.Dom.Node future
circle geometry additionalModifiers =
    Web.Svg.element "circle"
        ([ Web.Dom.attribute "cx" ((geometry.position.x |> String.fromFloat) ++ "px")
         , Web.Dom.attribute "cy" ((geometry.position.y |> String.fromFloat) ++ "px")
         , Web.Dom.attribute "r" ((geometry.radius |> String.fromFloat) ++ "px")
         ]
            ++ additionalModifiers
        )
        []


ellipse : { position : { x : Float, y : Float }, radiusX : Float, radiusY : Float } -> List (Web.Dom.Modifier future) -> Web.Dom.Node future
ellipse geometry additionalModifiers =
    Web.Svg.element "ellipse"
        ([ Web.Dom.attribute "cx" ((geometry.position.x |> String.fromFloat) ++ "px")
         , Web.Dom.attribute "cy" ((geometry.position.y |> String.fromFloat) ++ "px")
         , Web.Dom.attribute "rx" ((geometry.radiusX |> String.fromFloat) ++ "px")
         , Web.Dom.attribute "ry" ((geometry.radiusY |> String.fromFloat) ++ "px")
         ]
            ++ additionalModifiers
        )
        []


rotated : { angle : Angle.Angle, center : { x : Float, y : Float } } -> Web.Dom.Modifier future_
rotated geometry =
    Web.Dom.attribute "transform"
        ([ "rotate("
         , geometry.angle |> Angle.inDegrees |> String.fromFloat
         , ", "
         , geometry.center.x |> String.fromFloat
         , ", "
         , geometry.center.y |> String.fromFloat
         , ")"
         ]
            |> String.concat
        )


fillUniform : Color -> Web.Dom.Modifier future_
fillUniform color =
    Web.Dom.attribute "fill" (color |> Color.toCssString)


strokeUniform : Color -> Web.Dom.Modifier future_
strokeUniform color =
    Web.Dom.attribute "stroke" (color |> Color.toCssString)


points : List { x : Float, y : Float } -> Web.Dom.Modifier future_
points =
    \points_ ->
        Web.Dom.attribute "points"
            ((case points_ of
                [ onlyElement ] ->
                    [ onlyElement, onlyElement ]

                notOnlyOne ->
                    notOnlyOne
             )
                |> List.map (\point -> [ point.x |> String.fromFloat, ",", point.y |> String.fromFloat ] |> String.concat)
                |> String.join " "
            )


polygon : List { x : Float, y : Float } -> List (Web.Dom.Modifier future) -> Web.Dom.Node future
polygon points_ additionalModifiers =
    Web.Svg.element "polyline"
        (points points_
            :: additionalModifiers
        )
        []


strokeWidth : Float -> Web.Dom.Modifier future_
strokeWidth pixels =
    Web.Dom.attribute "stroke-width" ((pixels |> String.fromFloat) ++ "px")


polyline : List { x : Float, y : Float } -> List (Web.Dom.Modifier future) -> Web.Dom.Node future
polyline points_ additionalModifiers =
    Web.Svg.element "polyline"
        (points points_
            :: additionalModifiers
        )
        []
