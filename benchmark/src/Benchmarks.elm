module Benchmarks exposing (benchmarks)

import Benchmark
import Benchmark.Alternative
import Json.Encode
import Rope exposing (Rope)
import StructuredId exposing (StructuredId)
import Web
import Web.Dom
import Web.Svg


benchmarks : Benchmark.Benchmark
benchmarks =
    Benchmark.describe "StructuredId"
        [ Benchmark.Alternative.rank "List String -> String"
            (\listOfStringToString -> listOfStringToString exampleListOfStrings)
            [ ( "map |> join", listOfStringToStringUsingMapJoin )
            , ( "foldr appending right", listOfStringToStringUsingFoldrAppendingRight )
            , ( "foldl appending right", listOfStringToStringUsingFoldlAppendingRight )
            , ( "foldr appending left", listOfStringToStringUsingFoldrAppendingLeft )
            , ( "foldl appending left", listOfStringToStringUsingFoldlAppendingLeft )
            , ( "json encode 0", listOfStringToStringUsingJsonEncode0 )
            , ( "json encode 1", listOfStringToStringUsingJsonEncode1 )
            , ( "json encode 2", listOfStringToStringUsingJsonEncode2 )
            , ( "json encode 3", listOfStringToStringUsingJsonEncode3 )
            , ( "json encode 4", listOfStringToStringUsingJsonEncode4 )
            ]
        , Benchmark.Alternative.rank "StructuredId -> String"
            (\toListOfString -> toListOfString exampleStructuredId)
            [ ( "rope", toStringUsingRope )
            , ( "json encode all the way", toStringUsingJsonEncodeAllTheWay )
            ]
        , Benchmark.Alternative.rank "Int -> String"
            (\toListOfString -> toListOfString 13243)
            [ ( "Json.Encode.int |> encode 0", intJsonEncode0 )
            , ( "String.fromInt", String.fromInt )
            ]
        , Benchmark.Alternative.rank "dom render"
            (\domRender -> domRender exampleDom)
            [ ( "nested recursion given path", domRenderUsingNestedRecursionWithPath )
            , ( "nested recursion with subs map, final List.map", renderUsingNestedRecursionWithSubsMapAndFinalListMap )
            ]
        ]


intJsonEncode0 : Int -> String
intJsonEncode0 =
    \int ->
        int |> Json.Encode.int |> Json.Encode.encode 0


listOfStringToStringUsingMapJoin : List String -> String
listOfStringToStringUsingMapJoin =
    \strings ->
        strings
            |> List.map (\part -> part |> String.replace "," ",,")
            |> String.join " , "


listOfStringToStringUsingFoldrAppendingRight : List String -> String
listOfStringToStringUsingFoldrAppendingRight =
    \strings ->
        strings
            |> List.foldr
                (\part soFar ->
                    soFar ++ " , " ++ (part |> String.replace "," ",,")
                )
                ""


listOfStringToStringUsingFoldlAppendingRight : List String -> String
listOfStringToStringUsingFoldlAppendingRight =
    \strings ->
        strings
            |> List.foldl
                (\part soFar ->
                    soFar ++ " , " ++ (part |> String.replace "," ",,")
                )
                ""


listOfStringToStringUsingFoldrAppendingLeft : List String -> String
listOfStringToStringUsingFoldrAppendingLeft =
    \strings ->
        strings
            |> List.foldr
                (\part soFar ->
                    (part |> String.replace "," ",,") ++ " , " ++ soFar
                )
                ""


listOfStringToStringUsingFoldlAppendingLeft : List String -> String
listOfStringToStringUsingFoldlAppendingLeft =
    \strings ->
        strings
            |> List.foldl
                (\part soFar ->
                    (part |> String.replace "," ",,") ++ " , " ++ soFar
                )
                ""


listOfStringToStringUsingJsonEncode0 : List String -> String
listOfStringToStringUsingJsonEncode0 =
    \strings ->
        strings |> Json.Encode.list Json.Encode.string |> Json.Encode.encode 0


listOfStringToStringUsingJsonEncode1 : List String -> String
listOfStringToStringUsingJsonEncode1 =
    \strings ->
        strings |> Json.Encode.list Json.Encode.string |> Json.Encode.encode 1


listOfStringToStringUsingJsonEncode2 : List String -> String
listOfStringToStringUsingJsonEncode2 =
    \strings ->
        strings |> Json.Encode.list Json.Encode.string |> Json.Encode.encode 2


listOfStringToStringUsingJsonEncode3 : List String -> String
listOfStringToStringUsingJsonEncode3 =
    \strings ->
        strings |> Json.Encode.list Json.Encode.string |> Json.Encode.encode 3


listOfStringToStringUsingJsonEncode4 : List String -> String
listOfStringToStringUsingJsonEncode4 =
    \strings ->
        strings |> Json.Encode.list Json.Encode.string |> Json.Encode.encode 4


toStringUsingRope : StructuredId -> String
toStringUsingRope =
    \structuredId ->
        structuredId |> toRopeOfString |> Rope.toList |> listOfStringToStringUsingJsonEncode0


toRopeOfString : StructuredId -> Rope String
toRopeOfString =
    \structuredId ->
        case structuredId of
            StructuredId.String string ->
                String.cons ' ' string |> Rope.singleton

            StructuredId.List elements ->
                Rope.append "]"
                    (List.foldl
                        (\el soFar ->
                            Rope.appendTo soFar (toRopeOfString el)
                        )
                        (Rope.singleton "[")
                        elements
                    )


toStringUsingJsonEncodeAllTheWay : StructuredId -> String
toStringUsingJsonEncodeAllTheWay =
    \structuredId ->
        structuredId |> toStringUsingToJson |> Json.Encode.encode 0


toStringUsingToJson : StructuredId -> Json.Encode.Value
toStringUsingToJson =
    \structuredId ->
        case structuredId of
            StructuredId.String string ->
                string |> Json.Encode.string

            StructuredId.List elements ->
                elements |> Json.Encode.list toStringUsingToJson


domRenderUsingNestedRecursionWithPath : Web.Dom.Node future -> Web.Interface future
domRenderUsingNestedRecursionWithPath =
    \domNode ->
        domNode |> nodeFlattenToRopeUsingNestedRecursionWithPath []


nodeFlattenToRopeUsingNestedRecursionWithPath :
    List Int
    -> Web.Dom.Node future
    -> Rope (Web.InterfaceSingle future)
nodeFlattenToRopeUsingNestedRecursionWithPath path =
    \node ->
        case node of
            Web.Dom.Text string ->
                { path = path, node = Web.DomText string }
                    |> Web.DomNodeRender
                    |> Rope.singleton

            Web.Dom.Element element_ ->
                Rope.prepend
                    ({ path = path, node = Web.DomElementHeader element_.header }
                        |> Web.DomNodeRender
                    )
                    (List.foldl
                        (\sub soFar ->
                            { subIndex = soFar.subIndex + 1
                            , rope =
                                Rope.appendTo soFar.rope
                                    (nodeFlattenToRopeUsingNestedRecursionWithPath (soFar.subIndex :: path) sub)
                            }
                        )
                        { subIndex = 0, rope = Rope.empty }
                        element_.subs
                    ).rope


renderUsingNestedRecursionWithSubsMapAndFinalListMap : Web.Dom.Node future -> Web.Interface future
renderUsingNestedRecursionWithSubsMapAndFinalListMap =
    \domNode ->
        domNode
            |> nodeFlattenUsingNestedRecursionWithSubsMapAndFinalListMap
            |> List.map (\nodeAndPath -> nodeAndPath |> Web.DomNodeRender)
            |> Rope.fromList


nodeFlattenUsingNestedRecursionWithSubsMapAndFinalListMap : Web.Dom.Node future -> List { path : List Int, node : Web.DomTextOrElementHeader future }
nodeFlattenUsingNestedRecursionWithSubsMapAndFinalListMap =
    \node -> node |> nodeFlattenToRopeUsingNestedRecursionWithSubsMapAndFinalListMap |> Rope.toList


nodeFlattenToRopeUsingNestedRecursionWithSubsMapAndFinalListMap : Web.Dom.Node future -> Rope { path : List Int, node : Web.DomTextOrElementHeader future }
nodeFlattenToRopeUsingNestedRecursionWithSubsMapAndFinalListMap =
    \node ->
        case node of
            Web.Dom.Text string ->
                { path = [], node = Web.DomText string } |> Rope.singleton

            Web.Dom.Element element_ ->
                Rope.prepend
                    { path = [], node = Web.DomElementHeader element_.header }
                    (List.foldl
                        (\sub soFar ->
                            { subIndex = soFar.subIndex + 1
                            , rope =
                                Rope.appendTo
                                    soFar.rope
                                    (Rope.map (\layerPart -> { layerPart | path = soFar.subIndex :: layerPart.path })
                                        (nodeFlattenToRopeUsingNestedRecursionWithSubsMapAndFinalListMap sub)
                                    )
                            }
                        )
                        { subIndex = 0, rope = Rope.empty }
                        element_.subs
                    ).rope



--


exampleListOfStrings : List String
exampleListOfStrings =
    List.range 5 100
        |> List.map
            (\n ->
                (List.range 0 n |> List.map Char.fromCode |> String.fromList)
                    ++ ",\"  ,,\"\""
                    ++ String.repeat n "b"
            )


exampleTree : ExampleTree
exampleTree =
    exampleTreeAtDepth 0 0


exampleTreeAtDepth : Int -> Int -> ExampleTree
exampleTreeAtDepth depth index =
    if depth >= 4 then
        Leaf (34 * index + depth)

    else if (index |> Basics.remainderBy 2) == 0 then
        Leaf (-200 // index - depth)

    else
        Branch (List.range 0 4 |> List.map (exampleTreeAtDepth (depth + 1)))


exampleTreeToStructuredId : ExampleTree -> StructuredId
exampleTreeToStructuredId =
    \tree ->
        case tree of
            Leaf int ->
                int |> StructuredId.ofInt

            Branch forest ->
                forest |> StructuredId.ofList exampleTreeToStructuredId


type ExampleTree
    = Leaf Int
    | Branch (List ExampleTree)


exampleStructuredId : StructuredId
exampleStructuredId =
    exampleTree |> exampleTreeToStructuredId


exampleDom : Web.Dom.Node future_
exampleDom =
    exampleTree |> treeToDom


treeToDom : ExampleTree -> Web.Dom.Node future_
treeToDom =
    \tree ->
        case tree of
            Leaf n ->
                Web.Dom.text
                    ((n |> String.fromInt)
                        ++ (List.range 0 n |> List.map Char.fromCode |> String.fromList)
                    )

            Branch subs ->
                Web.Svg.element "div"
                    [ Web.Dom.style "fill" "blue"
                    , Web.Dom.style "font-size" "3em"
                    , Web.Dom.attribute "text-anchor" "middle"
                    , Web.Dom.attribute "dominant-baseline" "middle"
                    , Web.Dom.attribute "font-weight" "bolder"
                    , Web.Dom.attribute "x" "50%"
                    , Web.Dom.attribute "y" "8%"
                    , Web.Dom.attribute "width" "50%"
                    , Web.Dom.attribute "height" "50%"
                    ]
                    (subs |> List.map treeToDom)
