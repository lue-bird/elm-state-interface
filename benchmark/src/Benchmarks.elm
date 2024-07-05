module Benchmarks exposing (benchmarks)

import Benchmark
import Benchmark.Alternative
import Json.Encode
import Rope exposing (Rope)
import StructuredId exposing (StructuredId)


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
        Leaf 34

    else if (index |> Basics.remainderBy 2) == 0 then
        Leaf -200

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
