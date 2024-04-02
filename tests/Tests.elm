module Tests exposing (tests)

import Expect
import Fuzz exposing (Fuzzer)
import StructuredId exposing (StructuredId)
import Test exposing (Test)


tests : Test
tests =
    Test.describe "StructuredId"
        [ Test.fuzz (Fuzz.pair treeFuzz treeFuzz)
            "== equivalent to on toListOfString =="
            (\( tree0, tree1 ) ->
                (tree0 |> exampleTreeToStructuredId |> StructuredId.toListOfString)
                    == (tree1 |> exampleTreeToStructuredId |> StructuredId.toListOfString)
                    |> Expect.equal
                        (tree0 == tree1)
            )
        , Test.fuzz (Fuzz.pair treeFuzz treeFuzz)
            "== equivalent to on toString =="
            (\( tree0, tree1 ) ->
                (tree0 |> exampleTreeToStructuredId |> StructuredId.toString)
                    == (tree1 |> exampleTreeToStructuredId |> StructuredId.toString)
                    |> Expect.equal
                        (tree0 == tree1)
            )
        ]


treeFuzz : Fuzzer ExampleTree
treeFuzz =
    treeFuzzAtDepth 0


treeFuzzAtDepth : Int -> Fuzzer ExampleTree
treeFuzzAtDepth depth =
    if depth >= 4 then
        Fuzz.map Leaf Fuzz.int

    else
        Fuzz.oneOf
            [ Fuzz.map Leaf Fuzz.int
            , Fuzz.map Branch (Fuzz.listOfLengthBetween 0 8 (treeFuzzAtDepth (depth + 1)))
            ]


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
