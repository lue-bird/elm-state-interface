module Tests exposing (tests)

import Expect
import Fuzz exposing (Fuzzer)
import List.LocalExtra
import StructuredId exposing (StructuredId)
import Test exposing (Test)


tests : Test
tests =
    Test.describe "elm-state-interface"
        [ Test.describe "StructuredId"
            [ Test.fuzz
                (Fuzz.constant (\a b -> { a = a, b = b })
                    |> Fuzz.andMap treeFuzz
                    |> Fuzz.andMap treeFuzz
                )
                "== equivalent to on toString =="
                (\trees ->
                    (trees.a |> exampleTreeToStructuredId |> StructuredId.toString)
                        == (trees.b |> exampleTreeToStructuredId |> StructuredId.toString)
                        |> Expect.equal
                            (trees.a == trees.b)
                )
            ]
        , Test.describe "List.LocalExtra"
            [ Test.fuzz (Fuzz.list (Fuzz.maybe Fuzz.int))
                "justsMapIndexed"
                (\list ->
                    list
                        |> List.LocalExtra.justsMapIndexed
                            (\index maybe ->
                                maybe |> Maybe.map (\just -> { just = just, index = index })
                            )
                        |> Expect.equalLists
                            (list
                                |> List.indexedMap
                                    (\index maybe ->
                                        maybe |> Maybe.map (\just -> { just = just, index = index })
                                    )
                                |> List.filterMap identity
                            )
                )
            ]
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
