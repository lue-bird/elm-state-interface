module Set.StructuredId exposing (Set, elementWithStructuredId, empty, fold2From, fromRope)

import FastDict
import Rope exposing (Rope)
import StructuredId exposing (StructuredId)


type alias Set element =
    FastDict.Dict (List String) element


fromRope : (element -> StructuredId) -> (Rope element -> Set element)
fromRope elementToStructuredId =
    \rope ->
        rope
            |> Rope.foldl
                (\interfaceSingle soFar ->
                    soFar |> insert elementToStructuredId interfaceSingle
                )
                empty


insert :
    (element -> StructuredId)
    -> element
    -> (Set element -> Set element)
insert elementToStructuredId element =
    \dict ->
        dict
            |> FastDict.insert
                (element |> elementToStructuredId |> StructuredId.toComparable)
                element


empty : Set element_
empty =
    FastDict.empty


{-| Find the element with the same [`StructureId`](StructureId#StructureId) again
-}
elementWithStructuredId : StructuredId -> (Set element -> Maybe element)
elementWithStructuredId structuredId =
    \dict ->
        dict |> FastDict.get (structuredId |> StructuredId.toComparable)


fold2From :
    result
    -> (first -> (result -> result))
    -> (( first, second ) -> (result -> result))
    -> (second -> result -> result)
    -> (( Set first, Set second ) -> result)
fold2From initialFolded onlyFirstReduce bothReduce onlySecondReduce =
    \( firstDict, secondDict ) ->
        FastDict.merge
            (\_ first soFar -> soFar |> onlyFirstReduce first)
            (\_ first second soFar -> soFar |> bothReduce ( first, second ))
            (\_ second soFar -> soFar |> onlySecondReduce second)
            firstDict
            secondDict
            initialFolded
