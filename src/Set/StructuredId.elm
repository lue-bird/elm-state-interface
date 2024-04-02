module Set.StructuredId exposing (Set, elementWithStructuredIdAsString, empty, fold2From, fromRope)

import FastDict
import Rope exposing (Rope)
import StructuredId exposing (StructuredId)


type alias Set element =
    FastDict.Dict String element


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
                (element |> elementToStructuredId |> StructuredId.toString)
                element


empty : Set element_
empty =
    FastDict.empty


{-| Find the element with the same comparable returned by `StructureId.toComparable` again
-}
elementWithStructuredIdAsString : String -> (Set element -> Maybe element)
elementWithStructuredIdAsString structuredIdAsString =
    \dict ->
        dict |> FastDict.get structuredIdAsString


fold2From :
    result
    -> (String -> first -> (result -> result))
    -> (String -> ( first, second ) -> (result -> result))
    -> (String -> second -> result -> result)
    -> (( Set first, Set second ) -> result)
fold2From initialFolded onlyFirstReduce bothReduce onlySecondReduce =
    \( firstDict, secondDict ) ->
        FastDict.merge
            (\id first soFar -> soFar |> onlyFirstReduce id first)
            (\id first second soFar -> soFar |> bothReduce id ( first, second ))
            (\id second soFar -> soFar |> onlySecondReduce id second)
            firstDict
            secondDict
            initialFolded
