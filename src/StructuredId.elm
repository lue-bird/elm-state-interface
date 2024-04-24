module StructuredId exposing
    ( StructuredId(..), toString
    , ofUnit, ofInt, ofString
    , ofParts, ofVariant, ofMaybe, ofList
    )

{-| Assigning each unique value one "structured id"
(a tree with just comparable values at the leaves).
This "structured id" can be compared or converted to a `comparable` (`String`)
which makes it possible to use as a key in a `Dict`

@docs StructuredId, toString

@docs ofUnit, ofInt, ofString
@docs ofParts, ofVariant, ofMaybe, ofList

-}

import Rope exposing (Rope)


type StructuredId
    = String String
    | List (List StructuredId)


ofUnit : StructuredId
ofUnit =
    List []


ofString : String -> StructuredId
ofString =
    \string ->
        String string


ofInt : Int -> StructuredId
ofInt =
    \int -> int |> String.fromInt |> ofString


ofParts : List StructuredId -> StructuredId
ofParts =
    \fieldValueStructureIds ->
        fieldValueStructureIds |> List


{-|

  - If a variant has no value, use [`StructuredId.ofUnit`](#ofUnit)
  - If a variant has more than one value, use [`StructuredId.ofParts`](#ofParts)

-}
ofVariant : { tag : String, value : StructuredId } -> StructuredId
ofVariant =
    \variant ->
        [ variant.tag |> ofString, variant.value ] |> ofParts


ofMaybe : (value -> StructuredId) -> (Maybe value -> StructuredId)
ofMaybe valueToStructuredId =
    \maybe ->
        ofVariant
            (case maybe of
                Nothing ->
                    { tag = "Nothing", value = ofUnit }

                Just value ->
                    { tag = "Just", value = value |> valueToStructuredId }
            )


ofList : (element -> StructuredId) -> (List element -> StructuredId)
ofList elementMap =
    \structuredIds ->
        structuredIds |> List.map elementMap |> List


toString : StructuredId -> String
toString =
    \structuredId ->
        structuredId |> toListOfString |> listOfStringToString


toListOfString : StructuredId -> List String
toListOfString =
    \structuredId ->
        structuredId |> toRopeOfString |> Rope.toList


toRopeOfString : StructuredId -> Rope String
toRopeOfString =
    \structuredId ->
        case structuredId of
            String string ->
                String.cons ' ' string |> Rope.singleton

            List elements ->
                Rope.append "]"
                    (List.foldl
                        (\el soFar ->
                            Rope.appendTo soFar (toRopeOfString el)
                        )
                        (Rope.singleton "[")
                        elements
                    )


listOfStringToString : List String -> String
listOfStringToString =
    \strings ->
        strings
            |> List.map (\part -> part |> String.replace "," ",,")
            |> String.join " , "
