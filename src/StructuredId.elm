module StructuredId exposing
    ( StructuredId, toString
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

import Json.Encode


type alias StructuredId =
    -- type StructuredId
    --     = String String
    --     | List (List StructuredId)
    --
    -- directly using the most efficient toString-able representation
    -- saves a toJson step but more importantly saves a List.map step in ofList
    Json.Encode.Value


ofUnit : StructuredId
ofUnit =
    Json.Encode.null


ofString : String -> StructuredId
ofString =
    \string ->
        string |> Json.Encode.string


ofInt : Int -> StructuredId
ofInt =
    \int -> int |> Json.Encode.int


ofParts : List StructuredId -> StructuredId
ofParts =
    \fieldValueStructureIds ->
        fieldValueStructureIds |> Json.Encode.list identity


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
ofList elementToStructuredId =
    \structuredIds ->
        structuredIds |> Json.Encode.list elementToStructuredId


toString : StructuredId -> String
toString =
    \structuredId ->
        structuredId |> toJson |> Json.Encode.encode 0


toJson : StructuredId -> Json.Encode.Value
toJson =
    \structuredId ->
        structuredId
