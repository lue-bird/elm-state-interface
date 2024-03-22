module StructuredId exposing
    ( StructuredId(..)
    , ofInt, ofTimePosix, ofString
    , ofVariant, ofMaybe, ofList
    , toComparable
    )

{-| Assigning each unique value one "structured id"
(a tree with just comparable values at the leaves).
This "structured id" can be compared or converted to a `comparable`
which makes it possible to use as a key in a `Dict`

@docs StructuredId

@docs ofInt, ofTimePosix, ofString
@docs ofVariant, ofMaybe, ofList


## use

You shouldn't use below helpers directly unless you want to build a library.
Instead, use [`Set.StructuredId`](Set-StructuredId)

@docs toComparable

-}

import Rope exposing (Rope)
import Time


type StructuredId
    = String String
    | List (List StructuredId)


ofString : String -> StructuredId
ofString =
    \string ->
        String string


ofInt : Int -> StructuredId
ofInt =
    \int -> int |> String.fromInt |> ofString


{-| In our project, variants only have zero or one value, so
the list in the second tuple part should describe the **fields** of that value.
-}
ofVariant : ( String, List StructuredId ) -> StructuredId
ofVariant =
    \( tag, attachmentFields ) ->
        ofList (ofString tag :: attachmentFields)


ofMaybe : (value -> StructuredId) -> (Maybe value -> StructuredId)
ofMaybe valueToStructuredId =
    \maybe ->
        ofVariant
            (case maybe of
                Nothing ->
                    ( "Nothing", [] )

                Just value ->
                    ( "Just", [ value |> valueToStructuredId ] )
            )


ofTimePosix : Time.Posix -> StructuredId
ofTimePosix =
    \timePosix ->
        timePosix |> Time.posixToMillis |> ofInt


ofList : List StructuredId -> StructuredId
ofList =
    \structuredIds ->
        List structuredIds


toComparable : StructuredId -> List String
toComparable =
    \structuredId ->
        structuredId |> toComparableRope |> Rope.toList


toComparableRope : StructuredId -> Rope String
toComparableRope =
    \structuredId ->
        case structuredId of
            String string ->
                String.cons ' ' string |> Rope.singleton

            List elements ->
                elements
                    |> List.foldl
                        (\el soFar ->
                            (el |> toComparableRope)
                                |> Rope.prependTo soFar
                        )
                        (Rope.singleton "(")
                    |> Rope.append ")"
