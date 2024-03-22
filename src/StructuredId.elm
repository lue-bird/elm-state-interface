module StructuredId exposing
    ( StructuredId(..)
    , ofInt, ofTimePosix, ofString
    , ofVariant, ofMaybe, ofList
    , toComparable, order
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

@docs toComparable, order

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


order : ( StructuredId, StructuredId ) -> Order
order =
    \( a, b ) ->
        case ( a, b ) of
            ( String aString, String bString ) ->
                compare aString bString

            ( String _, List _ ) ->
                LT

            ( List _, String _ ) ->
                GT

            ( List aList, List bList ) ->
                ( aList, bList ) |> listOrder


listOrder : ( List StructuredId, List StructuredId ) -> Order
listOrder =
    \( a, b ) ->
        case ( a, b ) of
            ( [], [] ) ->
                EQ

            ( [], _ :: _ ) ->
                LT

            ( _ :: _, [] ) ->
                GT

            ( head0 :: tail0, head1 :: tail1 ) ->
                case ( head0, head1 ) |> order of
                    LT ->
                        LT

                    GT ->
                        GT

                    EQ ->
                        listOrder ( tail0, tail1 )
