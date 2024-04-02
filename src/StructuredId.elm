module StructuredId exposing
    ( StructuredId(..)
    , ofInt, ofTimePosix, ofString
    , ofVariant, ofMaybe, ofList
    , toString
    )

{-| Assigning each unique value one "structured id"
(a tree with just comparable values at the leaves).
This "structured id" can be compared or converted to a `comparable`
which makes it possible to use as a key in a `Dict`

@docs StructuredId

@docs ofInt, ofTimePosix, ofString
@docs ofVariant, ofMaybe, ofList


## use

@docs toString

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
        ofString tag :: attachmentFields |> List


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
