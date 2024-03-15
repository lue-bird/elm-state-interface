module Json.Codec exposing
    ( JsonCodec, map, lazy
    , RecordJsonCodecBeingBuilt, record, field, recordFinish
    , enum, ChoiceJsonCodecBeingBuilt, choice, variant
    , unit, int, string
    , nullable, list, array, dict
    )

{-| Simple decoder-encoder pair for json.
We don't use [miniBill/elm-codec](https://dark.elm.dmy.fr/packages/miniBill/elm-codec/latest/) because we store variants as single-field records

@docs JsonCodec, map, lazy

@docs RecordJsonCodecBeingBuilt, record, field, recordFinish

@docs enum, ChoiceJsonCodecBeingBuilt, choice, variant

@docs unit, int, string
@docs nullable, list, array, dict

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode
import Json.Encode


type alias JsonCodec narrow =
    { toJson : narrow -> Json.Encode.Value
    , jsonDecoder : Json.Decode.Decoder narrow
    }


unit : JsonCodec ()
unit =
    { toJson = \() -> Json.Encode.null
    , jsonDecoder = Json.Decode.null ()
    }


string : JsonCodec String
string =
    { toJson = Json.Encode.string, jsonDecoder = Json.Decode.string }


int : JsonCodec Int
int =
    { toJson = Json.Encode.int, jsonDecoder = Json.Decode.int }


nullable : JsonCodec value -> JsonCodec (Maybe value)
nullable valueJsonCodec =
    { toJson =
        \maybe ->
            case maybe of
                Nothing ->
                    Json.Encode.null

                Just value ->
                    value |> valueJsonCodec.toJson
    , jsonDecoder = Json.Decode.nullable valueJsonCodec.jsonDecoder
    }


list : JsonCodec element -> JsonCodec (List element)
list elementJsonCodec =
    { toJson = \narrowList -> narrowList |> Json.Encode.list elementJsonCodec.toJson
    , jsonDecoder = Json.Decode.list elementJsonCodec.jsonDecoder
    }


array : JsonCodec element -> JsonCodec (Array element)
array elementJsonCodec =
    map Array.fromList Array.toList (list elementJsonCodec)


dict : JsonCodec { key : comparableKey, value : value } -> JsonCodec (Dict comparableKey value)
dict entryJsonCodec =
    map
        (\entries -> entries |> List.map (\entry -> ( entry.key, entry.value )) |> Dict.fromList)
        (\dict_ -> dict_ |> Dict.toList |> List.map (\( key, value ) -> { key = key, value = value }))
        (list entryJsonCodec)


map : (closerToBroad -> narrow) -> (narrow -> closerToBroad) -> (JsonCodec closerToBroad -> JsonCodec narrow)
map closerToBroadToNarrow narrowToCloserToBroad =
    \closerToBroadJsonCodec ->
        { toJson = \narrow -> narrow |> narrowToCloserToBroad |> closerToBroadJsonCodec.toJson
        , jsonDecoder = closerToBroadJsonCodec.jsonDecoder |> Json.Decode.map closerToBroadToNarrow
        }


lazy : (() -> JsonCodec narrow) -> JsonCodec narrow
lazy =
    \unitToJsonCodec ->
        { toJson = \narrow -> narrow |> (unitToJsonCodec () |> .toJson)
        , jsonDecoder = Json.Decode.lazy (\u -> unitToJsonCodec u |> .jsonDecoder)
        }


type alias RecordJsonCodecBeingBuilt narrow decoded =
    { toJson : narrow -> Dict String Json.Encode.Value
    , jsonDecoder : Json.Decode.Decoder decoded
    }


record : assemble -> RecordJsonCodecBeingBuilt narrow_ assemble
record assemble =
    { toJson = \_ -> Dict.empty
    , jsonDecoder = Json.Decode.succeed assemble
    }


field :
    ( narrow -> fieldValue, String )
    -> JsonCodec fieldValue
    ->
        (RecordJsonCodecBeingBuilt narrow (fieldValue -> decoded)
         -> RecordJsonCodecBeingBuilt narrow decoded
        )
field ( toFieldValue, fieldName ) fieldValueJsonCodec =
    \soFar ->
        { toJson =
            \narrow ->
                narrow
                    |> soFar.toJson
                    |> Dict.insert fieldName (narrow |> toFieldValue |> fieldValueJsonCodec.toJson)
        , jsonDecoder =
            Json.Decode.map2 (\f a -> f a)
                soFar.jsonDecoder
                (Json.Decode.field fieldName fieldValueJsonCodec.jsonDecoder)
        }


recordFinish : RecordJsonCodecBeingBuilt narrow narrow -> JsonCodec narrow
recordFinish =
    \recordJsonCodec ->
        { toJson =
            \narrow ->
                narrow
                    |> recordJsonCodec.toJson
                    |> Dict.toList
                    |> Json.Encode.object
        , jsonDecoder = recordJsonCodec.jsonDecoder
        }


enum : List narrow -> (narrow -> String) -> JsonCodec narrow
enum possibilities toString =
    { toJson =
        \narrow ->
            narrow |> toString |> Json.Encode.string
    , jsonDecoder =
        Json.Decode.andThen
            (\decodeString ->
                case possibilities |> List.filter (\possibility -> (possibility |> toString) == decodeString) of
                    [] ->
                        Json.Decode.fail
                            ("expected one of "
                                ++ (possibilities
                                        |> List.map (\possibility -> [ "\"", possibility |> toString, "\"" ] |> String.concat)
                                        |> String.join ", "
                                   )
                            )

                    possibility :: _ ->
                        possibility |> Json.Decode.succeed
            )
            Json.Decode.string
    }


type alias ChoiceJsonCodecBeingBuilt toJson narrow =
    { toJson : toJson
    , jsonDecoder : Json.Decode.Decoder narrow
    }


choice : choose -> ChoiceJsonCodecBeingBuilt choose narrow_
choice choose =
    { toJson = choose
    , jsonDecoder = Json.Decode.oneOf []
    }


variant :
    ( possibility -> choice, String )
    -> JsonCodec possibility
    -> ChoiceJsonCodecBeingBuilt ((possibility -> Json.Encode.Value) -> toJson) choice
    -> ChoiceJsonCodecBeingBuilt toJson choice
variant ( variantToChoice, variantName ) variantValueJsonCodec =
    let
        fieldName : String
        fieldName =
            variantName |> stringFirstCharToLowerCase
    in
    \soFar ->
        { toJson = soFar.toJson variantValueJsonCodec.toJson
        , jsonDecoder =
            Json.Decode.oneOf
                [ soFar.jsonDecoder
                , Json.Decode.map variantToChoice
                    (Json.Decode.field fieldName variantValueJsonCodec.jsonDecoder)
                ]
        }


stringFirstCharToLowerCase : String -> String
stringFirstCharToLowerCase =
    \string_ ->
        case string_ |> String.uncons of
            Nothing ->
                ""

            Just ( firstChar, afterFirstChar ) ->
                String.cons (firstChar |> Char.toLower) afterFirstChar
