module Json.Codec exposing
    ( JsonCodec, map, lazy
    , RecordJsonCodecBeingBuilt, record, field, recordFinish
    , enum, ChoiceJsonCodecBeingBuilt, choice, variant
    , unit, bool, int, float, string
    , nullable, list, dict
    )

{-| Simple decoder-encoder pair for json
similar to [miniBill/elm-codec](https://dark.elm.dmy.fr/packages/miniBill/elm-codec/latest/)

@docs JsonCodec, map, lazy

@docs RecordJsonCodecBeingBuilt, record, field, recordFinish

@docs enum, ChoiceJsonCodecBeingBuilt, choice, variant

@docs unit, bool, int, float, string
@docs nullable, list, dict

-}

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


bool : JsonCodec Bool
bool =
    { toJson = Json.Encode.bool, jsonDecoder = Json.Decode.bool }


string : JsonCodec String
string =
    { toJson = Json.Encode.string, jsonDecoder = Json.Decode.string }


int : JsonCodec Int
int =
    { toJson = Json.Encode.int, jsonDecoder = Json.Decode.int }


float : JsonCodec Float
float =
    { toJson = Json.Encode.float, jsonDecoder = Json.Decode.float }


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
            variantName |> stringFirstCharToUpperCase
    in
    \soFar ->
        { toJson =
            soFar.toJson
                (\fieldValue ->
                    Json.Encode.object
                        [ ( "tag", fieldName |> Json.Encode.string )
                        , ( "value", fieldValue |> variantValueJsonCodec.toJson )
                        ]
                )
        , jsonDecoder =
            Json.Decode.oneOf
                [ soFar.jsonDecoder
                , Json.Decode.map2 (\() variantValue -> variantValue |> variantToChoice)
                    (Json.Decode.field "tag" (onlyStringJsonDecoder fieldName))
                    (Json.Decode.field "value" variantValueJsonCodec.jsonDecoder)
                ]
        }


onlyStringJsonDecoder : String -> Json.Decode.Decoder ()
onlyStringJsonDecoder specificAllowedString =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                if str == specificAllowedString then
                    () |> Json.Decode.succeed

                else
                    ([ "expected only \"", specificAllowedString, "\"" ] |> String.concat)
                        |> Json.Decode.fail
            )


stringFirstCharToUpperCase : String -> String
stringFirstCharToUpperCase =
    \string_ ->
        case string_ |> String.uncons of
            Nothing ->
                ""

            Just ( firstChar, afterFirstChar ) ->
                String.cons (firstChar |> Char.toUpper) afterFirstChar
