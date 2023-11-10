module BrowserApp exposing (BrowserApp, Event(..), Interface(..), InterfaceId(..), State, on, toProgram)

import Array exposing (Array)
import AssocList
import Color exposing (Color)
import Json.Decode
import Json.Decode.Extra
import Json.Encode
import N0To255 exposing (N0To255)
import Time


type alias State appState =
    { interface : AssocList.Dict InterfaceId (Interface appState)
    , appState : appState
    }


type alias BrowserApp state =
    { initialState : state
    , interface : state -> List (Interface state)
    , ports :
        { out : Json.Encode.Value -> Cmd Never
        , in_ : (Json.Encode.Value -> Event state) -> Sub (Event state)
        }
    }


type alias Color0To255 =
    { red : N0To255, green : N0To255, blue : N0To255, alpha : N0To255 }


type Interface state
    = Display { config : String, on : () -> state }
    | RequestTimeNow { on : Time.Posix -> state }
    | Draw { config : Array (Array Color), on : () -> state }
    | ListenToHtmlEvent { config : String, on : Json.Decode.Value -> state }


type InterfaceId
    = IdDisplay String
    | IdRequestTimeNow
    | IdDraw (Array (Array Color0To255))
    | IdListenToHtmlEvent String


on : (state -> mappedState) -> (Interface state -> Interface mappedState)
on stateChange =
    \interface ->
        case interface of
            Display display ->
                { config = display.config, on = \event -> display.on event |> stateChange }
                    |> Display

            RequestTimeNow requestTimeNow ->
                { on = \event -> requestTimeNow.on event |> stateChange }
                    |> RequestTimeNow

            Draw draw ->
                { config = draw.config, on = \event -> draw.on event |> stateChange }
                    |> Draw

            ListenToHtmlEvent listenToHtmlEvent ->
                { config = listenToHtmlEvent.config, on = \event -> listenToHtmlEvent.on event |> stateChange }
                    |> ListenToHtmlEvent


type Event appState
    = InterfaceIdFailedToDecode Json.Decode.Error
    | InterfaceEventDataFailedToDecode Json.Decode.Error
    | BrowserInterfaceDesynchronized
    | AppEventToNewAppState appState


toProgram : BrowserApp state -> Program () (State state) (Event state)
toProgram appConfig =
    Platform.worker
        { init =
            \() ->
                let
                    initialInterface =
                        appConfig.initialState
                            |> appConfig.interface
                            |> List.map (\interface -> ( interface |> interfaceToId, interface ))
                            |> AssocList.fromList
                in
                ( { interface = initialInterface
                  , appState = appConfig.initialState
                  }
                , initialInterface
                    |> AssocList.keys
                    |> List.map
                        (\interfaceId ->
                            appConfig.ports.out
                                (Json.Encode.object
                                    [ ( "addedInterface"
                                      , interfaceId |> interfaceIdToJson
                                      )
                                    ]
                                )
                        )
                    |> Cmd.batch
                    |> Cmd.map never
                )
        , update =
            \event ->
                case event of
                    BrowserInterfaceDesynchronized ->
                        \state ->
                            let
                                _ =
                                    Debug.log "BrowserInterfaceDesynchronized" ()
                            in
                            ( state, Cmd.none )

                    InterfaceIdFailedToDecode jsonError ->
                        \state ->
                            let
                                _ =
                                    Debug.log "InterfaceIdFailedToDecode" (jsonError |> Json.Decode.errorToString)
                            in
                            ( state, Cmd.none )

                    InterfaceEventDataFailedToDecode jsonError ->
                        \state ->
                            let
                                _ =
                                    Debug.log "InterfaceEventDataFailedToDecode" (jsonError |> Json.Decode.errorToString)
                            in
                            ( state, Cmd.none )

                    AppEventToNewAppState updatedAppState ->
                        \oldState ->
                            let
                                updatedInterface : AssocList.Dict InterfaceId (Interface state)
                                updatedInterface =
                                    updatedAppState
                                        |> appConfig.interface
                                        |> List.map (\interface -> ( interface |> interfaceToId, interface ))
                                        |> AssocList.fromList
                            in
                            ( { interface = updatedInterface, appState = updatedAppState }
                            , [ AssocList.diff updatedInterface oldState.interface
                                    |> AssocList.keys
                                    |> List.map
                                        (\interfaceId ->
                                            appConfig.ports.out
                                                (Json.Encode.object
                                                    [ ( "addedInterface"
                                                      , interfaceId |> interfaceIdToJson
                                                      )
                                                    ]
                                                )
                                        )
                              , AssocList.diff oldState.interface updatedInterface
                                    |> AssocList.keys
                                    |> List.map
                                        (\interfaceId ->
                                            appConfig.ports.out
                                                (Json.Encode.object
                                                    [ ( "removedInterface"
                                                      , interfaceId |> interfaceIdToJson
                                                      )
                                                    ]
                                                )
                                        )
                              ]
                                |> List.concat
                                |> Cmd.batch
                                |> Cmd.map never
                            )
        , subscriptions =
            \state ->
                -- re-associate event based on current interface
                appConfig.ports.in_
                    (\interfaceJson ->
                        case interfaceJson |> Json.Decode.decodeValue (Json.Decode.field "interface" interfaceIdJsonDecoder) of
                            Ok interfaceId ->
                                case
                                    state.interface
                                        |> AssocList.toList
                                        |> listFirstJust
                                            (\( stateInterfaceId, stateInterface ) ->
                                                if stateInterfaceId == interfaceId then
                                                    stateInterface |> Just

                                                else
                                                    Nothing
                                            )
                                of
                                    Just reAssociatedInterface ->
                                        case Json.Decode.decodeValue (Json.Decode.field "eventData" (eventDataAndConstructStateJsonDecoder reAssociatedInterface)) interfaceJson of
                                            Ok appEvent ->
                                                appEvent |> AppEventToNewAppState

                                            Err eventDataJsonDecodeError ->
                                                eventDataJsonDecodeError |> InterfaceEventDataFailedToDecode

                                    Nothing ->
                                        BrowserInterfaceDesynchronized

                            Err interfaceIdJsonDecodeError ->
                                interfaceIdJsonDecodeError |> InterfaceIdFailedToDecode
                    )
        }


interfaceToId : Interface state_ -> InterfaceId
interfaceToId =
    \interface ->
        case interface of
            Display display ->
                IdDisplay display.config

            RequestTimeNow _ ->
                IdRequestTimeNow

            Draw draw ->
                IdDraw (draw.config |> Array.map (Array.map colorTo0To255))

            ListenToHtmlEvent listenToHtmlEvent ->
                IdListenToHtmlEvent listenToHtmlEvent.config


eventDataAndConstructStateJsonDecoder : Interface state -> Json.Decode.Decoder state
eventDataAndConstructStateJsonDecoder interface =
    case interface of
        Display display ->
            Json.Decode.succeed display.on
                |> Json.Decode.Extra.andMap (Json.Decode.null ())

        RequestTimeNow requestTimeNow ->
            Json.Decode.succeed requestTimeNow.on
                |> Json.Decode.Extra.andMap (Json.Decode.map Time.millisToPosix Json.Decode.int)

        Draw draw ->
            Json.Decode.succeed draw.on
                |> Json.Decode.Extra.andMap (Json.Decode.null ())

        ListenToHtmlEvent listenToHtmlEvent ->
            Json.Decode.succeed listenToHtmlEvent.on
                |> Json.Decode.Extra.andMap Json.Decode.value


color0To255JsonDecoder : Json.Decode.Decoder Color0To255
color0To255JsonDecoder =
    Json.Decode.succeed (\r g b a -> { red = r, green = g, blue = b, alpha = a })
        |> Json.Decode.Extra.andMap (Json.Decode.field "red" N0To255.jsonDecoder)
        |> Json.Decode.Extra.andMap (Json.Decode.field "green" N0To255.jsonDecoder)
        |> Json.Decode.Extra.andMap (Json.Decode.field "blue" N0To255.jsonDecoder)
        |> Json.Decode.Extra.andMap (Json.Decode.field "alpha" N0To255.jsonDecoder)


color0To255ToJson : Color0To255 -> Json.Encode.Value
color0To255ToJson =
    \color ->
        Json.Encode.object
            [ ( "red", color.red |> N0To255.toJson )
            , ( "green", color.green |> N0To255.toJson )
            , ( "blue", color.blue |> N0To255.toJson )
            , ( "alpha", color.alpha |> N0To255.toJson )
            ]


colorTo0To255 : Color -> Color0To255
colorTo0To255 =
    \color ->
        let
            components =
                color |> Color.toRgba
        in
        { red = components.red |> N0To255.fromPercentage
        , green = components.green |> N0To255.fromPercentage
        , blue = components.blue |> N0To255.fromPercentage
        , alpha = components.alpha |> N0To255.fromPercentage
        }


interfaceIdToJson : InterfaceId -> Json.Encode.Value
interfaceIdToJson =
    \interface ->
        Json.Encode.object
            [ case interface of
                IdDisplay string ->
                    ( "display", string |> Json.Encode.string )

                IdRequestTimeNow ->
                    ( "requestTimeNow", Json.Encode.null )

                IdDraw pixels ->
                    ( "draw", pixels |> Json.Encode.array (Json.Encode.array color0To255ToJson) )

                IdListenToHtmlEvent eventName ->
                    ( "listenToHtmlEvent", eventName |> Json.Encode.string )
            ]


interfaceIdJsonDecoder : Json.Decode.Decoder InterfaceId
interfaceIdJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map IdDisplay
            (Json.Decode.field "display" Json.Decode.string)
        , Json.Decode.map (\() -> IdRequestTimeNow)
            (Json.Decode.field "requestTimeNow" (Json.Decode.null ()))
        , Json.Decode.map IdDraw
            (Json.Decode.field "draw" (Json.Decode.array (Json.Decode.array color0To255JsonDecoder)))
        , Json.Decode.map IdListenToHtmlEvent
            (Json.Decode.field "listenToHtmlEvent" Json.Decode.string)
        ]


listFirstJust : (element -> Maybe found) -> List element -> Maybe found
listFirstJust tryMapToFound list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            case tryMapToFound head of
                Just b ->
                    Just b

                Nothing ->
                    listFirstJust tryMapToFound tail
