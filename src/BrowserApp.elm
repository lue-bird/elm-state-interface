module BrowserApp exposing (BrowserApp, Event(..), Interface(..), InterfaceId(..), State, on, toProgram)

import AssocList
import Json.Decode
import Json.Decode.Extra
import Json.Encode
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


type Interface state
    = Display { config : String, on : () -> state }
    | RequestTimeNow { on : Time.Posix -> state }


type InterfaceId
    = IdDisplay String
    | IdRequestTimeNow


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
                                        |> AssocList.values
                                        |> listFirstJust
                                            (\stateInterface ->
                                                if (stateInterface |> interfaceToId) == interfaceId then
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


eventDataAndConstructStateJsonDecoder : Interface state -> Json.Decode.Decoder state
eventDataAndConstructStateJsonDecoder interface =
    case interface of
        Display display ->
            Json.Decode.succeed display.on
                |> Json.Decode.Extra.andMap (Json.Decode.null ())

        RequestTimeNow requestTimeNow ->
            Json.Decode.succeed requestTimeNow.on
                |> Json.Decode.Extra.andMap (Json.Decode.map Time.millisToPosix Json.Decode.int)


interfaceIdToJson : InterfaceId -> Json.Encode.Value
interfaceIdToJson =
    \interface ->
        Json.Encode.object
            [ case interface of
                IdDisplay string ->
                    ( "display", string |> Json.Encode.string )

                IdRequestTimeNow ->
                    ( "requestTimeNow", Json.Encode.null )
            ]


interfaceIdJsonDecoder : Json.Decode.Decoder InterfaceId
interfaceIdJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map IdDisplay
            (Json.Decode.field "display" Json.Decode.string)
        , Json.Decode.map (\() -> IdRequestTimeNow)
            (Json.Decode.field "requestTimeNow" (Json.Decode.null ()))
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
