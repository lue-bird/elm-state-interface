port module Main exposing (main)

import Angle
import AppUrl exposing (AppUrl)
import Color exposing (Color)
import Dict exposing (Dict)
import Duration
import Json.Decode
import Json.Encode
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Serialize
import Set
import Svg.LocalExtra
import Time
import Url
import Web
import Web.Audio
import Web.Audio.Parameter
import Web.Dom
import Web.Gamepads
import Web.Navigation
import Web.Svg
import Web.Time
import Web.Window


main : Program () (Web.ProgramState State) (Web.ProgramEvent State)
main =
    Web.program programConfig


programConfig : Web.ProgramConfig State
programConfig =
    { initialState = WaitingForInitialUrl
    , interface =
        \stateChoice ->
            case stateChoice of
                WaitingForInitialUrl ->
                    Web.Navigation.urlRequest
                        |> Web.interfaceFutureMap
                            (\initialUrl ->
                                case initialUrl |> appUrlToState of
                                    Just initialState ->
                                        initialState |> Initialized

                                    Nothing ->
                                        StartingRoom
                                            { name = Nothing
                                            , gemCount = 0
                                            , mousePoint = { x = 0, y = 0 }
                                            , posix = Time.millisToPosix 0
                                            , timezone = Time.utc
                                            }
                                            |> Initialized
                            )

                Initialized initialized ->
                    [ initialized |> initializedInterface
                    , Web.Navigation.pushUrl (initialized |> stateToAppUrl)
                    , Web.Navigation.movementListen
                        |> Web.interfaceFutureMap
                            (\newUrl ->
                                case newUrl |> appUrlToState of
                                    Nothing ->
                                        let
                                            _ =
                                                Debug.log "failed to decode AppUrl" newUrl
                                        in
                                        initialized

                                    Just newState ->
                                        case initialized of
                                            StartingRoom startingRoomState ->
                                                case newState of
                                                    StartingRoom newStartingRoomState ->
                                                        { newStartingRoomState
                                                            | posix = startingRoomState.posix
                                                            , timezone = startingRoomState.timezone
                                                            , mousePoint = startingRoomState.mousePoint
                                                        }
                                                            |> StartingRoom

                                                    otherNewState ->
                                                        otherNewState

                                            AtSign _ ->
                                                newState

                                            PickingApples pickingApplesState ->
                                                case newState of
                                                    PickingApples newPickingApplesState ->
                                                        { newPickingApplesState
                                                            | eatAppleAudio = pickingApplesState.eatAppleAudio
                                                            , eatAppleTimes = pickingApplesState.eatAppleTimes
                                                        }
                                                            |> PickingApples

                                                    otherNewState ->
                                                        otherNewState

                                            ShowingMapWithExit ->
                                                newState
                            )
                    ]
                        |> Web.interfaceBatch
                        |> Web.interfaceFutureMap Initialized
    , ports = { fromJs = fromJs, toJs = toJs }
    }


initializedInterface : InitializedState -> Web.Interface InitializedState
initializedInterface =
    \stateChoice ->
        case stateChoice of
            StartingRoom startingRoomState ->
                startingRoomState |> startingRoomInterface

            AtSign atSignState ->
                atSignState |> atSignInterface

            PickingApples pickingApplesState ->
                pickingApplesState |> pickApplesInterface

            ShowingMapWithExit ->
                mapWithExitInterface


type State
    = WaitingForInitialUrl
    | Initialized InitializedState


type InitializedState
    = StartingRoom StartingRoomState
    | AtSign AtSignState
    | PickingApples PickApplesState
    | ShowingMapWithExit


type alias StartingRoomState =
    RecordWithoutConstructorFunction
        { name : Maybe String
        , gemCount : Int
        , mousePoint : { x : Int, y : Int }
        , timezone : Time.Zone
        , posix : Time.Posix
        }


type StartingRoomEvent
    = NameChanged (Result Json.Decode.Error String)
    | MouseMovedTo (Result Json.Decode.Error { x : Int, y : Int })
    | GemCountDecreaseClicked
    | GemCountIncreaseClicked
    | TimePassed Time.Posix
    | TimeZoneReceived Time.Zone
    | WalkToSignClicked


startingRoomInterface : StartingRoomState -> Web.Interface InitializedState
startingRoomInterface =
    \state ->
        [ narrativeUiFrame
            [ Web.Dom.listenTo "mousemove"
                |> Web.Dom.modifierFutureMap
                    (\mouseEvent ->
                        mouseEvent
                            |> Json.Decode.decodeValue
                                (Json.Decode.map2 (\x y -> { x = x, y = y })
                                    (Json.Decode.field "clientX" Json.Decode.int)
                                    (Json.Decode.field "clientY" Json.Decode.int)
                                )
                            |> MouseMovedTo
                    )
            ]
            [ "You find yourself trapped in a state-interface. The old clock on the wall shows " |> Web.Dom.text
            , clockUi { posix = state.posix, timezone = state.timezone }
            , ". Countless questions rush through your head:" |> Web.Dom.text
            , Web.Dom.element "ul"
                []
                [ Web.Dom.element "li"
                    []
                    [ "\"How did you get here?\"" |> Web.Dom.text ]
                , Web.Dom.element "li"
                    []
                    [ "\"Why do I know that you're exactly at "
                        ++ ("x" ++ (state.mousePoint.x |> String.fromInt) ++ " y" ++ (state.mousePoint.y |> String.fromInt))
                        ++ "?\""
                        |> Web.Dom.text
                    ]
                , Web.Dom.element "li"
                    []
                    [ "\"How do I know your name " |> Web.Dom.text
                    , textInputUi state.name |> Web.Dom.futureMap NameChanged
                    , "?\"" |> Web.Dom.text
                    ]
                , Web.Dom.element "li"
                    []
                    [ "Why is there a tutl?" |> Web.Dom.text
                    , Web.Svg.element "svg"
                        [ Web.Dom.attribute "viewBox" "0 12 96 40"
                        , Web.Dom.attribute "width" "96"
                        , Web.Dom.attribute "height" "40"
                        ]
                        [ Web.Svg.element "image"
                            [ Web.Dom.attribute "width" "72"
                            , Web.Dom.attribute "height" "72"
                            , Web.Dom.attribute "href" "https://elm-lang.org/images/turtle.gif"
                            ]
                            []
                        ]
                    ]
                ]
            , "\"Don't worry\", I say. \"I know how we can get out. See this little bird on the sign over there? It will give us a map for 💎3\"" |> Web.Dom.text
            , Web.Dom.element "br" [] []
            , "The voice repeats: \"Don't worry. Here,  take a couple 💎 if you want"
                ++ (case state.name of
                        Nothing ->
                            ""

                        Just name ->
                            ", " ++ name
                   )
                ++ ":\""
                |> Web.Dom.text
            , Web.Dom.element "div"
                [ Web.Dom.style "padding-top" "30px"
                , Web.Dom.style "padding-bottom" "30px"
                ]
                [ buttonUi
                    [ Web.Dom.style "height" "60px"
                    , Web.Dom.style "width" "60px"
                    , Web.Dom.style "text-align" "center"
                    ]
                    [ "+" |> Web.Dom.text ]
                    |> Web.Dom.futureMap (\() -> GemCountIncreaseClicked)
                , Web.Dom.element "b"
                    [ Web.Dom.style "padding" "15px 15px"
                    ]
                    [ "💎" ++ (state.gemCount |> String.fromInt) |> Web.Dom.text ]
                , buttonUi
                    [ Web.Dom.style "height" "60px"
                    , Web.Dom.style "width" "60px"
                    , Web.Dom.style "text-align" "center"
                    ]
                    [ "-" |> Web.Dom.text ]
                    |> Web.Dom.futureMap (\() -> GemCountDecreaseClicked)
                ]
            , buttonUi []
                [ "walk towards the sign as "
                    ++ (case state.name of
                            Nothing ->
                                "nameless"

                            Just name ->
                                name
                       )
                    |> Web.Dom.text
                ]
                |> Web.Dom.futureMap (\() -> WalkToSignClicked)
            ]
            |> Web.Dom.render
        , Web.Time.zoneRequest |> Web.interfaceFutureMap TimeZoneReceived
        , Web.Time.periodicallyListen Duration.second |> Web.interfaceFutureMap TimePassed
        ]
            |> Web.interfaceBatch
            |> Web.interfaceFutureMap
                (\event ->
                    case event of
                        MouseMovedTo (Ok newMousePoint) ->
                            StartingRoom { state | mousePoint = newMousePoint }

                        MouseMovedTo (Err _) ->
                            StartingRoom state

                        GemCountDecreaseClicked ->
                            StartingRoom { state | gemCount = state.gemCount - 1 }

                        GemCountIncreaseClicked ->
                            StartingRoom { state | gemCount = state.gemCount + 1 }

                        TimePassed newTime ->
                            StartingRoom { state | posix = newTime }

                        TimeZoneReceived timezone ->
                            StartingRoom { state | timezone = timezone }

                        NameChanged (Err jsonError) ->
                            StartingRoom { state | name = jsonError |> Json.Decode.errorToString |> Just }

                        NameChanged (Ok name) ->
                            StartingRoom
                                { state
                                    | name =
                                        case name |> String.trimLeft of
                                            "" ->
                                                Nothing

                                            nonBlankName ->
                                                nonBlankName |> Just
                                }

                        WalkToSignClicked ->
                            AtSign
                                { name =
                                    case state.name of
                                        Nothing ->
                                            "nameless"

                                        Just name ->
                                            name
                                , gemCount = state.gemCount
                                , appleCount = 0
                                , birdConversationState = WaitingForTalk
                                }
                )


type alias AtSignState =
    RecordWithoutConstructorFunction
        { gemCount : Int
        , appleCount : Int
        , name : String
        , birdConversationState : BirdConversationState
        }


type BirdConversationState
    = WaitingForTalk
    | GreetingAndAskingForWhatYouWant
    | BirdTellAboutItself
    | AskedBirdForMap
    | TooHungryToSell


type AtSignEvent
    = TalkToBirdClicked
    | PickApplesClicked
    | BirdTellAboutYourselfClicked
    | BuyMapClicked
    | OpenMapClicked


atSignInterface : AtSignState -> Web.Interface InitializedState
atSignInterface =
    \state ->
        [ narrativeUiFrame []
            [ Web.Dom.element "p"
                []
                [ "\"And there we are, " ++ state.name ++ "!\"" |> Web.Dom.text ]
            , Web.Dom.element "div"
                [ Web.Dom.style "text-align" "center"
                , Web.Dom.style "width" "50%"
                ]
                [ "🕊️" |> Web.Dom.text ]
            , Web.Dom.element "div"
                [ Web.Dom.style "text-align" "center"
                , Web.Dom.style "width" "50%"
                ]
                [ "🎏" |> Web.Dom.text ]
            , Web.Dom.element "p"
                []
                [ (case state.appleCount of
                    0 ->
                        "\"Don't you think the bird looks a bit hungry...\""

                    non0AppleCount ->
                        "You've already picked "
                            ++ (non0AppleCount |> String.fromInt)
                            ++ " 🍎s"
                  )
                    |> Web.Dom.text
                ]
            , case state.birdConversationState of
                WaitingForTalk ->
                    Web.Dom.element "div"
                        []
                        [ buttonUi []
                            [ "talk to the bird" |> Web.Dom.text
                            ]
                            |> Web.Dom.futureMap (\() -> TalkToBirdClicked)
                        , " or " |> Web.Dom.text
                        , buttonUi []
                            [ (case state.appleCount of
                                0 ->
                                    "pick some 🍎s"

                                _ ->
                                    "pick even more 🍎s"
                              )
                                |> Web.Dom.text
                            ]
                            |> Web.Dom.futureMap (\() -> PickApplesClicked)
                        ]

                GreetingAndAskingForWhatYouWant ->
                    Web.Dom.element "div"
                        []
                        [ "\"chirp chirp. Thanks for coming by!"
                            ++ " I usually sell for 💎 but since your new here, a couple of 🍎s would make me happy as well :)\" "
                            |> Web.Dom.text
                        , buttonUi []
                            [ "Ask for an introduction" |> Web.Dom.text
                            ]
                            |> Web.Dom.futureMap (\() -> BirdTellAboutYourselfClicked)
                        , " or " |> Web.Dom.text
                        , buttonUi []
                            [ "Buy map with the exit" |> Web.Dom.text
                            ]
                            |> Web.Dom.futureMap (\() -> BuyMapClicked)
                        ]

                BirdTellAboutItself ->
                    Web.Dom.element "div"
                        []
                        [ "Jo jo. I'm the map and info dealer in this village since I fly around a lot."
                            ++ " If you want to catch me to suggest some offers I could make you, write me a "
                            |> Web.Dom.text
                        , Web.Dom.element "a"
                            [ Web.Dom.attribute "href" "https://github.com/lue-bird/elm-state-interface/discussions/new/choose"
                            , Web.Dom.style "color" "inherit"
                            ]
                            [ "letter" |> Web.Dom.text ]
                        , buttonUi []
                            [ "Buy map with the exit" |> Web.Dom.text
                            ]
                            |> Web.Dom.futureMap (\() -> BuyMapClicked)
                        ]

                AskedBirdForMap ->
                    Web.Dom.element "div"
                        []
                        [ "\"Hope you'll come by again!\" says the bird, looking a bit down"
                            |> Web.Dom.text
                        , buttonUi []
                            [ "Open the map" |> Web.Dom.text
                            ]
                            |> Web.Dom.futureMap (\() -> OpenMapClicked)
                        ]

                TooHungryToSell ->
                    Web.Dom.element "div"
                        []
                        [ "\"Nah, I'm hungry, I will need more of these fresh 🍎s\""
                            |> Web.Dom.text
                        , buttonUi []
                            [ "pick 🍎s" |> Web.Dom.text
                            ]
                            |> Web.Dom.futureMap (\() -> PickApplesClicked)
                        ]
            ]
            |> Web.Dom.render
        ]
            |> Web.interfaceBatch
            |> Web.interfaceFutureMap
                (\event ->
                    case event of
                        TalkToBirdClicked ->
                            AtSign { state | birdConversationState = GreetingAndAskingForWhatYouWant }

                        BuyMapClicked ->
                            if state.appleCount <= 9 then
                                AtSign { state | birdConversationState = TooHungryToSell }

                            else
                                AtSign
                                    { state
                                        | birdConversationState = AskedBirdForMap
                                        , appleCount = state.appleCount - 10
                                    }

                        BirdTellAboutYourselfClicked ->
                            AtSign { state | birdConversationState = BirdTellAboutItself }

                        OpenMapClicked ->
                            ShowingMapWithExit

                        PickApplesClicked ->
                            PickingApples
                                ({ name = state.name
                                 , gemCount = state.gemCount
                                 , appleCountBefore = state.appleCount
                                 }
                                    |> initialPickingApplesState
                                )
                )


initialPickingApplesState : { name : String, gemCount : Int, appleCountBefore : Int } -> PickApplesState
initialPickingApplesState state =
    { name = state.name
    , gemCount = state.gemCount
    , windowSize = dummyWindowSize
    , headDirection = Right
    , headLocation = { x = 4, y = 5 }
    , tailSegments = [ { x = 3, y = 5 }, { x = 3, y = 6 } ]
    , appleLocation = { x = 3, y = 2 }
    , appleCountBefore = state.appleCountBefore
    , pickedAppleCount = 0
    , eatAppleAudio = Nothing
    , eatAppleTimes = []
    }


dummyWindowSize : { width : Int, height : Int }
dummyWindowSize =
    { width = 1920, height = 1080 }



-- pick apples


type SnakeDirection
    = Up
    | Right
    | Down
    | Left


type alias PickApplesState =
    RecordWithoutConstructorFunction
        { name : String
        , gemCount : Int
        , appleCountBefore : Int
        , windowSize : { width : Int, height : Int }
        , headDirection : SnakeDirection
        , headLocation : PickApplesLocation
        , tailSegments : List PickApplesLocation
        , appleLocation : PickApplesLocation
        , pickedAppleCount : Int
        , eatAppleAudio : Maybe (Result Web.AudioSourceLoadError Web.AudioSource)
        , eatAppleTimes : List { time : Time.Posix, nthPickedApple : Int }
        }


type alias PickApplesLocation =
    RecordWithoutConstructorFunction
        { x : Int
        , y : Int
        }


type PickApplesEvent
    = PickApplesKeyPressed (Result Json.Decode.Error String)
    | PickApplesGamepadReceived (Maybe Web.Gamepad)
    | PickApplesSimulationTick Time.Posix
    | WindowSizeReceived { width : Int, height : Int }
    | EatAppleAudioReceived (Result Web.AudioSourceLoadError Web.AudioSource)


worldSizeCells : { x : Int, y : Int }
worldSizeCells =
    { x = 16, y = 12 }


pickApplesInterface : PickApplesState -> Web.Interface InitializedState
pickApplesInterface state =
    [ case state.eatAppleAudio of
        Just (Ok eatAppleAudioSource) ->
            state.eatAppleTimes
                |> List.map
                    (\eatAppleAudio ->
                        Web.Audio.fromSource eatAppleAudioSource eatAppleAudio.time
                            |> Web.Audio.speedScaleBy
                                (Web.Audio.Parameter.at
                                    (2 ^ ((eatAppleAudio.nthPickedApple |> Basics.toFloat) * 0.01))
                                )
                    )
                |> List.map Web.Audio.play
                |> Web.interfaceBatch

        _ ->
            Web.Audio.sourceLoad "eat-apple.mp3"
                |> Web.interfaceFutureMap EatAppleAudioReceived
    , Web.Time.periodicallyListen (Duration.milliseconds 110)
        |> Web.interfaceFutureMap PickApplesSimulationTick
    , [ Web.Window.sizeRequest, Web.Window.resizeListen ]
        |> Web.interfaceBatch
        |> Web.interfaceFutureMap WindowSizeReceived
    , Web.Window.listenTo "keydown"
        |> Web.interfaceFutureMap
            (\event ->
                event
                    |> Json.Decode.decodeValue
                        (Json.Decode.field "key" Json.Decode.string)
                    |> PickApplesKeyPressed
            )
    , [ Web.Gamepads.request, Web.Gamepads.changeListen ]
        |> Web.interfaceBatch
        |> Web.interfaceFutureMap
            (\gamepads ->
                PickApplesGamepadReceived
                    (gamepads |> Dict.foldr (\_ gamepad _ -> gamepad |> Just) Nothing)
            )
    , let
        rectangleAtCellLocation : Color.Color -> PickApplesLocation -> Web.DomNode state_
        rectangleAtCellLocation fill cellLocation =
            Web.Svg.element "rect"
                [ Svg.LocalExtra.fillUniform fill
                , Web.Dom.attribute "width" (((cellSideLength * 0.9) |> String.fromFloat) ++ "px")
                , Web.Dom.attribute "height" (((cellSideLength * 0.9) |> String.fromFloat) ++ "px")
                , Web.Dom.attribute "x" (((cellSideLength * 0.05 + toFloat cellLocation.x * cellSideLength) |> String.fromFloat) ++ "px")
                , Web.Dom.attribute "y" (((cellSideLength * 0.05 + toFloat cellLocation.y * cellSideLength) |> String.fromFloat) ++ "px")
                ]
                []

        worldUi : Web.DomNode state_
        worldUi =
            Web.Svg.element "rect"
                [ Svg.LocalExtra.fillUniform Color.black
                , Web.Dom.attribute "width" "100%"
                , Web.Dom.attribute "height" "100%"
                ]
                []

        splitIntoSegmentsThatDoNotWrapAround : List { x : Int, y : Int } -> List (List { x : Int, y : Int })
        splitIntoSegmentsThatDoNotWrapAround =
            \points ->
                case points of
                    [] ->
                        []

                    head :: tail ->
                        let
                            segmented : { previousPoint : { x : Int, y : Int }, currentSegment : List { x : Int, y : Int }, finishedSegments : List (List { x : Int, y : Int }) }
                            segmented =
                                tail
                                    |> List.foldl
                                        (\point soFar ->
                                            if
                                                (((point.x - soFar.previousPoint.x) |> abs) >= 2)
                                                    || (((point.y - soFar.previousPoint.y) |> abs) >= 2)
                                            then
                                                { previousPoint = point
                                                , currentSegment = [ point ]
                                                , finishedSegments = soFar.currentSegment :: soFar.finishedSegments
                                                }

                                            else
                                                { previousPoint = point
                                                , currentSegment = point :: soFar.currentSegment
                                                , finishedSegments = soFar.finishedSegments
                                                }
                                        )
                                        { previousPoint = head, currentSegment = [ head ], finishedSegments = [] }
                        in
                        segmented.currentSegment :: segmented.finishedSegments

        headTailUi : Web.DomNode future_
        headTailUi =
            let
                segments : List (List { x : Int, y : Int })
                segments =
                    (state.headLocation :: state.tailSegments)
                        |> splitIntoSegmentsThatDoNotWrapAround

                legsUi : Web.DomNode future_
                legsUi =
                    segments
                        |> List.concat
                        |> listTakeEveryAndEnds 4
                        |> List.map
                            (\point ->
                                [ Svg.LocalExtra.line
                                    { start =
                                        { x = 0.1 * cellSideLength + cellSideLength * (point.x |> Basics.toFloat)
                                        , y = 0.9 * cellSideLength + cellSideLength * (point.y |> Basics.toFloat)
                                        }
                                    , end =
                                        { x = 0.9 * cellSideLength + cellSideLength * (point.x |> Basics.toFloat)
                                        , y = 0.1 * cellSideLength + cellSideLength * (point.y |> Basics.toFloat)
                                        }
                                    }
                                    [ Svg.LocalExtra.strokeUniform (Color.rgb 0.5 0.7 0.7)
                                    , Svg.LocalExtra.strokeWidth (cellSideLength * 0.3)
                                    , Web.Dom.attribute "stroke-linecap" "round"
                                    ]
                                , Svg.LocalExtra.line
                                    { start =
                                        { x = 0.9 * cellSideLength + cellSideLength * (point.x |> Basics.toFloat)
                                        , y = 0.9 * cellSideLength + cellSideLength * (point.y |> Basics.toFloat)
                                        }
                                    , end =
                                        { x = 0.1 * cellSideLength + cellSideLength * (point.x |> Basics.toFloat)
                                        , y = 0.1 * cellSideLength + cellSideLength * (point.y |> Basics.toFloat)
                                        }
                                    }
                                    [ Svg.LocalExtra.strokeUniform (Color.rgb 0.5 0.7 0.7)
                                    , Svg.LocalExtra.strokeWidth (cellSideLength * 0.3)
                                    , Web.Dom.attribute "stroke-linecap" "round"
                                    ]
                                ]
                                    |> Web.Svg.element "g" []
                            )
                        |> Web.Svg.element "g" []

                warpAnimationUi : Web.DomNode future_
                warpAnimationUi =
                    case segments |> List.reverse of
                        (lastPoint :: beforeLastPoint) :: _ ->
                            case beforeLastPoint of
                                _ :: _ :: _ ->
                                    Web.Dom.text ""

                                [ _ ] ->
                                    [ Svg.LocalExtra.circle
                                        { radius = cellSideLength * 2.5
                                        , position =
                                            { x = cellSideLength * 0.5 + (lastPoint.x |> Basics.toFloat) * cellSideLength
                                            , y = cellSideLength * 0.5 + (lastPoint.y |> Basics.toFloat) * cellSideLength
                                            }
                                        }
                                        [ Svg.LocalExtra.fillUniform colorInvisible
                                        , Svg.LocalExtra.strokeUniform (Color.rgba 1 1 1 0.01)
                                        , Svg.LocalExtra.strokeWidth (cellSideLength * 1)
                                        ]
                                    , Svg.LocalExtra.circle
                                        { radius = cellSideLength * 0.4
                                        , position =
                                            { x = cellSideLength * 0.5 + (lastPoint.x |> Basics.toFloat) * cellSideLength
                                            , y = cellSideLength * 0.5 + (lastPoint.y |> Basics.toFloat) * cellSideLength
                                            }
                                        }
                                        [ Svg.LocalExtra.fillUniform colorInvisible
                                        , Svg.LocalExtra.strokeUniform (Color.rgba 1 1 1 0.075)
                                        , Svg.LocalExtra.strokeWidth (cellSideLength * 0.7)
                                        ]
                                    ]
                                        |> Web.Svg.element "g" []

                                [] ->
                                    [ Svg.LocalExtra.circle
                                        { radius = cellSideLength * 3.5
                                        , position =
                                            { x = cellSideLength * 0.5 + (lastPoint.x |> Basics.toFloat) * cellSideLength
                                            , y = cellSideLength * 0.5 + (lastPoint.y |> Basics.toFloat) * cellSideLength
                                            }
                                        }
                                        [ Svg.LocalExtra.fillUniform colorInvisible
                                        , Svg.LocalExtra.strokeUniform (Color.rgba 1 1 1 0.01)
                                        , Svg.LocalExtra.strokeWidth (cellSideLength * 2)
                                        ]
                                    , Svg.LocalExtra.circle
                                        { radius = cellSideLength * 1.8
                                        , position =
                                            { x = cellSideLength * 0.5 + (lastPoint.x |> Basics.toFloat) * cellSideLength
                                            , y = cellSideLength * 0.5 + (lastPoint.y |> Basics.toFloat) * cellSideLength
                                            }
                                        }
                                        [ Svg.LocalExtra.fillUniform colorInvisible
                                        , Svg.LocalExtra.strokeUniform (Color.rgba 1 1 1 0.015)
                                        , Svg.LocalExtra.strokeWidth (cellSideLength * 0.5)
                                        ]
                                    , Svg.LocalExtra.circle
                                        { radius = cellSideLength * 0.4
                                        , position =
                                            { x = cellSideLength * 0.5 + (lastPoint.x |> Basics.toFloat) * cellSideLength
                                            , y = cellSideLength * 0.5 + (lastPoint.y |> Basics.toFloat) * cellSideLength
                                            }
                                        }
                                        [ Svg.LocalExtra.fillUniform colorInvisible
                                        , Svg.LocalExtra.strokeUniform (Color.rgba 1 1 1 0.2)
                                        , Svg.LocalExtra.strokeWidth (cellSideLength * 0.7)
                                        ]
                                    ]
                                        |> Web.Svg.element "g" []

                        _ ->
                            Web.Dom.text ""

                facePoints : { x : Int, y : Int } -> Float -> List { x : Float, y : Float }
                facePoints head size =
                    [ { x = cellSideLength * ((head.x |> Basics.toFloat) + (1 - size) / 2)
                      , y = cellSideLength * ((head.y |> Basics.toFloat) + ((1 - size) / 2 + 0.5 * size))
                      }
                    , { x = cellSideLength * ((head.x |> Basics.toFloat) + ((1 - size) / 2 + 0.5 * size))
                      , y = cellSideLength * ((head.y |> Basics.toFloat) + ((1 - size) / 2 + size))
                      }
                    , { x = cellSideLength * ((head.x |> Basics.toFloat) + ((1 - size) / 2 + size))
                      , y = cellSideLength * ((head.y |> Basics.toFloat) + ((1 - size) / 2 + 0.5 * size))
                      }
                    , { x = cellSideLength * ((head.x |> Basics.toFloat) + ((1 - size) / 2 + 0.5 * size))
                      , y = cellSideLength * ((head.y |> Basics.toFloat) + (1 - size) / 2)
                      }
                    ]

                headUi : Web.DomNode future_
                headUi =
                    [ Svg.LocalExtra.polygon (facePoints state.headLocation 0.8)
                        [ Svg.LocalExtra.fillUniform (Color.rgba 0 0.5 1 0.5)
                        ]
                    , Svg.LocalExtra.polygon (facePoints state.headLocation 0.4)
                        [ Svg.LocalExtra.fillUniform (Color.rgba 1 1 1 1)
                        ]
                    ]
                        |> Web.Svg.element "g" []
            in
            [ legsUi
            , warpAnimationUi
            , segments
                |> List.map
                    (\segmentPoints ->
                        Svg.LocalExtra.polyline
                            (segmentPoints
                                |> List.map
                                    (\location ->
                                        { x = cellSideLength * 0.5 + cellSideLength * (location.x |> Basics.toFloat)
                                        , y = cellSideLength * 0.5 + cellSideLength * (location.y |> Basics.toFloat)
                                        }
                                    )
                            )
                            [ Svg.LocalExtra.strokeUniform (Color.rgb 0.9 0.9 0.9)
                            , Svg.LocalExtra.fillUniform colorInvisible
                            , Web.Dom.attribute "stroke-linecap" "round"
                            , Web.Dom.attribute "stroke-linejoin" "round"
                            , Svg.LocalExtra.strokeWidth cellSideLength
                            ]
                    )
                |> Web.Svg.element "g" []
            , headUi
            ]
                |> Web.Svg.element "g" []

        appleUi : Web.DomNode future_
        appleUi =
            [ Svg.LocalExtra.circle
                { radius = cellSideLength * 0.45
                , position =
                    { x = cellSideLength * 0.5 + toFloat state.appleLocation.x * cellSideLength
                    , y = cellSideLength * 0.5 + toFloat state.appleLocation.y * cellSideLength
                    }
                }
                [ Svg.LocalExtra.fillUniform (Color.rgb 0.8 0.1 0.03)
                ]
            , Svg.LocalExtra.line
                { start =
                    { x = cellSideLength * 0.5 + toFloat state.appleLocation.x * cellSideLength
                    , y = cellSideLength * 0.17 + toFloat state.appleLocation.y * cellSideLength
                    }
                , end =
                    { x = cellSideLength * 0.39 + toFloat state.appleLocation.x * cellSideLength
                    , y = cellSideLength * -0.05 + toFloat state.appleLocation.y * cellSideLength
                    }
                }
                [ Svg.LocalExtra.strokeUniform (Color.rgb 0.34 0.19 0.01)
                , Svg.LocalExtra.strokeWidth (cellSideLength * 0.16)
                , Web.Dom.attribute "stroke-linecap" "round"
                ]
            , Svg.LocalExtra.line
                { start =
                    { x = cellSideLength * 0.44 + toFloat state.appleLocation.x * cellSideLength
                    , y = cellSideLength * 0.94 + toFloat state.appleLocation.y * cellSideLength
                    }
                , end =
                    { x = cellSideLength * 0.56 + toFloat state.appleLocation.x * cellSideLength
                    , y = cellSideLength * 0.94 + toFloat state.appleLocation.y * cellSideLength
                    }
                }
                [ Svg.LocalExtra.strokeUniform (Color.rgba 0.2 0.12 0 0.7)
                , Svg.LocalExtra.strokeWidth (cellSideLength * 0.07)
                , Web.Dom.attribute "stroke-linecap" "round"
                ]
            , let
                position : { x : Float, y : Float }
                position =
                    { x = cellSideLength * 0.82 + toFloat state.appleLocation.x * cellSideLength
                    , y = cellSideLength * 0.08 + toFloat state.appleLocation.y * cellSideLength
                    }
              in
              Svg.LocalExtra.ellipse
                { radiusX = cellSideLength * 0.34
                , radiusY = cellSideLength * 0.12
                , position = position
                }
                [ Svg.LocalExtra.fillUniform (Color.rgb 0.1 0.5 0)
                , Svg.LocalExtra.rotated { center = position, angle = Angle.turns -0.042 }
                ]
            , let
                position : { x : Float, y : Float }
                position =
                    { x = cellSideLength * 0.68 + toFloat state.appleLocation.x * cellSideLength
                    , y = cellSideLength * 0.2 + toFloat state.appleLocation.y * cellSideLength
                    }
              in
              Svg.LocalExtra.ellipse
                { radiusX = cellSideLength * 0.2
                , radiusY = cellSideLength * 0.1
                , position = position
                }
                [ Svg.LocalExtra.fillUniform (Color.rgba 0.1 0.5 0 0.5)
                , Svg.LocalExtra.rotated { center = position, angle = Angle.turns 0.083 }
                ]
            , let
                position : { x : Float, y : Float }
                position =
                    { x = cellSideLength * 0.25 + toFloat state.appleLocation.x * cellSideLength
                    , y = cellSideLength * 0.2 + toFloat state.appleLocation.y * cellSideLength
                    }
              in
              Svg.LocalExtra.ellipse
                { radiusX = cellSideLength * 0.13
                , radiusY = cellSideLength * 0.05
                , position = position
                }
                [ Svg.LocalExtra.fillUniform (Color.rgba 1 1 1 0.2)
                , Svg.LocalExtra.rotated { center = position, angle = Angle.turns -0.083 }
                ]
            ]
                |> Web.Svg.element "g" []

        pickedAppleCountUi : Web.DomNode future_
        pickedAppleCountUi =
            Web.Svg.element "text"
                [ Svg.LocalExtra.fillUniform (Color.rgba 0.3 1 0.5 0.13)
                , Web.Dom.style "font-size" "30em"
                , Web.Dom.attribute "text-anchor" "middle"
                , Web.Dom.attribute "dominant-baseline" "middle"
                , Web.Dom.attribute "font-weight" "bolder"
                , Web.Dom.attribute "x" "50%"
                , Web.Dom.attribute "y" "50%"
                , Web.Dom.attribute "width" "50%"
                , Web.Dom.attribute "height" "50%"
                ]
                [ state.pickedAppleCount |> String.fromInt |> Web.Dom.text ]

        controlsUi : Web.DomNode state_
        controlsUi =
            Web.Svg.element "text"
                [ Svg.LocalExtra.fillUniform (Color.rgb 0.3 0.7 0.5)
                , Web.Dom.style "font-size" "3em"
                , Web.Dom.attribute "text-anchor" "middle"
                , Web.Dom.attribute "dominant-baseline" "middle"
                , Web.Dom.attribute "font-weight" "bolder"
                , Web.Dom.attribute "x" "50%"
                , Web.Dom.attribute "y" "8%"
                , Web.Dom.attribute "width" "50%"
                , Web.Dom.attribute "height" "50%"
                ]
                [ (if state.pickedAppleCount >= 3 then
                    ""

                   else
                    "arrow keys or left controller thumbstick"
                  )
                    |> Web.Dom.text
                ]

        cellSideLength : Float
        cellSideLength =
            worldSize.width / (worldSizeCells.x |> Basics.toFloat)

        worldSize : { width : Float, height : Float }
        worldSize =
            let
                ratioWidthToHeight : Float
                ratioWidthToHeight =
                    (worldSizeCells.x |> Basics.toFloat) / (worldSizeCells.y |> Basics.toFloat)
            in
            if (state.windowSize.width |> Basics.toFloat) < (state.windowSize.height |> Basics.toFloat) * ratioWidthToHeight then
                -- disproportional in height
                { width = state.windowSize.width |> Basics.toFloat
                , height = (state.windowSize.width |> Basics.toFloat) / ratioWidthToHeight
                }

            else
                -- might be disproportional in width
                { width = (state.windowSize.height |> Basics.toFloat) * ratioWidthToHeight
                , height = state.windowSize.height |> Basics.toFloat
                }
      in
      Web.Dom.element "div"
        [ Web.Dom.style "background-color" (Color.rgb 0.05 0.05 0.05 |> Color.toCssString)
        , Web.Dom.style "position" "fixed"
        , Web.Dom.style "top" "0"
        , Web.Dom.style "right" "0"
        , Web.Dom.style "bottom" "0"
        , Web.Dom.style "left" "0"
        ]
        [ Web.Svg.element "svg"
            [ Web.Dom.attribute "viewBox" ("0 0 " ++ (worldSize.width |> String.fromFloat) ++ " " ++ (worldSize.height |> String.fromFloat))
            , Web.Dom.attribute "width" ((worldSize.width |> String.fromFloat) ++ "px")
            , Web.Dom.attribute "height" ((worldSize.height |> String.fromFloat) ++ "px")
            , Web.Dom.style "display" "block"
            , Web.Dom.style "margin" "auto"
            ]
            [ worldUi
            , pickedAppleCountUi
            , controlsUi
            , headTailUi
            , appleUi
            ]
        ]
        |> Web.Dom.render
    ]
        |> Web.interfaceBatch
        |> Web.interfaceFutureMap
            (\event ->
                case event of
                    WindowSizeReceived windowSize ->
                        PickingApples { state | windowSize = windowSize }

                    PickApplesSimulationTick newTime ->
                        let
                            headMovement =
                                directionToXYOffset state.headDirection

                            newHeadLocation =
                                { x = (state.headLocation.x + headMovement.x) |> modBy worldSizeCells.x
                                , y = (state.headLocation.y + headMovement.y) |> modBy worldSizeCells.y
                                }

                            applePicked : Bool
                            applePicked =
                                newHeadLocation == state.appleLocation

                            newTailSegments =
                                if applePicked then
                                    state.headLocation :: state.tailSegments

                                else
                                    (state.headLocation :: state.tailSegments)
                                        -- TODO something like List.Extra.init
                                        |> List.reverse
                                        |> List.drop 1
                                        |> List.reverse
                        in
                        if newTailSegments |> List.member newHeadLocation then
                            AtSign
                                { name = state.name
                                , gemCount = state.gemCount
                                , appleCount = state.appleCountBefore + state.pickedAppleCount
                                , birdConversationState = WaitingForTalk
                                }

                        else
                            PickingApples
                                { state
                                    | pickedAppleCount =
                                        if applePicked then
                                            state.pickedAppleCount + 1

                                        else
                                            state.pickedAppleCount
                                    , eatAppleTimes =
                                        if applePicked then
                                            state.eatAppleTimes
                                                |> (::) { time = newTime, nthPickedApple = state.pickedAppleCount + 1 }

                                        else
                                            state.eatAppleTimes
                                    , headLocation = newHeadLocation
                                    , tailSegments = newTailSegments
                                    , appleLocation =
                                        if not applePicked then
                                            state.appleLocation

                                        else
                                            let
                                                cellsLocationsWithoutSnake : List ( Int, Int )
                                                cellsLocationsWithoutSnake =
                                                    Set.diff
                                                        (List.range 0 (worldSizeCells.x - 1)
                                                            |> List.concatMap
                                                                (\x ->
                                                                    List.range 0 (worldSizeCells.y - 1)
                                                                        |> List.map (\y -> ( x, y ))
                                                                )
                                                            |> Set.fromList
                                                        )
                                                        ((newHeadLocation :: newTailSegments)
                                                            |> List.map
                                                                (\location -> ( location.x, location.y ))
                                                            |> Set.fromList
                                                        )
                                                        |> Set.toList
                                            in
                                            -- TODO use Random.Extra.choose instead
                                            cellsLocationsWithoutSnake
                                                |> List.drop (15485863 |> modBy ((cellsLocationsWithoutSnake |> List.length) - 1))
                                                |> List.head
                                                |> Maybe.map (\( x, y ) -> { x = x, y = y })
                                                |> Maybe.withDefault { x = -1, y = -1 }
                                }

                    PickApplesKeyPressed (Err _) ->
                        PickingApples state

                    PickApplesKeyPressed (Ok key) ->
                        case snakeDirectionFromKeyboardKey |> Dict.get key of
                            Nothing ->
                                PickingApples state

                            Just snakeDirection ->
                                PickingApples { state | headDirection = snakeDirection }

                    PickApplesGamepadReceived Nothing ->
                        PickingApples state

                    PickApplesGamepadReceived (Just gamepad) ->
                        case gamepad.thumbstickLeft |> snakeDirectionFromThumbstick of
                            Nothing ->
                                PickingApples state

                            Just snakeDirection ->
                                PickingApples { state | headDirection = snakeDirection }

                    EatAppleAudioReceived received ->
                        PickingApples { state | eatAppleAudio = received |> Just }
            )


colorInvisible : Color
colorInvisible =
    Color.rgba 0 0 0 0


listTakeEveryAndEnds : Int -> (List a -> List a)
listTakeEveryAndEnds step =
    \list ->
        case list of
            [] ->
                []

            head :: tail ->
                tail
                    |> List.foldr
                        (\element soFar ->
                            if soFar.dropCount <= 0 then
                                { dropCount = step, result = soFar.result |> (::) element }

                            else
                                { dropCount = soFar.dropCount - 1, result = soFar.result }
                        )
                        { result = [], dropCount = 0 }
                    |> .result
                    |> (::) head


snakeDirectionFromKeyboardKey : Dict String SnakeDirection
snakeDirectionFromKeyboardKey =
    Dict.fromList
        [ ( "w", Up )
        , ( "a", Left )
        , ( "s", Down )
        , ( "d", Right )
        , ( "ArrowUp", Up )
        , ( "ArrowDown", Down )
        , ( "ArrowLeft", Left )
        , ( "ArrowRight", Right )
        ]


snakeDirectionFromThumbstick : { x : Float, y : Float } -> Maybe SnakeDirection
snakeDirectionFromThumbstick =
    \thumbCoordinates ->
        if (thumbCoordinates.y |> abs) <= 0.3 && (thumbCoordinates.x |> abs) <= 0.3 then
            Nothing

        else
            (if (thumbCoordinates.y |> abs) > (thumbCoordinates.x |> abs) then
                if thumbCoordinates.y < 0 then
                    Up

                else
                    Down

             else if thumbCoordinates.x < 0 then
                Left

             else
                Right
            )
                |> Just


directionToXYOffset : SnakeDirection -> { x : Int, y : Int }
directionToXYOffset direction =
    case direction of
        Up ->
            { x = 0, y = -1 }

        Down ->
            { x = 0, y = 1 }

        Left ->
            { x = -1, y = 0 }

        Right ->
            { x = 1, y = 0 }


mapWithExitInterface : Web.Interface future_
mapWithExitInterface =
    Web.Navigation.load "https://dark.elm.dmy.fr/packages/lue-bird/elm-state-interface/latest/"



-- Ui


uiFrame : List (Web.Dom.Modifier state) -> List (Web.DomNode state) -> Web.DomNode state
uiFrame modifiers subs =
    Web.Dom.element "div"
        ([ Web.Dom.style "font-size" "2em"
         , Web.Dom.style "padding-left" "80px"
         , Web.Dom.style "padding-right" "80px"
         , Web.Dom.style "position" "fixed"
         , Web.Dom.style "top" "0"
         , Web.Dom.style "right" "0"
         , Web.Dom.style "bottom" "0"
         , Web.Dom.style "left" "0"
         , Web.Dom.style "background-color" (Color.rgb 0 0 0 |> Color.toCssString)
         , Web.Dom.style "color" (Color.rgb 1 1 1 |> Color.toCssString)
         ]
            ++ modifiers
        )
        subs


narrativeUiFrame : List (Web.Dom.Modifier state_) -> List (Web.DomNode state_) -> Web.DomNode state_
narrativeUiFrame modifiers subs =
    uiFrame
        modifiers
        [ Web.Dom.element "div"
            [ Web.Dom.style "max-width" "870px"
            , Web.Dom.style "padding-top" "80px"
            ]
            subs
        ]


buttonUi : List (Web.Dom.Modifier ()) -> List (Web.DomNode ()) -> Web.DomNode ()
buttonUi modifiers subs =
    Web.Dom.element "button"
        ([ Web.Dom.listenTo "click"
            |> Web.Dom.modifierFutureMap (\_ -> ())
         , Web.Dom.style "background-color" "#000000"
         , Web.Dom.style "border" "3px solid"
         , Web.Dom.style "border-radius" "50px"
         , Web.Dom.style "color" "#FFFFFF"
         , Web.Dom.style "padding" "5px 15px"
         , Web.Dom.style "margin" "7px 0px"
         , Web.Dom.style "text-align" "center"
         , Web.Dom.style "display" "inline-block"
         , Web.Dom.style "font-size" "1em"
         , Web.Dom.style "font-family" "inherit"
         ]
            ++ modifiers
        )
        subs


textInputUi : Maybe String -> Web.DomNode (Result Json.Decode.Error String)
textInputUi currentInputValue =
    Web.Dom.element "input"
        [ Web.Dom.attribute "type" "text"
        , Web.Dom.attribute "value"
            (case currentInputValue of
                Nothing ->
                    ""

                Just inputValue ->
                    inputValue
            )
        , Web.Dom.listenTo "input"
            |> Web.Dom.modifierFutureMap
                (Json.Decode.decodeValue
                    (Json.Decode.field "target" (Json.Decode.field "value" Json.Decode.string))
                )
        , Web.Dom.style "font-size" "1em"
        , Web.Dom.style "background-color" "transparent"
        , Web.Dom.style "border-bottom" "3px solid white"
        , Web.Dom.style "border-top" "none"
        , Web.Dom.style "border-left" "none"
        , Web.Dom.style "border-right" "none"
        , Web.Dom.style "color" "inherit"
        , Web.Dom.style "font-family" "inherit"
        ]
        []


clockUi : { posix : Time.Posix, timezone : Time.Zone } -> Web.DomNode state_
clockUi state =
    let
        hour : Int
        hour =
            Time.toHour state.timezone state.posix

        minute : Int
        minute =
            Time.toMinute state.timezone state.posix

        second : Int
        second =
            Time.toSecond state.timezone state.posix
    in
    Web.Svg.element "svg"
        [ Web.Dom.attribute "viewBox" "0 0 60 60"
        , Web.Dom.attribute "width" "60"
        , Web.Dom.attribute "height" "60"
        ]
        [ Svg.LocalExtra.circle
            { radius = 30
            , position =
                { x = 30
                , y = 30
                }
            }
            [ Svg.LocalExtra.fillUniform (Color.rgba 1 1 1 0.15)
            ]
        , clockHandUi { width = 4, length = 15, turns = (hour |> Basics.toFloat) / 12 }
        , clockHandUi { width = 3, length = 20, turns = (minute |> Basics.toFloat) / 60 }
        , clockHandUi { width = 2, length = 22, turns = (second |> Basics.toFloat) / 60 }
        ]


clockHandUi : { width : Int, length : Float, turns : Float } -> Web.DomNode state_
clockHandUi config =
    let
        clockTurns : Float
        clockTurns =
            config.turns - 0.25
    in
    Svg.LocalExtra.line
        { start = { x = 30, y = 30 }
        , end =
            { x = 30 + config.length * cos (Basics.turns clockTurns)
            , y = 30 + config.length * sin (Basics.turns clockTurns)
            }
        }
        [ Svg.LocalExtra.strokeUniform (Color.rgb 1 1 1)
        , Svg.LocalExtra.strokeWidth (config.width |> Basics.toFloat)
        , Web.Dom.attribute "stroke-linecap" "round"
        ]



-- app url


stateToAppUrl : InitializedState -> AppUrl
stateToAppUrl =
    \state ->
        { path = []
        , queryParameters = Dict.singleton "" [ state |> Serialize.encodeToString stateCodec ]
        , fragment = Nothing
        }


stateCodec : Serialize.Codec error_ InitializedState
stateCodec =
    Serialize.customType
        (\startingRoomStateVariant atSignStateVariant pickApplesStateVariant showingMapWithExitVariant state ->
            case state of
                StartingRoom startingRoomState ->
                    startingRoomStateVariant startingRoomState

                AtSign atSignState ->
                    atSignStateVariant atSignState

                PickingApples pickApplesState ->
                    pickApplesStateVariant pickApplesState

                ShowingMapWithExit ->
                    showingMapWithExitVariant
        )
        |> Serialize.variant1 StartingRoom startingRoomStateCodec
        |> Serialize.variant1 AtSign atSignStateCodec
        |> Serialize.variant1 PickingApples pickApplesStateCodec
        |> Serialize.variant0 ShowingMapWithExit
        |> Serialize.finishCustomType


appUrlToState : AppUrl -> Maybe InitializedState
appUrlToState =
    \appUrl ->
        appUrl.queryParameters
            |> Dict.get ""
            |> Maybe.andThen List.head
            |> Maybe.andThen (\str -> str |> Serialize.decodeFromString stateCodec |> Result.toMaybe)


startingRoomStateCodec : Serialize.Codec error_ StartingRoomState
startingRoomStateCodec =
    Serialize.record
        (\name gemCount ->
            { name = name
            , gemCount = gemCount
            , mousePoint = { x = 0, y = 0 }
            , timezone = Time.utc
            , posix = Time.millisToPosix 0
            }
        )
        |> Serialize.field .name (Serialize.maybe Serialize.string)
        |> Serialize.field .gemCount Serialize.int
        |> Serialize.finishRecord


atSignStateCodec : Serialize.Codec error_ AtSignState
atSignStateCodec =
    Serialize.record
        (\name gemCount appleCount birdConversationState ->
            { name = name
            , gemCount = gemCount
            , appleCount = appleCount
            , birdConversationState = birdConversationState
            }
        )
        |> Serialize.field .name Serialize.string
        |> Serialize.field .gemCount Serialize.int
        |> Serialize.field .gemCount Serialize.int
        |> Serialize.field .birdConversationState birdConversationStateCodec
        |> Serialize.finishRecord


birdConversationStateCodec : Serialize.Codec error_ BirdConversationState
birdConversationStateCodec =
    Serialize.customType
        (\waitingForTalk greetingAndAskingForWhatYouWant birdTellAboutItself askedBirdForMap tooHungryToSell birdConversationState ->
            case birdConversationState of
                WaitingForTalk ->
                    waitingForTalk

                GreetingAndAskingForWhatYouWant ->
                    greetingAndAskingForWhatYouWant

                BirdTellAboutItself ->
                    birdTellAboutItself

                AskedBirdForMap ->
                    askedBirdForMap

                TooHungryToSell ->
                    tooHungryToSell
        )
        |> Serialize.variant0 WaitingForTalk
        |> Serialize.variant0 GreetingAndAskingForWhatYouWant
        |> Serialize.variant0 BirdTellAboutItself
        |> Serialize.variant0 AskedBirdForMap
        |> Serialize.variant0 TooHungryToSell
        |> Serialize.finishCustomType


pickApplesStateCodec : Serialize.Codec error_ PickApplesState
pickApplesStateCodec =
    Serialize.record
        (\name gemCount appleCountBefore pickedAppleCount ->
            initialPickingApplesState
                { name = name
                , gemCount = gemCount
                , appleCountBefore = appleCountBefore + pickedAppleCount
                }
        )
        |> Serialize.field .name Serialize.string
        |> Serialize.field .gemCount Serialize.int
        |> Serialize.field .appleCountBefore Serialize.int
        |> Serialize.field .pickedAppleCount Serialize.int
        |> Serialize.finishRecord


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
