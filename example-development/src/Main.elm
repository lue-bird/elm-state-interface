port module Main exposing (main)

import AppUrl exposing (AppUrl)
import Codec exposing (Codec)
import Color
import Dict exposing (Dict)
import Duration
import Json.Decode
import Json.Encode
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Set
import Time
import Url
import Web
import Web.Audio
import Web.Audio.Parameter
import Web.Dom
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
                                { name = state.name
                                , gemCount = state.gemCount
                                , windowSize = dummyWindowSize
                                , headDirection = Right
                                , headLocation = { x = 4, y = 5 }
                                , tailSegments = [ { x = 3, y = 5 } ]
                                , appleLocation = { x = 3, y = 2 }
                                , appleCountBefore = state.appleCount
                                , pickedAppleCount = 0
                                , eatAppleAudio = Nothing
                                , eatAppleTimes = []
                                }
                )


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
    , Web.Time.periodicallyListen (Duration.milliseconds 125)
        |> Web.interfaceFutureMap PickApplesSimulationTick
    , [ Web.Window.sizeRequest, Web.Window.resizeListen ]
        |> Web.interfaceBatch
        |> Web.interfaceFutureMap WindowSizeReceived
    , Web.Dom.documentEventListen "keydown"
        |> Web.interfaceFutureMap
            (\event ->
                event
                    |> Json.Decode.decodeValue
                        (Json.Decode.field "key" Json.Decode.string)
                    |> PickApplesKeyPressed
            )
    , let
        rectangleAtCellLocation : Color.Color -> PickApplesLocation -> Web.DomNode state_
        rectangleAtCellLocation fill cellLocation =
            Web.Svg.element "rect"
                [ Web.Dom.style "fill" (fill |> Color.toCssString)
                , Web.Dom.attribute "width" (((cellSideLength * 0.9) |> String.fromFloat) ++ "px")
                , Web.Dom.attribute "height" (((cellSideLength * 0.9) |> String.fromFloat) ++ "px")
                , Web.Dom.attribute "x" (((cellSideLength * 0.05 + toFloat cellLocation.x * cellSideLength) |> String.fromFloat) ++ "px")
                , Web.Dom.attribute "y" (((cellSideLength * 0.05 + toFloat cellLocation.y * cellSideLength) |> String.fromFloat) ++ "px")
                ]
                []

        worldUi : Web.DomNode state_
        worldUi =
            Web.Svg.element "rect"
                [ Web.Dom.style "fill" (Color.black |> Color.toCssString)
                , Web.Dom.attribute "width" "100%"
                , Web.Dom.attribute "height" "100%"
                ]
                []

        headTailUi : Web.DomNode state_
        headTailUi =
            Web.Svg.element "g"
                []
                (state.headLocation
                    :: state.tailSegments
                    |> List.map (rectangleAtCellLocation Color.lightGrey)
                )

        appleUi : Web.DomNode state_
        appleUi =
            Web.Svg.element "g"
                []
                [ Web.Svg.element "circle"
                    [ Web.Dom.style "fill" (Color.rgb 0.9 0.1 0.05 |> Color.toCssString)
                    , Web.Dom.attribute "r" (((cellSideLength * 0.45) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "cx" (((cellSideLength * 0.5 + toFloat state.appleLocation.x * cellSideLength) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "cy" (((cellSideLength * 0.5 + toFloat state.appleLocation.y * cellSideLength) |> String.fromFloat) ++ "px")
                    ]
                    []
                , Web.Svg.element "ellipse"
                    [ Web.Dom.style "fill" (Color.rgb 0.1 0.5 0 |> Color.toCssString)
                    , Web.Dom.attribute "rx" (((cellSideLength * 0.24) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "ry" (((cellSideLength * 0.12) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "cx" (((cellSideLength * 0.7 + toFloat state.appleLocation.x * cellSideLength) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "cy" (((cellSideLength * 0.1 + toFloat state.appleLocation.y * cellSideLength) |> String.fromFloat) ++ "px")
                    ]
                    []
                , Web.Svg.element "ellipse"
                    [ Web.Dom.style "fill" (Color.rgb 0.1 0.5 0 |> Color.toCssString)
                    , Web.Dom.attribute "rx" (((cellSideLength * 0.2) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "ry" (((cellSideLength * 0.1) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "cx" (((cellSideLength * 0.3 + toFloat state.appleLocation.x * cellSideLength) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "cy" (((cellSideLength * 0.12 + toFloat state.appleLocation.y * cellSideLength) |> String.fromFloat) ++ "px")
                    ]
                    []
                ]

        pickedAppleCountUi : Web.DomNode state_
        pickedAppleCountUi =
            Web.Svg.element "text"
                [ Web.Dom.style "fill" (Color.rgba 0.3 1 0.5 0.13 |> Color.toCssString)
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

                    EatAppleAudioReceived received ->
                        PickingApples { state | eatAppleAudio = received |> Just }
            )


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
    Web.Navigation.load
        { protocol = Url.Https
        , host = "dark.elm.dmy.fr"
        , port_ = Nothing
        , path = "/packages/lue-bird/elm-state-interface/latest/"
        , query = Nothing
        , fragment = Nothing
        }



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
        [ Web.Svg.element "circle"
            [ Web.Dom.attribute "cx" "30"
            , Web.Dom.attribute "cy" "30"
            , Web.Dom.attribute "r" "30"
            , Web.Dom.attribute "fill" (Color.rgba 1 1 1 0.15 |> Color.toCssString)
            ]
            []
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
    Web.Svg.element "line"
        [ Web.Dom.attribute "x1" "30"
        , Web.Dom.attribute "y1" "30"
        , Web.Dom.attribute "x2"
            (30 + config.length * cos (Basics.turns clockTurns) |> String.fromFloat)
        , Web.Dom.attribute "y2"
            (30 + config.length * sin (Basics.turns clockTurns) |> String.fromFloat)
        , Web.Dom.attribute "stroke" "white"
        , Web.Dom.attribute "stroke-width" (String.fromInt config.width)
        , Web.Dom.attribute "stroke-linecap" "round"
        ]
        []



-- app url


stateToAppUrl : InitializedState -> AppUrl
stateToAppUrl =
    \state ->
        { path = []
        , queryParameters = Dict.singleton "" [ state |> Codec.encodeToString 0 stateCodec ]
        , fragment = Nothing
        }


stateCodec : Codec InitializedState
stateCodec =
    Codec.custom
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
        |> Codec.variant1 "StartingRoom" StartingRoom startingRoomStateCodec
        |> Codec.variant1 "AtSign" AtSign atSignStateCodec
        |> Codec.variant1 "PickingApples" PickingApples pickApplesStateCodec
        |> Codec.variant0 "ShowingMapWithExit" ShowingMapWithExit
        |> Codec.buildCustom


appUrlToState : AppUrl -> Maybe InitializedState
appUrlToState =
    \appUrl ->
        appUrl.queryParameters
            |> Dict.get ""
            |> Maybe.andThen List.head
            |> Maybe.andThen (\str -> str |> Codec.decodeString stateCodec |> Result.toMaybe)


startingRoomStateCodec : Codec StartingRoomState
startingRoomStateCodec =
    Codec.object
        (\name gemCount ->
            { name = name
            , gemCount = gemCount
            , mousePoint = { x = 0, y = 0 }
            , timezone = Time.utc
            , posix = Time.millisToPosix 0
            }
        )
        |> Codec.field "name" .name (Codec.nullable Codec.string)
        |> Codec.field "gemCount" .gemCount Codec.int
        |> Codec.buildObject


atSignStateCodec : Codec AtSignState
atSignStateCodec =
    Codec.object
        (\name gemCount appleCount birdConversationState ->
            { name = name
            , gemCount = gemCount
            , appleCount = appleCount
            , birdConversationState = birdConversationState
            }
        )
        |> Codec.field "name" .name Codec.string
        |> Codec.field "gemCount" .gemCount Codec.int
        |> Codec.field "appleCount" .gemCount Codec.int
        |> Codec.field "birdConversationState" .birdConversationState birdConversationStateCodec
        |> Codec.buildObject


birdConversationStateCodec : Codec BirdConversationState
birdConversationStateCodec =
    Codec.custom
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
        |> Codec.variant0 "WaitingForTalk" WaitingForTalk
        |> Codec.variant0 "GreetingAndAskingForWhatYouWant" GreetingAndAskingForWhatYouWant
        |> Codec.variant0 "BirdTellAboutItself" BirdTellAboutItself
        |> Codec.variant0 "AskedBirdForMap" AskedBirdForMap
        |> Codec.variant0 "TooHungryToSell" TooHungryToSell
        |> Codec.buildCustom


pickApplesStateCodec : Codec PickApplesState
pickApplesStateCodec =
    Codec.object
        (\name gemCount appleCountBefore pickedAppleCount headDirection headLocation tailSegments appleLocation ->
            { name = name
            , gemCount = gemCount
            , appleCountBefore = appleCountBefore
            , pickedAppleCount = pickedAppleCount
            , headDirection = headDirection
            , headLocation = headLocation
            , tailSegments = tailSegments
            , appleLocation = appleLocation
            , windowSize = dummyWindowSize
            , eatAppleAudio = Nothing
            , eatAppleTimes = []
            }
        )
        |> Codec.field "name" .name Codec.string
        |> Codec.field "gemCount" .gemCount Codec.int
        |> Codec.field "appleCountBefore" .appleCountBefore Codec.int
        |> Codec.field "pickedAppleCount" .pickedAppleCount Codec.int
        |> Codec.field "headDirection" .headDirection directionCodec
        |> Codec.field "headLocation" .headLocation pickApplesLocationCodec
        |> Codec.field "tailSegments" .tailSegments (Codec.list pickApplesLocationCodec)
        |> Codec.field "appleLocation" .appleLocation pickApplesLocationCodec
        |> Codec.buildObject


pickApplesLocationCodec : Codec PickApplesLocation
pickApplesLocationCodec =
    Codec.object (\x y -> { x = x, y = y })
        |> Codec.field "x" .x Codec.int
        |> Codec.field "y" .y Codec.int
        |> Codec.buildObject


directionCodec : Codec SnakeDirection
directionCodec =
    Codec.custom
        (\left right up down direction ->
            case direction of
                Left ->
                    left

                Right ->
                    right

                Up ->
                    up

                Down ->
                    down
        )
        |> Codec.variant0 "Left" Left
        |> Codec.variant0 "Right" Right
        |> Codec.variant0 "Up" Up
        |> Codec.variant0 "Down" Down
        |> Codec.buildCustom


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
