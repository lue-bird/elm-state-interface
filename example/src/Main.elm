port module Main exposing (main)

import Array
import Color
import Dict exposing (Dict)
import Duration
import Json.Decode
import Json.Decode.Local
import Json.Encode
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Set
import Time
import Url
import Web
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
    { initialState =
        StartingRoom
            { name = Nothing
            , gemCount = 0
            , mousePoint = { x = 0, y = 0 }
            , posix = Time.millisToPosix 0
            , timezone = Time.utc
            }
    , interface =
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
    , ports = { fromJs = fromJs, toJs = toJs }
    }


type State
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


startingRoomInterface : StartingRoomState -> Web.Interface State
startingRoomInterface =
    \state ->
        [ narrativeUiFrame
            [ Web.Dom.listenTo "mousemove"
                |> Web.Dom.modifierMap
                    (\mouseEvent ->
                        mouseEvent
                            |> Json.Decode.decodeValue
                                (Json.Decode.succeed (\x y -> { x = x, y = y })
                                    |> Json.Decode.Local.andMap (Json.Decode.field "clientX" Json.Decode.int)
                                    |> Json.Decode.Local.andMap (Json.Decode.field "clientY" Json.Decode.int)
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
                    , textInputUi |> Web.Dom.map NameChanged
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
                    |> Web.Dom.map (\() -> GemCountIncreaseClicked)
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
                    |> Web.Dom.map (\() -> GemCountDecreaseClicked)
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
                |> Web.Dom.map (\() -> WalkToSignClicked)
            ]
            |> Web.Dom.render
        , Web.Time.zoneRequest |> Web.interfaceMap TimeZoneReceived
        , Web.Time.periodicallyListen Duration.second |> Web.interfaceMap TimePassed
        ]
            |> Web.interfaceBatch
            |> Web.interfaceMap
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
        , birdConversationState : ConversationWithBirdState
        }


type ConversationWithBirdState
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


atSignInterface : AtSignState -> Web.Interface State
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
                            |> Web.Dom.map (\() -> TalkToBirdClicked)
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
                            |> Web.Dom.map (\() -> PickApplesClicked)
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
                            |> Web.Dom.map (\() -> BirdTellAboutYourselfClicked)
                        , " or " |> Web.Dom.text
                        , buttonUi []
                            [ "Buy map with the exit" |> Web.Dom.text
                            ]
                            |> Web.Dom.map (\() -> BuyMapClicked)
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
                            |> Web.Dom.map (\() -> BuyMapClicked)
                        ]

                AskedBirdForMap ->
                    Web.Dom.element "div"
                        []
                        [ "\"Hope you'll come by again!\" says the bird, looking a bit down"
                            |> Web.Dom.text
                        , buttonUi []
                            [ "Open the map" |> Web.Dom.text
                            ]
                            |> Web.Dom.map (\() -> OpenMapClicked)
                        ]

                TooHungryToSell ->
                    Web.Dom.element "div"
                        []
                        [ "\"Nah, I'm hungry, I will need more of these fresh 🍎s\""
                            |> Web.Dom.text
                        , buttonUi []
                            [ "pick 🍎s" |> Web.Dom.text
                            ]
                            |> Web.Dom.map (\() -> PickApplesClicked)
                        ]
            ]
            |> Web.Dom.render
        ]
            |> Web.interfaceBatch
            |> Web.interfaceMap
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
                                , windowSize = { width = 1920, height = 1080 }
                                , headDirection = Right
                                , headLocation = { x = 4, y = 5 }
                                , tailSegments = [ { x = 3, y = 5 } ]
                                , appleLocation = { x = 3, y = 2 }
                                , appleCountBefore = state.appleCount
                                , pickedAppleCount = 0
                                }
                )



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


worldSizeCells : { x : Int, y : Int }
worldSizeCells =
    { x = 16, y = 12 }


pickApplesInterface : PickApplesState -> Web.Interface State
pickApplesInterface state =
    [ Web.Time.periodicallyListen (Duration.milliseconds 125)
        |> Web.interfaceMap PickApplesSimulationTick
    , [ Web.Window.sizeRequest, Web.Window.resizeListen ]
        |> Web.interfaceBatch
        |> Web.interfaceMap WindowSizeReceived
    , Web.Dom.documentEventListen "keydown"
        |> Web.interfaceMap
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
                    [ Web.Dom.style "fill" (Color.red |> Color.toCssString)
                    , Web.Dom.attribute "r" (((cellSideLength * 0.45) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "cx" (((cellSideLength * 0.5 + toFloat state.appleLocation.x * cellSideLength) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "cy" (((cellSideLength * 0.5 + toFloat state.appleLocation.y * cellSideLength) |> String.fromFloat) ++ "px")
                    ]
                    []
                , Web.Svg.element "ellipse"
                    [ Web.Dom.style "fill" (Color.rgb 0.1 0.5 0 |> Color.toCssString)
                    , Web.Dom.attribute "rx" (((cellSideLength * 0.1) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "ry" (((cellSideLength * 0.2) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "cx" (((cellSideLength * 0.6 + toFloat state.appleLocation.x * cellSideLength) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "cy" (((cellSideLength * 0.2 + toFloat state.appleLocation.y * cellSideLength) |> String.fromFloat) ++ "px")
                    ]
                    []
                , Web.Svg.element "ellipse"
                    [ Web.Dom.style "fill" (Color.rgb 0.1 0.5 0 |> Color.toCssString)
                    , Web.Dom.attribute "transform" "rotate(45deg)"
                    , Web.Dom.attribute "rx" (((cellSideLength * 0.1) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "ry" (((cellSideLength * 0.2) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "cx" (((cellSideLength * 0.5 + toFloat state.appleLocation.x * cellSideLength) |> String.fromFloat) ++ "px")
                    , Web.Dom.attribute "cy" (((cellSideLength * 0.2 + toFloat state.appleLocation.y * cellSideLength) |> String.fromFloat) ++ "px")
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
      Web.Svg.element "svg"
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
        |> Web.Dom.render
    ]
        |> Web.interfaceBatch
        |> Web.interfaceMap
            (\event ->
                case event of
                    WindowSizeReceived windowSize ->
                        PickingApples { state | windowSize = windowSize }

                    PickApplesSimulationTick _ ->
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


mapWithExitInterface : Web.Interface state_
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
         , Web.Dom.style "height" "100vh"
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
            |> Web.Dom.modifierMap (\_ -> ())
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


textInputUi : Web.DomNode (Result Json.Decode.Error String)
textInputUi =
    Web.Dom.element "input"
        [ Web.Dom.attribute "type" "text"
        , Web.Dom.listenTo "input"
            |> Web.Dom.modifierMap
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


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
