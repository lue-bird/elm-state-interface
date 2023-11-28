port module Main exposing (main)

import Array
import BrowserApp
import BrowserApp.Dom
import BrowserApp.Svg
import BrowserApp.Time
import BrowserApp.Window
import Color
import Dict exposing (Dict)
import Duration
import Json.Decode
import Json.Decode.Local
import Json.Encode
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import Set
import Time


main : Program () (BrowserApp.State State) (BrowserApp.Event State)
main =
    app |> BrowserApp.toProgram


app : BrowserApp.Config State
app =
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
    , ports = { fromJs = fromJs, toJs = toJs }
    }


type State
    = StartingRoom StartingRoomState
    | AtSign AtSignState
    | PickingApples PickApplesState


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
    | MouseMovedTo { x : Int, y : Int }
    | GemCountDecreaseClicked
    | GemCountIncreaseClicked
    | TimePassed Time.Posix
    | TimeZoneReceived Time.Zone
    | WalkToSignClicked


startingRoomInterface : StartingRoomState -> BrowserApp.Interface State
startingRoomInterface =
    \state ->
        [ uiFrame
            [ BrowserApp.Dom.listenTo "mousemove"
                |> BrowserApp.Dom.modifierMap
                    (\mouseEvent ->
                        mouseEvent
                            |> Json.Decode.decodeValue
                                (Json.Decode.succeed (\x y -> { x = x, y = y })
                                    |> Json.Decode.Local.andMap (Json.Decode.field "clientX" Json.Decode.int)
                                    |> Json.Decode.Local.andMap (Json.Decode.field "clientY" Json.Decode.int)
                                )
                            |> Result.withDefault { x = -1, y = -1 }
                            |> MouseMovedTo
                    )
            ]
            [ "You find yourself trapped in a state-interface. The old clock on the wall shows " |> BrowserApp.Dom.text
            , clockUi { posix = state.posix, timezone = state.timezone }
            , ". Countless questions rush through your mind:" |> BrowserApp.Dom.text
            , BrowserApp.Dom.element "ul"
                []
                [ BrowserApp.Dom.element "li"
                    []
                    [ "\"How did you get here?\"" |> BrowserApp.Dom.text ]
                , BrowserApp.Dom.element "li"
                    []
                    [ "\"Why do I know that you're exactly at "
                        ++ ("x" ++ (state.mousePoint.x |> String.fromInt) ++ " y" ++ (state.mousePoint.y |> String.fromInt))
                        ++ "?\""
                        |> BrowserApp.Dom.text
                    ]
                , BrowserApp.Dom.element "li"
                    []
                    [ "\"How do I know your name " |> BrowserApp.Dom.text
                    , textInputUi |> BrowserApp.Dom.map NameChanged
                    , "?\"" |> BrowserApp.Dom.text
                    ]
                , BrowserApp.Dom.element "li"
                    []
                    [ "Why is there a tutl?" |> BrowserApp.Dom.text
                    , BrowserApp.Svg.element "svg"
                        [ BrowserApp.Dom.attribute "viewBox" "0 12 96 40"
                        , BrowserApp.Dom.attribute "width" "96"
                        , BrowserApp.Dom.attribute "height" "40"
                        ]
                        [ BrowserApp.Svg.element "image"
                            [ BrowserApp.Dom.attribute "width" "72"
                            , BrowserApp.Dom.attribute "height" "72"
                            , BrowserApp.Dom.attribute "href" "https://elm-lang.org/images/turtle.gif"
                            ]
                            []
                        ]
                    ]
                ]
            , "\"Don't worry\", I say. \"I know how we can get out. See this little bird on the sign over there? It will give us a map for 💎3\"" |> BrowserApp.Dom.text
            , BrowserApp.Dom.element "br" [] []
            , "The voice repeats: \"Don't worry. Here,  take a couple 💎 if you want"
                ++ (case state.name of
                        Nothing ->
                            ""

                        Just name ->
                            ", " ++ name
                   )
                ++ ":\""
                |> BrowserApp.Dom.text
            , BrowserApp.Dom.element "div"
                [ BrowserApp.Dom.style "padding-top" "30px"
                , BrowserApp.Dom.style "padding-bottom" "30px"
                ]
                [ buttonUi
                    [ BrowserApp.Dom.style "height" "60px"
                    , BrowserApp.Dom.style "width" "60px"
                    , BrowserApp.Dom.style "text-align" "center"
                    ]
                    [ "+" |> BrowserApp.Dom.text ]
                    |> BrowserApp.Dom.map (\() -> GemCountIncreaseClicked)
                , BrowserApp.Dom.element "b"
                    [ BrowserApp.Dom.style "padding" "15px 15px"
                    ]
                    [ "💎" ++ (state.gemCount |> String.fromInt) |> BrowserApp.Dom.text ]
                , buttonUi
                    [ BrowserApp.Dom.style "height" "60px"
                    , BrowserApp.Dom.style "width" "60px"
                    , BrowserApp.Dom.style "text-align" "center"
                    ]
                    [ "-" |> BrowserApp.Dom.text ]
                    |> BrowserApp.Dom.map (\() -> GemCountDecreaseClicked)
                ]
            , buttonUi []
                [ "walk towards the sign as "
                    ++ (case state.name of
                            Nothing ->
                                "nameless"

                            Just name ->
                                name
                       )
                    |> BrowserApp.Dom.text
                ]
                |> BrowserApp.Dom.map (\() -> WalkToSignClicked)
            ]
            |> BrowserApp.Dom.render
        , BrowserApp.Time.zoneRequest |> BrowserApp.map TimeZoneReceived
        , BrowserApp.Time.periodicallyListen Duration.second |> BrowserApp.map TimePassed
        ]
            |> BrowserApp.batch
            |> BrowserApp.map
                (\event ->
                    case event of
                        MouseMovedTo newMousePoint ->
                            StartingRoom { state | mousePoint = newMousePoint }

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
                                }
                )


type alias AtSignState =
    RecordWithoutConstructorFunction
        { gemCount : Int
        , name : String
        }


type AtSignEvent
    = TalkToBirdClicked
    | PickApplesClicked


atSignInterface : AtSignState -> BrowserApp.Interface State
atSignInterface =
    \state ->
        [ uiFrame []
            [ BrowserApp.Dom.element "p"
                []
                [ "\"And there we are, " ++ state.name ++ "!\"" |> BrowserApp.Dom.text ]
            , BrowserApp.Dom.element "div"
                [ BrowserApp.Dom.style "text-align" "center"
                , BrowserApp.Dom.style "width" "50%"
                ]
                [ "🕊️" |> BrowserApp.Dom.text ]
            , BrowserApp.Dom.element "div"
                [ BrowserApp.Dom.style "text-align" "center"
                , BrowserApp.Dom.style "width" "50%"
                ]
                [ "🎏" |> BrowserApp.Dom.text ]
            , BrowserApp.Dom.element "p"
                []
                [ "\"Don't you think the bird looks a bit hungry...\"" |> BrowserApp.Dom.text ]
            , BrowserApp.Dom.element "div"
                [ BrowserApp.Dom.style "padding" "17px" ]
                [ buttonUi []
                    [ "talk to the bird" |> BrowserApp.Dom.text
                    ]
                    |> BrowserApp.Dom.map (\() -> TalkToBirdClicked)
                , " or " |> BrowserApp.Dom.text
                , buttonUi []
                    [ "pick some 🍎s" |> BrowserApp.Dom.text
                    ]
                    |> BrowserApp.Dom.map (\() -> PickApplesClicked)
                ]
            ]
            |> BrowserApp.Dom.render
        ]
            |> BrowserApp.batch
            |> BrowserApp.map
                (\event ->
                    case event of
                        TalkToBirdClicked ->
                            AtSign state

                        PickApplesClicked ->
                            PickingApples
                                { headDirection = Right
                                , headLocation = { x = 4, y = 5 }
                                , tailSegments = [ { x = 3, y = 5 } ]
                                , appleLocation = { x = 3, y = 2 }
                                }
                )



-- pick apples


type SnakeDirection
    = Up
    | Right
    | Down
    | Left


type alias PickApplesState =
    { headDirection : SnakeDirection
    , headLocation : Location
    , tailSegments : List Location
    , appleLocation : Location
    }


type alias Location =
    RecordWithoutConstructorFunction
        { x : Int
        , y : Int
        }


type PickApplesEvent
    = PickApplesKeyPressed (Result Json.Decode.Error String)
    | PickApplesSimulationTick Time.Posix


cellSideLength : number
cellSideLength =
    80


worldSizeCells : { x : Int, y : Int }
worldSizeCells =
    { x = 16, y = 12 }


worldWidth : Float
worldWidth =
    cellSideLength * toFloat worldSizeCells.x


worldHeight : Float
worldHeight =
    cellSideLength * toFloat worldSizeCells.y


pickApplesInterface : PickApplesState -> BrowserApp.Interface State
pickApplesInterface state =
    [ BrowserApp.Time.periodicallyListen (Duration.milliseconds 125)
        |> BrowserApp.map PickApplesSimulationTick
    , BrowserApp.Dom.documentEventListen "keydown"
        |> BrowserApp.map
            (\event ->
                event
                    |> Json.Decode.decodeValue
                        (Json.Decode.field "key" Json.Decode.string)
                    |> PickApplesKeyPressed
            )
    , let
        rectangleAtCellLocation : Color.Color -> Location -> BrowserApp.DomNode state_
        rectangleAtCellLocation fill cellLocation =
            BrowserApp.Svg.element "rect"
                [ BrowserApp.Dom.style "fill" (fill |> Color.toCssString)
                , BrowserApp.Dom.attribute "width" (((cellSideLength * 0.9) |> String.fromFloat) ++ "px")
                , BrowserApp.Dom.attribute "height" (((cellSideLength * 0.9) |> String.fromFloat) ++ "px")
                , BrowserApp.Dom.attribute "x" (((cellSideLength * 0.05 + toFloat cellLocation.x * cellSideLength) |> String.fromFloat) ++ "px")
                , BrowserApp.Dom.attribute "y" (((cellSideLength * 0.05 + toFloat cellLocation.y * cellSideLength) |> String.fromFloat) ++ "px")
                ]
                []

        worldShape =
            BrowserApp.Svg.element "rect"
                [ BrowserApp.Dom.style "fill" (Color.black |> Color.toCssString)
                , BrowserApp.Dom.attribute "width" "100%"
                , BrowserApp.Dom.attribute "height" "100%"
                ]
                []

        snakeUi =
            BrowserApp.Svg.element "g"
                []
                (state.headLocation
                    :: state.tailSegments
                    |> List.map (rectangleAtCellLocation Color.lightGrey)
                )

        appleUi =
            rectangleAtCellLocation Color.red state.appleLocation
      in
      BrowserApp.Svg.element "svg"
        [ BrowserApp.Dom.attribute "viewBox" ("0 0 " ++ (worldWidth |> String.fromFloat) ++ " " ++ (worldHeight |> String.fromFloat))
        , BrowserApp.Dom.attribute "width" ((worldWidth |> String.fromFloat) ++ "px")
        , BrowserApp.Dom.attribute "height" ((worldHeight |> String.fromFloat) ++ "px")
        , BrowserApp.Dom.style "display" "block"
        , BrowserApp.Dom.style "margin" "auto"
        ]
        [ worldShape, snakeUi, appleUi ]
        |> BrowserApp.Dom.render
    ]
        |> BrowserApp.batch
        |> BrowserApp.map
            (\event ->
                case event of
                    PickApplesSimulationTick _ ->
                        let
                            headMovement =
                                directionToXYOffset state.headDirection

                            newHeadLocation =
                                { x = (state.headLocation.x + headMovement.x) |> modBy worldSizeCells.x
                                , y = (state.headLocation.y + headMovement.y) |> modBy worldSizeCells.y
                                }

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
                        PickingApples
                            { state
                                | headLocation = newHeadLocation
                                , tailSegments = newTailSegments
                                , appleLocation =
                                    if not applePicked then
                                        state.appleLocation

                                    else
                                        let
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



-- Ui


uiFrame : List (BrowserApp.Dom.Modifier state) -> List (BrowserApp.DomNode state) -> BrowserApp.DomNode state
uiFrame modifiers subs =
    BrowserApp.Dom.element "div"
        ([ BrowserApp.Dom.style "font-size" "2em"
         , BrowserApp.Dom.style "padding-top" "50px"
         , BrowserApp.Dom.style "padding-left" "80px"
         , BrowserApp.Dom.style "padding-right" "80px"
         , BrowserApp.Dom.style "height" "100vh"
         , BrowserApp.Dom.style "background-color" (Color.rgb 0 0 0 |> Color.toCssString)
         , BrowserApp.Dom.style "color" (Color.rgb 1 1 1 |> Color.toCssString)
         ]
            ++ modifiers
        )
        subs


buttonUi : List (BrowserApp.Dom.Modifier ()) -> List (BrowserApp.DomNode ()) -> BrowserApp.DomNode ()
buttonUi modifiers subs =
    BrowserApp.Dom.element "button"
        ([ BrowserApp.Dom.listenTo "click"
            |> BrowserApp.Dom.modifierMap (\_ -> ())
         , BrowserApp.Dom.style "background-color" "#000000"
         , BrowserApp.Dom.style "border" "3px solid"
         , BrowserApp.Dom.style "border-radius" "50px"
         , BrowserApp.Dom.style "color" "#FFFFFF"
         , BrowserApp.Dom.style "padding" "5px 15px"
         , BrowserApp.Dom.style "text-align" "center"
         , BrowserApp.Dom.style "display" "inline-block"
         , BrowserApp.Dom.style "font-size" "1em"
         , BrowserApp.Dom.style "font-family" "inherit"
         ]
            ++ modifiers
        )
        subs


textInputUi : BrowserApp.DomNode (Result Json.Decode.Error String)
textInputUi =
    BrowserApp.Dom.element "input"
        [ BrowserApp.Dom.attribute "type" "text"
        , BrowserApp.Dom.listenTo "input"
            |> BrowserApp.Dom.modifierMap
                (Json.Decode.decodeValue
                    (Json.Decode.field "target" (Json.Decode.field "value" Json.Decode.string))
                )
        , BrowserApp.Dom.style "font-size" "1em"
        , BrowserApp.Dom.style "background-color" "transparent"
        , BrowserApp.Dom.style "border-bottom" "3px solid white"
        , BrowserApp.Dom.style "border-top" "none"
        , BrowserApp.Dom.style "border-left" "none"
        , BrowserApp.Dom.style "border-right" "none"
        , BrowserApp.Dom.style "color" "inherit"
        , BrowserApp.Dom.style "font-family" "inherit"
        ]
        []


clockUi : { posix : Time.Posix, timezone : Time.Zone } -> BrowserApp.DomNode state_
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
    BrowserApp.Svg.element "svg"
        [ BrowserApp.Dom.attribute "viewBox" "0 0 60 60"
        , BrowserApp.Dom.attribute "width" "60"
        , BrowserApp.Dom.attribute "height" "60"
        ]
        [ BrowserApp.Svg.element "circle"
            [ BrowserApp.Dom.attribute "cx" "30"
            , BrowserApp.Dom.attribute "cy" "30"
            , BrowserApp.Dom.attribute "r" "30"
            , BrowserApp.Dom.attribute "fill" (Color.rgba 1 1 1 0.15 |> Color.toCssString)
            ]
            []
        , clockHandUi { width = 4, length = 15, turns = (hour |> Basics.toFloat) / 12 }
        , clockHandUi { width = 3, length = 20, turns = (minute |> Basics.toFloat) / 60 }
        , clockHandUi { width = 2, length = 22, turns = (second |> Basics.toFloat) / 60 }
        ]


clockHandUi : { width : Int, length : Float, turns : Float } -> BrowserApp.DomNode state_
clockHandUi config =
    let
        clockTurns : Float
        clockTurns =
            config.turns - 0.25
    in
    BrowserApp.Svg.element "line"
        [ BrowserApp.Dom.attribute "x1" "30"
        , BrowserApp.Dom.attribute "y1" "30"
        , BrowserApp.Dom.attribute "x2"
            (30 + config.length * cos (Basics.turns clockTurns) |> String.fromFloat)
        , BrowserApp.Dom.attribute "y2"
            (30 + config.length * sin (Basics.turns clockTurns) |> String.fromFloat)
        , BrowserApp.Dom.attribute "stroke" "white"
        , BrowserApp.Dom.attribute "stroke-width" (String.fromInt config.width)
        , BrowserApp.Dom.attribute "stroke-linecap" "round"
        ]
        []


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
