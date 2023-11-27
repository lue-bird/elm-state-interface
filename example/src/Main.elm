port module Main exposing (main)

import Array
import BrowserApp
import BrowserApp.Dom
import BrowserApp.Svg
import BrowserApp.Time
import BrowserApp.Window
import Color
import Duration
import Json.Decode
import Json.Decode.Local
import Json.Encode
import Time


main : Program () (BrowserApp.State State) (BrowserApp.Event State)
main =
    app |> BrowserApp.toProgram


app : BrowserApp.Config State
app =
    { initialState =
        State
            { counter = 0
            , mousePoint = { x = 0, y = 0 }
            , posix = Time.millisToPosix 0
            , timezone = Time.utc
            }
    , interface =
        \(State state) ->
            [ BrowserApp.Dom.element "div"
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
                [ BrowserApp.Dom.element "p"
                    []
                    [ "hello state-interface! Mouse coords:" |> BrowserApp.Dom.text
                    , BrowserApp.Dom.element "br" [] []
                    , ("x = " ++ (state.mousePoint.x |> String.fromInt)) |> BrowserApp.Dom.text
                    , BrowserApp.Dom.element "br" [] []
                    , ("y = " ++ (state.mousePoint.y |> String.fromInt)) |> BrowserApp.Dom.text
                    ]
                , BrowserApp.Dom.element "div"
                    []
                    [ BrowserApp.Dom.element "button"
                        [ BrowserApp.Dom.listenTo "click"
                            |> BrowserApp.Dom.modifierMap (\_ -> CounterIncreaseClicked)
                        ]
                        [ "+" |> BrowserApp.Dom.text ]
                    , BrowserApp.Dom.element "div"
                        []
                        [ (state.counter |> String.fromInt) |> BrowserApp.Dom.text ]
                    , BrowserApp.Dom.element "button"
                        [ BrowserApp.Dom.listenTo "click"
                            |> BrowserApp.Dom.modifierMap (\_ -> CounterDecreaseClicked)
                        ]
                        [ "-" |> BrowserApp.Dom.text ]
                    ]
                , clockUi { posix = state.posix, timezone = state.timezone }
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
                                State { state | mousePoint = newMousePoint }

                            CounterDecreaseClicked ->
                                State { state | counter = state.counter - 1 }

                            CounterIncreaseClicked ->
                                State { state | counter = state.counter + 1 }

                            TimePassed newTime ->
                                State { state | posix = newTime }

                            TimeZoneReceived timezone ->
                                State { state | timezone = timezone }
                    )
    , ports = { fromJs = fromJs, toJs = toJs }
    }


type State
    = State
        { counter : Int
        , mousePoint : { x : Int, y : Int }
        , timezone : Time.Zone
        , posix : Time.Posix
        }


type Event
    = MouseMovedTo { x : Int, y : Int }
    | CounterDecreaseClicked
    | CounterIncreaseClicked
    | TimePassed Time.Posix
    | TimeZoneReceived Time.Zone



-- Ui


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
        [ BrowserApp.Dom.attribute "viewBox" "0 0 400 400"
        , BrowserApp.Dom.attribute "width" "400"
        , BrowserApp.Dom.attribute "height" "400"
        ]
        [ BrowserApp.Svg.element "circle"
            [ BrowserApp.Dom.attribute "cx" "200"
            , BrowserApp.Dom.attribute "cy" "200"
            , BrowserApp.Dom.attribute "r" "120"
            , BrowserApp.Dom.attribute "fill" "#1293D8"
            ]
            []
        , clockHandUi { width = 8, length = 60, turns = (hour |> Basics.toFloat) / 12 }
        , clockHandUi { width = 5, length = 90, turns = (minute |> Basics.toFloat) / 60 }
        , clockHandUi { width = 3, length = 90, turns = (second |> Basics.toFloat) / 60 }
        ]


clockHandUi : { width : Int, length : Float, turns : Float } -> BrowserApp.DomNode state_
clockHandUi config =
    let
        clockTurns : Float
        clockTurns =
            config.turns - 0.25
    in
    BrowserApp.Svg.element "line"
        [ BrowserApp.Dom.attribute "x1" "200"
        , BrowserApp.Dom.attribute "y1" "200"
        , BrowserApp.Dom.attribute "x2"
            (200 + config.length * cos (Basics.turns clockTurns) |> String.fromFloat)
        , BrowserApp.Dom.attribute "y2"
            (200 + config.length * sin (Basics.turns clockTurns) |> String.fromFloat)
        , BrowserApp.Dom.attribute "stroke" "white"
        , BrowserApp.Dom.attribute "stroke-width" (String.fromInt config.width)
        , BrowserApp.Dom.attribute "stroke-linecap" "round"
        ]
        []


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
