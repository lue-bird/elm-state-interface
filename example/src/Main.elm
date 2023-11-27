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
            { name = Nothing
            , counter = 0
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
                , BrowserApp.Dom.style "font-size" "2em"
                , BrowserApp.Dom.style "padding-top" "50px"
                , BrowserApp.Dom.style "padding-left" "80px"
                , BrowserApp.Dom.style "padding-right" "80px"
                , BrowserApp.Dom.style "height" "100vh"
                , BrowserApp.Dom.style "background-color" "#000000"
                , BrowserApp.Dom.style "color" "#FFFFFF"
                ]
                [ "You find yourself trapped in a state-interface. The old clock on the wall shows " |> BrowserApp.Dom.text
                , clockUi { posix = state.posix, timezone = state.timezone }
                , ". Countless questions rush through in your mind:" |> BrowserApp.Dom.text
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
                            [ BrowserApp.Dom.attribute "viewBox" "0 0 96 40"
                            , BrowserApp.Dom.attribute "width" "96"
                            , BrowserApp.Dom.attribute "height" "96"
                            ]
                            [ BrowserApp.Svg.element "image"
                                [ BrowserApp.Dom.attribute "width" "96"
                                , BrowserApp.Dom.attribute "height" "96"
                                , BrowserApp.Dom.attribute "href" "https://elm-lang.org/images/turtle.gif"
                                ]
                                []
                            ]
                        ]
                    ]
                , "\"Don't worry\", a soft voice says. \"I know how we can get out. See this little bird on the sign over there? It will give us a map for 💎3\"" |> BrowserApp.Dom.text
                , BrowserApp.Dom.element "br" [] []
                , "The voice repeats: \"Don't worry\". My pockets are deep, "
                    ++ (case state.name of
                            Nothing ->
                                "so take"

                            Just name ->
                                name ++ ". Take"
                       )
                    ++ " as many 💎 you want:"
                    |> BrowserApp.Dom.text
                , BrowserApp.Dom.element "div"
                    []
                    [ buttonUi
                        [ "+" |> BrowserApp.Dom.text ]
                        |> BrowserApp.Dom.map (\() -> CounterIncreaseClicked)
                    , BrowserApp.Dom.element "b"
                        [ BrowserApp.Dom.style "padding" "15px 15px"
                        ]
                        [ "💎" ++ (state.counter |> String.fromInt) |> BrowserApp.Dom.text ]
                    , buttonUi
                        [ "-" |> BrowserApp.Dom.text ]
                        |> BrowserApp.Dom.map (\() -> CounterDecreaseClicked)
                    ]
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

                            NameChanged (Err jsonError) ->
                                State { state | name = jsonError |> Json.Decode.errorToString |> Just }

                            NameChanged (Ok name) ->
                                State
                                    { state
                                        | name =
                                            case name |> String.trimLeft of
                                                "" ->
                                                    Nothing

                                                nonBlankName ->
                                                    nonBlankName |> Just
                                    }
                    )
    , ports = { fromJs = fromJs, toJs = toJs }
    }


type State
    = State
        { name : Maybe String
        , counter : Int
        , mousePoint : { x : Int, y : Int }
        , timezone : Time.Zone
        , posix : Time.Posix
        }


type Event
    = NameChanged (Result Json.Decode.Error String)
    | MouseMovedTo { x : Int, y : Int }
    | CounterDecreaseClicked
    | CounterIncreaseClicked
    | TimePassed Time.Posix
    | TimeZoneReceived Time.Zone



-- Ui


buttonUi : List (BrowserApp.DomNode ()) -> BrowserApp.DomNode ()
buttonUi subs =
    BrowserApp.Dom.element "button"
        [ BrowserApp.Dom.listenTo "click"
            |> BrowserApp.Dom.modifierMap (\_ -> ())
        , BrowserApp.Dom.style "background-color" "#000000"
        , BrowserApp.Dom.style "border" "none"
        , BrowserApp.Dom.style "color" "#FFFFFF"
        , BrowserApp.Dom.style "padding" "15px 15px"
        , BrowserApp.Dom.style "text-align" "center"
        , BrowserApp.Dom.style "display" "inline-block"
        , BrowserApp.Dom.style "font-size" "2em"
        ]
        subs


textInputUi : BrowserApp.DomNode (Result Json.Decode.Error String)
textInputUi =
    BrowserApp.Dom.element "input"
        [ BrowserApp.Dom.attribute "type" "text"
        , BrowserApp.Dom.listenTo "input"
            |> BrowserApp.Dom.modifierMap
                (\mouseEvent ->
                    mouseEvent
                        |> Json.Decode.decodeValue
                            (Json.Decode.field "target" (Json.Decode.field "value" Json.Decode.string))
                )
        , BrowserApp.Dom.style "font-size" "1em"
        , BrowserApp.Dom.style "background-color" "#000000"
        , BrowserApp.Dom.style "border-bottom" "3px solid white"
        , BrowserApp.Dom.style "border-top" "none"
        , BrowserApp.Dom.style "border-left" "none"
        , BrowserApp.Dom.style "border-right" "none"
        , BrowserApp.Dom.style "color" "#FFFFFF"
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
