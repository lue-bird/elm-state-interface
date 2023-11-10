port module Main exposing (main)

import Array
import BrowserApp exposing (BrowserApp)
import Color
import Json.Decode
import Json.Decode.Extra
import Json.Encode


main : Program () (BrowserApp.State State) (BrowserApp.Event State)
main =
    app |> BrowserApp.toProgram


type State
    = State { mousePoint : { x : Int, y : Int } }


type Event
    = MouseMovedTo { x : Int, y : Int }
    | Displayed
    | Drawn


app : BrowserApp State
app =
    { initialState = State { mousePoint = { x = 0, y = 0 } }
    , interface =
        \(State state) ->
            [ BrowserApp.ListenToHtmlEvent
                { config = "onmousemove"
                , on =
                    \mouseEvent ->
                        mouseEvent
                            |> Json.Decode.decodeValue
                                (Json.Decode.succeed (\x y -> { x = x, y = y })
                                    |> Json.Decode.Extra.andMap (Json.Decode.field "offsetX" Json.Decode.int)
                                    |> Json.Decode.Extra.andMap (Json.Decode.field "offsetY" Json.Decode.int)
                                )
                            |> Result.withDefault { x = 0, y = 0 }
                            |> MouseMovedTo
                }
            , BrowserApp.Display { config = "hello world", on = \() -> Displayed }
            , BrowserApp.Draw
                { config =
                    Array.initialize 360
                        (\y ->
                            Array.initialize 600
                                (\x ->
                                    if abs (x - state.mousePoint.x) < 20 && abs (y - state.mousePoint.y) < 20 then
                                        Color.darkBlue

                                    else
                                        let
                                            distanceToCenter : Float
                                            distanceToCenter =
                                                sqrt
                                                    ((abs ((x |> Basics.toFloat) - 300) ^ 2)
                                                        + (abs ((y |> Basics.toFloat) - 180) ^ 2)
                                                    )
                                        in
                                        if distanceToCenter < 100 then
                                            Color.red

                                        else
                                            Color.rgb
                                                0
                                                ((y |> Basics.toFloat) / 360)
                                                ((x |> Basics.toFloat) / 600)
                                )
                        )
                , on = \() -> Drawn
                }
            ]
                |> List.map
                    (BrowserApp.on
                        (\event ->
                            case event of
                                Displayed ->
                                    State state

                                Drawn ->
                                    State state

                                MouseMovedTo newMousePoint ->
                                    State { state | mousePoint = newMousePoint }
                        )
                    )
    , ports = { in_ = portIn, out = portOut }
    }


port portOut : Json.Encode.Value -> Cmd event_


port portIn : (Json.Encode.Value -> event) -> Sub event
