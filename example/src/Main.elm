port module Main exposing (main)

import Array
import BrowserApp
import Color
import Json.Decode
import Json.Decode.Local
import Json.Encode
import BrowserApp.Dom


main : Program () (BrowserApp.State State) (BrowserApp.Event State)
main =
    app |> BrowserApp.toProgram


app : BrowserApp.Config State
app =
    { initialState = State { counter = 0, mousePoint = { x = 0, y = 0 } }
    , interface =
        \(State state) ->
            [ BrowserApp.Dom.element "div"
                |> BrowserApp.Dom.elementAddSubs
                    [ BrowserApp.Dom.element "p"
                        |> BrowserApp.Dom.elementAddSubs
                            [ "hello state-interface! Mouse coords:" |> BrowserApp.Dom.text
                            , BrowserApp.Dom.element "br" |> BrowserApp.Dom.elementToNode
                            , ("x = " ++ (state.mousePoint.x |> String.fromInt)) |> BrowserApp.Dom.text
                            , BrowserApp.Dom.element "br" |> BrowserApp.Dom.elementToNode
                            , ("y = " ++ (state.mousePoint.y |> String.fromInt)) |> BrowserApp.Dom.text
                            ]
                        |> BrowserApp.Dom.elementToNode
                    , BrowserApp.Dom.element "div"
                        |> BrowserApp.Dom.elementAddSubs
                            [ BrowserApp.Dom.element "button"
                                |> BrowserApp.Dom.elementAddSubs  [ "+" |> BrowserApp.Dom.text ]
                                |> BrowserApp.Dom.elementOnEvent "click"
                                    (\_ -> CounterIncreaseClicked)
                                |> BrowserApp.Dom.elementToNode
                            , BrowserApp.Dom.element "div"
                                |> BrowserApp.Dom.elementAddSubs
                                    [ (state.counter |> String.fromInt) |> BrowserApp.Dom.text ]
                                |> BrowserApp.Dom.elementToNode
                            , BrowserApp.Dom.element "button"
                                |> BrowserApp.Dom.elementAddSubs  [ "-" |> BrowserApp.Dom.text ]
                                |> BrowserApp.Dom.elementOnEvent "click"
                                    (\_ -> CounterDecreaseClicked)
                                |> BrowserApp.Dom.elementToNode
                            ]
                        |> BrowserApp.Dom.elementToNode
                    ]
                |> BrowserApp.Dom.elementOnEvent "mousemove"
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
                |> BrowserApp.Dom.elementToNode
                |> BrowserApp.Dom.render
            ]
                |> List.map
                    (BrowserApp.on
                        (\event ->
                            case event of
                                MouseMovedTo newMousePoint ->
                                    State { state | mousePoint = newMousePoint }
                                
                                CounterDecreaseClicked ->
                                    State { state | counter = state.counter - 1 }
                                
                                CounterIncreaseClicked ->
                                    State { state | counter = state.counter + 1 }
                        )
                    )
    , ports = { fromJs = fromJs, toJs = toJs }
    }


type State
    = State { counter : Int, mousePoint : { x : Int, y : Int } }


type Event
    = MouseMovedTo { x : Int, y : Int }
    | CounterDecreaseClicked
    | CounterIncreaseClicked


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event
