port module Main exposing (main)

import Array
import BrowserApp
import Color
import Json.Decode
import Json.Decode.Extra
import Json.Encode


main : Program () (BrowserApp.State State) (BrowserApp.Event State)
main =
    app |> BrowserApp.toProgram


app : BrowserApp.Config State
app =
    { initialState = State { counter = 0, mousePoint = { x = 0, y = 0 } }
    , interface =
        \(State state) ->
            [ BrowserApp.domElement "div"
                |> BrowserApp.domElementAddSubs
                    [ BrowserApp.domElement "p"
                        |> BrowserApp.domElementAddSubs
                            [ "hello state-interface! Mouse coords:" |> BrowserApp.DomText
                            , BrowserApp.domElement "br" |> BrowserApp.DomElement
                            , ("x = " ++ (state.mousePoint.x |> String.fromInt)) |> BrowserApp.DomText
                            , BrowserApp.domElement "br" |> BrowserApp.DomElement
                            , ("y = " ++ (state.mousePoint.y |> String.fromInt)) |> BrowserApp.DomText
                            ]
                        |> BrowserApp.DomElement
                    , BrowserApp.domElement "div"
                        |> BrowserApp.domElementAddSubs
                            [ BrowserApp.domElement "button"
                                |> BrowserApp.domElementAddSubs  [ "+" |> BrowserApp.DomText ]
                                |> BrowserApp.domOnEvent "click"
                                    (\_ -> CounterIncreaseClicked)
                                |> BrowserApp.DomElement
                            , BrowserApp.domElement "div"
                                |> BrowserApp.domElementAddSubs
                                    [ (state.counter |> String.fromInt) |> BrowserApp.DomText ]
                                |> BrowserApp.DomElement
                            , BrowserApp.domElement "button"
                                |> BrowserApp.domElementAddSubs  [ "-" |> BrowserApp.DomText ]
                                |> BrowserApp.domOnEvent "click"
                                    (\_ -> CounterDecreaseClicked)
                                |> BrowserApp.DomElement
                            ]
                        |> BrowserApp.DomElement
                    ]
                |> BrowserApp.domOnEvent "mousemove"
                    (\mouseEvent ->
                        mouseEvent
                            |> Json.Decode.decodeValue
                                (Json.Decode.succeed (\x y -> { x = x, y = y })
                                    |> Json.Decode.Extra.andMap (Json.Decode.field "clientX" Json.Decode.int)
                                    |> Json.Decode.Extra.andMap (Json.Decode.field "clientY" Json.Decode.int)
                                )
                            |> Result.withDefault { x = -1, y = -1 }
                            |> MouseMovedTo
                    )
                |> BrowserApp.DomElement
                |> BrowserApp.RenderDomNode
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
