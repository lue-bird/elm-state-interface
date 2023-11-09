port module Main exposing (main)

import Array
import BrowserApp exposing (BrowserApp)
import Color
import Json.Encode


main : Program () (BrowserApp.State State) (BrowserApp.Event State)
main =
    app |> BrowserApp.toProgram


type State
    = HasNotDisplayed
    | HasDisplayed


type Event
    = Displayed
    | Drawn


app : BrowserApp State
app =
    { initialState = HasNotDisplayed
    , interface =
        \state ->
            [ BrowserApp.Display { config = "hello world", on = \() -> Displayed }
            , BrowserApp.Draw
                { config =
                    Array.initialize 360
                        (\y ->
                            Array.initialize 600
                                (\x ->
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
                                    HasDisplayed

                                Drawn ->
                                    HasDisplayed
                        )
                    )
    , ports = { in_ = portIn, out = portOut }
    }


port portOut : Json.Encode.Value -> Cmd event_


port portIn : (Json.Encode.Value -> event) -> Sub event
