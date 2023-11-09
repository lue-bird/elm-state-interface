port module Main exposing (main)

import BrowserApp exposing (BrowserApp)
import Json.Encode


main : Program () (BrowserApp.State State) (BrowserApp.Event State)
main =
    app |> BrowserApp.toProgram


type State
    = HasNotDisplayed
    | HasDisplayed


type Event
    = Displayed


app : BrowserApp State
app =
    { initialState = HasNotDisplayed
    , interface =
        \state ->
            [ BrowserApp.Display { config = "hello world", on = \() -> Displayed } ]
                |> List.map
                    (BrowserApp.on
                        (\event ->
                            case event of
                                Displayed ->
                                    HasDisplayed
                        )
                    )
    , ports = { in_ = portIn, out = portOut }
    }


port portOut : Json.Encode.Value -> Cmd event_


port portIn : (Json.Encode.Value -> event) -> Sub event
