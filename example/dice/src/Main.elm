port module Main exposing (main)

import Color
import Json.Decode
import Json.Encode
import Random.Pcg.Extended
import Web
import Web.Dom
import Web.Random


main : Web.Program State
main =
    Web.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event


type State
    = WaitingForInitialRandomness
    | DiceUiState { diceEyes : Int, seed : Random.Pcg.Extended.Seed }


type DiceUiEvent
    = RerollClicked


diceEyesRandomGenerator : Random.Pcg.Extended.Generator Int
diceEyesRandomGenerator =
    Random.Pcg.Extended.int 1 6


initialState : State
initialState =
    WaitingForInitialRandomness


interface : State -> Web.Interface State
interface =
    \state ->
        case state of
            WaitingForInitialRandomness ->
                Web.Random.unsignedInt32s 4
                    |> Web.interfaceFutureMap
                        (\unsignedInt32s ->
                            let
                                initialSeed : Random.Pcg.Extended.Seed
                                initialSeed =
                                    Random.Pcg.Extended.initialSeed (unsignedInt32s |> List.head |> Maybe.withDefault 0) (unsignedInt32s |> List.drop 1)

                                ( diceEyes, newSeed ) =
                                    Random.Pcg.Extended.step diceEyesRandomGenerator initialSeed
                            in
                            DiceUiState { diceEyes = diceEyes, seed = newSeed }
                        )

            DiceUiState randomStuff ->
                Web.Dom.element "div"
                    [ Web.Dom.style "background-color" (Color.rgb 0 0 0 |> Color.toCssString)
                    , Web.Dom.style "color" (Color.rgb 1 1 1 |> Color.toCssString)
                    , Web.Dom.style "padding-left" "80px"
                    , Web.Dom.style "padding-right" "80px"
                    , Web.Dom.style "position" "fixed"
                    , Web.Dom.style "top" "0"
                    , Web.Dom.style "right" "0"
                    , Web.Dom.style "bottom" "0"
                    , Web.Dom.style "left" "0"
                    ]
                    [ Web.Dom.element "span"
                        [ Web.Dom.style "font-size" "24em"
                        ]
                        [ randomStuff.diceEyes |> diceEyesToSymbol |> Web.Dom.text ]
                    , Web.Dom.element "br" [] []
                    , buttonUi
                        [ Web.Dom.style "font-size" "4em"
                        ]
                        [ Web.Dom.text "roll the dice" ]
                        |> Web.Dom.futureMap (\() -> RerollClicked)
                    ]
                    |> Web.Dom.render
                    |> Web.interfaceFutureMap
                        (\RerollClicked ->
                            let
                                ( diceEyes, newSeed ) =
                                    Random.Pcg.Extended.step diceEyesRandomGenerator randomStuff.seed
                            in
                            DiceUiState { diceEyes = diceEyes, seed = newSeed }
                        )


buttonUi : List (Web.Dom.Modifier ()) -> List (Web.Dom.Node ()) -> Web.Dom.Node ()
buttonUi modifiers subs =
    Web.Dom.element "button"
        ([ Web.Dom.listenTo "click"
            |> Web.Dom.modifierFutureMap (\_ -> ())
         , Web.Dom.style "background-color" (Color.rgba 0 0 0 0 |> Color.toCssString)
         , Web.Dom.style "border-top" "none"
         , Web.Dom.style "border-left" "none"
         , Web.Dom.style "border-right" "none"
         , Web.Dom.style "border-bottom" ("5px solid " ++ (Color.rgba 1 1 1 0.5 |> Color.toCssString))
         , Web.Dom.style "border-radius" "20px"
         , Web.Dom.style "color" "inherit"
         , Web.Dom.style "padding" "4px 13px"
         , Web.Dom.style "margin" "0px 0px"
         ]
            ++ modifiers
        )
        subs


diceEyesToSymbol : Int -> String
diceEyesToSymbol =
    \diceEyes ->
        case diceEyes of
            1 ->
                "⚀"

            2 ->
                "⚁"

            3 ->
                "⚂"

            4 ->
                "⚃"

            5 ->
                "⚄"

            _ ->
                "⚅"
