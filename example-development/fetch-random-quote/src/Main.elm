port module Main exposing (main)

import Color
import Json.Decode
import Json.Encode
import Random.Pcg.Extended
import Web
import Web.Dom
import Web.Http
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
    = State
        { movies : Maybe (Result Web.HttpError (Result { actualBody : String, jsonError : Json.Decode.Error } { content : String, author : String }))
        }


type DiceUiEvent
    = MoreClicked
    | MoviesReceived (Result Web.HttpError (Result { actualBody : String, jsonError : Json.Decode.Error } { content : String, author : String }))


codeRandomGenerator : Random.Pcg.Extended.Generator Int
codeRandomGenerator =
    Random.Pcg.Extended.int 100 500


initialState : State
initialState =
    State { movies = Nothing }


interface : State -> Web.Interface State
interface =
    \state ->
        case state of
            State randomStuff ->
                [ case randomStuff.movies of
                    Just _ ->
                        Web.interfaceNone

                    Nothing ->
                        Web.Http.get
                            { url = "https://api.quotable.io/quotes/random"
                            , headers = []
                            , expect =
                                Web.Http.expectJson
                                    (Json.Decode.index 0
                                        (Json.Decode.map2 (\content author -> { content = content, author = author })
                                            (Json.Decode.field "content" Json.Decode.string)
                                            (Json.Decode.field "author" Json.Decode.string)
                                        )
                                    )
                            }
                            |> Web.Http.request
                            |> Web.interfaceFutureMap MoviesReceived
                , Web.Dom.element "div"
                    [ Web.Dom.style "background-color" (Color.rgb 0 0 0 |> Color.toCssString)
                    , Web.Dom.style "color" (Color.rgb 1 1 1 |> Color.toCssString)
                    , Web.Dom.style "font-size" "2em"
                    , Web.Dom.style "padding-left" "80px"
                    , Web.Dom.style "padding-right" "80px"
                    , Web.Dom.style "position" "fixed"
                    , Web.Dom.style "top" "0"
                    , Web.Dom.style "right" "0"
                    , Web.Dom.style "bottom" "0"
                    , Web.Dom.style "left" "0"
                    ]
                    [ Web.Dom.element "div"
                        [ Web.Dom.style "max-width" "870px"
                        , Web.Dom.style "padding-top" "80px"
                        ]
                        [ Web.Dom.element "h1" [] [ Web.Dom.text "random quote" ]
                        , Web.Dom.element "div"
                            []
                            [ Web.Dom.element "div"
                                [ Web.Dom.style "font-size" "1.2em"
                                ]
                                [ case randomStuff.movies of
                                    Nothing ->
                                        Web.Dom.text "waiting for response"

                                    Just (Err Web.HttpBadUrl) ->
                                        Web.Dom.text "Malformed URL"

                                    Just (Err Web.HttpNetworkError) ->
                                        Web.Dom.text "Network error"

                                    Just (Err (Web.HttpBadStatus _)) ->
                                        Web.Dom.text "Bad response status"

                                    Just (Ok (Err decodeError)) ->
                                        Web.Dom.text
                                            (decodeError.jsonError |> Json.Decode.errorToString)

                                    Just (Ok (Ok movies)) ->
                                        Web.Dom.element "div"
                                            []
                                            [ Web.Dom.element "blockquote" [ Web.Dom.style "textAlign" "center" ] [ Web.Dom.text movies.content ]
                                            , Web.Dom.element "i" [ Web.Dom.style "textAlign" "center" ] [ Web.Dom.text ("by " ++ movies.author) ]
                                            ]
                                ]
                            , Web.Dom.element "br" [] []
                            , buttonUi
                                [ Web.Dom.style "font-size" "2em"
                                ]
                                [ Web.Dom.text "next!" ]
                                |> Web.Dom.futureMap (\() -> MoreClicked)
                            ]
                        ]
                    ]
                    |> Web.Dom.render
                ]
                    |> Web.interfaceBatch
                    |> Web.interfaceFutureMap
                        (\event ->
                            case event of
                                MoreClicked ->
                                    State { randomStuff | movies = Nothing }

                                MoviesReceived moviesResponse ->
                                    State { randomStuff | movies = moviesResponse |> Just }
                        )


buttonUi : List (Web.Dom.Modifier ()) -> List (Web.Dom.Node ()) -> Web.Dom.Node ()
buttonUi modifiers subs =
    Web.Dom.element "button"
        ([ Web.Dom.listenTo "click"
            |> Web.Dom.modifierFutureMap (\_ -> ())
         , Web.Dom.style "background-color" "#000000"
         , Web.Dom.style "border-top" "none"
         , Web.Dom.style "border-left" "none"
         , Web.Dom.style "border-right" "none"
         , Web.Dom.style "border-bottom" ("5px solid " ++ (Color.rgba 1 1 1 0.2 |> Color.toCssString))
         , Web.Dom.style "border-radius" "20px"
         , Web.Dom.style "color" "#FFFFFF"
         , Web.Dom.style "padding" "4px 13px"
         , Web.Dom.style "margin" "0px 0px"
         , Web.Dom.style "text-align" "center"
         , Web.Dom.style "display" "inline-block"
         , Web.Dom.style "font-family" "inherit"
         ]
            ++ modifiers
        )
        subs
