port module Main exposing (main)

import Color
import Json.Decode
import Json.Encode
import Web
import Web.Dom


main : Web.Program State
main =
    Web.program
        { initialState = initialState
        , interface = interface
        , ports = { fromJs = fromJs, toJs = toJs }
        }


port toJs : Json.Encode.Value -> Cmd event_


port fromJs : (Json.Encode.Value -> event) -> Sub event


type alias State =
    { todos : List Todo
    , userInput : String
    , visibilityFilter : VisibilityFilter
    }


type alias Todo =
    { completed : Bool
    , content : String
    }


type VisibilityFilter
    = AllVisible
    | OnlyTodoVisible
    | OnlyCompletedVisible


type Event
    = InputTextSubmitClicked
    | InputTextChanged (Result Json.Decode.Error String)
    | TodoRemoved Int
    | TodoCompletenessToggled Int
    | ResetAllToUncompletedClicked
    | VisibilityFilterSet VisibilityFilter
    | RemoveCompleted


initialState : State
initialState =
    { todos = []
    , userInput = ""
    , visibilityFilter = AllVisible
    }


interface : State -> Web.Interface State
interface =
    \state ->
        [ ui state |> Web.Dom.render
        ]
            |> Web.interfaceBatch
            |> Web.interfaceFutureMap
                (\msg ->
                    case msg of
                        InputTextSubmitClicked ->
                            case state.userInput of
                                "" ->
                                    state

                                nonEmptyUserInput ->
                                    let
                                        newTodo =
                                            { content = nonEmptyUserInput
                                            , completed = False
                                            }

                                        newTodos =
                                            newTodo :: state.todos
                                    in
                                    { state | userInput = "", todos = newTodos }

                        InputTextChanged (Err _) ->
                            state

                        InputTextChanged (Ok str) ->
                            { state | userInput = str }

                        TodoRemoved todoId ->
                            let
                                filteredTodos =
                                    List.indexedMap Tuple.pair state.todos
                                        |> List.filter (\( i, _ ) -> i /= todoId)
                                        |> List.map (\( _, todo ) -> todo)
                            in
                            { state | todos = filteredTodos }

                        TodoCompletenessToggled todoIndex ->
                            { state
                                | todos =
                                    state.todos
                                        |> List.indexedMap
                                            (\i todo ->
                                                if i == todoIndex then
                                                    { todo | completed = not todo.completed }

                                                else
                                                    todo
                                            )
                            }

                        ResetAllToUncompletedClicked ->
                            { state
                                | todos =
                                    state.todos |> List.map (\todo -> { todo | completed = False })
                            }

                        VisibilityFilterSet newVisibilityFilter ->
                            { state | visibilityFilter = newVisibilityFilter }

                        RemoveCompleted ->
                            { state | todos = state.todos |> List.filter (\todo -> not todo.completed) }
                )


ui : State -> Web.Dom.Node Event
ui =
    \state ->
        Web.Dom.element "div"
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
                [ Web.Dom.element "h1" [] [ Web.Dom.text "todos" ]
                , Web.Dom.element "div"
                    []
                    [ todoNewItemInputUi state.userInput
                    , todoListInfoAndActionsUi state.todos state.visibilityFilter
                    , case state.todos of
                        [] ->
                            Web.Dom.element "div" [] []

                        todo0 :: todo1Up ->
                            Web.Dom.element "div"
                                [ Web.Dom.style "padding" "34px 34px 0px 0px" ]
                                [ visibilityOptionsUi state.visibilityFilter
                                , todoListUi { todos = todo0 :: todo1Up, visibilityFilter = state.visibilityFilter }
                                ]
                    ]
                ]
            ]


todoNewItemInputUi : String -> Web.Dom.Node Event
todoNewItemInputUi userInput =
    Web.Dom.element "div"
        []
        [ textInputUi InputTextChanged
            userInput
            [ Web.Dom.attribute "placeholder" "What needs to be done?"
            ]
        , buttonUi [] [ Web.Dom.text "add" ]
            |> Web.Dom.futureMap (\() -> InputTextSubmitClicked)
        ]


visibilityOptionsUi : VisibilityFilter -> Web.Dom.Node Event
visibilityOptionsUi currentVisibility =
    Web.Dom.element "div"
        []
        (Web.Dom.element "span"
            [ Web.Dom.style "padding" "0px 2px 0px 0px" ]
            [ Web.Dom.text ("showing " ++ (currentVisibility |> visibilityFilterToString) ++ ". Try also ")
            ]
            :: ([ AllVisible, OnlyTodoVisible, OnlyCompletedVisible ]
                    |> List.filter (\v -> v /= currentVisibility)
                    |> List.map (visibilityButtonUi currentVisibility)
               )
        )


visibilityButtonUi : VisibilityFilter -> VisibilityFilter -> Web.Dom.Node Event
visibilityButtonUi currentVisibilityFilter visibilityFilterToSetTo =
    buttonUi
        []
        [ Web.Dom.text (visibilityFilterToSetTo |> visibilityFilterToString) ]
        |> Web.Dom.futureMap (\() -> VisibilityFilterSet visibilityFilterToSetTo)


todoListInfoAndActionsUi : List Todo -> VisibilityFilter -> Web.Dom.Node Event
todoListInfoAndActionsUi todos currentVisibility =
    case todos of
        [] ->
            Web.Dom.text "no todos, yet."

        todo0 :: todo1Up ->
            let
                todoCount : Int
                todoCount =
                    todos |> List.filter (\todo -> not todo.completed) |> List.length
            in
            Web.Dom.element "div"
                []
                ((if (todo0 :: todo1Up) |> List.all .completed then
                    [ Web.Dom.element "div"
                        [ Web.Dom.style "color" (Color.rgba 1 1 1 0.5 |> Color.toCssString)
                        ]
                        [ Web.Dom.text "all completed." ]
                    , buttonUi [] [ Web.Dom.text "reset all as to do" ]
                        |> Web.Dom.futureMap (\() -> ResetAllToUncompletedClicked)
                    ]

                  else
                    [ Web.Dom.element "div"
                        [ Web.Dom.style "color" (Color.rgba 1 1 1 0.5 |> Color.toCssString)
                        ]
                        [ let
                            todoPluralized : String
                            todoPluralized =
                                case todoCount of
                                    1 ->
                                        "todo"

                                    _ ->
                                        "todos"
                          in
                          ((todoCount |> String.fromInt) ++ " " ++ todoPluralized ++ " left.")
                            |> Web.Dom.text
                        ]
                    ]
                 )
                    ++ [ buttonUi
                            []
                            [ Web.Dom.text "clear completed" ]
                            |> Web.Dom.futureMap (\() -> RemoveCompleted)
                       ]
                )


todoListUi : { todos : List Todo, visibilityFilter : VisibilityFilter } -> Web.Dom.Node Event
todoListUi state =
    let
        toListItem : ( Int, Todo ) -> Web.Dom.Node Event
        toListItem ( index, todo ) =
            Web.Dom.element "div"
                []
                [ buttonUi []
                    [ "✔" |> Web.Dom.text
                    ]
                    |> Web.Dom.futureMap (\() -> TodoCompletenessToggled index)
                , buttonUi
                    []
                    [ "⌫" |> Web.Dom.text ]
                    |> Web.Dom.futureMap (\() -> TodoRemoved index)
                , Web.Dom.element "span"
                    [ Web.Dom.style "padding" "0px 0px 0px 20px"
                    , if todo.completed then
                        [ Web.Dom.style "color" (Color.rgba 1 1 1 0.4 |> Color.toCssString)
                        , Web.Dom.style "text-decoration" "line-through"
                        ]
                            |> Web.Dom.modifierBatch

                      else
                        Web.Dom.modifierNone
                    ]
                    [ todo.content |> Web.Dom.text ]
                ]

        visibleTodos : List Todo
        visibleTodos =
            case state.visibilityFilter of
                AllVisible ->
                    state.todos

                OnlyTodoVisible ->
                    state.todos |> List.filter (\todo -> not todo.completed)

                OnlyCompletedVisible ->
                    state.todos |> List.filter .completed
    in
    Web.Dom.element "div"
        [ Web.Dom.style "padding" "11px 11px 0px 0px" ]
        (visibleTodos
            |> List.indexedMap (\i todo -> ( i, todo ) |> toListItem)
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
         , Web.Dom.style "border-bottom" ("2px solid " ++ (Color.rgba 1 1 1 0.2 |> Color.toCssString))
         , Web.Dom.style "border-radius" "20px"
         , Web.Dom.style "color" "#FFFFFF"
         , Web.Dom.style "padding" "4px 13px"
         , Web.Dom.style "margin" "0px 0px"
         , Web.Dom.style "text-align" "center"
         , Web.Dom.style "display" "inline-block"
         , Web.Dom.style "font-size" "0.9em"
         , Web.Dom.style "font-family" "inherit"
         ]
            ++ modifiers
        )
        subs


textInputUi :
    (Result Json.Decode.Error String -> future)
    -> String
    -> List (Web.Dom.Modifier future)
    -> Web.Dom.Node future
textInputUi toFuture inputValue modifiers =
    Web.Dom.element "input"
        ([ Web.Dom.attribute "type" "text"
         , Web.Dom.stringProperty "value" inputValue
         , Web.Dom.listenTo "input"
            |> Web.Dom.modifierFutureMap
                (Json.Decode.decodeValue
                    (Json.Decode.field "target" (Json.Decode.field "value" Json.Decode.string))
                )
            |> Web.Dom.modifierFutureMap toFuture
         , Web.Dom.style "font-size" "1em"
         , Web.Dom.style "background-color" "transparent"
         , Web.Dom.style "border-bottom" ("2px solid " ++ (Color.rgba 1 1 1 0.5 |> Color.toCssString))
         , Web.Dom.style "border-top" "none"
         , Web.Dom.style "border-left" "none"
         , Web.Dom.style "border-right" "none"
         , Web.Dom.style "color" "inherit"
         , Web.Dom.style "font-family" "inherit"
         ]
            ++ modifiers
        )
        []


visibilityFilterToString : VisibilityFilter -> String
visibilityFilterToString =
    \visibilityFilter ->
        case visibilityFilter of
            AllVisible ->
                "all"

            OnlyTodoVisible ->
                "only todo"

            OnlyCompletedVisible ->
                "only completed"
