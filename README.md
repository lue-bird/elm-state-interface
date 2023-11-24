The Elm Architecture with its model, view, msg, update, sub, cmd, task
can be reduced down to state and interface, making it simpler, safer and more declarative.

First: Big thanks to [`andrewMacmurray/elm-concurrent-task`](https://dark.elm.dmy.fr/packages/andrewMacmurray/elm-concurrent-task/latest/) for many of the js implementations in this package and to the [elm-radio episode about concurrent-task](https://elm-radio.com/episode/elm-concurrent-task) which motivated me to make this package.

## the ideas
- cmds, tasks, subs and view are combined into one big view/subscriptions-like function
  `state -> Interface state`.
  This means that actions are not tied to a specific event happening but to a specific state
- Instead of the update casing on the relevant state+event combination,
  the event itself holds all the relevant state.
  Don't worry, even if the interface responds late, the state information will be up to date.

## the extras
- an event type only exists as an optional intermediate type (still highly recommended, though most of the time)
- update is part of `interface` via [`BrowserApp.on`](BrowserApp#on)

The classic counter example:

```elm
import BrowserApp
import BrowserApp.Dom as Ui
import Json.Encode

type alias State =
    Int

type Event
    = CounterDecreaseClicked
    | CounterIncreaseClicked

app : BrowserApp.Config State
app =
    { initialState = 0
    , interface =
        \counter ->
            Ui.element "div"
                |> Ui.elementAddSubs
                    [ Ui.element "button"
                        |> Ui.elementAddSubs [ "+" |> Ui.text ]
                        |> Ui.elementOnEvent "click"
                            (\_ -> CounterIncreaseClicked)
                        |> Ui.elementToNode
                    , Ui.element "div"
                        |> Ui.elementAddSubs
                            [ counter |> String.fromInt |> Ui.text ]
                        |> Ui.elementToNode
                    , Ui.element "button"
                        |> Ui.elementAddSubs [ "-" |> Ui.text ]
                        |> Ui.elementOnEvent "click"
                            (\_ -> CounterDecreaseClicked)
                        |> Ui.elementToNode
                    ]
                |> Ui.elementToNode
                |> Ui.render
                |> BrowserApp.on
                    (\event ->
                        case event of
                            CounterDecreaseClicked ->
                                counter - 1
                            
                            CounterIncreaseClicked ->
                                counter + 1
                    )
    , ports = { fromJs = fromJs, toJs = toJs }
    }

main : Program () (BrowserApp.State State) (BrowserApp.Event State)
main =
    app |> BrowserApp.toProgram

port toJs : Json.Encode.Value -> Cmd event_
port fromJs : (Json.Encode.Value -> event) -> Sub event
```

## setup
```bash
npm install @lue-bird/elm-state-interface
```
in js
```js
import * as BrowserApp from "@lue-bird/elm-state-interface"

const elmApp = Elm.Main.init({});
BrowserApp.start({
    elmPorts : elmApp.ports,
    domElement : document.getElementById("your-app-element")
})
```

## state-interface as an alternative to tasks

Simplified examples.

with [`andrewMacmurray/elm-concurrent-task`](https://dark.elm.dmy.fr/packages/andrewMacmurray/elm-concurrent-task/latest/):
```elm
type alias State =
    Result
        Http.NonSuccessStatus
        { icon : Image
        , content : String
        }

type Event  
    = IconAndContentArrived (Result Http.Error { icon : Image, content : String })

{ init =
    \() ->
        ( Err Http.NotAsked
        , ConcurrentTask.succeed (\icon content -> { icon = icon, content = content })
            |> ConcurrentTask.andMap
                (Http.request { url = "...", decoder = Image.jsonDecoder })
            |> ConcurrentTask.andMap
                (Http.request { url = "...", decoder = Json.Decode.string })
            |> ConcurrentTask.attempt { onComplete = IconAndContentArrived }
        )
, update =
    \event state ->
        case event of
            IconAndContentArrived iconAndContent ->
                ( iconAndContent |> Result.mapError Http.Error
                , Cmd.none
                )
, view =
    \state ->
        case state of
            Ok iconAndContent ->
                ..your ui using iconAndContent..
            
            Err ... ->
                ..error ui..
, subscriptions = ...
}
```
with state-interface:
```elm
type alias State =
    { icon : Result Http.NonSuccessStatus Image
    , content : Result Http.NonSuccessStatus String
    }

{ initialState = { icon = Err Http.NotAsked, content = Err Http.NotAsked }
, interface =
    \state ->
        case ( state.icon, state.content ) of
            ( Ok icon, Ok content ) ->
                ..your ui using icon and content..
            
            _ ->
                [ case state.icon of
                    Ok _ ->
                        BrowserApp.none
                    Err _ ->
                        Http.request { url = "...", decoder = Image.jsonDecoder }
                            |> Interface.on (\result -> { state | icon = result })
                , case state.content of
                    Ok _ ->
                        BrowserApp.none
                    Err _ ->
                        Http.request { url = "...", decoder = Json.Decode.string }
                            |> Interface.on (\result -> { state | content = result })
                , ..error ui..
                ]
                    |> BrowserApp.batch
}
```
which feels a bit more explicit, declarative and less wiring-heavy at least.

Note: This example is supposed to show differences in architecture.
This package is (currently) not a replacement
for [`andrewMacmurray/elm-concurrent-task`](https://dark.elm.dmy.fr/packages/andrewMacmurray/elm-concurrent-task/latest/) which allows custom tasks.
A goal is to publish those APIs in this elm package instead of users doing the work only for their own projects. I'm a noob in the js world, so feedback and contributions are welcome ❀
