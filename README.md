> ⚠️ experimental

The Elm Architecture with its model, view, msg, update, sub, cmd, task
can be reduced down to state and interface, making it simpler, safer and more declarative.

#### the ideas
- cmds, tasks, subs and view are combined into one view/subscriptions-like
  `state -> Interface state`.
  Now actions are not tied to a specific event happening but to a specific state
- Instead of the update casing on the relevant state+event combination,
  the event itself holds all the relevant state.
  Don't worry, even if the interface responds late, the state information will be up to date

#### the extras
- an event type only exists as an optional intermediate type (still highly recommended, though most of the time)
- update is part of the interface via [`BrowserApp.map`](https://package.elm-lang.org/packages/lue-bird/elm-state-interface/1.0.0/BrowserApp#map)

The classic counter example:

```elm
import BrowserApp
import BrowserApp.Dom as Ui
import Json.Encode

type alias State =
    Int

type Event
    = DecreaseClicked
    | IncreaseClicked

app : BrowserApp.Config State
app =
    { initialState = 0
    , interface =
        \counter ->
            Ui.element "div"
                []
                [ Ui.element "button"
                    [ Ui.listenTo "click" |> Ui.modifierMap (\_ -> IncreaseClicked) ]
                    [ "+" |> Ui.text ]
                , Ui.element "div"
                    []
                    [ counter |> String.fromInt |> Ui.text ]
                , Ui.element "button"
                    [ Ui.listenTo "click" |> Ui.modifierMap (\_ -> DecreaseClicked) ]
                    [ "-" |> Ui.text ]
                ]
                |> Ui.render
                |> BrowserApp.map
                    (\event ->
                        case event of
                            DecreaseClicked ->
                                counter - 1
                            
                            IncreaseClicked ->
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

Big thanks to [`andrewMacmurray/elm-concurrent-task`](https://dark.elm.dmy.fr/packages/andrewMacmurray/elm-concurrent-task/latest/) 🌱 for many of the js implementations in this package and to the [elm-radio episode about concurrent-task](https://elm-radio.com/episode/elm-concurrent-task) as _the_ motivation to make a package out of it.

> The complete example and more in [example/](https://github.com/lue-bird/elm-state-interface/tree/main/example)

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
                            |> BrowserApp.map (\result -> { state | icon = result })
                , case state.content of
                    Ok _ ->
                        BrowserApp.none
                    Err _ ->
                        Http.request { url = "...", decoder = Json.Decode.string }
                            |> BrowserApp.map (\result -> { state | content = result })
                , ..error ui..
                ]
                    |> BrowserApp.batch
}
```
which feels a bit more explicit, declarative and less wiring-heavy at least.

Note: This example is supposed to show differences in architecture.
This package is (currently) not a replacement
for [`andrewMacmurray/elm-concurrent-task`](https://dark.elm.dmy.fr/packages/andrewMacmurray/elm-concurrent-task/latest/) which allows custom tasks.
A goal is to publish more browser APIs like webstorage in this elm package instead of users doing the work only for their own projects. I'm a noob in the js world, so feedback and contributions are welcome ❀

## future

  - 🧩 add many [example](https://github.com/lue-bird/elm-state-interface/tree/main/example) projects. Would you like to see something specific? Or maybe you're motivated to make one yourself 👀
  - 🔊 add audio interface similar to [`MartinSStewart/elm-audio`](https://dark.elm.dmy.fr/packages/MartinSStewart/elm-audio/latest/)
  - 🔋 The we API you miss the most
  - 🗃️ only with help: basic `node` APIs

If you have knowledge in any of these fields on the js side, have pointers or already 
a basic implementation using ports, [come by](https://github.com/lue-bird/elm-state-interface/discussions/new/choose)!

Note: The package is very much not designed to be easily extensible.
Adding stuff _will_ force a major version bump.
The module and interface structure is also not equipped to support multiple platforms.
