> ⚠️ experimental

The Elm Architecture with its model, view, msg, update, sub, cmd, task
can be reduced down to state and interface, making it simpler, safer and more declarative.

cmds, tasks, subs and view are combined into one view/subscriptions-like `state -> Interface state`
which means

- actions (like changing the url) aren't tied to a specific event happening but to a specific state
- update is part of the interface and having an intermediate event type is optional (but often useful)
- when updating based on an event, there's no need to case on the relevant state. (Either use the state from the `case` in the interface in an inner update, or safely include the state in the event)

The classic counter example including managing the url:

```elm
import Web
import Web.Dom
import AppUrl exposing (AppUrl) -- lydell/elm-app-url
import Json.Encode -- elm/json

type State
    = Counter Int
    | WaitingForInitialUrl

type CounterEvent
    = MinusClicked
    | PlusClicked
    | UserWentToUrl AppUrl

programConfig : Web.ProgramConfig State
programConfig =
    { initialState = WaitingForInitialUrl
    , interface =
        \state ->
            case state of
                Counter counter ->
                    [ Web.Dom.element "div"
                        []
                        [ Web.Dom.element "button"
                            [ Web.Dom.listenTo "click" ]
                            [ "+" |> Web.Dom.text ]
                            |> Web.Dom.map (\_ -> PlusClicked)
                        , Web.Dom.element "div"
                            []
                            [ counter |> String.fromInt |> Web.Dom.text ]
                        , Web.Dom.element "button"
                            [ Web.Dom.listenTo "click" ]
                            [ "-" |> Web.Dom.text ]
                            |> Web.Dom.map (\_ -> MinusClicked)
                        ]
                        |> Web.Dom.render
                    , Web.Navigation.pushUrl
                        { path = []
                        , queryParameters = Dict.singleton "counter" [ counter |> String.fromInt ]
                        , fragment = Nothing
                        }
                    , Web.Navigation.movementListen |> Web.interfaceStateMap UserWentToUrl
                    ]
                        |> Web.interfaceBatch
                        |> Web.interfaceStateMap
                            (\event ->
                                case event of
                                    MinusClicked ->
                                        Counter (counter - 1)
                                    
                                    PlusClicked ->
                                        Counter (counter + 1)
                                    
                                    UserWentToUrl newUrl ->
                                        Counter (newUrl |> counterUrlParse |> Maybe.withDefault counter)
                            )
                
                WaitingForInitialUrl ->
                    Web.Navigation.urlRequest
                        |> Web.interfaceStateMap
                            (\initialUrl ->
                                Counter (initialUrl |> counterUrlParse |> Maybe.withDefault 0)
                            )
    , ports = { fromJs = fromJs, toJs = toJs }
    }

counterUrlParse : AppUrl -> Maybe Int
counterUrlParse appUrl =
    appUrl.queryParameters
        |> Dict.get "counter"
        |> Maybe.andThen List.head
        |> Maybe.map String.fromInt

main : Program () (Web.ProgramState State) (Web.ProgramEvent State)
main =
    Web.program programConfig

port toJs : Json.Encode.Value -> Cmd event_
port fromJs : (Json.Encode.Value -> event) -> Sub event
```

## setup
```bash
npm install @lue-bird/elm-state-interface
```
in js
```js
import * as Web from "@lue-bird/elm-state-interface"

const elmApp = Elm.Main.init({});
Web.programStart({
    elmPorts : elmApp.ports,
    domElement : document.getElementById("your-app-element")
})
```

> The complete example and more in [example/](https://github.com/lue-bird/elm-state-interface/tree/main/example)

## `Interface` for an action ≠ `Cmd`
```elm
Web.Console.log "Hello world"
```
when will it print to the console? All the time? Every time the state changes?

Here's where an `Interface` is different from a `Cmd` and similar imperative code.
There are two triggers for execution of js code:
  - the updated `Interface` has an interface the old `Interface` didn't.
    E.g. we include logging each individual internal warning from the state in the `Interface`
    → log whenever a new warning is added
  - a previously existing interface is absent in the updated `Interface`.
    E.g. we don't include a HTTP GET request that we once had in the `Interface`
    → the request gets canceled if it's still active

## `Interface` that requests ≠ `Interface` that listens
Elm uses `Cmd`/`Task` types for one and `Sub` types for the other.
In state-interface land, these 2 look identical:
```elm
Web.Window.sizeRequest : Interface { width : Int, height : Int }
Web.Window.resizeListen : Interface { width : Int, height : Int }
```
"-Listen" is equivalent to elms `Sub`, "-Request" is roughly like `Cmd`/`Task`.
So, trying to use `sizeRequest` to keep your window size state updated is not going to work.
```elm
Web.Window.sizeRequest
    |> Web.interfaceStateMap (\windowSize -> { state | windowSize = windowSize })
```
As discussed in the previous section, the request will only be executed once.

So the full solution to always get the current window size is
```elm
[ Web.Window.sizeRequest, Web.Window.resizeListen ]
    |> Web.interfaceBatch
    |> Web.interfaceStateMap (\windowSize -> { state | windowSize = windowSize })
```
  - `sizeRequest` will send you the initial window size first, then never again
  - `resizeListen` sends you all the changes to the size
    (for as long as you have it in your interface)

Why can't we do the same in the counter + url example above?
```elm
[ Navigation.urlRequest, Web.Navigation.movementListen ]
    |> Web.interfaceBatch
    |> Web.interfaceStateMap UserWentToUrl
```
In combination with editing the url programmatically
you have to keep one thing in mind:
It could happen that you push a new url before the requested initial url is sent to you
in which case you'll receive the url pushed by you.

Whenever **order of actions** is important, let your **state** represent that!

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
                        Web.interfaceNone
                    Err _ ->
                        Http.request { url = "...", decoder = Image.jsonDecoder }
                            |> Web.interfaceStateMap (\result -> { state | icon = result })
                , case state.content of
                    Ok _ ->
                        Web.interfaceNone
                    Err _ ->
                        Http.request { url = "...", decoder = Json.Decode.string }
                            |> Web.interfaceStateMap (\result -> { state | content = result })
                , ..error ui..
                ]
                    |> Web.interfaceBatch
}
```
which feels a bit more explicit, declarative and less wiring-heavy at least.

Note: This example is only supposed to show differences in architecture.
Unlike [`andrewMacmurray/elm-concurrent-task`](https://dark.elm.dmy.fr/packages/andrewMacmurray/elm-concurrent-task/latest/), `elm-state-interface` does not allow custom tasks/interfaces.
Instead, the goal of this package is to publish more browser APIs like webstorage instead of users doing the work only for their own projects. Since I'm a noob in the js world, feedback and contributions are super welcome ❀

## the 1.0.0 release

Should bring feature-parity with elm's exposed browser APIs ([tell me](https://github.com/lue-bird/elm-state-interface/issues/new) if I've missed some!).
For now, I also left out some more niche interfaces like [`Browser.Dom.setViewportOf`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Dom#setViewportOf) and [`WebGL.Texture.loadWith`](https://dark.elm.dmy.fr/packages/elm-explorations/webgl/latest/WebGL-Texture#loadWith).

## future

  - 🧩 add [example](https://github.com/lue-bird/elm-state-interface/tree/main/example) projects. Would you like to see something specific? Or maybe you're motivated to make one yourself 👀
  - 🔊 add audio interface similar to [`MartinSStewart/elm-audio`](https://dark.elm.dmy.fr/packages/MartinSStewart/elm-audio/latest/)
  - 🔋 The web API you miss the most. Maybe [notifications](https://developer.mozilla.org/en-US/docs/Web/API/Notifications_API), [geolocation](https://developer.mozilla.org/en-US/docs/Web/API/Geolocation_API), [MIDI](https://developer.mozilla.org/en-US/docs/Web/API/Web_MIDI_API), [speech](https://developer.mozilla.org/en-US/docs/Web/API/Web_Speech_API) or [sensors](https://developer.mozilla.org/en-US/docs/Web/API/Sensor_APIs)?
  - 🗃️ only with help: basic `node` APIs

If you have knowledge in any of these fields on the js side, have pointers or already 
a basic implementation using ports, [come by](https://github.com/lue-bird/elm-state-interface/discussions/new/choose)!

Note: The package is very much not designed to be easily extensible.
Adding stuff _will_ force a major version bump.
The module and interface structure is also not equipped to support multiple platforms.

## thanks 🌱
  - [`andrewMacmurray/elm-concurrent-task`](https://dark.elm.dmy.fr/packages/andrewMacmurray/elm-concurrent-task/latest/) was used as a base for many of the js implementations
  - [elm-radio episode about concurrent-task](https://elm-radio.com/episode/elm-concurrent-task) gave me the motivation to make a package out of it
  - [`kageurufu/elm-websockets`](https://dark.elm.dmy.fr/packages/kageurufu/elm-websockets/latest/) was used as a base for the websocket js implementation
