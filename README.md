> ⚠️ experimental

The Elm Architecture with its model, view, msg, update, sub, cmd, task
can be reduced down to state and interface, making it simpler, safer and more declarative.

cmds, tasks, subs and view are combined into one view/subscriptions-like `state -> Interface state`
which means

- actions (like changing the url) aren't tied to a specific event happening but to a specific state
- update is part of the interface (having an intermediate event type is optional but often useful)
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
    { initialState = 0
    , interface =
        \counter ->
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
                    , Web.Navigation.byUserListen |> Web.interfaceMap UserWentToUrl
                    ]
                        |> Web.interfaceBatch
                        |> Web.interfaceMap
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
                        |> Web.interfaceMap
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

Big thanks to [`andrewMacmurray/elm-concurrent-task`](https://dark.elm.dmy.fr/packages/andrewMacmurray/elm-concurrent-task/latest/) 🌱 for many of the js implementations in this package and to the [elm-radio episode about concurrent-task](https://elm-radio.com/episode/elm-concurrent-task) as the motivation to make a package out of it.

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
                        Web.interfaceNone
                    Err _ ->
                        Http.request { url = "...", decoder = Image.jsonDecoder }
                            |> Web.interfaceMap (\result -> { state | icon = result })
                , case state.content of
                    Ok _ ->
                        Web.interfaceNone
                    Err _ ->
                        Http.request { url = "...", decoder = Json.Decode.string }
                            |> Web.interfaceMap (\result -> { state | content = result })
                , ..error ui..
                ]
                    |> Web.interfaceBatch
}
```
which feels a bit more explicit, declarative and less wiring-heavy at least.

Note: This example is only supposed to show differences in architecture.
Unlike [`andrewMacmurray/elm-concurrent-task`](https://dark.elm.dmy.fr/packages/andrewMacmurray/elm-concurrent-task/latest/), `elm-state-interface` does not allow custom tasks/interfaces.
Instead, the goal of this package is to publish more browser APIs like webstorage instead of users doing the work only for their own projects. Since I'm a noob in the js world, feedback and contributions are super welcome ❀

## TODO before publish
- Convert Interface (Result Error ...) to without possible error
    - Web.Navigation.byUserListen
    - Web.Window.resizeListen
    - ModifierEventListener add preventDefault

## the 1.0.0 release

Should bring feature-parity with elm's exposed browser APIs ([tell me](https://github.com/lue-bird/elm-state-interface/issues/new) if I've missed some!).
For now, I also left out some more niche interfaces like [`Browser.Dom.setViewportOf`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Dom#setViewportOf) and [`WebGL.Texture.loadWith`](https://dark.elm.dmy.fr/packages/elm-explorations/webgl/latest/WebGL-Texture#loadWith).

## future

  - 🧩 add [example](https://github.com/lue-bird/elm-state-interface/tree/main/example) projects. Would you like to see something specific? Or maybe you're motivated to make one yourself 👀
  - 🔊 add audio interface similar to [`MartinSStewart/elm-audio`](https://dark.elm.dmy.fr/packages/MartinSStewart/elm-audio/latest/)
  - 🔋 The we API you miss the most
  - 🗃️ only with help: basic `node` APIs

If you have knowledge in any of these fields on the js side, have pointers or already 
a basic implementation using ports, [come by](https://github.com/lue-bird/elm-state-interface/discussions/new/choose)!

Note: The package is very much not designed to be easily extensible.
Adding stuff _will_ force a major version bump.
The module and interface structure is also not equipped to support multiple platforms.
