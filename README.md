We'll explore an architecture similar to the traditional model-view-update
which is simpler, safer and more declarative.

Elm has concepts of model, view, msg, update, sub, cmd, task.
We'll reduce this to just state and interface!

First: Props to [`andrewMacmurray/elm-concurrent-task`](https://dark.elm.dmy.fr/packages/andrewMacmurray/elm-concurrent-task/latest/) for the js-related stuff in combination with [the elm-radio episode](https://elm-radio.com/episode/elm-concurrent-task) for the inspiration to pursue this.

## TODO before release

mirror elm commands & tasks listed in
  - https://github.com/andrewMacmurray/elm-concurrent-task/blob/1.0.0/runner/index.ts
  - https://github.com/lamdera/program-test/blob/b64f089d7ef846b58bda73dd1780819e7b5e5d82/src/Effect/Internal.elm#L80

## the ideas
- cmds, tasks, subs and view are combined into one big view/subscriptions-like function
  `state -> Interface ...`
- Instead of the update casing on the relevant state+event combination,
  the event itself holds all the relevant state.
  Don't worry, even if the interface responds late, the state information will be up to date.

## the extras
- an event only exists as an optional intermediate type (still highly recommended, though most of the time)
- update is part of `interface` via `BrowserApp.on`

All knobs turned to 11, we get "state-interface":

```elm
module Game exposing (main)

type State
    = MenuState Game.Menu.State
    | PlayingState Game.Playing.State

interface : State -> Interface State
interface state =
    case state of
        MenuState menuState ->
            Interface.on MenuState (App.Menu.interface menuState)
        
        GameState gameState ->
            Interface.on GameState (Game.Playing.interface gameState)
```
```elm
module Game.Menu exposing (interface, State)

type alias State =
    { selected : Selectable
    , settingMusic : MusicSetting
    }

type MusicSetting
    = MusicOff
    | MusicOn Fraction

type Selectable
    = Play
    | Settings

type Event
    = Selected Selectable State
    | NavigationArrowPressed NavigationArrow State

type NavigationArrow
    = NavigationArrowUp
    | NavigationArrowDown

interface : State -> Interface State
```
```elm
module Game.Menu exposing (interface, State)

type alias State =
    { selected : Selectable }

type Selectable
    = Play
    | Settings

interface : State -> Interface State
```

### required changes to TEA
- giving the current state to that update function is unnecessary because `interface`
  makes sure that once an event arrives, it will hold the latest state.

I think it's cute. I tried implementing this exact thing in this project and it works :)
I will keep adding more capabilities to that example. Feedback and contributions welcome
as I'm very new to the js world.

### alternative to tasks

Simplified examples:

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
                [ state.icon
                    |> Result.withDefault
                        (Http.request { url = "...", decoder = Image.jsonDecoder }
                            |> Interface.on (\result -> { state | icon = result })
                        )
                , state.content
                    |> Result.withDefault
                        (Http.request { url = "...", decoder = Json.Decode.string }
                            |> Interface.on (\result -> { state | content = result })
                        )
                , ..error ui..
                ]
                    |> List.filterMap identity
}
```
which feels more explicit, declarative and less wiring-heavy at least.

## setup
```bash
npm install @lue-bird/elm-state-interface
```
in js
```js
import * as BrowserApp from "@lue-bird/elm-state-interface"

const elmApp = Elm.Main.init({});
BrowserApp.start(elmApp.ports, document.getElementById("app")) // with your main element
```
(if the state-interface is embedded in a larger elm app,
the given argument should be the inner app's element.)

in elm
```elm
port module Main exposing (main)

import BrowserApp
import Json.Encode

main : Program () (BrowserApp.State ..State..) (BrowserApp.Event ..State..)
main =
    app |> BrowserApp.toProgram

app : BrowserApp.Config ..State..
app =
    { initialState = ...
    , interface = ...
    , ports = { fromJs = fromJs, toJs = toJs }
    }

port toJs : Json.Encode.Value -> Cmd event_
port fromJs : (Json.Encode.Value -> event) -> Sub event
```
