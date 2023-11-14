## event with state

The idea: Instead of the update casing on the relevant state+event combination,
the event itself holds all the relevant state.

All knobs turned to 11, we get "state-interface", a simpler, safer take on TEA/model-view-update/MVU.

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
- cmds (and tasks) are part of `interface`
- subs are part of `interface`
- view is part of `interface`
- a msg is part of an event which now also contains the state and only exists as an intermediate type
- update is part of `interface` via `...Interface.on`
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
                ..your ui.. iconAndContent
            
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
                ..your ui.. { icon = icon, content = content }
            
            _ ->
                [ state.icon
                    |> Result.withDefault
                        (Http.request { url = "...", decoder = Image.jsonDecoder }
                            |> Interface.on (\content -> { state | content = Ok content })
                        )
                , state.content
                    |> Result.withDefault
                        (Http.request { url = "...", decoder = Json.Decode.string }
                            |> Interface.on (\content -> { state | content = Ok content })
                        )
                , ..error ui..
                ]
                    |> List.filterMap identity
}
```
which feels more explicit, declarative and less wiring-heavy at least.
