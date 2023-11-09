## event with state

The idea: Instead of the update casing on the relevant state+event combination,
the event itself holds all the relevant state.

```elm
module Game exposing (main)

type State
    = MenuState Game.Menu.State
    | PlayingState Game.Playing.State

interface : State -> Interface State
interface state =
    case state of
        MenuState menuState ->
            App.Menu.interface menuState
        
        GameState gameState ->
            Game.Playing.interface gameState
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
- update is part of `interface` via `...Interface.on`
- giving the current state to that update function is unnecessary because `interface`
  makes sure that once an event arrives, it will hold the latest state.

I think it's cute. I tried implementing this exact thing in this project and it works :)
I will keep adding more capabilities to that example. Feedback and contributions welcome
as I'm very new to the js world.
