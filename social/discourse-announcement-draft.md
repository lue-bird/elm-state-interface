You can now [define an elm app like this](https://dark.elm.dmy.fr/packages/lue-bird/elm-state-interface/latest/):
```elm
{ initialState : YourState
, interface : YourState -> Interface YourState
}
```

State is equivalent to "model" and `Interface` is all incoming and outgoing effects,
with the ability to come back with a new state.

Here's a simple app that shows a number which gets incremented every second
```elm
{ initialState = 0
, interface =
    \state ->
        [ state |> String.fromInt |> Dom.text |> Dom.render
        , Time.periodicallyListen Duration.second
            |> interfaceFutureMap (\_ -> state + 1)
        ]
            |> interfaceBatch
}
```

A lot works the same as in TEA. A few changes make it declarative, safe and simple:

### simple and beginner friendly
  - one `Interface` instead of `Sub`, `Cmd`, `Task`, init command and `view` 
  - no `sandbox` vs `element` vs `document` vs `application`. Any app is `{ initialState, interface }` and complexity like an event type, title and url handling can be introduced gradually

### safe
Once an event ("msg") comes to your `update` in TEA, you don't have the latest state of the same kind it was fired in.
This can get quite boilerplate-y if you have lots of branches (like in a story game) or add shared events and all that.

in TEA
```elm
type Page
    = HomePage HomePage
    | SettingsPage SettingsPage

type Msg
    = HomePageMsg HomePageMsg
    | SettingsPageMsg SettingsPageMsg

update msg page =
    case page of
        HomePage homePage ->
            case msg of
                HomePageMsg homePageMsg ->
                    ...
                
                -- this should be impossible
                _ ->
                    page

        SettingsPage settingsPage ->
            case msg of
                SettingsPageMsg settingsPageMsg ->
                    ...
                
                -- this should be impossible
                _ ->
                    page
```
in state-interface
```elm
type Page
    = HomePage HomePage
    | SettingsPage SettingsPage

interface page =
    case page of
        HomePage homePage ->
            ..interfaces..
                |> interfaceFutureMap
                    (\homePageEvent ->
                        ..you can use homePage here..
                    )

        SettingsPage settingsPage ->
            ..interfaces..
                |> interfaceFutureMap
                    (\settingPageEvent ->
                        ..you can use settingsPage here..
                    )
```
or alternatively
```elm
type Page
    = HomePage HomePage
    | SettingsPage SettingsPage

type Event
    = HomePageEvent { state : HomePage, event : HomePageEvent }
    | SettingsPageEvent { state : SettingsPage, event : SettingsPageEvent }

interface page =
    (case page of
        HomePage homePage ->
            ..interfaces..
                |> interfaceFutureMap
                    (\homePageEvent ->
                        HomePageEvent { state = homePage, event = homePageEvent }
                    )

        SettingsPage settingsPage ->
            ..interfaces..
                |> interfaceFutureMap
                    (\settingsPageEvent ->
                        SettingsPageEvent { state = settingsPage, event = settingsPageEvent }
                    )
    )
        |> interfaceFutureMap
            (\event ->
                case event of
                    HomePageEvent homePage ->
                        ..homePage.state will be up to date..

                    SettingsPageEvent settingsPage ->
                        ..settingsPage.state will be up to date..
            )
```
It can seem odd that the state in the event that arrives contains the latest info, even though we seem to send an event with a state that will be outdated in the near future.
Interfaces that are fired in the past don't actually save their event handling.
Only the latest interface of the same kind gets that privilege!

### declarative
Often, there isn't a single event after which to initiate actions, requests, etc. on the outside. It's more a matter of what we already know

in TEA
```elm
update msg state =
    case msg of
        MenuGamepadStartClicked ->
            ( { state | mode = initialGameState Gamepad }
            , state.audio of
                Nothing ->
                    loadAudioCmd ... |> Cmd.map AudioLoaded
                
                Just _ ->
                    Cmd.none
            )
        
        MenuStartButtonClicked ->
            ( { state | mode = initialGameState Mouse }
            , state.audio of
                Nothing ->
                    loadAudioCmd ... |> Cmd.map AudioLoaded
                
                Just _ ->
                    Cmd.none
            )
        
        AudioLoaded loaded ->
            ( { state | audio = loaded |> Just }, Cmd.none )
```
same with effects like starting and stopping audio etc.

in state-interface, we can list those effects very similar to `subscriptions` or `view`
```elm
menuInterface state =
    [ state.audio of
        Nothing ->
            Audio.sourceLoad ...
                |> interfaceFutureMap AudioLoaded
                
        Just _ ->
            Cmd.none
    , ..user interface..
    ]
        |> interfaceBatch
        |> interfaceFutureMap
            (\menuEvent ->
                case menuEvent of
                    GamepadStartClicked ->
                        { state | mode = initialGameState Gamepad }
                    
                    StartButtonClicked ->
                        { state | mode = initialGameState Mouse }
                    
                    AudioLoaded loaded ->
                        { state | audio = loaded |> Just }
            )
```
[The state-interface documentation](https://dark.elm.dmy.fr/packages/lue-bird/elm-state-interface/latest/) has examples and explanations so you can develop an intuition.

### web APIs
Basically all `elm/` browser effects have equivalents in the [state-interface package](https://dark.elm.dmy.fr/packages/lue-bird/elm-state-interface/latest/).
And audio, localstorage, gamepads, websockets, geo location, notification and more are also included.
