### define an app in a simple, safe and declarative way

> 🌱 New to elm? Check out the [core language](https://guide.elm-lang.org/core_language), [install elm](https://guide.elm-lang.org/install/elm), set up an editor like [vs code](https://github.com/elm-tooling/elm-language-client-vscode?tab=readme-ov-file#install) and read about [types](https://guide.elm-lang.org/types/), [error handling](https://guide.elm-lang.org/error_handling/) and [webapp structure](https://guide.elm-lang.org/webapps/structure)

> If you know TEA, [get quick overview of the differences](#comparison-to-the-elm-architecture)

Here's a simple app that shows a number and a button that increments it

```elm
import Web
import Web.Dom

app =
    { initialState = 0
    , interface =
        \state ->
            Web.Dom.element "div"
                []
                [ state |> String.fromInt |> Web.Dom.text
                , Web.Dom.element "button"
                    [ Web.Dom.listenTo "click" ]
                    [ "+" |> Web.Dom.text ]
                    |> Web.Dom.futureMap (\_ -> state + 1)
                ]
                |> Web.Dom.render
    }
```

> To play around with the examples, set up a [playground](https://github.com/lue-bird/elm-state-interface-hello):
> ```bash
> git clone https://github.com/lue-bird/elm-state-interface-hello.git && cd elm-state-interface-hello && npm install && npx vite
> ```
> http://localhost:5173/ now shows your app. Open `src/App.elm` in your editor to paste in examples. The website updates automatically.

The "state" is everything your app knows internally. Here it's the counter number, starting at 0.

We build the interface to the outside world (html, audio, console logs, server communication, ...) based on our current state.
In our example, this function has the type
```elm
interface : Int -> Interface Int
```
`Interface Int` means that the interface can come back with an `Int` some time in the future,
in our case an incremented counter state.
The app will use this result as the new state.

We use [`Web.Dom.listedTo`](https://dark.elm.dmy.fr/packages/lue-bird/elm-state-interface/latest/Web-Dom#listenTo) to be notified when a user clicks the button.
Without [`Web.Dom.futureMap`](https://dark.elm.dmy.fr/packages/lue-bird/elm-state-interface/latest/Web-Dom#futureMap), our interface would have the type
```elm
Web.Dom.element "button" [ Web.Dom.listenTo "click" ] []
    |> Web.Dom.render
: Interface Json.Decode.Value
```
which means this interface will on click come back with an event as [json](https://dark.elm.dmy.fr/packages/elm/json/latest/).
Later, we'll see how to get information out of this kind of event.

Right now, we use [`Web.Dom.futureMap`](https://dark.elm.dmy.fr/packages/lue-bird/elm-state-interface/latest/Web-Dom#futureMap) to change the information the interface will send back to us in the future by just ignoring the event `\_ ->` and returning the incremented state.

You can change the value that comes back from an interface in many places, like
```elm
Web.Dom.element "button" [ Web.Dom.listenTo "click" ] []
    |> Web.Dom.render
    |> Web.interfaceFutureMap (\_ -> state + 1)
: Interface Int
```
or
```elm
Web.Dom.element "button"
    [ Web.Dom.listenTo "click"
        |> Web.Dom.modifierFutureMap (\_ -> state + 1)
    , Web.Dom.listenTo "dblclick"
        |> Web.Dom.modifierFutureMap (\_ -> state + 10)
    ]
    []
    |> Web.Dom.render
: Interface Int
```
Basically anywhere a type used for an interface has data it can come back with.

If we only perform actions without any events coming back, we get the type
```elm
"never change" |> Web.Dom.text |> Web.Dom.render
: Interface nothingEverComesBack

"never change" |> Web.Console.log
: Interface nothingEverComesBack
```
Because `nothingEverComesBack` is a variable, it will fit for any other interface type.


It's nice to give this number state you pass around a name

```elm
import Web
import Web.Dom

type State
    = Counter Int

app : { initialState : State, interface : State -> Web.Interface State }
app =
    { initialState = Counter 0
    , interface =
        \(Counter counter) ->
            Web.Dom.element "div"
                []
                [ counter |> String.fromInt |> Web.Dom.text
                , Web.Dom.element "button"
                    [ Web.Dom.listenTo "click"
                        |> Web.Dom.modifierFutureMap
                            (\_ -> Counter (counter + 1))
                    ]
                    [ "+" |> Web.Dom.text ]
                ]
                |> Web.Dom.render
    }
```
It allows us to annotate pieces of our app,
prevents mixing up numbers with your state values
and makes it easy to in the future add other possible states.

Try adding another button that decrements the shown number.

Done?
Here we go:

```elm
import Web
import Web.Dom

type State
    = Counter Int

app =
    { initialState = Counter 0
    , interface =
        \(Counter counter) ->
            Web.Dom.element "div"
                []
                [ Web.Dom.element "button"
                    [ Web.Dom.listenTo "click"
                        |> Web.Dom.modifierFutureMap
                            (\_ -> Counter (counter + 1))
                    ]
                    [ "+" |> Web.Dom.text ]
                , counter |> String.fromInt |> Web.Dom.text
                , Web.Dom.element "button"
                    [ Web.Dom.listenTo "click"
                        |> Web.Dom.modifierFutureMap
                            (\_ -> Counter (counter - 1))
                    ]
                    [ "-" |> Web.Dom.text ]
                ]
                |> Web.Dom.render
    }
```

it can be nice to separate behaviour from the interface by explicitly listing all the possible things we expect to see on the outside

```elm
import Web
import Web.Dom

type State
    = Counter Int

type Event
    = MinusClicked
    | PlusClicked

app =
    { initialState = WaitingForInitialUrl
    , interface =
        \(Counter counter) ->
            Web.Dom.element "div"
                []
                [ Web.Dom.element "button"
                    [ Web.Dom.listenTo "click" ]
                    [ "+" |> Web.Dom.text ]
                    |> Web.Dom.futureMap (\_ -> PlusClicked)
                , Web.Dom.element "div"
                    []
                    [ counter |> String.fromInt |> Web.Dom.text ]
                , Web.Dom.element "button"
                    [ Web.Dom.listenTo "click" ]
                    [ "-" |> Web.Dom.text ]
                    |> Web.Dom.futureMap (\_ -> MinusClicked)
                ]
                |> Web.Dom.render
                |> Web.interfaceFutureMap
                    (\event ->
                        case event of
                            MinusClicked ->
                                Counter (counter - 1)
                            
                            PlusClicked ->
                                Counter (counter + 1)
                    )
    }
```
Now you just need a quick look at `Event` to see what kind of user interactions the app will react to.

Soon we'll extend this example app with the ability to manage its url.
Before that, we have to know when anything from the interface actually triggers something on the outside.

In this example we have a text input field together with
a text showing whether the entered text is a palindrome or not.

```elm
import Web
import Web.Dom

type State
    = State { text : String, warnings : List String }

type Event
    = TextFieldContentChanged (Result Json.Decode.Error String)

app =
    { initialState = State { text = "", warnings = [] }
    , interface =
        \(State state) ->
            [ Web.Dom.element "div"
                []
                [ Web.Dom.element "input"
                    [ Web.Dom.attribute "value" state.text
                    , Web.Dom.listenTo "change"
                        |> Web.Dom.modifierFutureMap
                            (\eventJson ->
                                eventJson
                                    |> Json.Decode.decodeString
                                        (Json.Decode.field "target" (Json.Decode.field "value" Json.Decode.string))
                                    |> TextFieldContentChanged
                            )
                    ]
                    []
                , Web.Dom.text
                    (if state.text == (state.text |> String.reverse) then
                        "is a palindrome"
                    
                     else
                        "is not a palindrome"
                    )
                ]
                |> Web.Dom.render
            , state.warnings
                |> List.map (\warning -> Web.Console.warn warning)
                |> Web.interfaceBatch
            ]
                |> Web.interfaceBatch
                |> Web.interfaceFutureMap
                    (\event ->
                        case event of
                            TextFieldContentChanged (Ok newText) ->
                                State { state | text = newText }

                            TextFieldContentChanged (Err error) ->
                                State
                                    { state
                                        | warnings =
                                            state.warnings
                                                |> (::) (error |> Json.Decode.errorToString)
                                    }
                    )
    }
```
To learn about these "json decoder" things, you can read [the official guide](https://guide.elm-lang.org/effects/json). You can skip the first section with the app code.

Now, how does the `warnings` thing work?
> **an `Interface` for an action ≠ performing that action now**

```elm
interface =
    \_ ->
        Web.Console.log "Hello world"
```
when will it print to the console? All the time? Every time the state changes?

Here's where an `Interface` is different from a command and similar imperative code.
There are two triggers for execution of js code:
  - the updated `Interface` has an interface the old `Interface` didn't.
    E.g. we include logging each individual internal warning from the state in the `Interface`
    → log whenever a new warning is added
  - a previously existing interface is absent in the updated `Interface`.
    E.g. we don't include a HTTP GET request that we once had in the `Interface`
    → the request gets canceled if it's still active


Now then, here's the promised counter+url example
```elm
import Web
import Web.Dom
import AppUrl exposing (AppUrl) -- lydell/elm-app-url

type State
    = Counter Int
    | WaitingForInitialUrl

type CounterEvent
    = MinusClicked
    | PlusClicked
    | UserWentToUrl AppUrl

app =
    { initialState = WaitingForInitialUrl
    , interface = interface
    }

interface : State -> Web.Interface State
interface =
    \state ->
        case state of
            Counter counter ->
                [ Web.Dom.element "div"
                    []
                    [ Web.Dom.element "button"
                        [ Web.Dom.listenTo "click" ]
                        [ "+" |> Web.Dom.text ]
                        |> Web.Dom.futureMap (\_ -> PlusClicked)
                    , Web.Dom.element "div"
                        []
                        [ counter |> String.fromInt |> Web.Dom.text ]
                    , Web.Dom.element "button"
                        [ Web.Dom.listenTo "click" ]
                        [ "-" |> Web.Dom.text ]
                        |> Web.Dom.futureMap (\_ -> MinusClicked)
                    ]
                    |> Web.Dom.render
                , Web.Navigation.pushUrl
                    { path = []
                    , queryParameters = Dict.singleton "counter" [ counter |> String.fromInt ]
                    , fragment = Nothing
                    }
                , Web.Navigation.movementListen |> Web.interfaceFutureMap UserWentToUrl
                ]
                    |> Web.interfaceBatch
                    |> Web.interfaceFutureMap
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
                    |> Web.interfaceFutureMap
                        (\initialUrl ->
                            Counter (initialUrl |> counterUrlParse |> Maybe.withDefault 0)
                        )

counterUrlParse : AppUrl -> Maybe Int
counterUrlParse appUrl =
    appUrl.queryParameters
        |> Dict.get "counter"
        |> Maybe.andThen List.head
        |> Maybe.map String.fromInt
```
Since events like a click on the minus button can only happen if we're in the `Counter` state,
we have everything we need to update the state.

If you want to learn a bit more about app url parsing and building, visit [lydell/elm-app-url](https://dark.elm.dmy.fr/packages/lydell/elm-app-url/latest/)

And what's the deal with [`movementListen`](https://dark.elm.dmy.fr/packages/lue-bird/elm-state-interface/latest/Web-Navigation#movementListen) vs [`urlRequest`](https://dark.elm.dmy.fr/packages/lue-bird/elm-state-interface/latest/Web-Navigation#urlRequest)?
Don't both just give you the latest url?

> **an `Interface` that requests ≠ `Interface` that listens**

The Elm Architecture uses command/task types for one and subscription types for the other.
In state-interface, these 2 look identical:
```elm
Web.Window.sizeRequest : Interface { width : Int, height : Int }
Web.Window.resizeListen : Interface { width : Int, height : Int }
```
"-Listen" is equivalent to subscription in The Elm Architecture, "-Request" is roughly like command/task.
So trying to keep your window size state updated using
```elm
Web.Window.sizeRequest
    |> Web.interfaceFutureMap (\windowSize -> { state | windowSize = windowSize })
```
is not going to work as the request will only be executed once.

So the full solution to always get the current window size is
```elm
[ Web.Window.sizeRequest, Web.Window.resizeListen ]
    |> Web.interfaceBatch
    |> Web.interfaceFutureMap (\windowSize -> { state | windowSize = windowSize })
```
  - `sizeRequest` will send you the initial window size first, then never again
  - `resizeListen` sends you all the changes to the size
    (for as long as you have it in your interface)

Why can't we do the same in the counter + url example above?
```elm
[ Navigation.urlRequest, Web.Navigation.movementListen ]
    |> Web.interfaceBatch
    |> Web.interfaceFutureMap UserWentToUrl
```
In combination with editing the url programmatically
you have to keep one thing in mind:
It could happen that you push a new url before the requested initial url is sent to you
in which case you'll receive the url pushed by you.

Whenever **order of actions** is important, let your **state** represent that!


## what we need to actually run it as an elm program

For a minimal working setup, the [playground](https://github.com/lue-bird/elm-state-interface-hello) has everything you need.
For a "showcase example" with a minigame etc, see [example/](https://github.com/lue-bird/elm-state-interface/tree/main/example). You'll see that the basic setup hasn't changed.

In case you want to create your own setup instead:

```elm
port module Main exposing (main)
```
```elm
import Web
import Json.Encode -- elm/json

main : Web.Program ..your state type..
main =
    Web.program
        { initialState = ..your initial state..
        , interface = ..your interface..
        , ports = { fromJs = fromJs, toJs = toJs }
        }

port toJs : Json.Encode.Value -> Cmd event_
port fromJs : (Json.Encode.Value -> event) -> Sub event
```

These "ports" are the connection points to the actual implementations of the interfaces.
To set them up:

```bash
npm install @lue-bird/elm-state-interface
```
in js
```javascript
import * as Web from "@lue-bird/elm-state-interface";
// import your Main.elm. Name and path depend on bundler+plugin

const elmApp = Main.init();
Web.programStart({
    elmPorts : elmApp.ports,
    domElement : document.getElementById("your-app-element-id")
});
```

If you're not familiar with The Elm Architecture, skip to ["future"](#future)

## comparison to The Elm Architecture

The Elm Architecture with its model, view, msg, update, sub, cmd, task
can be reduced down to state and interface, making it simpler, safer and more declarative.

cmds, tasks, subs and view are combined into one view/subscriptions-like `state -> Interface state`
which means

  - actions (like changing the url) aren't tied to a specific event happening but to a specific state
  - update is part of the interface and having an intermediate event type is optional (but often useful)
  - when updating based on an event, there's no need to case on the relevant state. (Either use the state from the `case` in the interface in an inner update, or safely include the state in the event)

[jump back up](#define-an-app-in-a-simple-safe-and-declarative-way)

## comparison to tasks

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
                            |> Web.interfaceFutureMap (\result -> { state | icon = result })
                , case state.content of
                    Ok _ ->
                        Web.interfaceNone
                    Err _ ->
                        Http.request { url = "...", decoder = Json.Decode.string }
                            |> Web.interfaceFutureMap (\result -> { state | content = result })
                , ..error ui..
                ]
                    |> Web.interfaceBatch
}
```
which feels a bit more explicit, declarative and less wiring-heavy at least.

Note: This example is only supposed to show differences in architecture.
Unlike [`andrewMacmurray/elm-concurrent-task`](https://dark.elm.dmy.fr/packages/andrewMacmurray/elm-concurrent-task/latest/), `elm-state-interface` does not allow custom tasks/interfaces.
Instead, the goal of this package is to publish more browser APIs like webstorage instead of users doing the work only for their own projects. Since I'm a noob in the js world, feedback and contributions are super welcome ❀

## present

There should be feature-parity with elm's exposed browser APIs ([tell me](https://github.com/lue-bird/elm-state-interface/issues/new) if I've missed some!) plus a couple of APIs that elm's exposed browser APIs don't offer, including websockets, localstorage, audio, clipboard.

For now, some more niche interfaces like [`Browser.Dom.setViewportOf`](https://dark.elm.dmy.fr/packages/elm/browser/latest/Browser-Dom#setViewportOf) and [`WebGL.Texture.loadWith`](https://dark.elm.dmy.fr/packages/elm-explorations/webgl/latest/WebGL-Texture#loadWith) are left out.

## future

  - 🧩 add more [example projects](https://github.com/lue-bird/elm-state-interface/tree/main/example). Would you like to see something specific? Or maybe you're motivated to make one yourself 👀
  - 🔋 The web API you miss the most. Maybe [notifications](https://developer.mozilla.org/en-US/docs/Web/API/Notifications_API), [gamepads](https://developer.mozilla.org/en-US/docs/Web/API/Gamepad_API) (which currently isn't provided because there's no event API for user input), [MIDI](https://developer.mozilla.org/en-US/docs/Web/API/Web_MIDI_API), [speech](https://developer.mozilla.org/en-US/docs/Web/API/Web_Speech_API) or [sensors](https://developer.mozilla.org/en-US/docs/Web/API/Sensor_APIs)?
  - 🗃️ only with help: basic `node` APIs

If you have knowledge in any of these fields on the js side, have pointers or already 
a basic implementation using ports, [come by](https://github.com/lue-bird/elm-state-interface/discussions/new/choose)!

Note: The package is very much not designed to be easily extensible.
Adding stuff _will_ force a major version bump.
The module and interface structure is also not equipped to support multiple platforms.

## thanks 🌱
  - [andrewMacmurray for `elm-concurrent-task`](https://dark.elm.dmy.fr/packages/andrewMacmurray/elm-concurrent-task/latest/) which was used as the base for many of the js implementations
  - [elm-radio hosts for the episode about concurrent-task](https://elm-radio.com/episode/elm-concurrent-task) which motivated me to make a package out of it
  - [@a-teammate](https://github.com/a-teammate) for lots of valuable feedback 💚
  - [MartinSStewart for `elm-audio`](https://dark.elm.dmy.fr/packages/MartinSStewart/elm-audio/latest/) inspired the audio API
  - [kageurufu for `elm-websockets`](https://dark.elm.dmy.fr/packages/kageurufu/elm-websockets/latest/) which inspired me to also provide a websocket API
