# change log

#### 2.0.4

  - correct readme js setup example.
    Thanks [Krasnikau Andrei](https://github.com/krasnikov-andrew) for [suggesting a fix](https://github.com/lue-bird/elm-state-interface/pull/2)!

#### 2.0.3

  - internal: minor-ish performance improvements

#### js package 2.0.3

  - fix issue with new internal dom interface representation

#### 2.0.2

  - internal: improve performance quite a bit
  - internal: change representation of dom interface

#### js package 2.0.2

  - catch setting readonly DOM object property.
    Thanks to [an issue comment by Simon Lydell](https://github.com/elm/virtual-dom/issues/173#issuecomment-792027590)
  - update to new internal dom interface representation

#### js package 2.0.1

  - disallow creating string properties for innerHTML and outerHTML
  - add checks around changing only properties of the specified type

#### 2.0.1

  - readme discourse link correct

## 2.0.0

  - rename ...map to ...futureMap thanks to feedback from [@a-teammate](https://github.com/a-teammate)
      - `Web.Dom.map` name → `Web.Dom.futureMap`
      - `Web.Dom.modifierMap` name → `Web.Dom.modifierFutureMap`
      - `Web.interfaceMap` name → `Web.interfaceFutureMap`
  - `module Web.Dom`:
      - `documentEventListen` move into `Web.Window` as `documentListenTo`
      - add `stringProperty`, `boolProperty`
      - add `scrollToShow`, `scrollPositionRequest`, `scrollToPosition` thanks to feedback from [@a-teammate](https://github.com/a-teammate)
  - `module Web.Navigation`
      - `load` argument change from `Url` to `String` for convenience
  - `module Web.Http`:
      - remove variant `Web.HttpExpectJson` in favor of explicit `Json.Decode.Error` handling in `Web.Http.expectJson`
      - remove `Web.HttpHeader` in favor of inlined types
      - remove headers field from the argument to `get` and `post` in favor of `addHeaders`
      - add `addHeaders`
      - add `bodyBytes`, `expectBytes`
      - remove timeout argument fields in favor of `Time.onceAt`
  - `module Web.Window`:
      - `eventListen` name → `listenTo`
      - add `visibilityChangeListen`
      - add `preferredLanguagesRequest`, `preferredLanguagesChangeListen`
      - add `titleReplaceBy`, `authorSet`, `keywordsSet`, `descriptionSet`
  - `module Web.Time`:
      - add `onceAt`
  - `module Web`:
      - add `type alias Program` for results of `Web.program`
      - internal: add `interfacesSingleEdits` to allow simulation
      - internal: remove distinction between "...WithReceive" and "...WithoutReceive" as separate types
      - internal: switch from `KeysSet` and all its dependencies to `miniBill/elm-fast-dict` by a structured id
      - internal: simplify and speed up re-associating an interface, removal and dom diff
      - internal: remove "id" types i favor of directly saving the structured id string
  - add `module Web.Audio` and `module Web.Audio.Parameter`
  - add `module Web.Socket`
  - add `module Web.LocalStorage`
  - add `module Web.GeoLocation`
  - add `module Web.Notification` thanks to feedback from [@a-teammate](https://github.com/a-teammate)
  - make documentation more friendly to beginners thanks to feedback from [@a-teammate](https://github.com/a-teammate)
