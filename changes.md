# change log

# TODO

  - from elm, send which attributes, attributes namespaced and styles have been fully removed (instead of removing all before re-adding some) (elm side finished, only index.ts to go)

## 2.0.0

  - rename ...map to ...futureMap
      - `Web.Dom.map` name → `Web.Dom.futureMap`
      - `Web.Dom.modifierMap` name → `Web.Dom.modifierFutureMap`
      - `Web.interfaceMap` name → `Web.interfaceFutureMap`
      - thanks to feedback from [@a-teammate](https://github.com/a-teammate)
  - `module Web.Dom`:
      - `documentEventListen` move into `Web.Window` as `documentListenTo`
      - add `stringProperty`, `boolProperty`
      - add `scrollToShow`, `scrollPositionRequest`, `scrollToPosition`
  - `module Web.Navigation`
      - `load` argument change from `Url` to `String` for convenience
  - `module Web.Http`:
      - remove variant `Web.HttpExpectJson` in favor of explicit `Json.Decode.Error` handling in `Web.Http.expectJson`
      - remove `Web.HttpHeader` in favor of inlined types
      - add `bodyBytes`, `expectBytes`
  - `module Web.Window`:
      - `eventListen` name → `listenTo`
      - add `visibilityChangeListen`
      - add `preferredLanguagesRequest`, `preferredLanguagesChangeListen`
      - add `titleReplaceBy`, `authorSet`, `keywordsSet`, `descriptionSet`
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
  - add `module Web.Notification`
