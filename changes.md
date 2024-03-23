# change log

## 2.0.0

  - rename ...map to ...futureMap
      - `Web.Dom.map` name → `Web.Dom.futureMap`
      - `Web.Dom.modifierMap` name → `Web.Dom.modifierFutureMap`
      - `Web.interfaceMap` name → `Web.interfaceFutureMap`
      - thanks to feedback from [@a-teammate](https://github.com/a-teammate)
  - `module Web.Dom`:
      - `documentEventListen` move into `Web.Window` as `documentListenTo`
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
      - internal: split off request, listen and action interface types
      - internal: rename ...Receive to ...Future
      - internal: switch from `KeysSet` and all its dependencies to `miniBill/elm-fast-dict`
        and a simpler and more performant process of comparing elements
      - internal: allow actual simulation using `interfaceAssociateFutureState` and `interfaceDiffs`
  - add `module Web.Audio` and `module Web.Audio.Parameter`
  - add `module Web.Socket`
  - add `module Web.LocalStorage`
  - add `module Web.GeoLocation`
