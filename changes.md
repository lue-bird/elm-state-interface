# change log

## 2.0.0

  - rename ...map to ...futureMap
      - `Web.Dom.futureMap` name → `Web.Dom.futureMap`
      - `Web.Dom.modifierMap` name → `Web.Dom.modifierFutureMap`
      - `Web.interfaceMap` name → `Web.interfaceFutureMap`
      - thanks to feedback from [@a-teammate](https://github.com/a-teammate)
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
      - internally, split off request, listen and action interface types
      - internally, rename ...Receive to ...Future
  - add `module Web.Audio` and `module Web.Audio.Parameter`
  - add `module Web.Socket`
  - add `module Web.LocalStorage`
  - add `module Web.GeoLocation`
  - TODO mention "An Interface for" in every interface documentation
  - TODO replace GeoLocationListen by GeoLocationChangeListen
