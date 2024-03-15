# change log

## 2.0.0

  - remove `HttpExpectJson` in favor of explicit `Json.Decode.Error` handling in `Web.Http.expectJson`
  - ...map name → ...futureMap
      - `Web.Dom.futureMap` name → `Web.Dom.futureMap`
      - `Web.Dom.modifierMap` name → `Web.Dom.modifierFutureMap`
      - `Web.interfaceMap` name → `Web.interfaceFutureMap`
      - thanks to feedback from [@a-teammate](https://github.com/a-teammate)
  - add `module Web.Audio` and `module Web.Audio.Parameter`
  - add `module Web.Socket`
  - add `module Web.LocalStorage`
  - TODO get example/ up to speed with a setup that's as easy as possible to reproduce, preferably even without a bundler
