# change log

## 2.0.0

  - remove `HttpExpectJson` in favor of explicit `Json.Decode.Error` handling in `Web.Http.expectJson`
  - ...map name → ...stateMap
      - `Web.Dom.map` name → `Web.Dom.stateMap`
      - `Web.Dom.modifierMap` name → `Web.Dom.stateModifierMap`
      - `Web.interfaceMap` name → `Web.interfaceStateMap`
      - thanks to feedback from [@a-teammate](https://github.com/a-teammate)
  - add `module Web.Audio`
      - TODO add https://developer.mozilla.org/en-US/docs/Web/API/PannerNode cone angle (outer = inner), surround volume, orientation, position (potentially rolloff if there's no nice default)
      - TODO add https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode/type lowpass and highpass
      - TODO https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode given an AudioSource
  - TODO use all xss attack protections from virtual-dom
  - TODO: allow http byte body https://github.com/andrewMacmurray/elm-concurrent-task/pull/26/files
  - TODO: add a simple websocket API
      - https://dark.elm.dmy.fr/packages/kageurufu/elm-websockets/latest/Websockets
      - https://dark.elm.dmy.fr/packages/bburdette/websocket/latest/WebSocket
