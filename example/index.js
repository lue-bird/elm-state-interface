import * as Web from "../runner-compiled/index.js" // "@lue-bird/elm-state-interface"

const elmApp = window.Elm.Main.init({ node: document.getElementById("app") })
Web.programStart({ ports: elmApp.ports, domElement: document.getElementById("app") })
