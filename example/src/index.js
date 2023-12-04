import * as Web from "@lue-bird/elm-state-interface" // "../../runner-compiled/index.js"
import { Elm } from "./Main.elm";

const elmApp = Elm.Main.init({ node: document.getElementById("app") })
Web.programStart({ ports: elmApp.ports, domElement: document.getElementById("app") })
