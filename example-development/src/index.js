import * as Web from "../../runner-compiled/index.js"
import { Elm } from "./../../example/src/Main.elm";

const elmApp = Elm.Main.init()
Web.programStart({ ports: elmApp.ports, domElement: document.getElementById("app") })
