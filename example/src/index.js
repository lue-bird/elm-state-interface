import * as Web from "../../runner-compiled/index.js"
import { Elm } from "./Main.elm";

const elmApp = Elm.Main.init({ node: document.getElementById("app") })
Web.programStart({ ports: elmApp.ports, domElement: document.getElementById("app") })
