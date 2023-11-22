import { Elm } from "./src/Main.elm";
import * as BrowserApp from "../../runner";

const appElement = document.getElementById("app")
if (appElement) {
    const elmApp_ = Elm.Main.init({ node: appElement });
    BrowserApp.start(elmApp_.ports, appElement)
}
