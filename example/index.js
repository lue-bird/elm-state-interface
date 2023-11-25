import * as BrowserApp from "../runner-compiled/index.js"

const elmApp = window.Elm.Main.init({ node: document.getElementById("app") })
BrowserApp.start({ ports: elmApp.ports, domElement: document.getElementById("app") })
