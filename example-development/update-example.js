#!/usr/bin/env node

const path = require("path")
const fs = require("fs")
// inspired by https://github.com/jfmengels/node-elm-review/blob/2651aa4cc53726bbf9107a42f0756b849db3e1e7/new-package/maintenance/update-examples-from-preview.js

fs.readdir(
    path.resolve(__dirname),
    (_errors, files) => files.forEach(sub => {
        if (sub !== "README.md" && sub !== "update-example.js") {
            copyExampleDevelopmentToExample(sub)
        }
    })
)

function copyExampleDevelopmentToExample(sub) {
    const packageElmJson = JSON.parse(fs.readFileSync(path.resolve(__dirname, "..", "elm.json")))
    const packageSrcPath = path.resolve(__dirname, "..", "src")
    const exampleElmJsonPath = path.resolve(__dirname, "..", "example", sub, "elm.json")

    fs.cpSync(
        path.resolve(__dirname, sub, "src"),
        path.resolve(__dirname, "..", "example", sub, "src"),
        { recursive: true, filter: (source, _) => !source.endsWith(".js") }
    )
    fs.cpSync(
        path.resolve(__dirname, sub, "elm.json"),
        exampleElmJsonPath
    )
    fs.cpSync(
        path.resolve(__dirname, sub, "vite.config.js"),
        path.resolve(__dirname, "..", "example", sub, "vite.config.js")
    )

    const exampleElmJson = JSON.parse(fs.readFileSync(exampleElmJsonPath))

    // Remove the source directory pointing to the package's src/
    exampleElmJson['source-directories'] = exampleElmJson['source-directories'].filter(
        (sourceDirectory) =>
            path.resolve(__dirname, "..", "example", sub, sourceDirectory) !== packageSrcPath
    )
    exampleElmJson.dependencies.direct[packageElmJson.name] = packageElmJson.version
    moveFromDirectToIndirect(exampleElmJson, "miniBill/elm-fast-dict")
    moveFromDirectToIndirect(exampleElmJson, "miniBill/elm-rope")
    moveFromDirectToIndirect(exampleElmJson, "elm/bytes")
    fs.writeFileSync(exampleElmJsonPath, JSON.stringify(exampleElmJson, null, 4))

    fs.writeFileSync(
        path.resolve(__dirname, "..", "example", sub, "src", "index.js"),
        `import * as Web from "@lue-bird/elm-state-interface"
import Main from "./Main.elm"

const elmApp = Main.init()
Web.programStart({ ports: elmApp.ports, domElement: document.getElementById("app") })
`
    )

    fs.writeFileSync(
        path.resolve(__dirname, "..", "example", sub, "package.json"),
        `{
    "type": "module",
    "main": "index.js",
    "dependencies": {
        "@lue-bird/elm-state-interface": "^2.0.0",
        "vite": "^5.1.2",
        "vite-plugin-elm-watch": "^1.3.2"
    }
}`
    )
}
function moveFromDirectToIndirect(elmJson, dependencyName) {
    elmJson.dependencies.indirect[dependencyName] =
        elmJson.dependencies.direct[dependencyName]
    delete elmJson.dependencies.direct[dependencyName]
}