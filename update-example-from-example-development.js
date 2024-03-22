#!/usr/bin/env node

// inspired by https://github.com/jfmengels/node-elm-review/blob/2651aa4cc53726bbf9107a42f0756b849db3e1e7/new-package/maintenance/update-examples-from-preview.js

const path = require("path")
const fs = require("fs")

module.exports = copyExampleDevelopmentToExample

function copyExampleDevelopmentToExample() {
    const packageElmJson = JSON.parse(fs.readFileSync(path.resolve(__dirname, "elm.json")))
    const packageSrcPath = path.resolve(__dirname, "src")
    const exampleElmJsonPath = path.resolve(__dirname, "example", "elm.json")

    fs.cpSync(
        path.resolve(__dirname, "example-development", "src"),
        path.resolve(__dirname, "example", "src"),
        { recursive: true, filter: (source, _) => !source.endsWith(".js") }
    )
    fs.cpSync(
        path.resolve(__dirname, "example-development", "elm.json"),
        exampleElmJsonPath,
        { recursive: true }
    )

    const exampleElmJson = JSON.parse(fs.readFileSync(exampleElmJsonPath))

    // Remove the source directory pointing to the package's src/
    exampleElmJson['source-directories'] = exampleElmJson['source-directories'].filter(
        (sourceDirectory) =>
            path.resolve(__dirname, "example", sourceDirectory) !== packageSrcPath
    )
    exampleElmJson.dependencies.direct[packageElmJson.name] = packageElmJson.version
    moveFromDirectToIndirect(exampleElmJson, "miniBill/elm-fast-dict")
    moveFromDirectToIndirect(exampleElmJson, "miniBill/elm-rope")
    moveFromDirectToIndirect(exampleElmJson, "elm/bytes")
    fs.writeFileSync(exampleElmJsonPath, JSON.stringify(exampleElmJson, null, 4))
}
function moveFromDirectToIndirect(elmJson, dependencyName) {
    elmJson.dependencies.indirect[dependencyName] =
        elmJson.dependencies.direct[dependencyName]
    delete elmJson.dependencies.direct[dependencyName]
}