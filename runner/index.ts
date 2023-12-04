export interface ElmPorts {
    toJs: {
        subscribe: (callback: (fromElm: any) => void) => void
    }
    fromJs: { send: (toElm: any) => void }
}

export function programStart(appConfig: { ports: ElmPorts, domElement: HTMLElement }) {
    const interfaceWithoutSendToElmImplementations: { [key: string]: (config: any) => void } = {
        "addConsoleLog": (config) => {
            console.log(config)
        },
        "addConsoleWarn": (config) => {
            console.warn(config)
        },
        "addConsoleError": (config) => {
            console.error(config)
        },
        "removeTimePeriodicallyListen": removeTimePeriodicallyListen,
        "removeDom": (_config) => {
            appConfig.domElement.replaceChildren()
        },
        "removeHttpRequest": (config) => {
            const maybeAbortController = httpRequestAbortControllers[config]
            if (maybeAbortController) {
                maybeAbortController.abort()
            }
        },
        "removeWindowEventListen": windowEventListenRemove,
        "removeAnimationFrameListen": (_config) => {
            removeAnimationFrameListen()
        },
        "removeDocumentEventListen": documentEventListenRemove,
        "addNavigationGo": (config) => { go(config) },
        "addNavigationReplaceUrl": (config) => { replaceUrl(config) },
        "addNavigationPushUrl": (config) => { pushUrl(config) },
        "addNavigationLoad": (config) => { load(config) },
        "addNavigationReload": (_config) => { reload() },
        "addFileDownloadBytes": (config) => { fileDownloadBytes(config) }
    }
    const interfaceWithSendToElmImplementations: { [key: string]: (config: any, sendToElm: (v: any) => void) => void } = {
        "addTimePosixRequest": (_config, sendToElm) => {
            sendToElm(Date.now())
        },
        "addTimezoneOffsetRequest": (_config, sendToElm) => {
            // Equivalent Elm Kernel code: https://github.com/elm/time/blob/1.0.0/src/Elm/Kernel/Time.js#L38-L52
            sendToElm(new Date().getTimezoneOffset())
        },
        "addTimezoneNameRequest": (_config, sendToElm) => {
            sendToElm(getTimezoneName())
        },
        "addTimePeriodicallyListen": (config, sendToElm) => {
            addTimePeriodicallyListen(config, sendToElm)
        },
        "addRandomUnsignedInt32sRequest": (config, sendToElm) => {
            sendToElm(crypto.getRandomValues(new Uint32Array(config)))
        },
        "replaceDomNode": (config, sendToElm) => {
            renderDomNode(config.path, config.domNode, sendToElm)
        },
        "addHttpRequest": (config, sendToElm) => {
            const abortController = new AbortController()
            httpRequestAbortControllers[config] = abortController
            httpFetch(config, abortController).then(response => { sendToElm(response) })
        },
        "addWindowSizeRequest": (_config, sendToElm) => {
            sendToElm({ width: window.innerWidth, height: window.innerHeight })
        },
        "addWindowEventListen": windowEventListenAdd,
        "addWindowAnimationFrameListen": (_config, sendToElm) => {
            addAnimationFrameListen(sendToElm)
        },
        "addNavigationUrlRequest": (_config, sendToElm) => {
            sendToElm(window.location.href)
        },
        "addDocumentEventListen": documentEventListenAdd,
    }


    appConfig.ports.toJs.subscribe(function (fromElm) {
        // console.log("elm → js: ", fromElm)
        function sendToElm(eventData: void) {
            const toElm = { diff: fromElm, eventData: eventData }
            appConfig.ports.fromJs.send(toElm)
            // console.log("js → elm: ", toElm)
        }
        const diff: [string, unknown] | undefined = Object.entries(fromElm)[0]
        if (diff) {
            const [diffKind, diffConfig] = diff
            const maybeAssociatedAddOrReplaceFunction = interfaceWithSendToElmImplementations[diffKind]
            if (maybeAssociatedAddOrReplaceFunction) {
                maybeAssociatedAddOrReplaceFunction(diffConfig, sendToElm)
            } else {
                const associatedRemoveFunction = interfaceWithoutSendToElmImplementations[diffKind]
                if (associatedRemoveFunction) {
                    associatedRemoveFunction(diffConfig)
                } else {
                    console.log("Unknown message kind " + diffKind + " from elm. Maybe you have a typo? Otherwise the associated js function might be missing.")
                }
            }
        } else {
            console.log("I the message {} from elm. I need a specific command as { actionToPerform : config }")
        }

    })

    function renderDomNode(path: number[], node: any, sendToElm: (v: any) => void) {
        const createdDomNode = createDomNode([], node, sendToElm)
        if (path.length === 0) {
            const parentDomNode = appConfig.domElement
            parentDomNode.replaceChildren() // remove all subs
            parentDomNode.appendChild(createdDomNode)
        } else {
            let parentDomNode: ChildNode | null = appConfig.domElement.firstChild
            if (parentDomNode) {
                path.slice(1, path.length).reverse().forEach(subIndex => {
                    const subNode = parentDomNode?.childNodes[subIndex]
                    if (subNode) {
                        parentDomNode = subNode
                    }
                })
                const oldDomNode = parentDomNode.childNodes[path[0] ?? 0]
                if (oldDomNode) {
                    parentDomNode.replaceChild(createdDomNode, oldDomNode)
                }
            }
        }
    }
}

const httpRequestAbortControllers: { [key: string]: AbortController } = {}

const timePeriodicallyListens: { [key: number]: number } = {}
function addTimePeriodicallyListen(intervalDuration: { milliSeconds: number }, sendToElm: (v: any) => any) {
    timePeriodicallyListens[intervalDuration.milliSeconds] =
        window.setInterval(
            () => { sendToElm(Date.now()) },
            intervalDuration.milliSeconds
        )
}
function removeTimePeriodicallyListen(intervalDuration: { milliSeconds: number }) {
    const maybeTimePeriodicallyListen = timePeriodicallyListens[intervalDuration.milliSeconds]
    if (maybeTimePeriodicallyListen) {
        window.clearInterval(maybeTimePeriodicallyListen)
    }
}

// Equivalent Elm Kernel code: https://github.com/elm/time/blob/1.0.0/src/Elm/Kernel/Time.js#L27-L35
function getTimezoneName(): string | number {
    try {
        return Intl.DateTimeFormat().resolvedOptions().timeZone
    } catch (e) {
        return new Date().getTimezoneOffset()
    }
}

function createDomNode(innerPath: number[], node: any, sendToElm: (v: any) => any): Element | Text {
    if (node?.text) {
        return document.createTextNode(node.text)
    } else { // if (node?.element)
        const createdDomElement: (Element & ElementCSSInlineStyle) =
            node.element?.namespace ?
                document.createElementNS(node.element.namespace, noScript(node.element.tag))
                :
                document.createElement(noScript(node.element.tag))

        for (let [attributeKey, attributeValue] of Object.entries(node.element.attributes)) {
            createdDomElement.setAttribute(attributeKey, attributeValue as string)
        }
        node.element.attributesNamespaced.forEach((attributeNamespaced: { namespace: string, key: string, value: string }) => {
            createdDomElement.setAttributeNS(attributeNamespaced.namespace, attributeNamespaced.key, attributeNamespaced.value)
        })
        for (let [styleKey, styleValue] of Object.entries(node.element.styles)) {
            createdDomElement.style.setProperty(styleKey, styleValue as string)
        }
        node.element.eventListens.forEach((eventListenName: string) => {
            createdDomElement.addEventListener(
                eventListenName,
                (triggeredEvent) => {
                    sendToElm({ innerPath: innerPath, name: eventListenName, event: triggeredEvent })
                }
            )
        })
        node.element.subs.forEach((sub: any, subIndex: number) => {
            createdDomElement.appendChild(
                createDomNode([subIndex].concat(innerPath), sub, sendToElm)
            )
        })
        return createdDomElement
    }
}

// copied and edited from https://github.com/elm/virtual-dom/blob/master/src/Elm/Kernel/VirtualDom.js
// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.
const RE_script = /^script$/i
function noScript(tag: string) {
    return RE_script.test(tag) ? 'p' : tag
}

function windowEventListenAdd(eventName: string, sendToElm: (v: any) => any) {
    (window as { [key: string]: any })["on" + eventName] = sendToElm
}
function windowEventListenRemove(eventName: string) {
    (window as { [key: string]: any })["on" + eventName] = null
}

function documentEventListenAdd(eventName: string, sendToElm: (v: any) => any) {
    (window as { [key: string]: any })["on" + eventName] = sendToElm
}
function documentEventListenRemove(eventName: string) {
    (window as { [key: string]: any })["on" + eventName] = null
}

function go(urlSteps: number) {
    history.go(urlSteps)
}
function pushUrl(appUrl: string) {
    history.pushState({ appUrl: appUrl }, "", window.location.origin + appUrl)
}
function replaceUrl(appUrl: string) {
    history.replaceState({ appUrl: appUrl }, "", window.location.origin + appUrl)
}

function reload() {
    document.location.reload()
}
function load(url: string) {
    try {
        window.location.href = url
    }
    catch (err) {
        // Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
        // Other browsers reload the page, so let's be consistent about that.
        reload()
    }
}

let runningAnimationFrameLoopId: number | undefined = undefined
function addAnimationFrameListen(sendToElm: (v: any) => any) {
    runningAnimationFrameLoopId =
        window.requestAnimationFrame(_timestamp => {
            if (runningAnimationFrameLoopId) {
                sendToElm(Date.now())
                addAnimationFrameListen(sendToElm)
            }
        })
}
function removeAnimationFrameListen() {
    if (runningAnimationFrameLoopId) {
        window.cancelAnimationFrame(runningAnimationFrameLoopId)
        runningAnimationFrameLoopId = undefined
    }
}

function fileDownloadBytes(config: { mimeType: string, name: string, content: number[] }) {
    const temporaryAnchorDomElement: HTMLAnchorElement = window.document.createElement('a')
    const blob = new Blob(
        [new Uint8Array(config.content)],
        { type: config.mimeType }
    )
    const objectUrl = URL.createObjectURL(blob)
    temporaryAnchorDomElement.href = objectUrl
    temporaryAnchorDomElement.download = config.name
    const event = new MouseEvent('click', {
        view: window,
        bubbles: true,
        cancelable: true
    })
    document.body.appendChild(temporaryAnchorDomElement)
    temporaryAnchorDomElement.dispatchEvent(event)
    document.body.removeChild(temporaryAnchorDomElement)
    URL.revokeObjectURL(objectUrl)
}

interface HttpRequest {
    url: string
    method: string
    headers: [name: string, value: string][]
    expect: Expect
    timeout: number | null
    body: string | null
}
type Expect = "STRING" | "JSON" | "WHATEVER"

type HttpResponse = { ok: ResponseSuccess } | { err: any }
interface ResponseSuccess {
    body: any | string | null
    url: string
    headers: { [header: string]: string }
    statusCode: number
    statusText: string
}

function httpFetch(request: HttpRequest, abortController: AbortController): Promise<HttpResponse> {
    if (request.timeout) {
        setTimeout(() => abortController?.abort(), request.timeout);
    }

    return fetch(request.url, {
        method: request.method,
        body: request.body || null,
        headers: new Headers(request.headers),
        signal: abortController?.signal,
    })
        .then((res: Response) => {
            const headers = Object.fromEntries(res.headers.entries());
            switch (request.expect) {
                case "STRING": {
                    return res.text().then((x) => ({
                        ok: {
                            url: res.url,
                            headers: headers,
                            statusCode: res.status,
                            statusText: res.statusText,
                            body: x || null,
                        }
                    }));
                }
                case "JSON": {
                    return res.json().then((x) => ({
                        ok: {
                            url: res.url,
                            headers: headers,
                            statusCode: res.status,
                            statusText: res.statusText,
                            body: x || null,
                        }
                    }));
                }
                case "WHATEVER": {
                    return {
                        ok: {
                            url: res.url,
                            headers: headers,
                            statusCode: res.status,
                            statusText: res.statusText,
                            body: null,
                        }
                    }
                }
            }
        })
        .catch((e) => { return { err: e } })
}