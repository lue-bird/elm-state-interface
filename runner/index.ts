export interface ElmPorts {
    toJs: {
        subscribe: (callback: (fromElm: any) => void) => void
    }
    fromJs: { send: (toElm: any) => void }
}

export function programStart(appConfig: { ports: ElmPorts, domElement: HTMLElement }) {
    const addInterfaceWithoutSendToElmImplementation: (tag: string) => ((config: any) => any) = tag => {
        switch (tag) {
            case "ConsoleLog": return (config: string) => { console.log(config) }
            case "ConsoleWarn": return (config: string) => { console.warn(config) }
            case "ConsoleError": return (config: string) => { console.error(config) }
            case "NavigationPushUrl": return (config: string) => { pushUrl(config) }
            case "NavigationReplaceUrl": return (config: string) => { replaceUrl(config) }
            case "NavigationGo": return (config: number) => { go(config) }
            case "NavigationLoad": return (config: string) => { load(config) }
            case "NavigationReload": return (_config: null) => { reload() }
            case "FileDownloadUnsignedInt8s": return (config: {
                mimeType: string;
                name: string;
                content: number[];
            }) => {
                fileDownloadBytes(config)
            }
            case "ClipboardReplaceBy": return (config: string) => { navigator.clipboard.writeText(config) }
            case "Audio": return addAudio
            case "SocketDisconnect": return (index: number) => {
                const socketToDisconnect = sockets.at(index)
                if (socketToDisconnect) {
                    socketToDisconnect.close()
                    sockets[index] = undefined
                } else { } // socket is already closed
            }
            case "SocketMessage": return (config: { id: number, data: string }) => {
                const socketToDisconnect = sockets.at(config.id)
                if (socketToDisconnect) {
                    socketToDisconnect.send(config.data)
                } else {
                    console.warn("trying to send messages on closed socket")
                }
            }
            case "LocalStorageSet": return (config: { key: string, value: string | null }) => {
                try {
                    if (config.value === null) {
                        window.localStorage.removeItem(config.key)
                    } else {
                        window.localStorage.setItem(config.key, config.value)
                    }
                } catch (disallowedByUserOrQuotaExceeded) {
                    console.warn(disallowedByUserOrQuotaExceeded)
                }
            }
            default: return (_config: any) => {
                notifyOfBug("Unknown message kind InterfaceWithoutFuture.Add." + tag + " from elm. The associated js function might be missing.")
            }
        }
    }
    const interfaceWithoutSendToElmImplementation: (tag: string) => ((config: any) => void) = tag => {
        switch (tag) {
            case "EditAudio": return editAudio
            case "RemoveDom": return (_config: null) => { appConfig.domElement.replaceChildren() }
            case "RemoveHttpRequest": return (config: string) => {
                const maybeAbortController = httpRequestAbortControllers[config]
                if (maybeAbortController) {
                    maybeAbortController.abort()
                }
            }
            case "RemoveAudio": return removeAudio
            case "RemoveSocketConnect": return (config: { address: string }) => {
                sockets
                    .flatMap(socket => socket ? [socket] : [])
                    .filter(socket => socket.url == config.address)
                    .forEach(socketToStopFromConnecting => {
                        socketToStopFromConnecting.onopen = null
                    })
            }
            case "RemoveListen": return (config: { tag: string, value: any }) => {
                interfaceListenRemoveImplementation(config.tag)(config.value)
            }
            case "Add": return (config: { tag: string, value: any }) => {
                addInterfaceWithoutSendToElmImplementation(config.tag)(config.value)
            }
            default: return (_config: any) => {
                notifyOfBug("Unknown message kind InterfaceWithoutFuture." + tag + " from elm. The associated js function might be missing.")
            }
        }
    }
    const interfaceListenAddImplementation: (tag: string) => ((config: any, sendToElm: (v: any) => void) => void) = tag => {
        switch (tag) {
            case "TimePeriodicallyListen": return (config, sendToElm) => {
                addTimePeriodicallyListen(config, sendToElm)
            }
            case "WindowEventListen": return windowEventListenAdd
            case "WindowAnimationFrameListen": return (_config: null, sendToElm) => {
                addAnimationFrameListen(sendToElm)
            }
            case "DocumentEventListen": return documentEventListenAdd
            case "SocketDisconnectListen": return (index: number, sendToElm) => {
                const socketToDisconnect = sockets.at(index)
                if (socketToDisconnect) {
                    socketToDisconnect.onclose = (event) => {
                        sendToElm({ code: event.code, reason: event.reason })
                    }
                } else { } // socket is already closed
            }
            case "SocketMessageListen": return (index: number, sendToElm) => {
                const socketToListenToMessagesFrom = sockets.at(index)
                if (socketToListenToMessagesFrom) {
                    socketToListenToMessagesFrom.onmessage = (event) => {
                        sendToElm(event.data)
                    }
                } else {
                    console.warn("trying to listen to messages on closed socket")
                }
            }
            case "LocalStorageRemoveOnADifferentTabListen": return (config: { key: string }, sendToElm) => {
                const abortController = new AbortController()
                window.addEventListener(
                    "storage",
                    storageEvent => {
                        if (storageEvent.key === config.key && storageEvent.newValue === null) {
                            sendToElm(storageEvent.url)
                        }
                    },
                    { signal: abortController.signal }
                )
                localStorageRemoveOnADifferentTabListenAbortControllers[config.key] = abortController
            }
            case "LocalStorageSetOnADifferentTabListen": return (config: { key: string }, sendToElm) => {
                const abortController = new AbortController()
                window.addEventListener(
                    "storage",
                    storageEvent => {
                        if (storageEvent.key === config.key && storageEvent.newValue !== null) {
                            sendToElm({ url: storageEvent.url, oldValue: storageEvent.oldValue, newValue: storageEvent.newValue })
                        }
                    },
                    { signal: abortController.signal }
                )
                localStorageSetOnADifferentTabListenAbortControllers[config.key] = abortController
            }
            default: return (_config: any, _sendToElm) => {
                notifyOfBug("Unknown message kind InterfaceWithFuture.AddListen." + tag + " from elm. The associated js function might be missing.")
            }
        }
    }
    const interfaceListenRemoveImplementation: (tag: string) => ((config: any) => void) = tag => {
        switch (tag) {
            case "TimePeriodicallyListen": return removeTimePeriodicallyListen
            case "WindowEventListen": return windowEventListenRemove
            case "AnimationFrameListen": return (_config: null) => { removeAnimationFrameListen() }
            case "DocumentEventListen": return documentEventListenRemove
            case "SocketDisconnectListen": return (index: number) => {
                const socket = sockets.at(index)
                if (socket) {
                    socket.onclose = null
                } else { } // already removed
            }
            case "SocketMessageListen": return (index: number) => {
                const socketToListenToMessagesFrom = sockets.at(index)
                if (socketToListenToMessagesFrom) {
                    socketToListenToMessagesFrom.onmessage = null
                } else { } // already removed
            }
            case "LocalStorageRemoveOnADifferentTabListen": return (config: { key: string }) => {
                localStorageRemoveOnADifferentTabListenAbortControllers[config.key]?.abort()
                delete localStorageRemoveOnADifferentTabListenAbortControllers[config.key]
            }
            case "LocalStorageSetOnADifferentTabListen": return (config: { key: string }) => {
                localStorageSetOnADifferentTabListenAbortControllers[config.key]?.abort()
                delete localStorageSetOnADifferentTabListenAbortControllers[config.key]
            }
            default: return (_config: any) => {
                notifyOfBug("Unknown message kind InterfaceWithFuture.RemoveListen." + tag + " from elm. The associated js function might be missing.")
            }
        }
    }
    const interfaceWithSendToElmImplementation: (tag: string) => ((config: any, sendToElm: (v: any) => void) => void) = tag => {
        switch (tag) {
            case "EditDom": return (config, sendToElm) => {
                editDom(config.path, config.replacement, sendToElm)
            }
            case "AddAudioSourceLoad": return audioSourceLoad
            case "AddSocketConnect": return (config: { address: string }, sendToElm) => {
                const createdSocket = new WebSocket(config.address)
                sockets.push(createdSocket)
                createdSocket.onopen = _event => {
                    sendToElm(sockets.length)
                    createdSocket.onopen = null
                }
            }
            case "AddListen": return (config: { tag: string, value: any }, sendToElm) => {
                interfaceListenAddImplementation(config.tag)(config.value, sendToElm)
            }
            case "AddRequest": return (config: { tag: string, value: any }, sendToElm) => {
                interfaceRequestImplementation(config.tag)(config.value)
                    .then(sendToElm)
                    .catch((_error: null) => {
                        notifyOfBug("Unknown message kind InterfaceWithFuture.AddRequest." + config.tag + " from elm. The associated js function might be missing.")
                    })
            }
            default: return (_config: any, _sendToElm) => {
                notifyOfBug("Unknown message kind InterfaceWithFuture." + tag + " from elm. The associated js function might be missing.")
            }
        }
    }
    const interfaceRequestImplementation: (tag: string) => ((config: any) => Promise<any>) = tag => {
        switch (tag) {
            case "LocalStorageRequest": return (config: { key: string }) => {
                return Promise.resolve(window.localStorage.getItem(config.key))
            }
            case "TimePosixRequest": return (_config: null) => {
                return Promise.resolve(Date.now())
            }
            case "TimezoneOffsetRequest": return (_config: null) => {
                // Equivalent Elm Kernel code: https://github.com/elm/time/blob/1.0.0/src/Elm/Kernel/Time.js#L38-L52
                return Promise.resolve(new Date().getTimezoneOffset())
            }
            case "TimezoneNameRequest": return (_config: null) => {
                return Promise.resolve(getTimezoneName())
            }
            case "RandomUnsignedInt32sRequest": return (config: number) => {
                return Promise.resolve(crypto.getRandomValues(new Uint32Array(config)))
            }
            case "HttpRequest": return (config: HttpRequest) => {
                const abortController = new AbortController()
                httpRequestAbortControllers[config.url] = abortController
                return httpFetch(config, abortController)
            }
            case "WindowSizeRequest": return (_config: null) => {
                return Promise.resolve({ width: window.innerWidth, height: window.innerHeight })
            }
            case "NavigationUrlRequest": return (_config: null) => {
                return Promise.resolve(window.location.href)
            }
            case "ClipboardRequest": return (_config: null) => {
                return navigator.clipboard.readText()
                    .catch(_notAllowed => {
                        console.warn("clipboard cannot be read")
                    })
            }
            default: return (_config: any) => Promise.reject(null)
        }
    }

    appConfig.ports.toJs.subscribe(function (fromElm: { tag: "InterfaceWithFuture" | "InterfaceWithoutFuture", value: { tag: string, value: any } }) {
        // console.log("elm → js: ", fromElm)
        function sendToElm(eventData: void) {
            const toElm = { diff: fromElm, eventData: eventData }
            appConfig.ports.fromJs.send(toElm)
            // console.log("js → elm: ", toElm)
        }
        switch (fromElm.tag) {
            case "InterfaceWithFuture": {
                interfaceWithSendToElmImplementation(fromElm.value.tag)(fromElm.value.value, sendToElm)
            }
            case "InterfaceWithoutFuture": {
                interfaceWithoutSendToElmImplementation(fromElm.value.tag)(fromElm.value.value)
            }
        }
    })

    function editDom(path: number[], replacement: any, sendToElm: (v: any) => void) {
        if (path.length === 0) {
            const parentDomNode = appConfig.domElement
            if (replacement?.node) {
                parentDomNode.replaceChildren() // remove all subs
                parentDomNode.appendChild(createDomNode([], replacement.node, sendToElm))
            } else {
                editDomModifiers(parentDomNode.firstChild as (Element & ElementCSSInlineStyle), replacement, path, sendToElm)
            }
        } else {
            let parentDomNode: ChildNode | null = appConfig.domElement.firstChild
            if (parentDomNode) {
                path.slice(1, path.length).reverse().forEach(subIndex => {
                    const subNode = parentDomNode?.childNodes[subIndex]
                    if (subNode) {
                        parentDomNode = subNode
                    }
                })
                const oldDomNode: ChildNode | undefined = parentDomNode.childNodes[path[0] ?? 0]
                if (oldDomNode) {
                    if (replacement?.node) {
                        parentDomNode.replaceChild(createDomNode([], replacement.node, sendToElm), oldDomNode)
                    } else {
                        editDomModifiers(oldDomNode as (Element & ElementCSSInlineStyle), replacement, path, sendToElm)
                    }
                }
            }
        }
    }
}

let sockets: (WebSocket | undefined)[] = []

let localStorageRemoveOnADifferentTabListenAbortControllers: Record<string, AbortController> = {}
let localStorageSetOnADifferentTabListenAbortControllers: Record<string, AbortController> = {}
let domListenAbortControllers: { domElement: Element, abortController: AbortController }[] = []


function editDomModifiers(domNodeToEdit: Element & ElementCSSInlineStyle, replacement: any, path: number[], sendToElm: (v: any) => void) {
    if (replacement?.styles) {
        domNodeToEdit.removeAttribute("style")
        domElementAddStyles(domNodeToEdit, replacement.styles)
    } else if (replacement?.attributes) {
        for (const attribute of domNodeToEdit.attributes) {
            if (attribute.name !== "style" && attribute.namespaceURI === null) {
                domNodeToEdit.removeAttribute(attribute.name)
            }
        }
        domElementAddAttributes(domNodeToEdit, replacement.attributes)
    } else if (replacement?.attributesNamespaced) {
        for (const attribute of domNodeToEdit.attributes) {
            if (attribute.name !== "style" && attribute.namespaceURI) {
                domNodeToEdit.removeAttributeNS(attribute.namespaceURI, attribute.name)
            }
        }
        domElementAddAttributesNamespaced(domNodeToEdit, replacement.attributesNamespaced)
    } else if (replacement?.eventListens) {
        domListenAbortControllers = domListenAbortControllers
            .filter(eventListener => {
                if (eventListener.domElement === domNodeToEdit) {
                    eventListener.abortController.abort()
                    return false
                }
                return true
            })
        domElementAddEventListens(domNodeToEdit, replacement.eventListens, path, sendToElm)
    } else {
        console.error("unknown dom modifier replacement kind", replacement)
    }
}

const httpRequestAbortControllers: { [key: string]: AbortController } = {}

const timePeriodicallyListens: { [key: number]: number } = {}
function addTimePeriodicallyListen(intervalDuration: { milliSeconds: number }, sendToElm: (v: any) => void) {
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
    } catch (err) {
        return new Date().getTimezoneOffset()
    }
}

function createDomNode(innerPath: number[], node: any, sendToElm: (v: any) => void): Element | Text {
    if (node?.text) {
        return document.createTextNode(node.text)
    } else { // if (node?.element)
        const createdDomElement: (Element & ElementCSSInlineStyle) =
            node.element?.namespace ?
                document.createElementNS(node.element.namespace, noScript(node.element.tag))
                :
                document.createElement(noScript(node.element.tag))

        domElementAddAttributes(createdDomElement, node.element.attributes)
        domElementAddAttributesNamespaced(createdDomElement, node.element.attributesNamespaced)
        domElementAddStyles(createdDomElement, node.element.styles)
        domElementAddEventListens(createdDomElement, node.element.eventListens, innerPath, sendToElm)
        node.element.subs.forEach((sub: any, subIndex: number) => {
            createdDomElement.appendChild(
                createDomNode([subIndex].concat(innerPath), sub, sendToElm)
            )
        })
        return createdDomElement
    }
}
function domElementAddStyles(domElement: Element & ElementCSSInlineStyle, styles: { key: string, value: string }[]) {
    styles.forEach(styleSingle => {
        domElement.style.setProperty(styleSingle.key, styleSingle.value)
    })
}
function domElementAddAttributes(domElement: Element, attributes: { key: string, value: string }[]) {
    attributes.forEach(attribute => {
        if (RE_js_html.test(attribute.value)) {
            console.error("This is an XSS vector. Please use an interface instead.")
        } else if (attribute.key === "src" && RE_js_html.test(attribute.value)) {
            console.error("This is an XSS vector. Please use an interface instead.")
        } else if (attribute.key === "action" || attribute.key === "href" && RE_js.test(attribute.value)) {
            console.error("This is an XSS vector. Please use an interface instead.")
        } else {
            domElement.setAttribute(
                noOnOrFormAction(attribute.key),
                attribute.value
            )
        }
    })
}
function domElementAddAttributesNamespaced(domElement: Element, attributesNamespaced: { namespace: string, key: string, value: string }[]) {
    attributesNamespaced.forEach(attributeNamespaced => {
        domElement.setAttributeNS(attributeNamespaced.namespace, attributeNamespaced.key, attributeNamespaced.value)
    })
}
function domElementAddEventListens(domElement: Element, eventListens: any, path: number[], sendToElm: (v: any) => void) {
    for (let [eventListenName, defaultActionHandling] of Object.entries(eventListens)) {
        const abortController: AbortController = new AbortController()
        domElement.addEventListener(
            eventListenName,
            (triggeredEvent) => {
                sendToElm({ innerPath: path, name: eventListenName, event: triggeredEvent })
                if (defaultActionHandling === "DefaultActionPrevent") {
                    triggeredEvent.preventDefault()
                }
            },
            { signal: abortController.signal }
        )
        domListenAbortControllers.push({ domElement: domElement, abortController: abortController })
    }
}

// copied and edited from https://github.com/elm/virtual-dom/blob/master/src/Elm/Kernel/VirtualDom.js
// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why RE_js and RE_js_html look
// so freaky.

const RE_script = /^script$/i
var RE_on_formAction = /^(on|formAction$)/i;
var RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;

function noScript(tag: string) {
    return RE_script.test(tag) ? 'p' : tag
}
function noOnOrFormAction(key: string) {
    return RE_on_formAction.test(key) ? "data-" + key : key
}


function windowEventListenAdd(eventName: string, sendToElm: (v: any) => void) {
    (window as { [key: string]: any })["on" + eventName] = sendToElm
}
function windowEventListenRemove(eventName: string) {
    (window as { [key: string]: any })["on" + eventName] = null
}

function documentEventListenAdd(eventName: string, sendToElm: (v: any) => void) {
    (window as { [key: string]: any })["on" + eventName] = sendToElm
}
function documentEventListenRemove(eventName: string) {
    (window as { [key: string]: any })["on" + eventName] = null
}

function go(urlSteps: number) {
    history.go(urlSteps)
}
function pushUrl(appUrl: string) {
    if (history.state === null || (history.state.appUrl !== appUrl)) {
        history.pushState({ appUrl: appUrl }, "", window.location.origin + appUrl)
    }
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
    } catch (err) {
        // Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
        // Other browsers reload the page, so let's be consistent about that.
        reload()
    }
}

let runningAnimationFrameLoopId: number | undefined = undefined
function addAnimationFrameListen(sendToElm: (v: any) => void) {
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
    headers: { name: string, value: string }[]
    expect: Expect
    timeout: number | null
    body: HttpRequestBody
}
type Expect = "string" | "bytes" | "whatever"
type HttpRequestBody = { tag: "uint8Array", value: Uint8Array }
    | { tag: "string", value: string } | { tag: "empty" }

type HttpResponse = { ok: ResponseSuccess } | { err: any }
interface ResponseSuccess {
    body: Uint8Array | string | null
    url: string
    headers: { [header: string]: string }
    statusCode: number
    statusText: string
}

function httpRequestBodyForFetch(body: HttpRequestBody) {
    switch (body.tag) {
        case "empty": return null
        case "string": return body.value || null
        case "uint8Array": return new Blob([body.value])
    }
}
function httpFetch(request: HttpRequest, abortController: AbortController): Promise<HttpResponse> {
    if (request.timeout) {
        setTimeout(() => abortController.abort(), request.timeout)
    }
    return fetch(request.url, {
        method: request.method,
        body: httpRequestBodyForFetch(request.body),
        headers: new Headers(request.headers.map(header => [header.name, header.value])),
        signal: abortController.signal
    })
        .then((res: Response) => {
            const headers = Object.fromEntries(res.headers.entries())
            switch (request.expect) {
                case "string":
                    return res.text().then((x) => ({
                        ok: {
                            url: res.url,
                            headers: headers,
                            statusCode: res.status,
                            statusText: res.statusText,
                            body: x || null as (string | null | Uint8Array)
                        }
                    }))
                case "bytes":
                    return res.blob()
                        .then(blob => blob.arrayBuffer())
                        .then((x) => ({
                            ok: {
                                url: res.url,
                                headers: headers,
                                statusCode: res.status,
                                statusText: res.statusText,
                                body: new Uint8Array(x)
                            }
                        }))
                case "whatever":
                    return {
                        ok: {
                            url: res.url,
                            headers: headers,
                            statusCode: res.status,
                            statusText: res.statusText,
                            body: null
                        }
                    }
            }
        })
        .catch((e) => { return { err: e } })
}


type AudioInfo = {
    url: string,
    startTime: number,
    volume: AudioParameterTimeline,
    speed: AudioParameterTimeline,
    stereoPan: AudioParameterTimeline,
    linearConvolutions: { sourceUrl: string }[],
    lowpasses: { cutoffFrequency: AudioParameterTimeline }[],
    highpasses: { cutoffFrequency: AudioParameterTimeline }[]
}
type AudioParameterTimeline = {
    startValue: number,
    keyFrames: { time: number, value: number }[]
}

const audioBuffers: { [key: string]: AudioBuffer } = {}
const audioContext = new AudioContext()
let audioPlaying: {
    url: string,
    startTime: number,
    sourceNode: AudioBufferSourceNode,
    gainNode: GainNode,
    stereoPanNode: StereoPannerNode,
    processingNodes: AudioNode[]
}[] = []

function audioSourceLoad(url: string, sendToElm: (v: any) => void) {
    const request = new XMLHttpRequest()
    request.open("GET", url, true)
    request.responseType = "arraybuffer"
    request.onerror = function () {
        sendToElm({ err: "NetworkError" })
    }
    request.onload = function () {
        audioContext.decodeAudioData(
            request.response,
            function (buffer) {
                audioBuffers[url] = buffer
                sendToElm({
                    ok: {
                        durationInSeconds: buffer.length / buffer.sampleRate
                    }
                })
            },
            function (error) {
                sendToElm({ err: error.message })
            }
        )
    }
    request.send()
}

function audioParameterTimelineApplyTo(audioParam: AudioParam, timeline: AudioParameterTimeline) {
    const currentTime = audioContext.currentTime
    audioParam.cancelScheduledValues(currentTime)
    const fullTimeline = [
        { time: currentTime, value: timeline.startValue },
        ...timeline.keyFrames.map(keyframe => { return { value: keyframe.value, time: posixToContextTime(keyframe.time, currentTime) } })
    ]
    forEachConsecutive(fullTimeline, pair => {
        if (currentTime >= pair.current.time) {
            audioParam.setValueAtTime(
                linearlyInterpolate(
                    pair.current.value,
                    pair.next.value,
                    // since start / duration
                    (currentTime - pair.current.time) / (pair.next.time - pair.current.time)
                ),
                0
            )
        }
        audioParam.linearRampToValueAtTime(pair.next.value, pair.next.time - pair.current.time)
    })
    return audioParam
}

function addAudio(config: AudioInfo) {
    const buffer = audioBuffers[config.url]
    if (buffer) {
        createAudio(config, buffer)
    } else {
        console.warn("lue-bird/elm-state-interface: tried to play audio from source that isn't loaded. Did you use Web.Audio.sourceLoad?")
    }
}
function createAudio(config: AudioInfo, buffer: AudioBuffer) {
    const currentTime = new Date().getTime()
    const source = audioContext.createBufferSource()
    source.buffer = buffer
    audioParameterTimelineApplyTo(source.playbackRate, config.speed)

    const gainNode = audioContext.createGain()
    audioParameterTimelineApplyTo(gainNode.gain, config.volume)

    const stereoPannerNode = new StereoPannerNode(audioContext)
    audioParameterTimelineApplyTo(stereoPannerNode.pan, config.stereoPan)

    const processingNodes = createProcessingNodes(config)

    forEachConsecutive(
        [source, gainNode, stereoPannerNode, ...processingNodes, audioContext.destination],
        pair => { pair.current.connect(pair.next) }
    )

    if (config.startTime >= currentTime) {
        source.start(posixToContextTime(config.startTime, currentTime), 0)
    } else {
        source.start(0, (currentTime - config.startTime) / 1000)
    }
    audioPlaying.push({
        url: config.url,
        startTime: config.startTime,
        sourceNode: source,
        gainNode: gainNode,
        stereoPanNode: stereoPannerNode,
        processingNodes: processingNodes,
    })
}
function createProcessingNodes(config: {
    linearConvolutions: { sourceUrl: string }[],
    lowpasses: { cutoffFrequency: AudioParameterTimeline }[],
    highpasses: { cutoffFrequency: AudioParameterTimeline }[]
}): AudioNode[] {
    const convolverNodes =
        config.linearConvolutions
            .map(linearConvolution => {
                const convolverNode = new ConvolverNode(audioContext)
                const buffer = audioBuffers[linearConvolution.sourceUrl]
                if (buffer) {
                    convolverNode.buffer = buffer
                } else {
                    console.warn("lue-bird/elm-state-interface: tried to create a linear convolution from source that isn't loaded. Did you use Web.Audio.sourceLoad?")
                }
                return convolverNode
            })

    const lowpassNodes =
        config.lowpasses
            .map(lowpass => {
                const biquadNode = new BiquadFilterNode(audioContext)
                biquadNode.type = "lowpass"
                audioParameterTimelineApplyTo(biquadNode.frequency, lowpass.cutoffFrequency)
                return biquadNode
            })

    const highpassNodes =
        config.highpasses
            .map(highpass => {
                const biquadNode = new BiquadFilterNode(audioContext)
                biquadNode.type = "highpass"
                audioParameterTimelineApplyTo(biquadNode.frequency, highpass.cutoffFrequency)
                return biquadNode
            })
    return [...convolverNodes, ...lowpassNodes, ...highpassNodes]
}
function removeAudio(config: { url: string, startTime: number }) {
    audioPlaying = audioPlaying.filter(audio => {
        if (audio.url === config.url && audio.startTime === config.startTime) {
            audio.sourceNode.stop()
            audio.sourceNode.disconnect()
            audio.gainNode.disconnect()
            audio.stereoPanNode.disconnect()
            audio.processingNodes.forEach(node => { node.disconnect() })
            return false
        }
        return true
    })
}
function editAudio(config: { url: string, startTime: number, replacement: any }) {
    audioPlaying.forEach(value => {
        if (value.url === config.url && value.startTime === config.startTime) {
            if (config.replacement?.volume) {
                audioParameterTimelineApplyTo(value.gainNode.gain, config.replacement.volume)
            } else if (config.replacement?.speed) {
                audioParameterTimelineApplyTo(value.sourceNode.playbackRate, config.replacement.speed)
            } else if (config.replacement?.stereoPan) {
                audioParameterTimelineApplyTo(value.stereoPanNode.pan, config.replacement.stereoPan)
            } else if (config.replacement?.processing) {
                value.stereoPanNode.disconnect()
                value.processingNodes.forEach(node => { node.disconnect() })

                value.processingNodes = createProcessingNodes(config.replacement.processing)

                forEachConsecutive(
                    [value.stereoPanNode, ...value.processingNodes, audioContext.destination],
                    pair => { pair.current.connect(pair.next) }
                )
            }
        }
    })
}

// helpers

function notifyOfBug(bugDescription: string) {
    console.error("lue-bird/elm-state-interface bug: " + bugDescription + ". Please open an issue on github.")
}

function posixToContextTime(posix: number, currentTimePosix: number) {
    return (posix - currentTimePosix) / 1000 + audioContext.currentTime
}

function linearlyInterpolate(startValue: number, endValue: number, progress: number) {
    return Number.isFinite(progress) ?
        progress * (endValue - startValue) + startValue
        :
        startValue
}

function forEachConsecutive<element>(array: element[], forPair: ((pair: { current: element, next: element }) => void)) {
    for (let i = 0; i <= array.length - 2; i++) {
        const current: element | undefined = array[i]
        const next: element | undefined = array[i + 1]
        if (current && next) { // should always work
            forPair({ current: current, next: next })
        }
    }
}
