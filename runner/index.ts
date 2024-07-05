export interface ElmPorts {
    toJs: {
        subscribe: (callback: (fromElm: any) => void) => void
    }
    fromJs: { send: (toElm: any) => void }
}

export function programStart(appConfig: { ports: ElmPorts, domElement: Element }) {
    appConfig.ports.toJs.subscribe(function (fromElm: { id: string, diff: { tag: "Add" | "Edit" | "Remove", value: any } }) {
        // console.log("elm → js: ", fromElm)
        function sendToElm(eventData: void) {
            const toElm = {
                id: fromElm.id,
                eventData: eventData
            }
            appConfig.ports.fromJs.send(toElm)
            // console.log("js → elm: ", toElm)
        }
        interfaceDiffImplementation(fromElm.diff.tag, sendToElm, fromElm.id)(fromElm.diff.value)
    })
    function interfaceDiffImplementation(tag: "Add" | "Edit" | "Remove", sendToElm: (v: any) => void, id: string): ((config: any) => void) {
        switch (tag) {
            case "Add": return (config: { tag: string, value: any }) => {
                const abortController = new AbortController()
                abortControllers.set(id, abortController)
                interfaceAddImplementation(id, config.tag, sendToElm, abortController.signal)(config.value)
            }
            case "Edit": return (config: { tag: string, value: any }) => {
                interfaceEditImplementation(id, config.tag, sendToElm)(config.value)
            }
            case "Remove": return (_config: null) => {
                const abortController = abortControllers.get(id)
                if (abortController !== undefined) {
                    abortController.abort()
                    abortControllers.delete(id)
                } else {
                    warn("bug: trying to remove an interface that was already aborted")
                }
            }
        }
    }
    function interfaceAddImplementation(id: string, tag: string, sendToElm: (v: any) => void, abortSignal: AbortSignal): ((config: any) => void) {
        switch (tag) {
            case "DocumentTitleReplaceBy": return (config: string) => {
                window.document.title = config
            }
            case "DocumentAuthorSet": return (config: string) => {
                getOrAddMeta("author").content = config
            }
            case "DocumentKeywordsSet": return (config: string) => {
                getOrAddMeta("keywords").content = config
            }
            case "DocumentDescriptionSet": return (config: string) => {
                getOrAddMeta("description").content = config
            }
            case "DocumentEventListen": return (config: string) => {
                window.document.addEventListener(config, sendToElm, { signal: abortSignal })
            }
            case "ConsoleLog": return (message: string) => {
                window?.console.log(message)
            }
            case "ConsoleWarn": return (message: string) => {
                window?.console.warn(message)
            }
            case "ConsoleError": return (message: string) => {
                window?.console.error(message)
            }
            case "NavigationReplaceUrl": return (appUrl: string) => {
                window.history.replaceState({ appUrl: appUrl }, "", window.location.origin + appUrl)
            }
            case "NavigationPushUrl": return (appUrl: string) => {
                if (window.history.state === null || (history.state.appUrl !== appUrl)) {
                    window.history.pushState({ appUrl: appUrl }, "", window.location.origin + appUrl)
                }
            }
            case "NavigationGo": return (config: number) => {
                window.history.go(config)
            }
            case "NavigationLoad": return (url: string) => {
                try {
                    window.location.href = url
                } catch (_error) {
                    // Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
                    // Other browsers reload the page, so let's be consistent about that.
                    window.document.location.reload()
                }
            }
            case "NavigationReload": return (_config: null) => {
                window.document.location.reload()
            }
            case "NavigationUrlRequest": return (_config: null) => {
                sendToElm(window.location.href)
            }
            case "FileDownloadUnsignedInt8s": return (config: { mimeType: string, name: string, content: number[] }) => {
                fileDownloadBytes(config)
            }
            case "ClipboardReplaceBy": return (config: string) => {
                window.navigator.clipboard.writeText(config)
            }
            case "ClipboardRequest": return (_config: null) => {
                window.navigator.clipboard.readText()
                    .catch(_notAllowed => {
                        warn("clipboard cannot be read")
                    })
                    .then(sendToElm)
            }
            case "AudioSourceLoad": return (config: string) => {
                audioSourceLoad(config, sendToElm, abortSignal)
            }
            case "AudioPlay": return (config: AudioInfo) => {
                const audioBuffer = audioBuffers.get(config.url)
                if (audioBuffer !== undefined) {
                    const createdAudioPlaying = createAudio(config, audioBuffer)
                    audioPlaying.set(id, createdAudioPlaying)
                    abortSignal.addEventListener("abort", _event => {
                        createdAudioPlaying.sourceNode.stop()
                        createdAudioPlaying.sourceNode.disconnect()
                        createdAudioPlaying.gainNode.disconnect()
                        createdAudioPlaying.stereoPanNode.disconnect()
                        createdAudioPlaying.processingNodes.forEach(node => { node.disconnect() })
                    })
                } else {
                    warn("tried to play audio from source that isn't loaded. Did you use Web.Audio.sourceLoad?")
                }
            }
            case "DomNodeRender": return (config: { pathReverse: number[], node: any }) => {
                const oldDomNodeToEdit = domElementOrDummyInElementAt(domElementOrDummyAtIndex(appConfig.domElement, 0), config.pathReverse)
                const newDomNode = createDomNode(id, config.node, oldDomNodeToEdit.childNodes, sendToElm)
                oldDomNodeToEdit.parentElement?.replaceChild(newDomNode, oldDomNodeToEdit)
                abortSignal.addEventListener("abort", _event => {
                    domListenAbortControllers.delete(id)
                    // it is possible that the "newDomNode" has been replaced
                    // by e.g.a text where there was an Element previously
                    const appConfigDomELementChildElementOnDelete = domElementAtIndex(appConfig.domElement, 0)
                    const toRemove =
                        appConfigDomELementChildElementOnDelete === null ?
                            null
                            :
                            domInElementAt(appConfigDomELementChildElementOnDelete, config.pathReverse)
                    if (toRemove !== null) {
                        while (toRemove.nextSibling !== null) {
                            toRemove.nextSibling.remove()
                        }
                        toRemove.remove()
                    }
                })
            }
            case "NotificationAskForPermission": return (_config: null) => {
                askForNotificationPermissionIfNotAsked()
            }
            case "NotificationShow": return (config: { id: string, message: string, details: string }) => {
                askForNotificationPermissionIfNotAsked().then(status => {
                    switch (status) {
                        case "denied": break
                        case "granted": {
                            const newNotification = new Notification(
                                config.message,
                                {
                                    body: config.details,
                                    tag: config.id
                                }
                            )
                            newNotification.addEventListener("click", _event => { sendToElm("Clicked") })
                            abortSignal.addEventListener("abort", _event => {
                                newNotification.close()
                            })
                        }
                    }
                })
            }
            case "HttpRequest": return (config: HttpRequest) => {
                httpFetch(config, abortSignal).then(sendToElm)
            }
            case "TimePosixRequest": return (_config: null) => {
                sendToElm(Date.now())
            }
            case "TimezoneOffsetRequest": return (_config: null) => {
                // Equivalent Elm Kernel code: https://github.com/elm/time/blob/1.0.0/src/Elm/Kernel/Time.js#L38-L52
                sendToElm(new Date().getTimezoneOffset())
            }
            case "TimezoneNameRequest": return (_config: null) => {
                sendToElm(Intl.DateTimeFormat().resolvedOptions().timeZone)
            }
            case "TimePeriodicallyListen": return (config: { milliSeconds: number }) => {
                const timePeriodicallyListenId =
                    window.setInterval(
                        () => { sendToElm(Date.now()) },
                        config.milliSeconds
                    )
                abortSignal.addEventListener("abort", _event => {
                    window.clearInterval(timePeriodicallyListenId)
                })
            }
            case "TimeOnce": return (config: { pointInTime: number }) => {
                const timeOnceId =
                    window.setTimeout(
                        () => { sendToElm(Date.now()) },
                        config.pointInTime - Date.now()
                    )
                abortSignal.addEventListener("abort", _event => {
                    window.clearInterval(timeOnceId)
                })
            }
            case "RandomUnsignedInt32sRequest": return (config: number) => {
                sendToElm(Array.from(window.crypto.getRandomValues(new Uint32Array(config))))
            }
            case "WindowSizeRequest": return (_config: null) => {
                sendToElm({ width: Math.trunc(window.innerWidth), height: Math.trunc(window.innerHeight) })
            }
            case "WindowPreferredLanguagesRequest": return (_config: null) => {
                sendToElm(window.navigator.languages)
            }
            case "WindowEventListen": return (config: string) => {
                window.addEventListener(config, sendToElm, { signal: abortSignal })
            }
            case "WindowVisibilityChangeListen": return (_config: null) => {
                window.document.addEventListener(
                    "visibilitychange",
                    _eventWhichDoesNotContainTheNewVisibility => {
                        sendToElm(window.document.visibilityState)
                    },
                    { signal: abortSignal }
                )
            }
            case "WindowAnimationFrameListen": return (_config: null) => {
                let runningAnimationFrameLoopId: number
                function addAnimationFrameListen(): number {
                    return window.requestAnimationFrame(_timestamp => {
                        if (!abortSignal.aborted) {
                            sendToElm(Date.now())
                            runningAnimationFrameLoopId = addAnimationFrameListen()
                        }
                    })
                }
                runningAnimationFrameLoopId = addAnimationFrameListen()
                abortSignal.addEventListener("abort", _event => {
                    window.cancelAnimationFrame(runningAnimationFrameLoopId)
                })
            }
            case "WindowPreferredLanguagesChangeListen": return (_config: null) => {
                window.addEventListener(
                    "languagechange",
                    _event => { sendToElm(window.navigator.languages) },
                    { signal: abortSignal }
                )
            }
            case "SocketConnect": return (config: { address: string }) => {
                const createdSocket = new WebSocket(config.address)
                sockets.push(createdSocket)
                const socketId = sockets.length
                createdSocket.addEventListener(
                    "open",
                    _event => {
                        sendToElm({ tag: "SocketConnected", value: socketId })
                    },
                    { signal: abortSignal }
                )
                createdSocket.addEventListener(
                    "close",
                    event => {
                        sendToElm({ tag: "SocketDisconnected", value: { code: event.code, reason: event.reason } })
                        sockets[socketId] = null
                    },
                    { signal: abortSignal }
                )
            }
            case "SocketMessage": return (config: { id: number, data: string }) => {
                const socketToMessage = sockets.at(config.id)
                if (socketToMessage !== undefined && socketToMessage !== null) {
                    socketToMessage.send(config.data)
                } else {
                    warn("trying to send messages on closed socket")
                }
            }
            case "SocketDisconnect": return (index: number) => {
                const socketToDisconnect = sockets.at(index)
                if (socketToDisconnect !== undefined && socketToDisconnect !== null) {
                    socketToDisconnect.close()
                } else { } // socket is already closed
            }
            case "SocketMessageListen": return (index: number) => {
                const socketToListenToMessagesFrom = sockets.at(index)
                if (socketToListenToMessagesFrom !== undefined && socketToListenToMessagesFrom !== null) {
                    socketToListenToMessagesFrom.addEventListener(
                        "message",
                        event => {
                            sendToElm(event.data)
                        },
                        { signal: abortSignal }
                    )
                } else {
                    warn("trying to listen to messages on closed socket")
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
                    warn("local storage cannot be written to: " + disallowedByUserOrQuotaExceeded)
                }
            }
            case "LocalStorageRequest": return (config: { key: string }) => {
                sendToElm(window.localStorage.getItem(config.key))
            }
            case "LocalStorageRemoveOnADifferentTabListen": return (config: { key: string }) => {
                window.addEventListener(
                    "storage",
                    storageEvent => {
                        if (storageEvent.key === config.key && storageEvent.newValue === null) {
                            sendToElm(storageEvent.url)
                        }
                    },
                    { signal: abortSignal }
                )
            }
            case "LocalStorageSetOnADifferentTabListen": return (config: { key: string }) => {
                window.addEventListener(
                    "storage",
                    storageEvent => {
                        if (storageEvent.key === config.key && storageEvent.newValue !== null) {
                            sendToElm({ url: storageEvent.url, oldValue: storageEvent.oldValue, newValue: storageEvent.newValue })
                        }
                    },
                    { signal: abortSignal }
                )
            }
            case "GeoLocationRequest": return (_config: null) => {
                window.navigator.geolocation.getCurrentPosition(
                    geoPosition => { sendToElm(geoPosition.coords) },
                    error => {
                        warn("geo location cannot be read: " + error)
                    },
                    { timeout: 10000 }
                )
            }
            case "GeoLocationChangeListen": return (_config: null) => {
                const geoLocationChangeListenId =
                    navigator.geolocation.watchPosition(
                        geoPosition => { sendToElm(geoPosition.coords) },
                        error => {
                            warn("geo location cannot be read: " + error)
                        }
                    )
                abortSignal.addEventListener("abort", _event => {
                    navigator.geolocation.clearWatch(geoLocationChangeListenId)
                })
            }
            case "GamepadsRequest": return (_config: null) => {
                sendToElm(window.navigator.getGamepads())
            }
            case "GamepadsChangeListen": return (_config: null) => {
                let gamepadsFromLastPoll: (Gamepad | null)[] | null = null
                const gamepadsChangePollingIntervalId = window.setInterval(
                    function () {
                        const newGamepads = window.navigator.getGamepads()
                        if (gamepadsFromLastPoll !== newGamepads) {
                            sendToElm(newGamepads)
                            gamepadsFromLastPoll = newGamepads
                        }
                    },
                    14
                )
                abortSignal.addEventListener("abort", _event => {
                    window.clearInterval(gamepadsChangePollingIntervalId)
                })
            }
            default: return (_config: any) => {
                notifyOfUnknownMessageKind("Add." + tag)
            }
        }
    }
    function interfaceEditImplementation(id: string, tag: string, sendToElm: (v: any) => void): ((config: any) => void) {
        switch (tag) {
            case "EditDom": return (config: { path: number[], replacement: any }) => {
                editDom(id, config.path, config.replacement, sendToElm)
            }
            case "EditAudio": return (config: any) => {
                editAudio(id, config)
            }
            case "EditNotification": return (config: { id: string, message: string, details: string }) => {
                const newNotification = new Notification(
                    config.message,
                    {
                        body: config.details,
                        tag: config.id
                    }
                )
                newNotification.addEventListener("click", _event => { sendToElm("Clicked") })
            }
            default: return (_config: any) => {
                notifyOfUnknownMessageKind("Edit." + tag)
            }
        }
    }

    function domElementAtIndex(parent: Element, index: number) {
        const childNode = parent.childNodes.item(index)
        if (parent === null || (!(childNode instanceof Element))) {
            return null
        } else {
            return childNode
        }
    }

    function domElementOrDummyAtIndex(parent: Element, index: number): Element {
        const childNode = parent.childNodes.item(index)
        if (childNode === null) {
            while (parent.childNodes.length <= index - 1) {
                parent.appendChild(document.createElement("div"))
            }
            const newDummy = document.createElement("div")
            parent.appendChild(newDummy)
            return newDummy
        } else if (!(childNode instanceof Element)) {
            const newDummyReplacement = document.createElement("div")
            parent.replaceChild(newDummyReplacement, childNode)
            return newDummyReplacement
        } else {
            return childNode
        }
    }

    function domInElementAt(overallParent: Element, pathReverse: number[]): ChildNode | null {
        let lastVisitedDom = overallParent
        for (let i = pathReverse.length - 1; i >= 0; i--) {
            const currentDom = domElementAtIndex(lastVisitedDom, pathReverse[i] ?? 0)
            if (currentDom === null) {
                return null
            } else {
                lastVisitedDom = currentDom
            }
        }
        return lastVisitedDom
    }
    function domElementOrDummyInElementAt(overallParent: Element, pathReverse: number[]): ChildNode {
        let lastVisitedDom = overallParent
        for (let i = pathReverse.length - 1; i >= 0; i--) {
            lastVisitedDom = domElementOrDummyAtIndex(lastVisitedDom, pathReverse[i] ?? 0)
        }
        return lastVisitedDom
    }

    function editDom(
        id: string,
        pathReverse: number[],
        replacement: { tag: "Node" | "Styles" | "Attributes" | "AttributesNamespaced" | "StringProperties" | "BoolProperties" | "ScrollToPosition" | "ScrollToShow" | "ScrollPositionRequest" | "EventListens", value: any },
        sendToElm: (v: any) => void
    ) {
        switch (replacement.tag) {
            case "Node": {
                const oldDomNodeToEdit = domElementOrDummyInElementAt(domElementOrDummyAtIndex(appConfig.domElement, 0), pathReverse)
                const newDomNode = createDomNode(id, replacement.value, oldDomNodeToEdit.childNodes, sendToElm)
                oldDomNodeToEdit.parentElement?.replaceChild(newDomNode, oldDomNodeToEdit)
                break
            }
            case "Styles": case "Attributes": case "AttributesNamespaced": case "StringProperties": case "BoolProperties": case "ScrollToPosition": case "ScrollToShow": case "ScrollPositionRequest": case "EventListens": {
                const oldDomNodeToEdit = domElementOrDummyInElementAt(domElementOrDummyAtIndex(appConfig.domElement, 0), pathReverse)
                if (oldDomNodeToEdit instanceof Element) {
                    editDomModifiers(
                        id,
                        oldDomNodeToEdit as (Element & ElementCSSInlineStyle),
                        { tag: replacement.tag, value: replacement.value },
                        sendToElm
                    )
                } else {
                    warn("the DOM element I wanted to edit has been replaced by text. Try to disable potential interfering extensions")
                }
                break
            }
        }
    }
}



//// state


const abortControllers: Map<string, AbortController> = new Map()
const domListenAbortControllers: Map<string, AbortController[]> = new Map()
const audioPlaying: Map<string, AudioPlaying> = new Map()

const audioBuffers: Map<string, AudioBuffer> = new Map()
let audioContext: AudioContext | null = null

const sockets: (WebSocket | null)[] = []


//// other helpers

function getOrInitializeAudioContext(): AudioContext {
    if (audioContext !== null) {
        return audioContext
    } else {
        audioContext = new AudioContext()
        return audioContext
    }
}

function editDomModifiers(
    id: string,
    domElementToEdit: Element & ElementCSSInlineStyle,
    replacement: {
        tag: "Styles" | "Attributes" | "AttributesNamespaced" | "StringProperties" | "BoolProperties" | "ScrollToPosition" | "ScrollToShow" | "ScrollPositionRequest" | "EventListens",
        value: any
    },
    sendToElm: (v: any) => void
) {
    switch (replacement.tag) {
        case "Styles": {
            replacement.value.remove.forEach((styleKey: string) => {
                domElementToEdit?.style.removeProperty(styleKey)
            })
            domElementAddStyles(domElementToEdit, replacement.value.edit)
            break
        }
        case "Attributes": {
            replacement.value.remove.forEach((attributeKey: string) => {
                domElementToEdit.removeAttribute(attributeKey)
            })
            domElementAddAttributes(domElementToEdit, replacement.value.edit)
            break
        }
        case "AttributesNamespaced": {
            replacement.value.remove.forEach((attributeNamespacedId: { namespace: string, key: string }) => {
                domElementToEdit.removeAttributeNS(attributeNamespacedId.namespace, attributeNamespacedId.key)
            })
            domElementAddAttributesNamespaced(domElementToEdit, replacement.value.edit)
            break
        }
        case "StringProperties": {
            replacement.value.remove.forEach((propertyKey: string) => {
                (domElementToEdit as { [key: string]: any })[propertyKey] = ""
            })
            domElementSetStringProperties(domElementToEdit, replacement.value.edit)
            break
        }
        case "BoolProperties": {
            replacement.value.remove.forEach((propertyKey: string) => {
                (domElementToEdit as { [key: string]: any })[propertyKey] = false
            })
            domElementSetBoolProperties(domElementToEdit, replacement.value.edit)
            break
        }
        case "ScrollToPosition": {
            if (replacement.value !== null) {
                domElementToEdit.scrollTo({ top: replacement.value.fromTop, left: replacement.value.fromLeft })
            }
            break
        }
        case "ScrollToShow": {
            if (replacement.value !== null) {
                domElementToEdit.scrollIntoView({ inline: replacement.value.x, block: replacement.value.y })
            }
            break
        }
        case "ScrollPositionRequest": {
            domElementAddScrollPositionRequest(domElementToEdit, sendToElm)
            break
        }
        case "EventListens": {
            domListenAbortControllers.get(id)?.forEach(abortController => {
                abortController.abort()
            })
            domListenAbortControllers.delete(id)
            domElementAddEventListens(id, domElementToEdit, replacement.value, sendToElm)
            break
        }
    }
}

function getOrAddMeta(name: string): HTMLMetaElement {
    const maybeExistingMeta: HTMLMetaElement | undefined =
        Array.from(document.getElementsByTagName("meta"))
            .find(meta => meta.name === name)
    if (maybeExistingMeta !== undefined) {
        return maybeExistingMeta
    } else {
        var meta = window.document.createElement("meta")
        meta.name = name
        window.document.getElementsByTagName('"head"')[0]?.appendChild(meta)
        return meta
    }
}

function createDomNode(id: string, node: { tag: "Text" | "Element", value: any }, subs: NodeListOf<ChildNode>, sendToElm: (v: any) => void): Element | Text {
    switch (node.tag) {
        case "Text": {
            return document.createTextNode(node.value)
        }
        case "Element": {
            const createdDomElement: HTMLElement =
                node.value.namespace !== null ?
                    document.createElementNS(node.value.namespace, noScript(node.value.tag))
                    :
                    document.createElement(noScript(node.value.tag))

            domElementAddAttributes(createdDomElement, node.value.attributes)
            domElementAddAttributesNamespaced(createdDomElement, node.value.attributesNamespaced)
            domElementAddStyles(createdDomElement, node.value.styles)
            domElementSetStringProperties(createdDomElement, node.value.stringProperties)
            domElementSetBoolProperties(createdDomElement, node.value.boolProperties)
            if (node.value.scrollToPosition !== null) {
                node.value.scrollTo({ top: node.value.scrollToPosition.fromTop, left: node.value.scrollToPosition.fromLeft })
            }
            if (node.value.scrollToShow !== null) {
                node.value.scrollIntoView({ inline: node.value.scrollToShow.x, block: node.value.scrollToShow.y })
            }
            if (node.value.scrollPositionRequest === true) {
                domElementAddScrollPositionRequest(createdDomElement, sendToElm)
            }
            domElementAddEventListens(id, createdDomElement, node.value.eventListens, sendToElm)
            createdDomElement.append(...subs)
            return createdDomElement
        }
    }
}
function domElementAddScrollPositionRequest(domElement: Element, sendToElm: (v: any) => void) {
    // guarantee it has painted drawn at least once
    window.requestAnimationFrame(_timestamp => {
        window.requestAnimationFrame(_timestamp => {
            sendToElm({
                tag: "ScrollPositionRequest",
                value: { fromLeft: domElement.scrollLeft, fromTop: domElement.scrollTop }
            })
        })
    })
}
function domElementAddStyles(domElement: Element & ElementCSSInlineStyle, styles: { key: string, value: string }[]) {
    styles.forEach(styleSingle => {
        domElement?.style.setProperty(styleSingle.key, styleSingle.value)
    })
}
function domElementSetStringProperties(domElement: Element, properties: { key: string, value: string }[]) {
    const domElementIndexable = (domElement as { [key: string]: any })
    properties.forEach(property => {
        if ((Object.hasOwn(domElement, property.key))
            && (typeof domElementIndexable[property.key] !== "string")
        ) {
            warn(`tried to set the existing non-string dom element property "${property.key}" to a string.`)
        } else if ((property.key === "innerHTML") || (property.key === "outerHTML")) {
            window?.console.error("This is an XSS vector. Please parse the html string instead and construct the dom from that.")
        } else if (RE_js_html.test(property.value)) {
            window?.console.error("This is an XSS vector. Please use an interface instead.")
        } else if (property.key === "src" && RE_js_html.test(property.value)) {
            window?.console.error("This is an XSS vector. Please use an interface instead.")
        } else if (property.key === "action" || property.key === "href" && RE_js.test(property.value)) {
            window?.console.error("This is an XSS vector. Please use an interface instead.")
        } else {
            try {
                domElementIndexable[property.key] = property.value
            } catch (error) {
                warn("tried to set the string property " + property.key + " failed: " + error)
            }
        }
    })
}
function domElementSetBoolProperties(domElement: Element, properties: { key: string, value: boolean }[]) {
    const domElementIndexable = (domElement as { [key: string]: any })
    properties.forEach(property => {
        if ((Object.hasOwn(domElement, property.key))
            && (typeof domElementIndexable[property.key] !== "boolean")
        ) {
            warn(`tried to set the existing non-boolean dom element property "${property.key}" to a boolean.`)
        } else {
            try {
                domElementIndexable[property.key] = property.value
            } catch (error) {
                warn("tried to set the string property " + property.key + " failed: " + error)
            }
        }
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
function domElementAddEventListens(
    id: string,
    domElement: Element,
    eventListens: { name: string, defaultActionHandling: "DefaultActionPrevent" | "DefaultActionExecute" }[],
    sendToElm: (v: any) => void
) {
    const elementAbortControllers =
        eventListens.map(eventListen => {
            const abortController: AbortController = new AbortController()
            domElement.addEventListener(
                eventListen.name,
                (triggeredEvent) => {
                    sendToElm({ tag: "EventListen", value: { name: eventListen.name, event: triggeredEvent } })
                    switch (eventListen.defaultActionHandling) {
                        case "DefaultActionPrevent": {
                            triggeredEvent.preventDefault()
                            break
                        }
                        case "DefaultActionExecute": { break }
                    }
                },
                { signal: abortController.signal }
            )
            return abortController
        })
    domListenAbortControllers.set(id, elementAbortControllers)
}

// copied and edited from https://github.com/elm/virtual-dom/blob/master/src/Elm/Kernel/VirtualDom.js
// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why RE_js and RE_js_html look
// so freaky.

const RE_script = /^script$/i
const RE_on_formAction = /^(on|formAction$)/i;
const RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
const RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;

function noScript(tag: string) {
    return RE_script.test(tag) ? 'p' : tag
}
function noOnOrFormAction(key: string) {
    return RE_on_formAction.test(key) ? "data-" + key : key
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
    body: HttpRequestBody
}
type Expect = | "String" | "Bytes" | "Whatever"
type HttpRequestBody =
    | { tag: "Uint8Array", value: Uint8Array }
    | { tag: "String", value: string }
    | { tag: "Empty", value: null }

type HttpResponse =
    | { tag: "Success", value: ResponseSuccess }
    | { tag: "Error", value: any }
interface ResponseSuccess {
    body: Uint8Array | string | null
    url: string
    headers: { [header: string]: string }
    statusCode: number
    statusText: string
}

function httpRequestBodyForFetch(body: HttpRequestBody) {
    switch (body.tag) {
        case "Empty": return null
        case "String": return body.value
        case "Uint8Array": return new Blob([body.value])
    }
}
function httpFetch(request: HttpRequest, abortSignal: AbortSignal): Promise<HttpResponse> {
    return fetch(request.url, {
        method: request.method,
        body: httpRequestBodyForFetch(request.body),
        headers: new Headers(request.headers.map(header => {
            // removing the type makes ts think that  tuple: string[]
            const tuple: [string, string] = [header.name, header.value]
            return tuple
        })),
        signal: abortSignal
    })
        .then((response: Response) => {
            const headers = Object.fromEntries(response.headers.entries())
            switch (request.expect) {
                case "String": return response.text()
                    .then((x) => ({
                        tag: "Success" as const,
                        value: {
                            url: response.url,
                            headers: headers,
                            statusCode: response.status,
                            statusText: response.statusText,
                            body: x as (string | null | Uint8Array) // without this as ts complains
                        }
                    }))
                case "Bytes": return response.blob()
                    .then(blob => blob.arrayBuffer())
                    .then((x) => ({
                        tag: "Success" as const,
                        value: {
                            url: response.url,
                            headers: headers,
                            statusCode: response.status,
                            statusText: response.statusText,
                            body: new Uint8Array(x)
                        }
                    }))
                case "Whatever": return {
                    tag: "Success" as const,
                    value: {
                        url: response.url,
                        headers: headers,
                        statusCode: response.status,
                        statusText: response.statusText,
                        body: null
                    }
                }
            }
        })
        .catch(error => { return { tag: "Error", value: error } })
}


type AudioInfo = {
    url: string,
    startTime: number,
    volume: AudioParameterTimeline,
    speed: AudioParameterTimeline,
    stereoPan: AudioParameterTimeline,
    processing: AudioProcessingInfo[]
}
type AudioProcessingInfo =
    | { tag: "LinearConvolution", value: { sourceUrl: string } }
    | { tag: "Lowpass", value: { cutoffFrequency: AudioParameterTimeline } }
    | { tag: "Highpass", value: { cutoffFrequency: AudioParameterTimeline } }

type AudioParameterTimeline = {
    startValue: number,
    keyFrames: { time: number, value: number }[]
}

function audioSourceLoad(url: string, sendToElm: (v: any) => void, abortSignal: AbortSignal) {
    const audioContext = getOrInitializeAudioContext()
    fetch(url, { signal: abortSignal })
        .then(data => data.arrayBuffer())
        .then(arrayBuffer => audioContext.decodeAudioData(arrayBuffer))
        .then(buffer => {
            audioBuffers.set(url, buffer)
            sendToElm({
                tag: "Success", value: { durationInSeconds: buffer.length / buffer.sampleRate }
            })
        })
        .catch(error => { sendToElm({ tag: "Error", value: error?.message !== undefined ? error.message : "NetworkError" }) })
}

function audioParameterTimelineApplyTo(audioParam: AudioParam, timeline: AudioParameterTimeline) {
    const audioContext = getOrInitializeAudioContext()
    const currentTime = audioContext.currentTime
    audioParam.cancelScheduledValues(currentTime)
    audioParam.setValueAtTime(timeline.startValue, 0)
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

type AudioPlaying = {
    sourceNode: AudioBufferSourceNode,
    gainNode: GainNode,
    stereoPanNode: StereoPannerNode,
    processingNodes: AudioNode[]
}
function createAudio(config: AudioInfo, buffer: AudioBuffer): AudioPlaying {
    const audioContext = getOrInitializeAudioContext()
    const source = audioContext.createBufferSource()
    source.buffer = buffer
    audioParameterTimelineApplyTo(source.playbackRate, config.speed)

    const gainNode = audioContext.createGain()
    audioParameterTimelineApplyTo(gainNode.gain, config.volume)

    const stereoPannerNode = new StereoPannerNode(audioContext)
    audioParameterTimelineApplyTo(stereoPannerNode.pan, config.stereoPan)

    const processingNodes = createProcessingNodes(config.processing)

    forEachConsecutive(
        [source, gainNode, stereoPannerNode, ...processingNodes, audioContext.destination],
        pair => { pair.current.connect(pair.next) }
    )

    const currentTime = new Date().getTime()
    if (config.startTime >= currentTime) {
        source.start(posixToContextTime(config.startTime, currentTime), 0)
    } else {
        source.start(0, (currentTime - config.startTime) / 1000)
    }
    return {
        sourceNode: source,
        gainNode: gainNode,
        stereoPanNode: stereoPannerNode,
        processingNodes: processingNodes,
    }
}
function createProcessingNodes(processingFirstToLast: AudioProcessingInfo[]): AudioNode[] {
    const audioContext = getOrInitializeAudioContext()
    return processingFirstToLast
        .map(processing => {
            switch (processing.tag) {
                case "Lowpass": {
                    const biquadNode = new BiquadFilterNode(audioContext)
                    biquadNode.type = "lowpass"
                    audioParameterTimelineApplyTo(biquadNode.frequency, processing.value.cutoffFrequency)
                    return biquadNode
                }
                case "Highpass": {
                    const biquadNode = new BiquadFilterNode(audioContext)
                    biquadNode.type = "highpass"
                    audioParameterTimelineApplyTo(biquadNode.frequency, processing.value.cutoffFrequency)
                    return biquadNode
                }
                case "LinearConvolution": {
                    const convolverNode = new ConvolverNode(audioContext)
                    const buffer = audioBuffers.get(processing.value.sourceUrl)
                    if (buffer !== undefined) {
                        convolverNode.buffer = buffer
                    } else {
                        warn("tried to create a linear convolution from source that isn't loaded. Did you use Web.Audio.sourceLoad?")
                    }
                    return convolverNode
                }
            }
        })
}

type AudioEdit = {
    speed: null | AudioParameterTimeline,
    volume: null | AudioParameterTimeline,
    stereoPan: null | AudioParameterTimeline,
    processing: null | (AudioProcessingInfo[])
}

function editAudio(
    id: string,
    config: {
        url: string,
        startTime: number,
        replacement: AudioEdit
    }
) {
    const audioPlayingToEdit = audioPlaying.get(id)
    if (audioPlayingToEdit !== undefined) {
        if (config.replacement.volume !== null) {
            audioParameterTimelineApplyTo(audioPlayingToEdit.gainNode.gain, config.replacement.volume)
        }
        if (config.replacement.speed !== null) {
            audioParameterTimelineApplyTo(audioPlayingToEdit.sourceNode.playbackRate, config.replacement.speed)
        }
        if (config.replacement.stereoPan !== null) {
            audioParameterTimelineApplyTo(audioPlayingToEdit.stereoPanNode.pan, config.replacement.stereoPan)
        }
        if (config.replacement.processing !== null) {
            const audioContext = getOrInitializeAudioContext()
            audioPlayingToEdit.stereoPanNode.disconnect()
            audioPlayingToEdit.processingNodes.forEach(node => { node.disconnect() })

            audioPlayingToEdit.processingNodes = createProcessingNodes(config.replacement.processing)

            forEachConsecutive(
                [audioPlayingToEdit.stereoPanNode, ...audioPlayingToEdit.processingNodes, audioContext.destination],
                pair => { pair.current.connect(pair.next) }
            )
        }
    }
}

function askForNotificationPermissionIfNotAsked(): Promise<"granted" | "denied"> {
    switch (Notification.permission) {
        case "granted": return Promise.resolve("granted")
        case "denied": return Promise.resolve("denied")
        case "default":
            return Notification.requestPermission()
                .then(permission => {
                    switch (permission) {
                        case "granted": return "granted"
                        case "denied": return "denied"
                        case "default": return "denied"
                    }
                })
                .catch(_error => "denied")
    }
}

// helpers

function warn(warning: string) {
    window?.console.warn(warning + " (lue-bird/elm-state-interface)")
}
function notifyOfUnknownMessageKind(messageTag: string) {
    notifyOfBug("Unknown message kind " + messageTag + " from elm. The associated js implementation is missing")
}
function notifyOfBug(bugDescription: string) {
    window?.console.error("bug: " + bugDescription + ". Please open an issue on github.com/lue-bird/elm-state-interface")
}

function posixToContextTime(posix: number, currentTimePosix: number) {
    const audioContext = getOrInitializeAudioContext()
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
        if (current !== undefined && next !== undefined) { // should always work
            forPair({ current: current, next: next })
        }
    }
}
