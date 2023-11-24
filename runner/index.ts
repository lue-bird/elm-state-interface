import * as fetchAdapter from "./http/fetch.js";
/*
import {
    DomError,
    DomElement,
    Viewport,
    SetViewportOptions,
    SetViewportOfOptions,
} from "./browser/index.js";
import * as dom from "./browser/dom.js"; */

export * from "./http/index.js";
export * from "./browser/index.js";

export interface ElmPorts {
    toJs: {
        subscribe: (callback: (fromElm: any) => void) => void;
    };
    fromJs: { send: (toElm: any) => void };
}

export function start(config: { ports: ElmPorts, domElement: HTMLElement }) {
    const interfaceImplementations: { on: (event: any) => any, run: (config: any, sendToElm: (v: any) => void) => void }[] = [
        {
            on: event => event?.addRequestTimeNow,
            run: (_config, sendToElm) => {
                sendToElm(Date.now())
            },
        },
        {
            on: event => event?.addRequestTimezoneOffset,
            run: (_config, sendToElm) => {
                // // Equivalent Elm Kernel code: https://github.com/elm/time/blob/1.0.0/src/Elm/Kernel/Time.js#L38-L52
                sendToElm(-new Date().getTimezoneOffset())
            }
        },
        {
            on: event => event?.addRequestTimezoneName,
            run: (_config, sendToElm) => {
                sendToElm(getTimezoneName())
            }
        },
        {
            on: event => event?.addConsoleLog,
            run: (config, _sendToElm) => {
                console.log(config)
            },
        },
        {
            on: event => event?.replaceDomNode,
            run: (config, sendToElm) => {
                renderDomNode(config.path, config.domNode, sendToElm)
            }
        },
        {
            on: event => event?.removeDom,
            run: (_config, _sendToElm) => {
                config.domElement.replaceChildren()
            }
        },
        {
            on: event => event?.addHttpRequest,
            run: (config, sendToElm) => {
                const abortController = new AbortController()
                httpRequestAbortControllers[config] = abortController
                fetchAdapter.http(config, abortController).then(response => { sendToElm(response) })
            }
        },
        {
            on: event => event?.removeHttpRequest,
            run: (config, _sendToElm) => {
                const maybeAbortController = httpRequestAbortControllers[config]
                if (maybeAbortController) {
                    maybeAbortController.abort()
                }
            }
        },
        {
            on: event => event?.addWindowEventListen,
            run: windowEventListenAdd
        },
        {
            on: event => event?.removeWindowEventListen,
            run: windowEventListenRemove
        },
        {
            on: event => event?.addWindowAnimationFrameListen,
            run: (_config, sendToElm) => { addAnimationFrameListen(sendToElm) }
        },
        {
            on: event => event?.removeAnimationFrameListen,
            run: (_config, _sendToElm) => { removeAnimationFrameListen() }
        },
        {
            on: event => event?.addDocumentEventListen,
            run: documentEventListenAdd
        },
        {
            on: event => event?.removeDocumentEventListen,
            run: documentEventListenRemove
        },
        {
            on: event => event?.addNavigationGo,
            run: (config, _sendToElm) => { go(config) }
        },
        {
            on: event => event?.addReplaceUrl,
            run: (config, _sendToElm) => { replaceUrl(config) }
        },
        {
            on: event => event?.addPushUrl,
            run: (config, _sendToElm) => { pushUrl(config) }
        },
        {
            on: event => event?.addLoad,
            run: (config, _sendToElm) => { load(config) }
        },
        {
            on: event => event?.addReload,
            run: (_config, _sendToElm) => { reload() }
        }
    ]


    config.ports.toJs.subscribe(function (fromElm) {
        // console.log("elm → js: ", fromElm)
        function sendToElm(eventData: void) {
            const toElm = { diff: fromElm, eventData: eventData }
            config.ports.fromJs.send(toElm)
            // console.log("js → elm: ", toElm)
        }

        interfaceImplementations.forEach(interfaceImplementation => {
            if (interfaceImplementation.on(fromElm)) {
                const specific = interfaceImplementation.on(fromElm)
                interfaceImplementation.run(specific, sendToElm)
            }
        })
    });

    function renderDomNode(path: number[], node: any, sendToElm: (v: any) => void) {
        const createdDomNode = createDomNode([], node, sendToElm)
        if (path.length === 0) {
            const parentDomNode = config.domElement
            parentDomNode.replaceChildren() // remove all subs
            parentDomNode.appendChild(createdDomNode)
        } else {
            let parentDomNode: ChildNode | null = config.domElement.firstChild
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

// Equivalent Elm Kernel code: https://github.com/elm/time/blob/1.0.0/src/Elm/Kernel/Time.js#L27-L35
function getTimezoneName(): string | number {
    try {
        return Intl.DateTimeFormat().resolvedOptions().timeZone;
    } catch (e) {
        return new Date().getTimezoneOffset();
    }
}

function createDomNode(innerPath: number[], node: any, sendToElm: (v: any) => any): HTMLElement | Text {
    if (node?.text) {
        return document.createTextNode(node.text)
    } else { // if (node?.element)
        const createdDomElement: HTMLElement = document.createElement(noScript(node.element.tag))
        for (let [attributeKey, attributeValue] of Object.entries(node.element.attributes)) {
            createdDomElement.setAttribute(attributeKey, attributeValue as string)
        }
        for (let [styleKey, styleValue] of Object.entries(node.element.styles)) {
            createdDomElement.style.setProperty(styleKey, styleValue as string)
        }
        node.element.eventListeners.forEach((eventListenName: string) => {
            createdDomElement.addEventListener(
                eventListenName,
                (triggeredEvent) => {
                    sendToElm({ innerPath: innerPath, name: eventListenName, event: triggeredEvent })
                }
            )
        })
        for (let subIndex = 0; subIndex <= node.element.subs.length - 1; subIndex++) {
            const sub = node.element.subs[subIndex]
            createdDomElement.appendChild(
                createDomNode([subIndex].concat(innerPath), sub, sendToElm)
            )
        }
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
const RE_script = /^script$/i;
function noScript(tag: string) {
    return RE_script.test(tag) ? 'p' : tag
}

function windowEventListenAdd(eventName: string, sendToElm: (v: any) => any) {
    (window as { [key: string]: any })["on" + eventName] = sendToElm;
}
function windowEventListenRemove(eventName: string) {
    (window as { [key: string]: any })["on" + eventName] = null
}

function documentEventListenAdd(eventName: string, sendToElm: (v: any) => any) {
    (window as { [key: string]: any })["on" + eventName] = sendToElm;
}
function documentEventListenRemove(eventName: string) {
    (window as { [key: string]: any })["on" + eventName] = null
}

function go(urlSteps: number) {
    history.go(urlSteps);
}

function pushUrl(url: string) {
    history.pushState({}, '', url);
}

function replaceUrl(url: string) {
    history.replaceState({}, '', url);
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

let runningAnimationFrameLoopId: number | undefined = undefined;
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
