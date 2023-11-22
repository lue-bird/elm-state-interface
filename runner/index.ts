import * as fetchAdapter from "./http/fetch.js";
/*
import {
    DomError,
    DomElement,
    Viewport,
    SetViewportOptions,
    SetViewportOfOptions,
} from "./browser";
import * as dom from "./browser/dom"; */

export * from "./http/index.js";
export * from "./browser/index.js";

export interface ElmPorts {
    toJs: {
        subscribe: (callback: (fromElm: any) => void) => void;
    };
    fromJs: { send: (toElm: any) => void };
}

export function start(ports: ElmPorts, appElement: HTMLElement) {
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
                // console.log("replace dom node ", config.path)
                renderDomNode(config.path, config.domNode, sendToElm)
            }
        },
        {
            on: event => event?.removeDom,
            run: (_config, _sendToElm) => {
                appElement.replaceChildren()
            }
        },
        {
            on: event => event?.http,
            run: (config, sendToElm) => {
                fetchAdapter.http(config).then(response => { sendToElm(response) })
            }
        }
    ]


    ports.toJs.subscribe(function (fromElm) {
        // console.log("elm → js: ", fromElm)
        function sendToElm(eventData: void) {
            const toElm = { diff: fromElm, eventData: eventData }
            ports.fromJs.send(toElm)
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
            const parentDomNode = appElement
            parentDomNode.replaceChildren() // remove all subs
            parentDomNode.appendChild(createdDomNode)
        } else {
            let parentDomNode: ChildNode | null = appElement.firstChild
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
        node.element.eventListeners.forEach((eventListenerName: string) => {
            createdDomElement.addEventListener(
                eventListenerName,
                (triggeredEvent) => {
                    sendToElm({ innerPath: innerPath, name: eventListenerName, event: triggeredEvent })
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