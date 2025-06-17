/**
 * # Helper Functions
 * 
 * Helper functions for creating DOM elements.
 * 
 * The `elem` function creates an HTML element of the specified `tag` type.
 * Optionally, any number of child nodes can be appended to the created element.
 * This provides a convenient way to construct DOM trees.
 */
export function elem<K extends keyof HTMLElementTagNameMap>(tag: K, 
    ...children: Node[]): HTMLElementTagNameMap[K] {
    let res = document.createElement(tag)
    if (children.length > 0)
        res.append(...children)
    return res
}
/**
 * Creates a text node containing the specified string data.
 */
export function text(data: string): Text {
    return document.createTextNode(data)
}
/**
 * This function replaces all whitespace characters in the given string
 * with visible Unicode symbols for easier debugging of test output.
 * 
 * - Tabs ("\t") are replaced with "⇥"
 * - Newlines ("\n") are replaced with "↩"
 * - Spaces (" ") are replaced with "␣"
 * 
 * Other whitespace characters are left unchanged.
 */
export function highlightWs(value: string): Node[] {
    let res: Node[] = []
    let ws = /\s/g
    let pos = 0
    let match: RegExpExecArray | null
    while (match = ws.exec(value)) {
        if (pos < match.index)
            res.push(text(value.substring(pos, match.index)))
        pos = match.index + match.length
        let span = elem('span', text(wsText(match[0])))
        span.className = "ws"
        res.push(span)
    }
    if (pos < value.length)
        res.push(text(value.slice(pos)))
    return res
}

function wsText(ch: string): string {
    let res: Element
    switch (ch) {
        case "\t": return "→"
        case "\n": return "↩\n"
        case " ": return "·"
        default: return ch
    }
}
