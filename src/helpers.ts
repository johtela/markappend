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
export function ws(value: string): string {
    return value.replaceAll(/\s/g, ch => {
        switch (ch) {
            case "\t": return "→"
            case "\n": return "↩\n"
            case " ": return "·"
            default: return ch
        }
    })
}