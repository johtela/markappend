/**
 * # Live Editor
 * 
 * This module contains the live editor that can be used to test MarkAppend.
 */
import { StyledElement } from 'litscript/lib/src/custom-elem'
import { elem } from './helpers'
import * as pr from './parser'
import * as am from 'asciimath2ml'
import './live-editor.css'
/**
 * Define an example CommonMark text as initial value.
 */
const example = `# Sample Markdown

## Introduction

This is a sample CommonMark file demonstrating basic Markdown syntax.

## Lists

- Item 1
- Item 2
    - Subitem 2.1
    - Subitem 2.2

## Code

\`\`\`python
def hello_world():
        print("Hello, world!")
\`\`\`

## Links

[CommonMark Specification](https://commonmark.org/)

## Images

![Pencil](images/pencil-solid.svg)

## Blockquote

> Markdown is a lightweight markup language.

## Emphasis

*Italic* and **bold** text.

***

## Extensibility

It's possible to extend the parser with custom inline parsers. For example,
[AsciiMath](https://asciimath.org/) equations like this:

$$sum_{i=1}^n i^3=({n(n+1)}/2)^2$$`
/**
 * ## Editor Web Component
 * 
 * We utilize the base component from [LiTScript] to create the Markdown editor.
 * We only need to define override the constructor and `connect` method to set 
 * up the component.
 * 
 * [LiTScript]: https://johtela.github.io/litscript/
 */
export class LiveEditor extends StyledElement {
    /**
     * Pass name of associated the style file (without `.css` extension) to the
     * parent constructor.
     */
    constructor() {
        super("live-editor")
        pr.addInlineParser({
            regexp: /(?<![\\$])(?<eqdelim>\$\$?)(?!\$)(?<eq>.+?)(?<![\\$])\k<eqdelim>(?!\$)/.source,
            matched: (state, match) => {
                let { eqdelim, eq } = match.groups!
                let html = am.asciiToMathML(eq, eqdelim.length == 1)
                pr.appendHtml(state, html)
            }
        })
    }
    /**
     * Create a textarea and div elements for editor and preview. Update the
     * contents of the div whenever the textarea changes.
     */
    protected override connect() {
        this.body.className = "body"
        let editor = elem('textarea')
        editor.name = "editor"
        editor.title = "Enter markdown here"
        let preview = elem('div')
        preview.className = "preview"
        this.body.append(editor, preview)
        editor.oninput = () => {
            while (preview.lastChild)
                preview.removeChild(preview.lastChild)
           pr.appendMarkdown(editor.value, preview)
        }
        editor.value = example
        pr.appendMarkdown(editor.value, preview)
    }
}
/**
 * Register the custom element.
 */
customElements.define("live-editor", LiveEditor)