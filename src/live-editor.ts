/**
 * # Live Editor
 * 
 * This module contains the live editor that can be used to test MarkAppend.
 */
import { StyledElement } from 'litscript/lib/src/custom-elem'
import { elem } from './helpers'
import { appendMarkdown } from './parser'
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

Divided paragraph.`
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
            appendMarkdown(editor.value, preview)
        }
        editor.value = example
        appendMarkdown(editor.value, preview)
    }
}
/**
 * Register the custom element.
 */
customElements.define("live-editor", LiveEditor)