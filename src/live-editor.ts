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
const example = `# Sample CommonMark Document

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

![Markdown Logo](https://markdown-here.com/img/icon256.png)

## Blockquote

> Markdown is a lightweight markup language.

## Emphasis

*Italic* and **bold** text.`
/**
 * ## Editor Web Component
 * 
 * We use the base component from [LiTScript] library to create the Markdown
 * editor.
 */
export class LiveEditor extends StyledElement {
    constructor() {
        super("live-editor")
    }

    protected override connect() {
        let editor = elem('textarea')
        editor.name = "editor"
        editor.title = "Enter markdown here"
        let preview = elem('div')
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
customElements.define("live-editor", LiveEditor)