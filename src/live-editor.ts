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
        let ta = elem('textarea')
        ta.value = ""
        
        this.root.append(ta)
    }    
}