export type Matcher = (state: ParserState, match: RegExpExecArray) => void

export interface Parser {
    regexp: string
    matched: Matcher
}

export interface DocumentBlock {
    element: Element
    inline: boolean
    text: string
    cont?: RegExp
}

export interface ParserState {
    input: string
    nextIndex: number
    blocks: DocumentBlock[]
}

export function parser(regexp: string, matched: Matcher): Parser {
    return { regexp, matched }
}

export function elem<K extends keyof HTMLElementTagNameMap>(tag: K, 
    ...children: Node[]): HTMLElementTagNameMap[K] {
    let res = document.createElement(tag)
    if (children.length > 0)
        res.append(...children)
    return res
}

export function text(data: string): Text {
    return document.createTextNode(data)
}

function openBlock(state: ParserState, element: Element, inline: boolean, 
    cont?: RegExp) {
    state.blocks.push({ element, inline, text: "", cont })
}

function closeLastBlock(state: ParserState) {
    state.blocks.pop()
}

function stateFrom(state: ParserState, input: string, nextIndex = 0):
    ParserState {
    return { input, nextIndex, blocks: state.blocks }
}

function lastBlock(state: ParserState): DocumentBlock {
    return state.blocks[state.blocks.length - 1]
}

function append(state: ParserState, ...nodes: Node[]) {
    lastBlock(state).element.append(...nodes)
}
/**
 * Get regular expression for a list of parsers.
 */
function regexpFor(parsers: Parser[]): RegExp {
    let re = parsers.map((p, i) => `(?<g${i}>${p.regexp})`).join("|")
    return new RegExp(re, "yu")
}
/**
 * Flush the input to result verbatim from current index until the specified 
 * position or end of input, if the position omitted.
 */
function flushInline(state: ParserState, index?: number) {
    if (!index || index > state.nextIndex)
        append(state, text(state.input.substring(state.nextIndex, index)))
}
/**
 * Select parser that matches the current state and run it.
 */
function parseNext(regexp: RegExp, parsers: Parser[], state: ParserState, 
    inline: boolean): boolean {
    regexp.lastIndex = state.nextIndex
    let match = regexp.exec(state.input)
    if (match && match.groups && 
        (inline || match.index == state.nextIndex)) {
        let parser = parsers.find((_, i) => match.groups![`g${i}`])
        if (parser) {
            if (inline)
                flushInline(state, match.index)
            else
                flushLastBlock(state)
            parser.matched(state, match)
            state.nextIndex = match.index + match[0].length
            return true
        }
    }
    return false
}
/**
 * Some reusabe regexp snippets.
 */
const wsOrPunct = (/[\s\p{P}\p{S}]/u).source
const emOrStrong = /(__?|\*\*?)/.source
const indentedCode = / {4}/yu
/**
 * Define inline parsers in priority order. 
 */
const inlineParsers = [
    parser(/\\(?<esc>.)/.source,
        (state, match) => {
            let { esc } = match.groups!
            append(state, esc == "\n" ? elem('br') : text(esc))
        }),
    parser(/(?<codedelim>`+)(?<code> .+ |[^`]+)\k<codedelim>/.source, 
        (state, match) => {
            let { code } = match.groups!
            let cnt = code.length
            if (cnt > 2 && code[0] == " " && code[cnt - 1] == " ")
                code = code.substring(1, cnt - 1)
            append(state, elem('code', text(code.replaceAll("\n", " "))))
        }),
    parser(/\[(?<link>(?:\\\[|\\\]|[^\[\]])+)\]\((?<linkdest>(?:\\\(|\\\)|[^\s()])+)\)/.source,
        (state, match) => {
            let { link, linkdest } = match.groups!
            linkdest = linkdest.replaceAll(/\\\(|\\\)/, str => str[1])
            let aelem = elem('a')
            aelem.href = linkdest
            openBlock(state, aelem, true)
            inlines(stateFrom(state, link))
            closeLastBlock(state)
        }),
    parser(/!\[(?<imgalt>(?:\\\[|\\\]|[^\[\]])+)\]\((?<imgsrc>(?:\\\(|\\\)|[^\s()])+)\)/.source,
        (state, match) => {
            let { imgalt, imgsrc } = match.groups!
            imgalt = imgsrc.replaceAll(/\\\[|\\\]/, str => str[1])
            imgsrc = imgsrc.replaceAll(/\\\(|\\\)/, str => str[1])
            let img = elem('img')
            img.src = imgsrc
            img.alt = imgalt
            append(state, img)
        }),
    parser(`(?<emdelim>${emOrStrong}(?!${wsOrPunct})|(?<=${wsOrPunct}|^)${
        emOrStrong})(?<em>.*)((?<!${wsOrPunct})\k<emdelim>|\k<emdelim>(?=${
        wsOrPunct}|$))`, 
        (state, match) => {
            let { emdelim, em } = match.groups!
            openBlock(state, elem(emdelim.length == 1 ? 'em' : 'strong'), true)
            inlines(stateFrom(state, em))
            closeLastBlock(state)
        }),
    parser(/<.+>/.source,
        (state, match) => {
            append(state, text(match[0]))
        })
]
let inlineRegexp: RegExp 
function getInlineRegexp(): RegExp {
    return inlineRegexp || (inlineRegexp = regexpFor(inlineParsers))
}

function inlines(state: ParserState) {
    while (parseNext(getInlineRegexp(), inlineParsers, state, true));
    flushInline(state)
}

const blockParsers = [
    parser(/ {0,3}(?<brkchar>[*\-_])(?:[ \t]*\k<brkchar>){2,}[ \t]*$/.source,
        (state,) => append(state, elem('hr'))),
    parser(/ {0,3}(?<atxlevel>#{1,6})[ \t]+(?<atxheader>.+?)[ \t]*$/.source,
        (state, match) => {
            let { atxlevel, atxheader } = match.groups!
            let level = atxlevel.length
            openBlock(state, elem(<keyof HTMLElementTagNameMap>`h${level}`), 
                true)
            inlines(stateFrom(state, atxheader))
            closeLastBlock(state)
        }),
    parser(indentedCode.source,
        (state,) => 
            openBlock(state, elem('pre', elem('code')), false, indentedCode))
]
const blockRegexp = regexpFor(blockParsers)

function flushLastBlock(state: ParserState) {
    let block = lastBlock(state)
    if (block.text.length > 0) {
        if (block.inline)
            inlines(stateFrom(state, block.text))
        else
            append(state, text(block.text))
    }
    block.text = ""
}

function closeBlocksToIndex(state: ParserState, index: number) {
    while (state.blocks.length > index) {
        flushLastBlock(state)
        closeLastBlock(state)
    }
}

function closeDiscontinuedBlocks(state: ParserState) {
    for (let i = 0; i < state.blocks.length; ++i) {
        let block = state.blocks[i]
        if (block.cont) {
            block.cont.lastIndex = state.nextIndex
            let match = block.cont.exec(state.input)
            if (!match || match.index != 0)
                return closeBlocksToIndex(state, i)
            state.nextIndex = block.cont.lastIndex
        }
    }
}

export function markdownToHtml(input: string, doc: Element) {
    let state: ParserState = {
        input,
        nextIndex: 0,
        blocks: []
    }
    openBlock(state, doc, true)
    let lines = input.split("\n")
    for (let i = 0; i < lines.length; ++i) {
        let line = lines[i]
        let st = stateFrom(state, line, 0)
        closeDiscontinuedBlocks(st)
        while (parseNext(blockRegexp, blockParsers, st, false));
        if (st.nextIndex < line.length)
            lastBlock(st).text += line.substring(st.nextIndex)
    }
    flushLastBlock(state)
}