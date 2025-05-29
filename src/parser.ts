type Opener = (state: ParserState, match: RegExpExecArray) => void
type Closer = (state: ParserState) => void

interface Parser {
    regexp: string
    open: Opener
}

interface DocumentBlock {
    element: HTMLElement
    cont?: RegExp
    close?: Closer
}

interface ParserState {
    input: string
    nextIndex: number
    blocks: DocumentBlock[]
}

function parser(regexp: string, run: Opener, cont?: RegExp): Parser {
    return { regexp, open: run }
}

function elem<K extends keyof HTMLElementTagNameMap>(tag: K, 
    ...children: Node[]): HTMLElementTagNameMap[K] {
    let res = document.createElement(tag)
    if (children.length > 0)
        res.append(...children)
    return res
}

function text(data: string): Text {
    return document.createTextNode(data)
}

function openBlock(state: ParserState, element: HTMLElement, cont?: RegExp, 
    close?: Closer) {
    state.blocks.push({ element, cont, close })
}

function closeBlock(state: ParserState) {
    state.blocks.pop()
}

function stateFrom(state: ParserState, input: string, nextIndex = 0):
    ParserState {
    return { input, nextIndex, blocks: state.blocks }
}

function append(state: ParserState, ...nodes: Node[]) {
    state.blocks[state.blocks.length - 1].element.append(...nodes)
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
function flush(state: ParserState, index?: number) {
    if (!index || index > state.nextIndex)
        append(state, text(state.input.substring(state.nextIndex, index)))
}
/**
 * Select parser that matches the current state and run it.
 */
function selectNext(regexp: RegExp, parsers: Parser[], state: ParserState):
    boolean {
    regexp.lastIndex = state.nextIndex
    let match = regexp.exec(state.input)
    if (match && match.groups) {
        let parser = parsers.find((_, i) => match.groups![`g${i}`])
        if (parser) {
            flush(state, match.index)
            parser.open(state, match)
            state.nextIndex = match.index + match[0].length
            return true
        }
    }
    flush(state)
    return false
}
/**
 * Some reusabe regexp snippets.
 */
const wsOrPunct = (/[\s\p{P}\p{S}]/u).source
const emOrStrong = /(__?|\*\*?)/.source
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
            openBlock(state, aelem)
            inlines(stateFrom(state, link))
            closeBlock(state)
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
            openBlock(state, elem(emdelim.length == 1 ? 'em' : 'strong'))
            inlines(stateFrom(state, em))
            closeBlock(state)
        }),
    parser(/<.+>/.source,
        (state, match) => {
            append(state, text(match[0]))
        })
]
const inlineRegexp = regexpFor(inlineParsers)

function inlines(state: ParserState) {
    while (selectNext(inlineRegexp, inlineParsers, state)) ;
}

const blockParsers = [
    parser(/^ {0,3}(?<brkchar>[*\-_])(?:[ \t]*\k<brkchar>){2,}[ \t]*$/.source,
        (state,) => {
            append(state, elem('hr'))
        }),
    parser(/^ {0,3}(?<atxlevel>#{1,6})[ \t]+(?<atxheader>.+?)[ \t]*$/.source,
        (state, match) => {
            let { atxlevel, atxheader } = match.groups!
            let level = atxlevel.length
            openBlock(state, elem(<keyof HTMLElementTagNameMap>`h${level}`))
            inlines(stateFrom(state, atxheader))
            closeBlock(state)
        }),
]

export function markdownToHtml(input: string): DocumentFragment {
    let state: ParserState = {
        input,
        nextIndex: 0,
        blocks: []
    }
    let lines = input.split("\n")

    inlines(state)
    return document.createDocumentFragment()
}