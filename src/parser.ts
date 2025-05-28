interface ParserState {
    input: string
    nextIndex: number
    result: string[]
    blocks: RegExp[]
}

type Runner = (state: ParserState, match: RegExpExecArray) => void | RegExp

interface Parser {
    regexp: string
    run: Runner
    cont?: RegExp
}

function parser(regexp: string, run: Runner, cont?: RegExp): Parser {
    return { regexp, run, cont }
}

function stateFrom(state: ParserState, input: string, nextIndex = 0):
    ParserState {
    return { input, nextIndex, result: state.result, blocks: state.blocks }
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
        state.result.push(state.input.substring(state.nextIndex, index))
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
            parser.run(state, match)
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
            state.result.push(esc == "\n" ? "<br/>" : esc)
        }),
    parser(/(?<codedelim>`+)(?<code> .+ |[^`]+)\k<codedelim>/.source, 
        (state, match) => {
            let { code } = match.groups!
            let cnt = code.length
            if (cnt > 2 && code[0] == " " && code[cnt - 1] == " ")
                code = code.substring(1, cnt - 1)
            state.result.push(/*html*/`<code>${
                code.replaceAll("\n", " ")}</code>`)
        }),
    parser(/\[(?<link>(?:\\\[|\\\]|[^\[\]])+)\]\((?<linkdest>(?:\\\(|\\\)|[^\s()])+)\)/.source,
        (state, match) => {
            let { link, linkdest } = match.groups!
            linkdest = linkdest.replaceAll(/\\\(|\\\)/, str => str[1])
            state.result.push(/*html*/`<a href="${linkdest}">`)
            inlines(stateFrom(state, link))
            state.result.push("</a>")
        }),
    parser(/!\[(?<imgalt>(?:\\\[|\\\]|[^\[\]])+)\]\((?<img>(?:\\\(|\\\)|[^\s()])+)\)/.source,
        (state, match) => {
            let { imgalt, img } = match.groups!
            imgalt = img.replaceAll(/\\\[|\\\]/, str => str[1])
            img = img.replaceAll(/\\\(|\\\)/, str => str[1])
            state.result.push(/*html*/`<img href="${img}" alt="${imgalt}">`)
        }),
    parser(/<.+>/.source,
        (state, match) => {
            state.result.push(match[0])
        }),
    parser(`(?<emdelim>${emOrStrong}(?!${wsOrPunct})|(?<=${wsOrPunct}|^)${
        emOrStrong})(?<em>.*)((?<!${wsOrPunct})\k<emdelim>|\k<emdelim>(?=${
        wsOrPunct}|$))`, 
        (state, match) => {
            let { emdelim, em } = match.groups!
            state.result.push(emdelim.length == 1 ? "<em>" : "<strong>")
            inlines(stateFrom(state, em))
            state.result.push(emdelim.length == 1 ? "</em>" : "</strong>")
        })
]
const inlineRegexp = regexpFor(inlineParsers)

function inlines(state: ParserState) {
    while (selectNext(inlineRegexp, inlineParsers, state)) ;
}

const blockParsers = [
    parser(/^ {0,3}(?<brkchar>[*\-_])(?:[ \t]*\k<brkchar>){2,}[ \t]*$/.source,
        (state,) => {
            state.result.push("\n<hr/>")
        }),
    parser(/^ {0,3}(?<atxlevel>#{1,6})[ \t]+(?<atxheader>.+?)[ \t]*$/.source,
        (state, match) => {
            let { atxlevel, atxheader } = match.groups!
            let level = atxlevel.length
            state.result.push(`<h${level}>`)
            inlines(stateFrom(state, atxheader))
            state.result.push(`</h${level}>`)
        }),
]

export function markdownToHtml(input: string): string {
    let state: ParserState = {
        input,
        nextIndex: 0,
        result: [],
        blocks: []
    }
    let lines = input.split("\n")

    inlines(state)
    return state.result.join("")
}