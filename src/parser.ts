interface ParserState {
    input: string
    nextIndex: number
    result: string[]
}

type Runner = (state: ParserState, match: RegExpExecArray) => number | void

interface Parser {
    regexp: string
    run: Runner
}

const wsOrPunct = (/[\s\p{P}\p{S}]/u).source
const emOrStrong = /(__?|\*\*?)/.source

function parser(regexp: string, run: Runner): Parser {
    return { regexp, run }
}

function stateFrom(state: ParserState, input: string, nextIndex = 0):
    ParserState {
    return { input, nextIndex, result: state.result }
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
            let next = parser.run(state, match)
            if (next) {
                state.nextIndex = next
                return true
            }
        }
    }
    flush(state)
    return false
}
/**
 * Define inline parsers in priority order. 
 */
const inlineParsers = [
    parser(/(`+)(?<code> .+ |[^`]+)\1/.source, 
        (state, match) => {
            let { code } = match.groups!
            let cnt = code.length
            if (cnt > 2 && code[0] == " " && code[cnt - 1] == " ")
                code = code.substring(1, cnt - 1)
            state.result.push(/*html*/`<code>${
                code.replaceAll("\n", " ")}</code>`)
            return match.index + match[0].length
        }),
    // (?<emdelim>(__?|\*\*?)(?![\s\p{P}\p{S}])|(?<=[\s\p{P}\p{S}]|^)(__?|\*\*?))(?<em>.*)((?<![\s\p{P}\p{S}])\k<emopen>|\k<emopen>(?=[\s\p{P}\p{S}]|$))
    parser(`(?<emdelim>${emOrStrong}(?!${wsOrPunct})|(?<=${wsOrPunct}|^)${
        emOrStrong})(?<em>.*)((?<!${wsOrPunct})\k<emdelim>|\k<emdelim>(?=${
        wsOrPunct}|$))`, 
        (state, match) => {
            let { emdelim, em } = match.groups!
            state.result.push(emdelim.length == 1 ? "<em>" : "<strong>")
            inlines(stateFrom(state, em))
            state.result.push(emdelim.length == 1 ? "</em>" : "</strong>")
            return match.index + match[0].length
        })
]
const inlineRegexp = regexpFor(inlineParsers)

function inlines(state: ParserState) {
    while (selectNext(inlineRegexp, inlineParsers, state)) ;
}

export function markdownToHtml(input: string): string {
    let state: ParserState = {
        input,
        nextIndex: 0,
        result: []
    }
    inlines(state)
    return state.result.join("")
}