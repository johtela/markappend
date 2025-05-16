interface ParserState {
    input: string
    result: string[]
    nextIndex: number
}

type Matcher = (state: ParserState, match: string) => number | void

interface Parser {
    regexp: string
    matcher: Matcher
}

const notSpace = /[^ ]/
const ecapedCodeSpan = / (.*) /

function parser(regexp: string, matcher: Matcher) {
    return { regexp, matcher }
}
/**
 * Get regular expression for a list of parsers.
 */
function regexpFor(parsers: Parser[]): RegExp {
    let re = parsers.map((p, i) => `(?<g${i}>${p.regexp})`).join("|")
    return new RegExp(re, "yu")
}
/**
 * Select parser that matches the current state and run it.
 */
function select(regexp: RegExp, parsers: Parser[], state: ParserState) {
    regexp.lastIndex = state.nextIndex
    let match = regexp.exec(state.input)
    if (match && match.groups) {
        for (let i = 0; i < parsers.length; i++) {
            let res = match.groups[`g${i}`]
            if (res) {
            }
        }
    }
}
/**
 * Define inline parsers in priority order. 
 */
const inlineParsers = [
    parser("`+", (state, match) => {
        let cnt = match.length
        let close = new RegExp(`(.*)\`{${cnt}}`, "yu")
        close.lastIndex = state.nextIndex + cnt
        let res = close.exec(state.input)
        if (res) {
            let escaped = ecapedCodeSpan.exec(res[1])
            state.result.push(/*html*/`<code>${
                escaped && notSpace.test(escaped[1]) ? 
                    escaped[1] : res[1]
                }</code>`)
            return close.lastIndex
        }
    })
]
const inlineRegexp = regexpFor(inlineParsers)

export function markdownToHtml(input: string): string {
    let state: ParserState = {
        input,
        result: [],
        nextIndex: 0
    }

    return state.result.join("")
}