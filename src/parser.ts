interface ParserState {
    input: string
    nextIndex: number
    result: string[]
}

type Matcher = (state: ParserState, match: RegExpExecArray) => number | void

interface Parser {
    regexp: string
    matcher: Matcher
}

const notSpace = /[^ ]/

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
function select(regexp: RegExp, parsers: Parser[], state: ParserState): 
    boolean {
    regexp.lastIndex = state.nextIndex
    let match = regexp.exec(state.input)
    if (match && match.groups) {
        let parser = parsers.find((_, i) => match.groups![`g${i}`])
        if (parser) {
            let next = parser.matcher(state, match)
            if (next) {
                state.nextIndex = next
                return true
            }
        }
    }
    return false
}
/**
 * Define inline parsers in priority order. 
 */
const inlineParsers = [
    parser("(?<codeopen>`+)(?<codeinner> .+ |[^`]+)(?<codeclose>`+)", 
        (state, match) => {
            let { codeopen, codeinner, codeclose } = match.groups!
            if (codeopen.length != codeclose.length) {
                state.result.push(codeopen)
                return match.index + codeopen.length
            }
            let cnt = codeinner.length
            if (cnt > 2 && codeinner[0] == " " && codeinner[cnt - 1] == " ")
                codeinner = codeinner.substring(1, cnt - 1)
            state.result.push(/*html*/`<code>${codeinner.replaceAll("\n", " ")
                }</code>`)
            return match.index + match[0].length
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