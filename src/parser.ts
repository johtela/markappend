/**
 * # Markdown Parser
 * 
 * The parser code lives in this single module. Notice we don't import anything, 
 * so there are no dependencies to other libraries. The parsing process matches 
 * pretty  closely the strategy outlined in the [CommonMark specification][]. 
 * However, the parser is not fully CommonMark compliant. The most obscure 
 * rules have been omitted in purpose to keep the code simple.
 *
 * ## Matchers and Parsers
 * 
 * We start by defining a _matcher_, a function that is called when a regular 
 * expression matches a pattern that represents some markdown element. It gets 
 * the current parser state and the match(es) as arguments. We need to provide 
 * a matcher for all markdown [blocks and inlines][].
 * 
 * [CommonMark specification]: https://spec.commonmark.org/0.31.2/#appendix-a-parsing-strategy
 * [blocks and inlines]: https://spec.commonmark.org/0.31.2/#blocks-and-inlines
 */
export type Matcher = (state: ParserState, match: RegExpExecArray) => void
/**
 * A matcher is wrapped to a _parser_ interface which couples the regular
 * expression and matcher together.
 */
export interface Parser {
    regexp: string
    matched: Matcher
}
/**
 * ## Blocks
 * 
 * A markdown document is split into _blocks_. A block corresponds to a HTML
 * a element whose `display` attribute is `block`. For example, headers, lists, 
 * paragraphs are blocks. Blocks in turn contain _inline_ elements. For example, 
 * links, emphasized or preformatted text. We store information about open 
 * blocks in the DocumentBlock interface.
 */
export interface DocumentBlock {
    /**
     * The element that corresponds to the block.
     */
    element: Element
    /**
     * Sometimes the child elements should go under a different parent element.
     * For those cases, you can set `parent` different for `element`.
     */
    parent: Element
    /**
     * This flag tells whether the block contains inline elements or just
     * verbatim text. This information is needed when the block is flushed.
     */
    inline: boolean
    /**
     * Another flag that tells whether this is a leaf block or container block.
     * A leaf block cannot contain child blocks, but a container block can.
     */
    leaf: boolean
    /**
     * The parsed markdown lines that belong to this block.
     */
    lines: string[]
    /**
     * Optional regular expression that that must match the start of 
     * subsequent markdown lines in  this block.
     */
    cont?: RegExp
}
/**
 * ## Parser State
 * 
 * The parser state is passed along to the parsers when they are called. The
 * state contains:
 * 
 *  -   the input string, 
 *  -   the next index to be parsed in the input, and 
 *  -   stack of open blocks.
 */
export interface ParserState {
    input: string
    nextIndex: number
    blocks: DocumentBlock[]
}
/**
 * ## Constructors
 * 
 * Here are helper functions to create parsers and elements.
 */
export function parser(matched: Matcher, regexp: string): Parser {
    return { matched, regexp }
}
/**
 * Create a new parser state by extending the given state. We will reuse
 * the blocks, but specify new input string, and optionally, starting position
 * for the parsers.
 */
function stateFrom(state: ParserState, input: string, nextIndex = 0):
    ParserState {
    return { input, nextIndex, blocks: state.blocks }
}
/**
 * The `elem` function creates an element with specified `tag`. You can
 * optionally add some `children` nodes to the result.
 */
export function elem<K extends keyof HTMLElementTagNameMap>(tag: K, 
    ...children: Node[]): HTMLElementTagNameMap[K] {
    let res = document.createElement(tag)
    if (children.length > 0)
        res.append(...children)
    return res
}
/**
 * The `text` function creates a text node with given `data` as content.
 */
export function text(data: string): Text {
    return document.createTextNode(data)
}
/**
 * ## Opening and Closing Blocks
 * 
 * When opening a new block, we must specify all the fields of the 
 * DocumentBlock interface. Some of them have default values, but you need to
 * specify at least the parser state, the element, and whether to open an
 * inline block. By default, we open a leaf block with same parent as the 
 * element, and with no continuation regexp. The lines of the block will
 * always be initialized to empty.
 */
function openBlock(state: ParserState, element: Element, inline: boolean, 
    leaf = true, parent = element, cont?: RegExp) {
    state.blocks.push({ element, parent, inline, leaf, lines: [], cont })
}
/**
 * Close the last opened block. The list of blocks in the parser state is a
 * stack structure. You can only modify the topmost block of the stack. When 
 * a block is closed, it's element is appended under the parent block's 
 * element.
 */
function closeLastBlock(state: ParserState) {
    let block = state.blocks.pop()
    lastBlock(state)?.parent!.append(block!.element)
}
/**
 * A helper function that returns the last (open) block of the parser state.
 */
function lastBlock(state: ParserState): DocumentBlock {
    return state.blocks[state.blocks.length - 1]
}
/**
 * Another helper that appends nodes under the topmost open block.
 */
function append(state: ParserState, ...nodes: Node[]) {
    lastBlock(state).parent.append(...nodes)
}
/**
 * ## Constructing Parsers
 * 
 * The parsers are triggered by regular expressions. This function combines 
 * the regexps for a list of parsers and returns a new regexp that matches any 
 * of them. A parser with index _i_ in the list ill correspond to a named match 
 * group `g`_i_. Resulted regexp is"sticky" meaning that it will update 
 * `nextIndex` property when match is found.
 */
function regexpFor(parsers: Parser[]): RegExp {
    let re = parsers.map((p, i) => `(?<g${i}>${p.regexp})`).join("|")
    return new RegExp(re, "yu")
}
/**
 * Once the combined regexp is created we can use it to return the next token 
 * from an input string. The `parseNext` function takes this regexp, a list of 
 * parsers specified for the previous function, a parser state, and a flag that 
 * indiciates whether we are parsing blocks or inline content. It returns `true`
 * if any of the parsers matched. In that case the function will:
 * 
 *  1. flush the text preceding the match, 
 *  2. trigger the matcher function of the activated parser, and 
 *  3. advance the `nextIndex` position.
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
 * ## Reusable RegExps
 * 
 * We define some reusabe regexp snippets used in the parsers.
 *
 * The first will match whitespace or unicode punctuation characters. 
 */
const wsOrPunct = (/[\s\p{P}\p{S}]/u).source
/**
 * This one matches one or two consequtive `_` or `*` characters. These
 * delimit `em` and `bold` inlines.
 */
const emOrStrong = /(__?|\*\*?)/.source
/**
 * Indented code blocks start with at least 4 spaces or one tab character.
 */
const indentedCode = / {4}| {0,3}\t/yu
/**
 * ## Inline Parsers
 * 
 * Flushing inlines appends the unprocessed input to current block verbatim 
 * from current index until the specified position or end of input, if the 
 * position omitted.
 */
function flushInline(state: ParserState, index?: number) {
    if (!index || index > state.nextIndex)
        append(state, text(state.input.substring(state.nextIndex, index)))
}
/**
 * Now we can define all inline parsers in priority order. 
 */
const inlineParsers = [
    parser(
        /**
         * ### Escapes
         * 
         * You can escape all special characters by prepending them with a 
         * backslash. However if the backslash is at the end of a line, it
         * produces a `<br>` element.
         */
        (state, match) => {
            let { esc } = match.groups!
            append(state, esc == "\n" ? elem('br') : text(esc))
        },
        /\\(?<esc>.)/.source),
    parser(
        /**
         * ### Code Spans
         * 
         * Code spans are delimited with backticks. There are some rules about
         * how backticks are escaped in [section 6.1](https://spec.commonmark.org/0.31.2/#code-spans)
         * of CommonMark specification. We implement them mostly.
         */
        (state, match) => {
            let { code } = match.groups!
            let cnt = code.length
            if (cnt > 2 && code[0] == " " && code[cnt - 1] == " ")
                code = code.substring(1, cnt - 1)
            append(state, elem('code', text(code.replaceAll("\n", " "))))
        },
        /(?<codedelim>`+)(?<code> .+ |[^`]+)\k<codedelim>/.source),
    parser(
        /**
         * ### Links
         * 
         * We support only the simple link format and omit most of the obscure
         * rules in [section 6.3](https://spec.commonmark.org/0.31.2/#links).
         */
        (state, match) => {
            let { link, linkdest } = match.groups!
            linkdest = linkdest.replaceAll(/\\\(|\\\)/, str => str[1])
            let aelem = elem('a')
            aelem.href = linkdest
            openBlock(state, aelem, true)
            inlines(stateFrom(state, link))
            closeLastBlock(state)
        },
        /\[(?<link>(?:\\\[|\\\]|[^\[\]])+)\]\((?<linkdest>(?:\\\(|\\\)|[^\s()])+)\)/.source),
    parser(
        (state, match) => {
            let { imgalt, imgsrc } = match.groups!
            imgalt = imgsrc.replaceAll(/\\\[|\\\]/, str => str[1])
            imgsrc = imgsrc.replaceAll(/\\\(|\\\)/, str => str[1])
            let img = elem('img')
            img.src = imgsrc
            img.alt = imgalt
            append(state, img)
        },
        /!\[(?<imgalt>(?:\\\[|\\\]|[^\[\]])+)\]\((?<imgsrc>(?:\\\(|\\\)|[^\s()])+)\)/.source),
    parser( 
        (state, match) => {
            let { emdelim, em } = match.groups!
            openBlock(state, elem(emdelim.length == 1 ? 'em' : 'strong'), true)
            inlines(stateFrom(state, em))
            closeLastBlock(state)
        },
        `(?<emdelim>${emOrStrong}(?!${wsOrPunct})|(?<=${wsOrPunct}|^)${
        emOrStrong})(?<em>.*)((?<!${wsOrPunct})\k<emdelim>|\k<emdelim>(?=${
        wsOrPunct}|$))`),
    parser(
        (state, match) => {
            append(state, text(match[0]))
        },
        /<.+>/.source)
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
    parser(
        (state,) => append(state, elem('hr')),
        / {0,3}(?<brkchar>[*\-_])(?:[ \t]*\k<brkchar>){2,}[ \t]*$/.source),
    parser(
        (state, match) => {
            let { atxlevel, atxheader } = match.groups!
            let level = atxlevel.length
            openBlock(state, elem(<keyof HTMLElementTagNameMap>`h${level}`), 
                true)
            inlines(stateFrom(state, atxheader))
            closeLastBlock(state)
        },
        / {0,3}(?<atxlevel>#{1,6})[ \t]+(?<atxheader>.+?)[ \t]*$/.source),
    parser(
        (state,) => {
            let code = elem('code')
            openBlock(state, elem('pre', code), false, true, code,
                indentedCode)
        },
        indentedCode.source)
]
const blockRegexp = regexpFor(blockParsers)

function flushLastBlock(state: ParserState) {
    let block = lastBlock(state)
    if (block.lines.length > 0) {
        let lines = block.lines.join("\n")
        if (block.inline)
            inlines(stateFrom(state, lines))
        else
            append(state, text(lines))
        block.lines = []
    }
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
    openBlock(state, doc, true, false)
    let lines = input.split("\n")
    for (let i = 0; i < lines.length; ++i) {
        let line = lines[i]
        let st = stateFrom(state, line, 0)
        closeDiscontinuedBlocks(st)
        while (!lastBlock(st).leaf && 
            parseNext(blockRegexp, blockParsers, st, false));
        if (st.nextIndex < line.length) 
            lastBlock(st).lines.push(line.substring(st.nextIndex))
    }
    flushLastBlock(state)
    closeLastBlock(state)
}