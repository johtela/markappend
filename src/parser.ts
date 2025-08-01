/**
 * # Markdown Parser
 *
 * This module implements a self-contained Markdown parser. It does not import 
 * or depend on any external libraries. The parsing process closely follows the 
 * strategy outlined in the [CommonMark specification][].
 *
 * Note: The parser is not fully CommonMark compliant. Some of the more obscure 
 * rules have been intentionally omitted to keep the code simple.
 */
import { elem, text, ExpAuto } from './helpers'
/**
 *
 * ## Matchers and Parsers
 *
 * We start by defining a _matcher_, a function that is called when a regular 
 * expression matches a pattern representing a Markdown element. It receives the 
 * current parser state and the match(es) as arguments. A matcher must be 
 * provided for all Markdown [blocks and inlines][].
 *
 * [CommonMark specification]: 
 * https://spec.commonmark.org/0.31.2/#appendix-a-parsing-strategy
 * [blocks and inlines]: https://spec.commonmark.org/0.31.2/#blocks-and-inlines
 */
export type Matcher = (state: ParserState, match: RegExpExecArray) => void
/**
 * A matcher is combined with its corresponding regular expression in the
 * `Parser` interface. This interface associates a regular expression pattern
 * with the function that handles matches for that pattern.
 */
export interface Parser {
    regexp: string
    matched: Matcher
}
/**
 * ## Blocks
 *
 * A Markdown document is parsed as a sequence of blocks, each representing a 
 * structural element such as a paragraph, heading, list, code block, or HTML 
 * block. Each block corresponds to an HTML element (e.g., `<p>`, `<h1>`, 
 * `<ul>`, `<pre>`) and may contain either plain text, inline elements, or 
 * nested child blocks.
 *
 * The `DocumentBlock` interface stores information about each open block during 
 * parsing, including its associated DOM element, parent element for appending 
 * children, block type (text, inline, or HTML), whether it is a leaf or 
 * container, accumulated lines of content, an optional continuation regular 
 * expression to determine if the block should continue, and an optional closing 
 * handler.
 */
enum BlockType { Text, Inline, Html, Skip }
/**
 * An event function called when block continues or closes.
 */
export type BlockEvent = (state: ParserState, block: DocumentBlock) => void
/**
 * Represents a block-level element in a parsed Markdown document.
 */
export interface DocumentBlock {
    /**
     * The element that corresponds to the block.
     */
    element: Element
    /**
     * In some cases, child nodes should be appended to a different parent 
     * element than the block's main element. The `parent` property allows 
     * specifying an alternative parent element for such scenarios.
     */
    parent: Element
    /**
     * Indicates whether the block contains inline elements (such as links or 
     * emphasis) or only plain verbatim text. This flag determines how the 
     * block's content should be processed and rendered when the block is 
     * flushed.
     */
    type: BlockType
    /**
     * Indicates whether this block is a leaf block (cannot contain child 
     * blocks) or a container block (can contain child blocks). Leaf blocks 
     * contain only text or inline elements, while container blocks may nest
     * other blocks (e.g. lists or blockquotes).
     */
    leaf: boolean
    /**
     * Each entry in this array represents a line of markdown text that is part 
     * of the content for this block. These lines are accumulated as the parser 
     * processes the input, and are later used to generate the final HTML 
     * content for the block.
     */
    lines: string[]
    /**
     * Optional regular expression that must match the beginning of subsequent 
     * Markdown lines for this block to continue. If provided, lines that do not 
     * match will cause the block to be closed.
     */
    cont?: RegExp
    /**
     * A function that is called before the block is closed.
     */
    closing?: BlockEvent
    /**
     * Function that is called when the block continues to be open, i.e. when
     * the `cont` regexp matches for a following line.
     */
    continuing?: BlockEvent
}
/**
 * ## Link References
 * 
 * In Markdown, link reference definitions allow you to define a link'
 * s destination and optional title separately from where the link is used. For 
 * example:
 *
 *      [example]: https://example.com "Example Title"
 *
 * The `LinkRef` interface below is used to store the destination URL and the 
 * optional title for each reference label. These definitions can then be looked 
 * up when rendering reference-style links elsewhere in the document.
 */
export interface LinkRef {
    destination: string
    title?: string
}
/**
 * Dictionaries of link references and anchor tags whose targets are not 
 * resolved yet.
 */
type LinkRefs = Record<string, LinkRef>
type Links = Record<string, (HTMLAnchorElement | HTMLImageElement)[]>
/**
 * ## Parser State
 *
 * The parser state tracks the current context and position during parsing.
 * It contains:
 *
 *  - `input`: The Markdown source string currently being parsed.
 *  - `nextIndex`: The current position in the input string for parsing.
 *  - `blocks`: The stack of open `DocumentBlock` objects representing the block 
 *    hierarchy.
 *  - `links`: A mapping of link reference labels to their definitions.
 *
 * This state object is passed to matcher functions and updated as the parser 
 * processes the input.
 */
export interface ParserState {
    input: string
    nextIndex: number
    blocks: DocumentBlock[]
    linkRefs: LinkRefs
    links: Links
}
/**
 * ## Constructors
 * 
 * Helper functions for creating parsers, new states, and DOM elements.
 */
export function parser(matched: Matcher, regexp: string): Parser {
    return { matched, regexp }
}
/**
 * Creates a new parser state based on the given state, but with a new input 
 * string and optionally a new starting index. The `blocks` stack is shared with 
 * the original state.
 */
function stateFrom(state: ParserState, input: string, nextIndex = 0):
    ParserState {
    return { input, nextIndex, blocks: state.blocks, linkRefs: state.linkRefs,
        links: state.links }
}
/**
 * ## Opening and Closing Blocks
 *
 * Functions for managing the stack of open blocks during parsing.
 *
 * - `openBlock` pushes a new block onto the stack. You must specify the parser 
 *   state, the element to associate with the block, and whether the block is 
 *   inline. By default, the block is a leaf, the parent is the element itself, 
 *   and it has no continuation regexp. The block's lines array is always 
 *   initialized as empty.
 *
 * - `closeLastBlock` pops the most recently opened block from the stack and 
 *   appends its element to its parent element in the previous block.
 */
function openBlock(state: ParserState, element: Element, type: BlockType, 
    leaf = true, parent = element, cont?: RegExp, closing?: BlockEvent,
    continuing?: BlockEvent) {
    state.blocks.push(
        { element, parent, type, leaf, lines: [], cont, closing, continuing })
}
/**
 * Pops the last block from the stack and appends its element to the parent
 * element of the previous block in the stack.
 */
function closeLastBlock(state: ParserState) {
    let block = state.blocks.pop()
    let element = block!.element
    let parent = lastBlock(state)?.parent
    if (parent && parent != element)
        parent.append(element)
}
/**
 * Returns the topmost (most recently opened) block from the parser state's 
 * stack.
 */
function lastBlock(state: ParserState): DocumentBlock {
    return state.blocks[state.blocks.length - 1]
}
/**
 * Check whether the last block is a paragraph.
 */
function lastBlockIsParagraph(state: ParserState): boolean {
    return lastBlock(state).element.tagName == "P"
}
/**
 * Interrupts a paragraph block if one is open.
 */
function interruptParagraph(state: ParserState) {
    flushLastBlock(state)
    if (lastBlockIsParagraph(state))
        closeLastBlock(state)
}
/**
 * Appends one or more nodes to the parent element of the current (topmost) 
 * block.
 */
function append(state: ParserState, ...nodes: Node[]) {
    lastBlock(state).parent.append(...nodes)
}
/**
 * Append verbatitm HTML to the parent of the current block.
 */
function appendHtml(state: ParserState, html: string) {
    lastBlock(state).parent.insertAdjacentHTML('beforeend', html)
}
/** 
 * Add rest of the current input to the specified block. 
 */
function flushInputToBlock(state: ParserState, block: DocumentBlock) {
    block.lines.push(state.input.slice(state.nextIndex))
    state.nextIndex = state.input.length
}
/**
 * Trim empty lines from beginning and end of a block.
 */
function trimBlock(state: ParserState, block: DocumentBlock) {
    while (block.lines.length > 0 && block.lines[block.lines.length - 1] == "")
        block.lines.pop()
    while (block.lines.length > 0 && block.lines[0] == "")
        block.lines.shift()
}
/**
 * ## Constructing Combined Regular Expressions
 * 
 * This function takes a list of parsers and combines their regular expressions
 * into a single RegExp. Each parser's pattern is wrapped in a named capturing
 * group (`g0`, `g1`, ...), so we can later determine which parser matched.
 * The `sticky` parameter controls whether the resulting RegExp uses the "y"
 * (sticky) flag, which is needed for block parsing.
 */
function regexpFor(parsers: Parser[], sticky: boolean): RegExp {
    let re = parsers.map((p, i) => `(?<g${i}>${p.regexp})`).join("|")
    return new RegExp(re, sticky ? "yuis" : "guis")
}
/**
 * Constructs a regular expression that matches either an "open" or "close" 
 * pattern. The resulting RegExp has two named groups: "open" and "close", 
 * corresponding to the provided patterns. This is useful for finding matching 
 * pairs (e.g., brackets or delimiters).
 */
function openCloseRegexp(open: string, close: string): RegExp {
    return new RegExp(`(?<open>${open})|(?<close>${close})`, "guis")
}
/**
 * Execute the given regexp and return its match groups, if it matches the 
 * current input. Advance the `nextIndex` if match is found.
 */
function parseRegExp(state: ParserState, regexp: RegExp): 
    RegExpExecArray | null {
    regexp.lastIndex = state.nextIndex
    let res = regexp.exec(state.input)
    if (res)
        state.nextIndex = res.index + res[0].length
    return res
}
/**
 * The `parseNext` function attempts to match the next token in the input using 
 * the combined regular expression and the list of parsers. It takes the regexp, 
 * the parser array, the current parser state, and a flag indicating whether we 
 * are parsing inline content. If a match is found:
 * 
 *  1. It flushes any unprocessed text before the match (for inlines).
 *  2. It invokes the matcher function for the matched parser.
 *  3. It advances the `nextIndex` to continue parsing after the match.
 * 
 * Returns `true` if a parser matched, otherwise `false`.
 */
function parseNext(regexp: RegExp, parsers: Parser[], state: ParserState, 
    inline: boolean): boolean {
    if (state.nextIndex >= state.input.length)
        return false
    regexp.lastIndex = state.nextIndex
    let match = regexp.exec(state.input)
    if (match && match.groups) {
        let parser = parsers.find((_, i) => match.groups![`g${i}`] != undefined)
        if (parser) {
            if (inline)
                flushInline(state, match.index)
            state.nextIndex = match.index + match[0].length
            parser.matched(state, match)
            return true
        }
    }
    return false
}
/**
 * ## Reusable Regular Expressions
 *
 * Here are RegExp fragments used throughout the parser:
 *
 * - `escapes`: Matches Markdown escape sequences (backslash followed by 
 *   punctuation or newline).
 * - `entities`: Matches HTML entities and numeric character references.
 * - `indentedCode`: Matches lines that begin with at least four spaces or a 
 *   tab (for indented code blocks).
 * - `indentedCodeOrBlank`: Matches indented code or blank lines.
 * - `blockQuote`: Matches blockquote markers (`>`).
 * - `nonBlank`: Matches any line containing at least one non-whitespace 
 *    character.
 * - `codeSpan`: Matches inline code spans delimited by backticks.
 * - `emAsterisk` / `emUnderscore`: Matches emphasis and strong emphasis using 
 *   `*` or `_`.
 * - `linkLabel`, `linkDest`, `linkTitle`: Match link label, destination, and 
 *   title for Markdown links.
 * - `linkTextOpen`, `imageTextOpen`, `linkOrImageTextOpen`, 
 *    `linkOrImageTextClose`: Match opening/closing brackets for links/images.
 * - `autoLink`, `emailAutoLink`: Match autolinks and email autolinks.
 * - `rawHtml`: Matches raw HTML tags and their content.
 */
const escapes = /\\(?<esc>[!"#$%&'()*+,\-./:;<=>?@\[\\\]^_`{|}~\n])/guis
const entities = /(?<entity>&(?:[a-z]\w*|#\d{1,7}|#[Xx][\da-f]{1,6});)/.source
const indentedCode = / {4}| {0,3}\t/yuis
const indentedCodeOrBlank = / {4}| {0,3}\t|\s*$/yuis
const blockQuote = / {0,3}> ?/yuis
const nonBlank = /(?=\s*\S)/yuis
const codeSpan = /(?<codedelim>`+)(?<code>\s.+\s|(?:[^`]|(?!\k<codedelim>)`)+)\k<codedelim>/.source
const emAsterisk = /(?<emdelim>(?:(?<!\\)|(?<=\\\\))(?:\*\*?(?![\s\p{P}\p{S}]|$))|(?:(?<=[\s\p{P}\p{S}]|^)\*\*?(?![\P{P}*])))(?<em>.+)(?:(?<![\s\p{P}\p{S}\\*])\k<emdelim>(?!\*)|(?<![\P{P}\\*])\k<emdelim>(?=[\s\p{P}\p{S}])|(?<![\s\\*])\k<emdelim>$)/u.source
const emUnderscore = /(?<emdelim>(?:(?<!\\)|(?<=\\\\))(?:__?(?![\s\p{P}\p{S}]|$))|(?:(?<=[\s\p{P}\p{S}]|^)__?(?![\P{P}_])))(?<em>.+)(?:(?<![\s\p{P}\p{S}\\_])\k<emdelim>(?!_)|(?<![\P{P}\\_])\k<emdelim>(?=[\s\p{P}\p{S}])|(?<![\s\\_])\k<emdelim>$)/u.source
const linkLabel = /\[(?<linklabel>(?:\s*(?:[^\[\]\s]|(?<=\\)(?<!\\\\)[\[\]])+\s*)+)\]/.source
const linkDest = /(?:(?<!\\)<(?<linkdest>(?:[^<>\n]|(?<=\\)[<>])*)(?<!\\)>|(?<linkdest>(?!<)(?:[^\x00-\x1F\x7F ()]|(?<=\\)[()])*))/.source
const linkTitle = /(?:"(?<linktitle>(?:[^"]|(?<=\\)")+)"|'(?<linktitle>(?:[^']|(?<=\\)')+)'|\((?<linktitle>(?:[^()]|(?<=\\)[()])+)\))/.source
const linkTextOpen = /(?<!\\)\[/.source
const imageTextOpen = /(?<!\\)!\[/.source
const linkOrImageTextOpen = /(?<!\\)!?\[/.source
const linkOrImageTextClose = /(?:(?<!\\)|(?<=\\\\))\]/.source
const linkOrImageTextOpenClose = openCloseRegexp(linkOrImageTextOpen, linkOrImageTextClose)
const inlineLink = new RegExp(`\\(\\s*${linkDest}(?:\\s+${linkTitle})?\\s*\\)`, "yuis")
const fullReferenceLink = new RegExp(linkLabel, "yuis")
const collapsedReferenceLink = /(?![(:])(?:\[\])?/yuis
const autoLink = /<(?<autolink>[a-z][\w\-+.]{1,31}:[^\x00-\x1F\x7F <>]*)>/.source
const emailAutoLink = /<(?<email>[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*)>/.source
const rawHtml = /(?<tagstart><(?<tag>[a-z][a-z0-9\-]*)(?:\s+[a-z:_][\w.:\-]*\s*(?:=\s*"[^"]*"|=\s*'[^']*'|=[^\s"'=<>`]+)?)*\s*)(?<tagend>\/?>)(?:(?<innerhtml>.*)<\/\k<tag>\s*>)?/.source
/**
 * Link references can span multiple lines. Since the block parser reads input
 * line-by-line, we need an expression automaton to track the parser state. 
 * There are three sub-automata that parse thelink label, link destination and 
 * the optional title. The states and transitions of the automata are defined 
 * below.
 */
const linkLabelAuto = ExpAuto.create(1,
    (start, label, accept) => [
        [start, / {0,3}\[/, label],
        [label, /\s*(?:[^\[\]\s]|(?<=\\)(?<!\\\\)[\[\]])+/, label, "label"],
        [label, /\s*(?:(?<!\\)|(?<=\\\\))\]:\s*/, accept]
    ])
const linkDestAuto = ExpAuto.create(0,
    (start, accept) => [
        [start, 
            /\s*(?:<(?:[^<>]|(?<=\\)[<>])+(?<!\\)>|(?<!<)(?:[^\x00-\x1F\x7F ()]|(?<=\\)[()])+)(?:\s+|$)/, 
            accept, "dest"],
    ])
const linkTitleAuto = ExpAuto.create(3,
    (start, dquoted, squoted, parens, accept) => [
        [start, /\s*"/, dquoted],
        [dquoted, /(?:\s*(?:[^"\s]|(?<=\\)")+)+/, dquoted, "title"],
        [dquoted, /(?<!\\)"\s*$/, accept],
        [start, /\s*'/, squoted],
        [squoted, /(?:\s*(?:[^'\s]|(?<=\\)')+)+/, squoted, "title"],
        [squoted, /(?<!\\)'\s*$/, accept],
        [start, /\s*\(/, parens],
        [parens, /(?:\s*(?:[^()]|(?<=\\)[()])+)+/, parens, "title"],
        [parens, /(?<!\\)\)\s*$/, accept],
        [start, /(?!["'(])|^/, accept]
    ])
/**
 * Now we can construct the combined automaton for link references by 
 * concatenating the three automata above.
 */
const linkRef = ExpAuto.concat(linkLabelAuto, linkDestAuto, linkTitleAuto)
/**
 * ## Inline Parsers
 *
 * Inline parser handle Markdown elements that can appear within block-level
 * content, such as emphasis, links, code spans, and images. The `flushInline`
 * function appends any unprocessed input as a text node to the current block,
 * from the parser state's current index up to the specified position (or to 
 * the end of input if no position is given).
 */
function flushInline(state: ParserState, index?: number) {
    if (index == undefined || index > state.nextIndex) {
        let inp = state.input.substring(state.nextIndex, index)
        if (inp)
            append(state, text(inp))
    }
}
/**
 * Finds the closing delimiter for an inline element using opening and closing 
 * regex patterns. This works like a parenthesis-matching algorithm: starting 
 * just after the opening delimiter, it searches for the corresponding closing 
 * delimiter, correctly handling nested pairs. Returns the RegExpExecArray for 
 * the closing match, or `undefined` if no closing delimiter is found.
 */
function findClosingIndex(input: string, index: number, openClose: RegExp,
    allowedOpen: string): RegExpExecArray | undefined {
    openClose.lastIndex = index
    let match = openClose.exec(input)
    if (match) {
        let { open, close } = match.groups!
        index = match.index + match[0].length
        if (open && open == allowedOpen) {
            let next = findClosingIndex(input, index, openClose, allowedOpen)
            return !next ? undefined :
                 findClosingIndex(input, next.index + next[0].length, openClose,
                    allowedOpen)
        }
        else if (close)
            return match
    }
}
/**
 * ### Emphasis and Strong
 *
 * Helper for creating emphasis (`<em>`) and strong (`<strong>`) inline parsers.
 * This function takes a regular expression for emphasis delimiters (`*` or `_`)
 * and returns a parser that wraps the matched content in the appropriate tag.
 * The content between delimiters is parsed recursively for inline elements.
 */
function emOrStrong(regexp: string) {
    return parser(
        (state, match) => {
            let { emdelim, em } = match.groups!
            openBlock(state, elem(emdelim.length == 1 ? 'em' : 'strong'), 
                BlockType.Inline)
            inlines(stateFrom(state, em))
            closeLastBlock(state)
        },
        regexp)
}
/**
 * ### Links
 *
 * Creates an HTML anchor element for a Markdown link, sets its href and title,
 * parses and appends the link text as inline content, and returns the anchor.
 */
function outputLink(state: ParserState, linktext: string, linkdest?: string, 
    linktitle?: string): HTMLAnchorElement {
    let anchor = elem('a')
    if (linkdest)
        anchor.href = replaceEscapes(linkdest)!
    if (linktitle)
        anchor.title = replaceEscapes(linktitle)!
    openBlock(state, anchor, BlockType.Inline)
    inlines(stateFrom(state, linktext))
    closeLastBlock(state)
    return anchor
}
/**
 * Output a reference-style link. If the reference is defined, creates an anchor 
 * with the destination and title. Otherwise, creates a placeholder anchor and 
 * stores it for later resolution.
 */
function outputReferenceLink(state: ParserState, linklabel: string, 
    linktext: string) {
    linklabel = linklabel.toUpperCase()
    let linkRef = state.linkRefs[linklabel]
    if (linkRef)
        outputLink(state, linktext, linkRef.destination, linkRef.title)
    else {
        let anchors = state.links[linklabel] || []
        anchors.push(outputLink(state, linktext))
        state.links[linklabel] = anchors
    }
}
/**
 * ### Images
 * 
 * Creates an `<img>` element with the given alt text, src, and optional title, 
 * and appends it to the current block.
 */
function outputImage(state: ParserState, description: string, imgsrc?: string, 
    imgtitle?: string): HTMLImageElement {
    let image = elem('img')
    if (imgsrc)
        image.src = replaceEscapes(imgsrc)!
    image.alt = description
    if (imgtitle)
        image.title = imgtitle
    append(state, image)
    return image
}
/**
 * Output a reference-style image. If the reference is defined, creates an 
 * `<img>` with src and title; otherwise, creates a placeholder image and stores 
 * it for later resolution.
 */
function outputReferenceImage(state: ParserState, imglabel: string, 
    description: string) {
    imglabel = imglabel.toUpperCase()
    let linkRef = state.linkRefs[imglabel]
    if (linkRef)
        outputImage(state, description, linkRef.destination, linkRef.title)
    else {
        let anchors = state.links[imglabel] || []
        anchors.push(outputImage(state, description))
        state.links[imglabel] = anchors
    }
}
/**
 * Remove backslashes from escaped characters in a string.
 */
function replaceEscapes(text?: string): string | undefined {
    return text?.replaceAll(escapes, str => str[1])
}
/**
 * An array of inline Markdown parsers, each responsible for handling a specific
 * inline syntax element. Each parser is defined with a matching regular 
 * expression and a matcher function that processes the matched content and 
 * updates the parser state accordingly.
 *
 * The supported inline elements include:
 * - Escapes: Handles backslash-escaped characters and line breaks.
 * - Entities: Handles HTML entities and numeric character references.
 * - Code Spans: Handles inline code delimited by backticks.
 * - Links: Parses Markdown links of the form `[text](url)` and reference-style 
 *   links.
 * - Images: Parses image syntax `![alt](src)` and reference-style images.
 * - Autolinks: Handles automatic links and email autolinks.
 * - Emphasis & Strong: Handles emphasis (`*` or `_`) and strong emphasis 
 *   (`**` or `__`).
 * - Raw HTML: Passes through raw HTML tags and optionally parses inner content.
 */
const inlineParsers = [
    parser(
        /**
         * ### Escapes
         *
         * Handles Markdown escape sequences. A backslash before a punctuation 
         * or symbol character escapes it, rendering the character literally. If 
         * the backslash is at the end of a line, it produces a `<br>` element 
         * instead.
         */
        (state, match) => {
            let { esc } = match.groups!
            if (esc == "\n")
                append(state, elem('br'))
            append(state, text(esc))
        },
        escapes.source),
    parser(
        /**
         * ### Entity and Numeric Character References
         *
         * Handles HTML entities and numeric character references such as
         * `&amp;`, `&#123;`, and `&#x1F600;`. These are inserted as text nodes
         * so that the browser will decode them when rendering.
         */
        (state, match) => {
            let { entity } = match.groups!
            appendHtml(state, entity)
        }, entities),
    parser(
        /**
         * ### Code Spans
         *
         * Handles inline code spans delimited by backticks, following the main
         * rules from [section 6.1](https://spec.commonmark.org/0.31.2/#code-spans)
         * of the CommonMark specification:
         * 
         * - The opening and closing backtick sequences must match in length.
         * - Leading and trailing whitespace inside the code span are trimmed.
         * - Newlines inside code spans are replaced with spaces.
         * - Backticks inside code spans are allowed if multiple backticks open
         *   it.
         */
        (state, match) => {
            let { code } = match.groups!
            code = code.replaceAll("\n", " ")
            let trim = /^ (.*[^ ].*) $/.exec(code)
            if (trim)
                code = trim[1]
            append(state, elem('code', text(code)))
        }, codeSpan),
    parser(
        /**
         * ### Links
         *
         * Handles Markdown links in the format `[text](url)` as well as 
         * reference-style links. This parser supports inline links, full 
         * reference links, and collapsed reference links. Escaped brackets and 
         * parentheses are unescaped in both the link text and destination. If a 
         * reference is not defined, a placeholder anchor is created for later 
         * resolution.
         */
        (state, match) => {
            let close = findClosingIndex(state.input, state.nextIndex,
                linkOrImageTextOpenClose, "![")
            if (close) {
                let linktext = state.input.substring(state.nextIndex, 
                    close.index)
                state.nextIndex = close.index + close[0].length
                switch (state.input[state.nextIndex]) {
                    case "(":
                        let dest = parseRegExp(state, inlineLink)
                        if (dest) {
                            let { linkdest, linktitle } = dest.groups!
                            return outputLink(state, linktext, linkdest, 
                                linktitle)
                        }
                        break
                    case "[":
                        let fullref  = parseRegExp(state, fullReferenceLink)
                        if (fullref) {
                            let { linklabel } = fullref.groups!
                            return outputReferenceLink(state, linklabel, 
                                linktext)
                        }
                    default:
                        let collap = parseRegExp(state, collapsedReferenceLink)
                        if (collap)
                            return outputReferenceLink(state, linktext, 
                                linktext)
                }
                state.nextIndex = match.index + match[0].length
            }
            append(state, text(match[0]))
        }, linkTextOpen),
    parser(
        /**
         * ### Images
         *
         * Handles Markdown image syntax of the form `![alt](src)` and 
         * reference-style images. This parser extracts the alt text and image 
         * source, unescapes any escaped brackets or parentheses, and creates an 
         * `<img>` element with the appropriate `alt`, `src`, and optional 
         * `title` attributes. If a reference is not defined, a placeholder 
         * image is created for later resolution.
         */
        (state, match) => {
            let close = findClosingIndex(state.input, state.nextIndex,
                linkOrImageTextOpenClose, "[")
            if (close) {
                let description = state.input.substring(state.nextIndex, 
                    close.index)
                state.nextIndex = close.index + close[0].length
                switch (state.input[state.nextIndex]) {
                    case "(":
                        let dest = parseRegExp(state, inlineLink)
                        if (dest) {
                            let { linkdest, linktitle } = dest.groups!
                            return outputImage(state, description, linkdest, 
                                linktitle)
                        }
                        break
                    case "[":
                        let fullref  = parseRegExp(state, fullReferenceLink)
                        if (fullref) {
                            let { linklabel } = fullref.groups!
                            return outputReferenceImage(state, linklabel, 
                                description)
                        }
                    default:
                        let collap = parseRegExp(state, collapsedReferenceLink)
                        if (collap)
                            return outputReferenceImage(state, description, 
                                description)
                }
                state.nextIndex = match.index + match[0].length
            }
            append(state, text(match[0]))
        }, imageTextOpen),
    parser(
        /**
         * ### Autolinks
         *
         * Matches automatic links in the form `<scheme:...>`, where `scheme` is 
         * a valid URI scheme. Creates an anchor (`<a>`) element with the 
         * matched URL as both the href and text content.
         */
        (state, match) => {
            let { autolink } = match.groups!
            let link = elem('a', text(autolink))
            link.href = autolink
            append(state, link)
        }, autoLink),
    parser(
        /**
         * ### Email Autolinks
         *
         * Matches email addresses enclosed in angle brackets and creates a
         * mailto link (`<a href="mailto:...">`). The email address is used
         * as both the link text and the mailto target.
         */
        (state, match) => {
            let { email } = match.groups!
            let link = elem('a', text(email))
            link.href = "mailto:" + email
            append(state, link)
        }, emailAutoLink),
    emOrStrong(emAsterisk),
    emOrStrong(emUnderscore),
    parser(
        /**
         * ### Raw HTML
         *
         * Passes through raw HTML tags and their content. This parser matches
         * HTML tags (including self-closing tags) and, if the tag contains 
         * inner content and a matching closing tag, parses the inner content as 
         * inline Markdown. Otherwise, the tag is inserted as raw HTML into the 
         * output.
         */
        (state, match) => {
            let { tagstart, tagend, innerhtml } = match.groups!
            appendHtml(state, tagstart + tagend)
            if (tagend == ">" && innerhtml) {
                openBlock(state, lastBlock(state).parent.lastElementChild!,
                    BlockType.Inline)
                inlines(stateFrom(state, innerhtml))
                closeLastBlock(state)
            }
        }, rawHtml)
]
/**
 * We initialize the combined regexp when it is first used. Thus we can register 
 * new inline parsers before calling the parser. After that the list of parsers
 * is locked down.
 */
let inlineRegexp: RegExp 
/**
 * This function repeatedly applies inline parsers using a regular expression
 * matcher until no more matches are found. After all inline elements have been
 * parsed, it flushes any remaining inline content in the parser state.
 */
function inlines(state: ParserState) {
    if (/^\s*$/.test(state.input))
        return
    inlineRegexp = inlineRegexp || regexpFor(inlineParsers, false)
    while (parseNext(inlineRegexp, inlineParsers, state, true));
    flushInline(state)
}
/**
 * ## Block Parsers
 * 
 * Block parsers match block-level Markdown elements using regular expressions 
 * and associated matcher functions. Each block parser processes one line at a 
 * time, handling elements like headings, lists, code blocks, block quotes, and 
 * paragraphs.
 *
 * ### Lists
 * 
 * The function below opens a new list or list item block when a list marker is 
 * detected. It determines the correct list type (`<ul>` or `<ol>`), manages 
 * continuation and tight/loose list logic, and updates the parser state 
 * accordingly.
 */
function openList(state: ParserState, match: RegExpExecArray, bulletsep: string, 
    bulletno?: string) {
    let prefix = match[0]
    let allowEmpty = match.index + prefix.length >= state.input.length ?
        "" : "|\\s*$"
    let lastIndex = state.blocks.length - 1
    let last = state.blocks[lastIndex]
    if (last.element.tagName == "P") {
        let prev = state.blocks[lastIndex - 1]
        if (prev.element.tagName != "LI" && (allowEmpty.length == 0 ||
            (bulletno && bulletno != "1"))) {
            state.nextIndex = match.index
            return
        }
        flushLastBlock(state)
        if (prev.element.tagName == "LI")
            closeListItem(state, prev)
        else
            closeLastBlock(state)
    }
    let len = prefix.length
    let bslen = bulletsep.length
    if (bslen == 0)
        len++
    else if (bslen > 4 || allowEmpty.length == 0)
        len -= (bslen - 1)
    let block = lastBlock(state)
    let bulletRe = prefix.replaceAll(/\d+|[+*.)]|\s+$/g, m => 
            m[0] == " " ? `(?:${m}|$)` :
            Number.isFinite(Number(m)) ? "\\d{1,9}" : "\\" + m)
    let cont = new RegExp(`(?= {${len}}${allowEmpty}|${bulletRe})`, "yui")
    if (bulletno && block.element.tagName != "OL") {
        let ol = elem('ol')
        if (bulletno != "1")
            ol.start = Number.parseInt(bulletno)
        openBlock(state, ol, BlockType.Inline, false, undefined, cont,
            closeList)
    }
    else if (!bulletno && block.element.tagName != "UL")
        openBlock(state, elem('ul'), BlockType.Inline, false, undefined, cont,
            closeList)
    openBlock(state, elem('li'), BlockType.Inline, false, undefined,
        new RegExp(` {${len}}${allowEmpty}`, "yui"), closeListItem)
    state.nextIndex = match.index + len
}
/**
 * `closeListItem` determines whether the list item block to be closed is
 * tight or loose. Tight items do not contain empty lines, and thus will not
 * be wrapped inside `p` elements. By default we assume that items are loose,
 * and always create the `p` block.
 * 
 * Closing the list item block can happen before or after closing the `p` block 
 * depending on whether the list item is the last one. The first `if` statement
 * handles the case when we are not closing the last block, and the paragraph
 * block is still open. If we have not accumulated any empty rows into the list
 * item block, we know that the item is tight and can delete the `p` block 
 * &mdash; after we have moved its lines under the list item block.
 * 
 * The second if statement triggers if the list item is the last one. We know
 * that it's a last one, if there is no `p` block open anymore. The `closeList`
 * function below will reset the `closing` property of its block, if all the
 * items in the list are tight. Also the last item needs to be tight, that is,
 * it must contain only one child which is a paragraph. We can move `p` elements 
 * children directly under the `lì` element, if all the conditions above are 
 * true. Finally, we can delete the `p` element.
 */
function closeListItem(state: ParserState, block: DocumentBlock) {
    let last = lastBlock(state)
    let prev = state.blocks[state.blocks.length - 2]
    if (block.lines.length == 0 && block == prev && 
        block.element.childElementCount == 0 && last.element.tagName == "P") {
        let para = state.blocks.pop()!
        append(state, ...para.element.childNodes)
        block.lines = para.lines
    }
    else if (block == last && !prev.closing &&
        block.element.childElementCount == 1 && 
        block.element.firstElementChild!.tagName == "P") {
        let p = block.element.firstElementChild!
        block.element.append(...p.childNodes)
        p.remove()
    }
}
/**
 * When a list block is closing, we check whether all its items are tight; that 
 * is, there are no `p` elements directly underneath the items. If that 
 * condition holds, we reset the `closing` property of the block to indicate 
 * that the list is tight. Using the property for this is hacky as hell... But 
 * the alternative would be to introduce some class hierarchy for blocks, derive
 * `ListBlock` class from the `DocumentBlock`, and so on. This is is simply
 * too big of a change for implementing this one use case. We'll refactor this 
 * later, if need be.
 */
function closeList(state: ParserState, block: DocumentBlock) {
    if (!block.element.querySelector(":scope > li > p"))
        block.closing = undefined
}
/**
 * ### Closing Fenced Code Block
 *
 * This function trims empty lines from the fenced code block and advances the 
 * parser past the closing fence marker (e.g., ``` or ~~~) if present at the 
 * current position.
 */
function closeFencedCodeBlock(state: ParserState, block: DocumentBlock) {
    trimBlock(state, block)
    let endmarker = / {0,3}(?:`|~){3,}\s*$/yiu
    endmarker.lastIndex = state.nextIndex
    if (endmarker.exec(state.input))
        state.nextIndex = endmarker.lastIndex
}
/**
 * ### Coninuing and Closing a Link Reference
 * 
 * Continuing a block will store the matched text into the current block, and
 * feed the rest of the line to the expression automaton.
 */
function continueLinkRef(state: ParserState, block: DocumentBlock) {
    let [res, pos] = linkRef.exec(state.input, state.nextIndex)
    if (!res || linkRef.accepted) {
        terminateLinkRef(state, block)
        state.nextIndex = pos
    }
    else 
        state.nextIndex = state.input.length
}
/**
 * Called when a block regexp does not match any more. I.e. when an empty line
 * or end of input is reached.
 */
function closeLinkRef(state: ParserState, block: DocumentBlock) {
    linkRef.exec(state.input, state.nextIndex, true)
    terminateLinkRef(state, block)
}
/**
 * Handles the closure of a link reference block. Extracts the link label, 
 * destination, and optional title, stores them in the parser state, and updates 
 * any unresolved links or images with the same label.
 */
function terminateLinkRef(state: ParserState, block: DocumentBlock) {
    if (linkRef.accepted) {
        let { label, dest, title } = linkRef.groups
        if (label && dest) {
            label = label.trim().toUpperCase()
            dest = replaceEscapes(dest.trim())!
            title = replaceEscapes(title)!
            if (!state.linkRefs[label]) {
                if (/^<.*>$/.test(dest))
                    dest = dest.slice(1, dest.length - 1)
                state.linkRefs[label] = { destination: dest, title }
                state.links[label]?.forEach(aelem => {
                    if (dest) {
                        if(aelem instanceof HTMLAnchorElement)
                            aelem.href = dest
                        else
                            aelem.src = dest
                    }
                    if (title)
                        aelem.title = title
                })
                delete state.links[label]
            }
            closeLastBlock(state)
            return
        }
    }
    block.element = elem('p')
    block.parent = block.element
    block.type = BlockType.Inline
    block.cont = nonBlank
}
/**
 * ### Opening a Paragraph
 * 
 * A sequence of non-blank lines that cannot be interpreted as other kinds of 
 * blocks forms a paragraph.
 * 
 * The function below opens a paragraph if the last block is not already one.
 */
function openParagraph(state: ParserState) {
    if (!lastBlockIsParagraph(state)) {
        flushLastBlock(state)
        openBlock(state, elem('p'), BlockType.Inline, true, undefined, nonBlank)
    }
}
/**
 * The following array contains all block-level Markdown parsers. Each parser is 
 * defined with a regular expression and a matcher function that processes the 
 * matched block element. Supported block elements include headings, thematic 
 * breaks, code blocks, HTML blocks, block quotes, lists, link references, and 
 * paragraphs.
 */
const blockParsers = [
    parser(
        /**
         * ### Setext Headings
         *
         * Matches Setext-style headings, which are underlined with `=` or `-`.
         * If the previous block is a paragraph, it is converted to an `<h1>` 
         * (for `=`) or `<h2>` (for `-`) heading. If the line is a thematic 
         * break (three or more `-`), it creates an `<hr>`. Otherwise, starts 
         * a new paragraph block.
         */
        (state, match) => {
            let { setext, setextspaces } = match.groups!
            let block = lastBlock(state)
            let len = match[0].trim().length
            if (block.element.tagName == "P") {
                block.element = elem(setext == "=" ? 'h1' : 'h2')
                block.parent = block.element
                flushLastBlock(state)
                closeLastBlock(state)
            }
            else if (setext == "-" && len > 2) {
                flushLastBlock(state)
                append(state, elem('hr'))
            }
            else if (setext == "-" && len == 1)
                openList(state, match, setextspaces)
            else {
                openBlock(state, elem('p'), BlockType.Inline, true, undefined, 
                    nonBlank)
                append(state, text(match[0]))
            }
        },
        / {0,3}(?<setext>-|=)\k<setext>*(?<setextspaces>\s*)$/.source),
    parser(
        /**
         * ### Thematic Breaks
         *
         * Matches a horizontal rule (thematic break) as defined by CommonMark:
         * a line containing at least three consecutive `*`, `-`, or `_` 
         * characters, possibly separated by spaces or tabs, and nothing else.
         * Produces an `<hr>` element.
         */
        (state,) => {
            interruptParagraph(state)
            append(state, elem('hr'))
        },
        / {0,3}(?<brkchar>[*\-_])(?:\s*\k<brkchar>){2,}\s*$/.source),
    parser(
        /**
         * ### ATX Headers
         *
         * Matches ATX-style headers (lines starting with 1-6 `#` characters).
         * The number of `#` characters determines the header level (`<h1>` to 
         * `<h6>`). The header text is parsed for inline elements.
         */
        (state, match) => {
            interruptParagraph(state)
            let { atxlevel, atxheader } = match.groups!
            let level = atxlevel.length
            openBlock(state, elem(<keyof HTMLElementTagNameMap>`h${level}`), 
                BlockType.Inline)
            inlines(stateFrom(state, atxheader))
            closeLastBlock(state)
        },
        / {0,3}(?<atxlevel>#{1,6})\s+(?<atxheader>.*?)\s*$/.source),
    parser(
        /**
         * ### Indented Code Blocks
         *
         * Matches code blocks that are indented by at least 4 spaces or a tab.
         * The content is collected as-is and rendered inside a `<pre><code>` 
         * block.
         * 
         * Indented code blocks cannot interrupt paragraphs.
         */
        (state,) => {
            if (!lastBlockIsParagraph(state)) {
                flushLastBlock(state)
                let code = elem('code')
                openBlock(state, elem('pre', code), BlockType.Text, true, code,
                    indentedCodeOrBlank, trimBlock)
            }
        },
        indentedCode.source),
    parser(
        /**
         * ### Fenced Code Blocks
         *
         * Matches fenced code blocks delimited by three or more backticks 
         * (`` ``` ``)  or tildes (`~~~`). Optionally captures the language 
         * identifier after the opening fence. Opens a `<pre><code>` block, sets 
         * the language class if specified, and collects all lines until a 
         * closing fence of the same type and length is found. Trims empty lines 
         * and advances past the closing fence when the block ends.
         */
        (state, match) => {
            let { codefence, codelang } = match.groups!
            interruptParagraph(state)
            let cont = new RegExp(`(?! {0,3}${codefence}+\\s*$)`, "yui")
            let code = elem('code')
            if (codelang)
                code.className = `language-${replaceEscapes(codelang)}`
            openBlock(state, elem('pre', code), BlockType.Text, true, code, 
                cont, closeFencedCodeBlock)
        },
        / {0,3}(?<codefence>(?:`|~){3,})(?:\s*(?<codelang>[^\s`~]+))?\s*$/.source),
    parser(
        /**
         * ### HTML Blocks (Type 1)
         *
         * Only conditions 1 and 7 of CommonMark spefication are supported.
         * This parser matches HTML blocks that start with certain tags
         * (`<pre>`, `<script>`, `<style>`, or `<textarea>`) and continues
         * until the corresponding closing tag is found. The block includes
         * all lines as raw HTML, including blank lines, and the terminating
         * line is also included in the block.
         *
         * If the closing tag appears on the same line as the opening tag,
         * the entire line is appended as raw HTML without opening a new block.
         */
        (state,) => {
            flushLastBlock(state)
            let cont = /(?!.*(?:<\/pre>|<\/script>|<\/style>|<\/textarea>))/yui
            let line = state.input
            cont.lastIndex = state.nextIndex
             if (cont.test(line))
                openBlock(state, lastBlock(state).parent, BlockType.Html, 
                    true, undefined, cont, flushInputToBlock)
            else {
                appendHtml(state, line.slice(state.nextIndex) + "\n")
                state.nextIndex = line.length
            }
        }, 
        /(?= {0,3}<pre|<script|<style|<textarea)/.source),
    parser(
        /**
         * ### HTML Blocks (Type 7)
         *
         * Conditions 1 of CommonMark spefication are defined below. It allows 
         * any tag name but the complete opening tag has to be the only tag on
         * the first line. The rest of the line must be blank.
         */
        (state,) => {
            flushLastBlock(state)
            openBlock(state, lastBlock(state).parent, BlockType.Html, true, 
                undefined, nonBlank)
        },
        /(?= {0,3}<[a-z][a-z0-9\-]*(?:\s+[a-z:_][\w.:-]*\s*(?:=\s*"[^"]*"|\s*='[^']*'|=[^\s"'=<>`]+)?)*\s*>\s*$)/.source),
    parser(
        /**
         * ### Block Quotes
         *
         * Matches blockquote markers (`>`), following CommonMark rules.
         * When a blockquote is detected, interrupts any open paragraph,
         * then opens a new `<blockquote>` block. The continuation regexp
         * ensures subsequent lines starting with `>` are included in the block.
         */
        (state,) => {
            interruptParagraph(state)
            openBlock(state, elem('blockquote'), BlockType.Inline, false,
                undefined, blockQuote)
        },
        blockQuote.source),
    parser(
        /**
         * ### Lists
         *
         * Matches unordered (`-`, `+`, `*`) and ordered (`1.`, `2)`, etc.) 
         * list items. When a list marker is found, this parser opens a new 
         * `<ul>` or `<ol>` block if necessary, and then opens a new `<li>` 
         * block for the list item. The continuation regular expression ensures 
         * that subsequent lines are correctly associated with the current list 
         * item or list.
         */
        (state, match) => {
            let { bulletno, bulletsep } = match.groups!
            openList(state, match, bulletsep, bulletno)
        },
        / {0,3}(?:[\-+*]|(?<bulletno>\d{1,9})[.)])(?<bulletsep> +|$)/.source),
    parser(
        /**
         * ### Link References
         *
         * Matches link reference definitions, e.g. `[label]: <url> "title"`.
         * using the expression automaton defined above. If a valid link 
         * reference is found, it is stored in the parser state. Otherwise, the 
         * line is treated as a paragraph.
         */
        (state, match) => {
            if (!lastBlockIsParagraph(state)) {
                linkRef.init()
                let [res,] = linkRef.exec(state.input, match.index)
                if (res) {
                    flushLastBlock(state)
                    openBlock(state, lastBlock(state).parent, BlockType.Skip, 
                        true, undefined, nonBlank, 
                        closeLinkRef, continueLinkRef)
                    if (linkRef.accepted) {
                        state.nextIndex = state.input.length
                        return terminateLinkRef(state, lastBlock(state))
                    }
                }
                else
                    openParagraph(state)
            }
            state.nextIndex = match.index
        },
        linkRef.startRegExp),
    parser(openParagraph, nonBlank.source)
]
/**
 * The combined regexp for all block parsers.
 */
const blockRegexp = regexpFor(blockParsers, true)
/**
 * Flushes the lines collected in the last block of the parser state.
 * 
 * If the block contains any lines, they are joined into a single string.
 * Depending on whether the block is marked as inline, the function either:
 * - Processes the lines as inline content using the `inlines` function, or
 * - Appends the lines as a text block using the `append` function.
 * 
 * After flushing, the block's `lines` array is cleared.
 */
function flushLastBlock(state: ParserState) {
    let block = lastBlock(state)
    if (block.lines.length > 0) {
        switch (block.type) {
            case BlockType.Inline:
                inlines(stateFrom(state, block.lines
                    .map(l => l.trim())
                    .join("\n")))
                break
            case BlockType.Text:
                append(state, text(block.lines.join("\n")))
                break
            case BlockType.Html:
                block.lines.push("")
                appendHtml(state, block.lines.join("\n"))
                break
        }
        block.lines = []
    }
}
/**
 * Closes and flushes all open blocks in the parser state upto the specified 
 * index. This is used to unwind the block stack to a certain depth.
 */
function closeBlocksToIndex(state: ParserState, index: number) {
    for (let j = index; j < state.blocks.length; ++j) {
        let block = state.blocks[j]
        block.closing?.(state, block)
    }
    while (state.blocks.length > index) {
        flushLastBlock(state)
        closeLastBlock(state)
    }
}
/**
 * Iterates through the current parser state's block stack and closes any blocks
 * that are no longer continued by the input at the current parsing position.
 *
 * For each block, if it has a continuation regular expression (`cont`), we
 * attempt to match it against the input at the current position. If the match 
 * fails or does not start at the current position, all blocks from the current 
 * index onward are closed by calling `closeBlocksToIndex`.
 *
 * Updates the parser state's `nextIndex` to reflect the position after a 
 * successful match.
 */
function closeDiscontinuedBlocks(state: ParserState) {
    for (let i = 0; i < state.blocks.length; ++i) {
        let block = state.blocks[i]
        if (block.cont) {
            block.cont.lastIndex = state.nextIndex
            let match = block.cont.exec(state.input)
            if (!match)
                return closeBlocksToIndex(state, i)
            state.nextIndex = block.cont.lastIndex
            block.continuing?.(state, block)
        }
    }
}
/**
 * ## Main Parsing Function
 * 
 * `appendMarkdown` converts a Markdown string to HTML and appends the result 
 * to the given DOM element.
 *
 * The implementation works as follows:
 * 
 *  1.  Initializes the parser state with the input string and an empty block 
 *      stack.
 * 
 *  2.  Opens a root block associated with the provided DOM element.
 * 
 *  3.  Splits the input into lines and processes each line:
 * 
 *      1.  For each line, creates a temporary parser state for that line.
 *      2.  Closes any blocks that are no longer continued by the current line.
 *      3.  If the topmost block is not a leaf block, attempts to match 
 *          block-level elements using the registered block parsers.
 *      4.  If any unprocessed content remains, it is added to the current 
 *          block's lines.
 * 
 *  4.  After all lines are processed, flushes and closes all blocks, ensuring 
 *      that the resulting HTML structure is complete.
 */
export function appendMarkdown(input: string, root: Element) {
    let state: ParserState = {
        input: "",
        nextIndex: 0,
        blocks: [],
        linkRefs: {},
        links: {}
    }
    openBlock(state, root, BlockType.Inline, false)
    let lines = input.split("\n")
    for (let i = 0; i < lines.length; ++i) {
        let line = lines[i]
        let st = stateFrom(state, line, 0)
        closeDiscontinuedBlocks(st)
        let block = lastBlock(st)
        if (!block.leaf || block.element.tagName == "P")
            while (parseNext(blockRegexp, blockParsers, st, false)) {
                block = lastBlock(st)
                if (block.leaf) break
            }
        block.lines.push(line.slice(st.nextIndex))
    }
    closeBlocksToIndex(state, 0)
}