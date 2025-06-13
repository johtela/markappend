/**
 * # Markdown Parser
 *
 * This module implements a self-contained Markdown parser. It does not import 
 * or depend on any external libraries. The parsing process closely follows the 
 * strategy outlined in the [CommonMark specification][].
 *
 * Note: The parser is not fully CommonMark compliant. Some of the more obscure 
 * rules have been intentionally omitted to keep the code simple.
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
enum BlockType { Text, Inline, Html }
/**
 * A function type that handles closing a document block during parsing.
 */
export type Closer = (state: ParserState, block: DocumentBlock) => void
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
    closing?: Closer
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
 * Dictionary of link references.
 */
type LinkRefs = Record<string, LinkRef>
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
    links: LinkRefs
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
    return { input, nextIndex, blocks: state.blocks, links: state.links }
}
/**
 * The `elem` function creates an HTML element of the specified `tag` type.
 * Optionally, any number of child nodes can be appended to the created element.
 * This provides a convenient way to construct DOM trees.
 */
export function elem<K extends keyof HTMLElementTagNameMap>(tag: K, 
    ...children: Node[]): HTMLElementTagNameMap[K] {
    let res = document.createElement(tag)
    if (children.length > 0)
        res.append(...children)
    return res
}
/**
 * Creates a text node containing the specified string data.
 */
export function text(data: string): Text {
    return document.createTextNode(data)
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
    leaf = true, parent = element, cont?: RegExp, closing?: Closer) {
    state.blocks.push(
        { element, parent, type, leaf, lines: [], cont, closing })
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
 * Common RegExp fragments used throughout the parser:
 *
 * - `wsOrPunct`: Matches whitespace, punctuation, or symbol characters 
 *   (Unicode-aware).
 * - `emOrStrong`: Matches one or two consecutive `_` or `*` characters, for 
 *   emphasis and strong emphasis.
 * - `indentedCode`: Matches lines that begin with at least four spaces and/or
 *   a tab, used for indented code blocks.
 * - `nonBlank`: Matches any line containing at least one non-whitespace 
 *   character.
 */
const indentedCode = / {4}| {0,3}\t/yuis
const indentedCodeOrBlank = / {4}| {0,3}\t|\s*$/yuis
const nonBlank = /(?=\s*\S)/yuis
const emAsterisk = /(?<emdelim>(?:\*\*?(?![\s\p{P}\p{S}]|$))|(?:(?<=[\s\p{P}\p{S}]|^)\*\*?(?![\P{P}\*])))(?<em>.+?)(?:(?<![\s\p{P}\p{S}])\k<emdelim>|(?<![\P{P}\*])\k<emdelim>(?=[\s\p{P}\p{S}]|$))/u.source
const emUnderscore = /(?<emdelim>(?:__?(?![\s\p{P}\p{S}]|$))|(?:(?<=[\s\p{P}\p{S}]|^)__?(?![\P{P}_])))(?<em>.+?)(?:(?<![\s\p{P}\p{S}])\k<emdelim>|(?<![\P{P}_])\k<emdelim>(?=[\s\p{P}\p{S}]|$))/u.source
const spTabsOptNl = /(?:[ \t]+\n?[ \t]*|[ \t]*\n?[ \t]+|\n)/.source
const linkLabel = /\[(?<linklabel>(?:\s*(?:[^\]\s]|(?<=\\)\])+\s*)+)\]/.source
const linkDest = /(?:<(?<linkdest>(?:[^<>\n]|(?<=\\)[<>])+)(?<!\\)>|(?<linkdest>(?:[^\x00-\x1F\x7F ()]|(?<=\\)[()])+))/.source
const linkTitle = /(?:"(?<linktitle>(?:[^"\n]|(?<=\\)"|(?<!\n[ \t]*)\n)+)"|'(?<linktitle>(?:[^'\n]|(?<=\\)'|(?<!\n[ \t]*)\n)+)'|\((?<linktitle>(?:[^()\n]|(?<=\\)[()]|(?<!\n[ \t]*)\n)+)\))/.source
const linkref = `^ {0,3}${linkLabel}:${spTabsOptNl}${linkDest}${spTabsOptNl}${linkTitle}[ \t]*$`
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
 * An array of inline Markdown parsers, each responsible for handling a specific
 * inline syntax element. Each paser is defined with a matching regular 
 * expression and a matcher function that processes the matched content and 
 * updates the parser state accordingly.
 *
 * The supported inline elements include:
 * - Escapes: Handles backslash-escaped characters and line breaks.
 * - Code Spans: Handles inline code delimited by backticks.
 * - Links: Parses standard Markdown links of the form `[text](url)`.
 * - Images: Parses image syntax `![alt](src)`.
 * - Emphasis & Strong: Handles emphasis (`*` or `_`) and strong emphasis 
 *   (`**` or `__`).
 * - Raw HTML: Passes through raw HTML tags.
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
        /\\(?<esc>[\p{P}\p{S}\n])/u.source),
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
        },
        /(?<entity>&(?:[a-z]\w*|#\d{1,7}|#[Xx][\da-f]{1,6});)/.source),
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
        },
        /(?<codedelim>`+)(?<code>\s.+\s|[^`]+)\k<codedelim>/.source),
    parser(
        /**
         * ### Links
         *
         * Handles Markdown links in the format `[text](url)`. This 
         * implementation focuses on the most common use case and does not
         * support reference-style or nested links, in line with the goal of 
         * simplicity. Escaped brackets and parentheses are unescaped in both 
         * the link text and destination.
         */
        (state, match) => {
            let { link, linkdest } = match.groups!
            linkdest = linkdest.replaceAll(/\\\(|\\\)/, str => str[1])
            let aelem = elem('a')
            aelem.href = linkdest
            openBlock(state, aelem, BlockType.Inline)
            inlines(stateFrom(state, link))
            closeLastBlock(state)
        },
        /\[(?<link>(?:\\\[|\\\]|[^\[\]])+)\]\((?<linkdest>(?:\\\(|\\\)|[^\s()])+)\)/.source),
    parser(
        /**
         * ### Images
         *
         * Handles Markdown image syntax of the form `![alt](src)`. This parser 
         * extracts the alt text and image source, unescapes any escaped 
         * brackets or parentheses, and creates an `<img>` element with the 
         * appropriate `alt` and `src` attributes.
         */
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
        },
        /(?<tagstart><(?<tag>[a-z][a-z0-9\-]*)(?:\s+[a-z:_][\w.:-]*\s*(?:=\s*"[^"]*"|\s*='[^']*'|=[^\s"'=<>`]+)?)*\s*)(?<tagend>\/?>)(?:(?<innerhtml>.*)<\/\k<tag>\s*>)?/.source)
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
    inlineRegexp = inlineRegexp || regexpFor(inlineParsers, false)
    while (parseNext(inlineRegexp, inlineParsers, state, true));
    flushInline(state)
}
/**
 * ## Block Parsers
 * 
 * Block parsers are defined similarly to inline parsers, each with a regular 
 * expression and a matcher function. Block-level elements are parsed 
 * line-by-line, as blocks have a nested, hierarchical structure. The combined 
 * regular expression matches block prefixes that indicate different block 
 * types.
 * 
 * ### Closing List Item Block
 * 
 * The function below, `closeListItem`, is responsible for handling the closure
 * of a list item block. If the list item is empty and immediately followed by
 * an empty paragraph block, it removes the paragraph and merges its children
 * into the list item. This ensures that paragraph tags are not generated for 
 * tight lists, i.e. lists that do not have empty lines between its items.
 */
function closeListItem(state: ParserState, block: DocumentBlock) {
    let last = state.blocks.length - 1
    if (block.lines.length == 0 && block == state.blocks[last - 1] && 
        block.element.childElementCount == 0 && 
        state.blocks[last].element.tagName == "P") {
        let para = state.blocks.pop()!
        append(state, ...para.element.children)
        block.lines = para.lines
    }
}
/**
 * ### Closing Fenced Code block
 * 
 * Trim empty lines and skip the end marker if present.
 */
function closeFencedCodeBlock(state: ParserState, block: DocumentBlock) {
    trimBlock(state, block)
    let endmarker = / {0,3}(?:`|~){3,}\s*$/yiu
    endmarker.lastIndex = state.nextIndex
    if (endmarker.exec(state.input))
        state.nextIndex = endmarker.lastIndex
}
/**
 * Parser are defined here.
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
            let { setext } = match.groups!
            let block = lastBlock(state)
            if (block.element.tagName == "P") {
                block.element = elem(setext == "=" ? 'h1' : 'h2')
                block.parent = block.element
                flushLastBlock(state)
                closeLastBlock(state)
            }
            else if (setext == "-" && match[0].trim().length > 2) {
                flushLastBlock(state)
                append(state, elem('hr'))
            }
            else {
                openBlock(state, elem('p'), BlockType.Inline, true, undefined, 
                    nonBlank)
                append(state, text(match[0]))
            }
        },
        / {0,3}(?<setext>-|=)\k<setext>*\s*$/.source),
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
         * TODO: Explain what the function below does.
         */
        (state, match) => {
            let { codefence, codelang } = match.groups!
            interruptParagraph(state)
            let cont = new RegExp(`(?! {0,3}${codefence}+\\s*$)`, "yui")
            let code = elem('code')
            if (codelang)
                code.className = `language-${codelang}`
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
            let { bulletno } = match.groups!
            let prefix = match[0]
            let len = prefix.length
            let block = lastBlock(state)
            let cont = new RegExp(`(?= {${len}}|\\s*$|${
                prefix.replace(/\d+|[+*.)]/, m => 
                    Number.isFinite(Number(m)) ? "\\d{1,9}" : "\\" + m)})`, 
                "yui")
            if (bulletno && block.element.tagName != "OL") {
                let ol = elem('ol')
                if (bulletno != "1")
                    ol.start = Number.parseInt(bulletno)
                openBlock(state, ol, BlockType.Inline, false, undefined, cont)
            }
            else if (!bulletno && block.element.tagName != "UL")
                openBlock(state, elem('ul'), BlockType.Inline, false, undefined, 
                    cont)
            openBlock(state, elem('li'), BlockType.Inline, false, undefined,
                new RegExp(` {${len}}|\s*$`, "yui"), closeListItem)
        },
        / {0,3}(?:[\-+*]|(?<bulletno>\d{1,9})[.)]) {1,4}/.source),
    parser(
        /**
         * ### Paragraphs
         * 
         * A sequence of non-blank lines that cannot be interpreted as other 
         * kinds of blocks forms a paragraph.
         */
        (state,) => {
            if (!lastBlockIsParagraph(state)) {
                flushLastBlock(state)
                openBlock(state, elem('p'), BlockType.Inline, true, undefined, 
                    nonBlank)
            }
        },  
        nonBlank.source)
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
        }
    }
}
/**
 * ## Link References
 * 
 * Function to collect and remove link references from the input. Returns the
 * updated input and the dictionary of link references.
 */
function linkRefs(input: string): [string, LinkRefs] {
    let matcher = new RegExp(linkref, "uimg")
    let links: LinkRefs = {}
    input = input.replace(matcher, 
        (match, label, _2, destination, title) => {
            links[label] = { destination, title }
            return ""
        })
    return [ input, links ]
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
export function appendMarkdown(doc: string, root: Element) {
    let [ input, links ] = linkRefs(doc)
    let state: ParserState = {
        input: "",
        nextIndex: 0,
        blocks: [],
        links
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