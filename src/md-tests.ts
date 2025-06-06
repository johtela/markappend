/**
 * ---
 * {
 *  "modules": [ "./src/md-tests.ts" ]
 * }
 * ---
 * 
 * # CommonMark Tests
 * 
 * This module contains tests from [CommonMark][] test suite to verify that
 * the parser is working correctly.
 * 
 * [CommonMark]: https://spec.commonmark.org/0.31.2/
 */
import { test } from "lits-extras/lib/tester"
import 'lits-extras/lib/test-runner'
import * as sd from './parser'
/**
 * ## Testing Helper
 *
 * We need a helper function for running markdown-to-HTML conversion tests.
 * It takes a test description, input markdown, and expected HTML output,
 * then asserts that the conversion result matches the expectation.
 */
function mdTest(description: string, input: string, output: string) {
    test(description, t => {
        let doc = document.createElement('div')
        sd.markdownToHtml(input, doc)
        let html = doc.innerHTML
            t.equals(html, output, 
            `Convert: "${ws(input)}"\nExpected: "${ws(output)}"\nActual: "${
                ws(html)}"`)
    })
}
/**
 * This function replaces all whitespace characters in the given string
 * with visible Unicode symbols for easier debugging of test output.
 * 
 * - Tabs ("\t") are replaced with "⇥"
 * - Newlines ("\n") are replaced with "↩"
 * - Spaces (" ") are replaced with "␣"
 * 
 * Other whitespace characters are left unchanged.
 */
function ws(value: string): string {
    return value.replaceAll(/\s/g, ch => {
        switch (ch) {
            case "\t": return "⇥"
            case "\n": return "↩"
            case " ": return "␣"
            default: return ch
        }
    })
}
/**
 * ## Tabs
 * 
 * Tabs in lines are not expanded to spaces. However, in contexts where spaces 
 * help to define block structure, tabs behave as if they were replaced by 
 * spaces with a tab stop of 4 characters. 
 * 
 * Thus, for example, a tab can be used instead of four spaces in an indented 
 * code block. (Note, however, that internal tabs are passed through as literal 
 * tabs, not expanded to spaces.)
 */
mdTest(`Example1: Tabs are treated as 4 spaces`, 
`\tfoo\tbaz\t\tbim`,
`<pre><code>foo\tbaz\t\tbim</code></pre>`)
    
mdTest(`Example2`, 
`  \tfoo\tbaz\t\tbim`,
`<pre><code>foo\tbaz\t\tbim</code></pre>`)
    
mdTest(`Example3`, 
`    a\ta\n    ὐ\ta`,
`<pre><code>a\ta\nὐ\ta</code></pre>`)
/**
 * ## Backslash Escapes
 * 
 * Any ASCII punctuation character may be backslash-escaped.
 */
mdTest(`Example 12: Any ASCII punctuation character may be backslash-escaped`,
"\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~",
"<p>!\"#$%&amp;'()*+,-./:;&lt;=&gt;?@[\\]^_`{|}~</p>")
/**
 * Backslashes before other characters are treated as literal backslashes.
 */
mdTest(`Example 13: Backslashes before other characters are treated as literal
backslashes`,
`\\\t\\A\\a\\ \\3\\φ`,
`<p>\\\t\\A\\a\\ \\3\\φ</p>`)
/**
 * Escaped characters are treated as regular characters and do not have their 
 * usual Markdown meanings.
 */
mdTest(`Example 14: Escaped characters are treated as regular characters and
do not have their usual Markdown meanings`,

`\\*not emphasized*
\\<br/> not a tag
\\[not a link](/foo)
\\\`not code\`
1\\. not a list
\\* not a list
\\# not a heading
\\[foo]: /url "not a reference"
\\&ouml; not a character entity`,

`<p>*not emphasized*
&lt;br/&gt; not a tag
[not a link](/foo)
\`not code\`
1. not a list
* not a list
# not a heading
[foo]: /url "not a reference"
&amp;ouml; not a character entity</p>`)
/**
 * If a backslash is itself escaped, the following character is not.
 */
mdTest(`Example 15: If a backslash is itself escaped, the following character 
is not`,
`\\\\*emphasis*`,
`<p>\\<em>emphasis</em></p>`)
/**
 * A backslash at the end of the line is a hard line break.
 */
mdTest(`Example 16: A backslash at the end of the line is a hard line break`,
`foo\\
bar`,
`<p>foo<br>
bar</p>`)
/**
 * Backslash escapes do not work in code blocks, code spans, autolinks, or 
 * raw HTML.
 */
mdTest(`Example 17: Backslash escapes do not work in code blocks, code spans, 
autolinks, or raw HTML`,
"`` \\[\\` ``",
"<p><code>\\[\\`</code></p>")

mdTest("Example 18",
"    \\[\\]",
"<pre><code>\\[\\]</code></pre>")
/**
 * ## Entity and Numeric Character References
 * 
 * Valid HTML entity references and numeric character references can be used 
 * in place of the corresponding Unicode character, with the following 
 * exceptions:
 * 
 *  -   Entity and character references are not recognized in code blocks and 
 *      code spans.
 * 
 *  -   Entity and character references cannot stand in place of special 
 *      characters that define structural elements in CommonMark. For example, 
 *      although `&#42`; can be used in place of a literal * character, `&#42;`
 *      cannot replace * in emphasis delimiters, bullet list markers, or 
 *      thematic breaks.
 */
mdTest(`Example 25: HTML entities`,
`&nbsp; &amp; &copy; &AElig; &Dcaron;
&frac34; &HilbertSpace; &DifferentialD;
&ClockwiseContourIntegral; &ngE;`,
`<p>&nbsp; &amp; © Æ Ď
¾ ℋ ⅆ
∲ ≧̸</p>`)

mdTest(`Example 26: Decimal numeric character references`,
`&#35; &#1234; &#992; &#0;`,
`<p># Ӓ Ϡ �</p>`)

mdTest(`Example 27: Hexadecimal numeric character references`,
`&#X22; &#XD06; &#xcab;`,
`<p>" ആ ಫ</p>`)

mdTest(`Example 28: Nonentities`,
`&nbsp &x; &#; &#x;
&#87654321;
&#abcdef0;
&ThisIsNotDefined; &hi?;`,
`<p>&amp;nbsp &amp;x; &amp;#; &amp;#x;
&amp;#87654321;
&amp;#abcdef0;
&amp;ThisIsNotDefined; &amp;hi?;</p>`)

mdTest(`Example 29: Trailing semicolon missing`,
`&copy`,
`<p>&amp;copy</p>`)

mdTest(`Example 30: Made up entity`,
`&MadeUpEntity;`,
`<p>&amp;MadeUpEntity;</p>`)

mdTest(`Example 31: Entity and numeric character references are recognized in 
any context`,
`<a href="&ouml;&ouml;.html">`,
`<a href="&ouml;&ouml;.html">`)
/**
 * ## HTML Blocks
 * 
 * HTML tags designed to contain literal content (pre, script, style, textarea), 
 * comments, processing instructions, and declarations are treated somewhat 
 * differently. Instead of ending at the first blank line, these blocks end at 
 * the first line containing a corresponding end tag. As a result, these blocks 
 * can contain blank lines.
 * 
 * HTML tags designed to contain literal content (pre, script, style, textarea), 
 * comments, processing instructions, and declarations are treated somewhat 
 * differently. Instead of ending at the first blank line, these blocks end at 
 * the first line containing a corresponding end tag. As a result, these blocks 
 * can contain blank lines:
 */
mdTest(`Example 169: A pre tag (type 1)`,
`<pre language="haskell"><code>
import Text.HTML.TagSoup

main :: IO ()
main = print $ parseTags tags
</code></pre>
okay`,
`<pre language="haskell"><code>
import Text.HTML.TagSoup

main :: IO ()
main = print $ parseTags tags
</code></pre>
<p>okay</p>`)

mdTest(`Example 170: A script tag (type 1)`,
`<script type="text/javascript">
// JavaScript example

document.getElementById("demo").innerHTML = "Hello JavaScript!";
</script>
okay`,
`<script type="text/javascript">
// JavaScript example

document.getElementById("demo").innerHTML = "Hello JavaScript!";
</script>
<p>okay</p>`)

mdTest(`Example 171: A textarea tag (type 1)`,
`<textarea>

*foo*

_bar_

</textarea>`,
`<textarea>
*foo*

_bar_

</textarea>
`)

mdTest(`Example 172: A style tag (type 1)`,
`<style
  type="text/css">
h1 {color:red;}

p {color:blue;}
</style>
okay`,
`<style type="text/css">
h1 {color:red;}

p {color:blue;}
</style>
<p>okay</p>`)
/**
 * If there is no matching end tag, the block will end at the end of the 
 * document (or the enclosing block quote or list item).
 */
mdTest(`Example 173: No matching end tag`,
`<style
  type="text/css">

foo`,
`<style type="text/css">

foo
</style>`)

mdTest(`Example 176: the end tag can occur on the same line as the start tag`,
`<style>p{color:red;}</style>
*foo*`,
`<style>p{color:red;}</style>
<p><em>foo</em></p>`)

mdTest(`Example 178: Anything on the last line after the end tag will be 
included`,
`<script>
foo
</script>1. *bar*`,
`<script>
foo
</script>1. *bar*
`)

/**
 * ## Results
 * 
 * <test-runner name="CommonMark tests"></test-runner>
 */