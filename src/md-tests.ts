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
        sd.appendMarkdown(input, doc)
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
`<a href="öö.html">
</a>`)

mdTest(`Example 35: Entity and numeric character references are treated as 
 literal text in code spans and code blocks`,
"`f&ouml;&ouml;`",
`<p><code>f&amp;ouml;&amp;ouml;</code></p>`)

mdTest(`Example 36`,
`    f&ouml;f&ouml;`,
`<pre><code>f&amp;ouml;f&amp;ouml;</code></pre>`)
/**
 * Entity and numeric character references cannot be used in place of symbols 
 * indicating structure in CommonMark documents.
 */
mdTest(`Example 37: Entity and numeric character references cannot be used in 
place of symbols indicating structure in CommonMark documents.`,
`&#42;foo&#42;
*foo*`,
`<p>*foo*
<em>foo</em></p>`)

mdTest(`Example 38`,
`&#42; foo

* foo`,
`<p>* foo</p><ul><li>foo</li></ul>`)

mdTest(`Example 39`,
`foo&#10;&#10;bar`,
`<p>foo

bar</p>`)

mdTest(`Example 40`,
`&#9;foo`,
`<p>	foo</p>`)

mdTest(`Example 41`,
`[a](url &quot;tit&quot;)`,
`<p>[a](url "tit")</p>`)
/**
 * ## Thematic Breaks
 * 
 * A line consisting of optionally up to three spaces of indentation, followed 
 * by a sequence of three or more matching -, _, or * characters, each followed 
 * optionally by any number of spaces or tabs, forms a thematic break.
 */
mdTest(`Example 43: Basic thematic breaks`,
`***
---
___`,
`<hr><hr><hr>`)

mdTest(`Example 44: Wrong characters`,
`+++`,
`<p>+++</p>`)

mdTest(`Example 45`,
`===`,
`<p>===</p>`)

mdTest(`Example 46: Not enough characters`,
`--
**
__`,
`<p>--
**
__</p>`)

mdTest(`Example 47: Up to three spaces of indentation are allowed`,
` ***
  ***
   ***`,
`<hr><hr><hr>`)

mdTest(`Example 48: Four spaces of indentation is too many`,
`    ***`,
`<pre><code>***</code></pre>`)

mdTest(`Example 49`,
`Foo
    ***`,
`<p>Foo
***</p>`)

mdTest(`Example 50: More than three characters may be used`,
`_____________________________________`,
`<hr>`)

mdTest(`Example 51: Spaces and tabs are allowed between the characters`,
` - - -`,
`<hr>`)

mdTest(`Example 52`,
` **  * ** * ** * **`,
`<hr>`)

mdTest(`Example 53`,
`-     -      -      -`,
`<hr>`)

mdTest(`Example 54: Spaces and tabs are allowed at the end`,
`- - - -    `,
`<hr>`)

mdTest(`Example 55: However, no other characters may occur in the line`,
`_ _ _ _ a

a------

---a---`,
`<p>_ _ _ _ a</p><p>a------</p><p>---a---</p>`)

mdTest(`Example 56: It is required that all of the characters other than spaces 
or tabs be the same. So, this is not a thematic break`,
` *-*`,
`<p><em>-</em></p>`)

mdTest(`Example 57: Thematic breaks do not need blank lines before or after`,
`- foo
***
- bar`,
`<ul><li>foo</li></ul><hr><ul><li>bar</li></ul>`)

mdTest(`Example 58: Thematic breaks can interrupt a paragraph`,
`Foo
***
bar`,
`<p>Foo</p><hr><p>bar</p>`)

mdTest(`Example 59: Setext heading takes precedence over thematic break`,
`Foo
---
bar`,
`<h2>Foo</h2><p>bar</p>`)

mdTest(`Example 61: If you want a thematic break in a list item, use a different 
bullet`,
`- Foo
- * * *`,
`<ul><li>Foo</li><li><hr></li></ul>`)
/**
 * ## ATX Headings
 * 
 * An ATX heading consists of a string of characters, parsed as inline content, 
 * between an opening sequence of 1–6 unescaped `#` characters and an optional 
 * closing sequence of any number of unescaped `#` characters. The opening 
 * sequence of `#` characters must be followed by spaces or tabs, or by the end 
 * of line. The optional closing sequence of `#`s must be preceded by spaces or 
 * tabs and may be followed by spaces or tabs only. The opening `#` character 
 * may be preceded by up to three spaces of indentation. The raw contents of the 
 * heading are stripped of leading and trailing space or tabs before being 
 * parsed as inline content. The heading level is equal to the number of `#` 
 * characters in the opening sequence.
 */
mdTest(`Example 62: Simple headings`,
`# foo
## foo
### foo
#### foo
##### foo
###### foo`,
`<h1>foo</h1><h2>foo</h2><h3>foo</h3><h4>foo</h4><h5>foo</h5><h6>foo</h6>`)

mdTest(`Example 63: More than six # characters is not a heading`,
`####### foo`,
`<p>####### foo</p>`)
/**
 * At least one space or tab is required between the `#` characters and the 
 * heading’s contents, unless the heading is empty. Note that many 
 * implementations currently do not require the space. However, the space was 
 * required by the original ATX implementation, and it helps prevent things like 
 * the following from being parsed as headings.
 */
mdTest(`Example 64: Space required between # and heading`,
`#5 bolt

#hashtag`,
`<p>#5 bolt</p><p>#hashtag</p>`)

mdTest(`Example 65: This is not a heading, because the first # is escaped`,
`\\## foo`,
`<p>## foo</p>`)

mdTest(`Example 66: Contents are parsed as inlines`,
`# foo *bar* \\*baz\\*`,
`<h1>foo <em>bar</em> *baz*</h1>`)

mdTest(`Example 67: Leading and trailing spaces or tabs are ignored in parsing 
inline content`,
`#                  foo                     `,
`<h1>foo</h1>`)

mdTest(`Example 68: Up to three spaces of indentation are allowed`,
` ### foo
  ## foo
   # foo`,
`<h3>foo</h3><h2>foo</h2><h1>foo</h1>`)

mdTest(`Example 69: Four spaces of indentation is too many`,
`    # foo`,
`<pre><code># foo</code></pre>`)

mdTest(`Example 70`,
`foo
    # bar`,
`<p>foo
# bar</p>`)

mdTest(`Example 77: ATX headings need not be separated from surrounding content 
by blank lines, and they can interrupt paragraphs`,
`****
## foo
****`,
`<hr><h2>foo</h2><hr>`)

mdTest(`Example 78`,
`Foo bar
# baz
Bar foo`,
`<p>Foo bar</p><h1>baz</h1><p>Bar foo</p>`)

mdTest(`Example 79: ATX headings can be empty`,
`## 
# 
### `,
`<h2></h2><h1></h1><h3></h3>`)
/**
 * ## Setext Headings
 * 
 * A setext heading consists of one or more lines of text, not interrupted by a 
 * blank line, of which the first line does not have more than 3 spaces of 
 * indentation, followed by a setext heading underline. The lines of text must 
 * be such that, were they not followed by the setext heading underline, they 
 * would be interpreted as a paragraph: they cannot be interpretable as a code 
 * fence, ATX heading, block quote, thematic break, list item, or HTML block.
 * 
 * A setext heading underline is a sequence of `=` characters or a sequence of 
 * `-` characters, with no more than 3 spaces of indentation and any number of 
 * trailing spaces or tabs.
 * 
 * The heading is a level 1 heading if `=` characters are used in the setext 
 * heading underline, and a level 2 heading if `-` characters are used. The 
 * contents of the heading are the result of parsing the preceding lines of text 
 * as CommonMark inline content.
 * 
 * In general, a setext heading need not be preceded or followed by a blank 
 * line. However, it cannot interrupt a paragraph, so when a setext heading 
 * comes after a paragraph, a blank line is needed between them.
 */
mdTest(`Example 80: Simple examples`,
`Foo *bar*
=========

Foo *bar*
---------`,
`<h1>Foo <em>bar</em></h1>
<h2>Foo <em>bar</em></h2>`)

mdTest(`Example 81: The content of the header may span more than one line`,
`Foo *bar
baz*
====`,
`<h1>Foo <em>bar
baz</em></h1>`)

mdTest(`Example 82: The contents are the result of parsing the headings's raw 
content as inlines. The heading’s raw content is formed by concatenating the 
lines and removing initial and final spaces or tabs`,
`  Foo *bar
baz*	
====`,
`<h1>Foo <em>bar
baz</em></h1>`)

mdTest(`Example 83: The underlining can be any length`,
`Foo
-------------------------

Foo
=`,
`<h2>Foo</h2>
<h1>Foo</h1>`)

mdTest(`Example 84: The heading content can be preceded by up to three spaces of 
indentation, and need not line up with the underlining`,
`   Foo
---

  Foo
-----

  Foo
  ===`,
`<h2>Foo</h2>
<h2>Foo</h2>
<h1>Foo</h1>`)

mdTest(`Example 85: Four spaces of indentation is too many`,
`    Foo
    ---

    Foo
---`,
`<pre><code>Foo
---

Foo</code></pre><hr>`)

mdTest(`Example 86: The setext heading underline can be preceded by up to three 
spaces of indentation, and may have trailing spaces or tabs`,
`Foo
   ----      `,
`<h2>Foo</h2>`)

mdTest(`Example 87: Four spaces of indentation is too many`,
`Foo
    ---`,
`<p>Foo
---</p>`)

mdTest(`Example 88: The setext heading underline cannot contain internal spaces 
or tabs`,
`Foo
= =

Foo
--- -`,
`<p>Foo
= =</p><p>Foo</p><hr>`)

mdTest(`Example 89: Trailing spaces or tabs in the content line do not cause a 
hard line break`,
`Foo  
-----`,
`<h2>Foo</h2>`)

mdTest(`Example 90: Nor does a backslash at the end`,
`Foo\\
----`,
`<h2>Foo\\</h2>`)

mdTest(`Example 91: Since indicators of block structure take precedence over 
indicators of inline structure, the following are setext headings`,
`\`Foo
----
\`

<a title="a lot
---
of dashes"/>`,
`<h2>\`Foo</h2><p>\`</p><h2>&lt;a title="a lot</h2><p>of dashes"/&gt;</p>`)

mdTest(`Example 95: A blank line is needed between a paragraph and a following 
setext heading, since otherwise the paragraph becomes part of the heading's 
content`,
`Foo
Bar
---`,
`<h2>Foo
Bar</h2>`)

mdTest(`Example 96: But in general a blank line is not required before or after 
setext headings`,
`---
Foo
---
Bar
---
Baz`,
`<hr><h2>Foo</h2><h2>Bar</h2><p>Baz</p>`)

mdTest(`Example 97: Setext headings cannot be empty`,
`
====`,
`<p>====</p>`)

mdTest(`Example 98: Setext heading text lines must not be interpretable as block 
constructs other than paragraphs. So, the line of dashes in these examples gets 
interpreted as a thematic break`,
`---
---`,
`<hr><hr>`)

mdTest(`Example 99`,
`- foo
-----`,
`<ul><li>foo</li></ul><hr>`)

mdTest(`Example 100`,
`    foo
---`,
`<pre><code>foo</code></pre><hr>`)

mdTest(`Example 101`,
`> foo
-----`,
`<blockquote>
<p>foo</p>
</blockquote><hr>`)

mdTest(`Example 102: If you want a heading with > foo as its literal text, you 
can use backslash escapes`,
`\\> foo
------`,
`<h2>&gt; foo</h2>`)
/**
 * Compatibility note: Most existing Markdown implementations do not allow the 
 * text of setext headings to span multiple lines. But there is no consensus 
 * about how to interpret
 * 
 *      Foo
 *      bar
 *      ---
 *      baz
 * 
 * One can find four different interpretations:
 * 
 *  1.  paragraph “Foo”, heading “bar”, paragraph “baz”
*   2.  paragraph “Foo bar”, thematic break, paragraph “baz”
*   3.  paragraph “Foo bar — baz”
*   4.  heading “Foo bar”, paragraph “baz”
* 
* We find interpretation 4 most natural, and interpretation 4 increases the 
* expressive power of CommonMark, by allowing multiline headings. Authors who 
* want interpretation 1 can put a blank line after the first paragraph.
 */
mdTest(`Example 103`,
`Foo

bar
---
baz`,
`<p>Foo</p><h2>bar</h2><p>baz</p>`)

mdTest(`Example 104: Authors who want interpretation 2 can put blank lines 
around the thematic break`,
`Foo
bar

---

baz`,
`<p>Foo
bar</p><hr>
<p>baz</p>`)

mdTest(`Example 105: or use a thematic break that cannot count as a setext 
heading underline, such as`,
`Foo
bar
* * *
baz`,
`<p>Foo
bar</p><hr><p>baz</p>`)

mdTest(`Example 106: Authors who want interpretation 3 can use backslash 
escapes`,
`Foo
bar
\\---
baz`,
`<p>Foo
bar
---
baz</p>`)
/**
 * ## Indented Code Blocks
 * 
 * An indented code block is composed of one or more indented chunks separated 
 * by blank lines. An indented chunk is a sequence of non-blank lines, each 
 * preceded by four or more spaces of indentation. The contents of the code 
 * block are the literal contents of the lines, including trailing line endings, 
 * minus four spaces of indentation. An indented code block has no info string.
 * 
 * An indented code block cannot interrupt a paragraph, so there must be a blank 
 * line between a paragraph and a following indented code block. (A blank line 
 * is not needed, however, between a code block and a following paragraph.)
 */
mdTest(`Example 107: Simple indented code block`,
`    a simple
      indented code block`,
`<pre><code>a simple
  indented code block</code></pre>`)

mdTest(`Example 108: If there is any ambiguity between an interpretation of 
indentation as a code block and as indicating that material belongs to a list 
item, the list item interpretation takes precedence`,
`  - foo

    bar`,
`<ul><li><p>foo</p><p>bar</p></li></ul>`)

mdTest(`Example `,
``,
``)

mdTest(`Example `,
``,
``)
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
mdTest(`Example149: Basic HTML blocks of type 6`,
`<table>
  <tbody>
    <tr>
        <td>
            hi
        </td>
    </tr>
  </tbody>
</table>

okay.`,
`<table>
  <tbody>
    <tr>
        <td>
            hi
        </td>
    </tr>
  </tbody>
</table>
<p>okay.</p>`)

mdTest(`Example 150`,
` <div>
  *hello*
         <foo><a>`,
` <div>
  *hello*
         <foo><a>
</a></foo></div>`)

mdTest(`Example 151`,
`<div>
*foo*`,
`<div>
*foo*
</div>`)

mdTest(`Example 153: Partial first line`,
`<div id="foo" class="bar">
</div>`,
`<div id="foo" class="bar">
</div>
`)

mdTest(`Example 154`,
`<div id="foo" class="bar baz">
</div>`,
`<div id="foo" class="bar baz">
</div>
`)

mdTest(`Example 155: An open tag need not be closed`,
`<div>
*foo*

*bar*`,
`<div>
*foo*
</div><p><em>bar</em></p>`)

/**
 * To start an HTML block with a tag that is not in the list of block-level 
 * tags in (6), you must put the tag by itself on the first line (and it must 
 * be complete)
 */
mdTest(`Example 162`,
`<a href="foo">
*bar*
</a>`,
`<a href="foo">
*bar*
</a>
`)
/**
 * In type 7 blocks, the tag name can be anything.
 */
mdTest(`Example 163: Tag name can be anything`,
`<Warning>
*bar*
</Warning>`,
`<warning>
*bar*
</warning>
`)

mdTest(`Example 164`,
`<i class="foo">
*bar*
</i>`,
`<i class="foo">
*bar*
</i>
`)

mdTest(`Example 166`,
`<del>
*foo*
</del>`,
`<del>
*foo*
</del>
`)

mdTest(`Example 168`,
`<del>*foo*</del>`,
`<p><del><em>foo</em></del></p>`)

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
 * The opening tag can be preceded by up to three spaces of indentation, but not 
 * four.
 */
mdTest(`Example 184: Up to three spaces of indentation, but not four`,
`  <div>

    <div>`,
`  <div>
</div><pre><code>&lt;div&gt;</code></pre>`)

mdTest(`Example `,
``,
``)

mdTest(`Example `,
``,
``)

mdTest(`Example `,
``,
``)

mdTest(`Example `,
``,
``)

/**
 * ## Results
 * 
 * <test-runner name="CommonMark tests"></test-runner>
 */