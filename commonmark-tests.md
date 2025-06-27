---
{
    "modules": [ "./src/commonmark.ts" ]
}
---

# CommonMark Test Suite

## Tabs

Tabs in lines are not expanded to spaces. However, in contexts where spaces 
help to define block structure, tabs behave as if they were replaced by 
spaces with a tab stop of 4 characters. 

Thus, for example, a tab can be used instead of four spaces in an indented 
code block. (Note, however, that internal tabs are passed through as literal 
tabs, not expanded to spaces.)

<commonmark-runner examples="1-3,8,10,11"></commonmark-runner>

## Backslash Escapes

Any ASCII punctuation character may be backslash-escaped:

<commonmark-runner examples="12"></commonmark-runner>

Backslashes before other characters are treated as literal backslashes:

<commonmark-runner examples="13"></commonmark-runner>

Escaped characters are treated as regular characters and do not have their usual Markdown meanings:

<commonmark-runner examples="14"></commonmark-runner>

If a backslash is itself escaped, the following character is not:

<commonmark-runner examples="15"></commonmark-runner>

A backslash at the end of the line is a hard line break:

<commonmark-runner examples="16"></commonmark-runner>

Backslash escapes do not work in code blocks, code spans, autolinks, or raw 
HTML:

<commonmark-runner examples="17-21"></commonmark-runner>

## Entity and numeric character references

Valid HTML entity references and numeric character references can be used in place of the corresponding Unicode character, with the following exceptions:

 -  Entity and character references are not recognized in code blocks and code
    spans

 -  Entity and character references cannot stand in place of special characters
    that define structural elements in CommonMark. For example, although `&#42;` 
    can be used in place of a literal `*` character, `&#42;` cannot replace `*`
    in emphasis delimiters, bullet list markers, or thematic breaks.

Conforming CommonMark parsers need not store information about whether a particular character was represented in the source using a Unicode character or an entity reference.

Entity references consist of `&` + any of the valid HTML5 entity names + `;`. 
The document <https://html.spec.whatwg.org/entities.json> is used as an 
authoritative source for the valid entity references and their corresponding 
code points.

<commonmark-runner examples="25"></commonmark-runner>

Decimal numeric character references consist of `&#` + a string of 1–7 arabic 
digits + `;`. A numeric character reference is parsed as the corresponding 
Unicode character. Invalid Unicode code points will be replaced by the 
REPLACEMENT CHARACTER (U+FFFD). For security reasons, the code point U+0000 will 
also be replaced by U+FFFD.

<commonmark-runner examples="26"></commonmark-runner>

Hexadecimal numeric character references consist of `&#` + either `X` or `x` + 
a string of 1-6 hexadecimal digits + `;`. They too are parsed as the 
corresponding Unicode character (this time specified with a hexadecimal numeral 
instead of decimal).

<commonmark-runner examples="27"></commonmark-runner>

Here are some nonentities:

<commonmark-runner examples="28"></commonmark-runner>

Although HTML5 does accept some entity references without a trailing semicolon 
(such as &copy), these are not recognized here, because it makes the grammar too 
ambiguous:

<commonmark-runner examples="29"></commonmark-runner>

Strings that are not on the list of HTML5 named entities are not recognized as 
entity references either:

<commonmark-runner examples="30"></commonmark-runner>

Entity and numeric character references are treated as literal text in code 
spans and code blocks:

<commonmark-runner examples="35-36"></commonmark-runner>

Entity and numeric character references cannot be used in place of symbols indicating structure in CommonMark documents.

<commonmark-runner examples="37-41"></commonmark-runner>

## Blocks and Inlines

We can think of a document as a sequence of blocks—structural elements like 
paragraphs, block quotations, lists, headings, rules, and code blocks. Some 
blocks (like block quotes and list items) contain other blocks; others (like 
headings and paragraphs) contain inline content—text, links, emphasized text, 
images, code spans, and so on.

### Precedence

Indicators of block structure always take precedence over indicators of inline 
structure. So, for example, the following is a list with two items, not a list 
with one item containing a code span:

<commonmark-runner examples="42"></commonmark-runner>

This means that parsing can proceed in two steps: first, the block structure of 
the document can be discerned; second, text lines inside paragraphs, headings, 
and other block constructs can be parsed for inline structure. The second step 
requires information about link reference definitions that will be available 
only at the end of the first step. Note that the first step requires processing 
lines in sequence, but the second can be parallelized, since the inline parsing 
of one block element does not affect the inline parsing of any other.

### Container Blocks and Leaf Blocks

We can divide blocks into two types: container blocks, which can contain other 
blocks, and leaf blocks, which cannot.

## Leaf Blocks

This section describes the different kinds of leaf block that make up a Markdown 
document.

### Thematic Breaks

A line consisting of optionally up to three spaces of indentation, followed by a 
sequence of three or more matching `-`, `_`, or `*` characters, each followed 
optionally by any number of spaces or tabs, forms a thematic break.

<commonmark-runner examples="43"></commonmark-runner>

Wrong characters:

<commonmark-runner examples="44-45"></commonmark-runner>

Not enough characters:

<commonmark-runner examples="46"></commonmark-runner>

Up to three spaces of indentation are allowed:

<commonmark-runner examples="47"></commonmark-runner>

Four spaces of indentation is too many:

<commonmark-runner examples="48-49"></commonmark-runner>

More than three characters may be used:

<commonmark-runner examples="50"></commonmark-runner>

Spaces and tabs are allowed between the characters:

<commonmark-runner examples="51-53"></commonmark-runner>

Spaces and tabs are allowed at the end:

<commonmark-runner examples="54"></commonmark-runner>

However, no other characters may occur in the line:

<commonmark-runner examples="55"></commonmark-runner>

It is required that all of the characters other than spaces or tabs be the same. 
So, this is not a thematic break:

<commonmark-runner examples="56"></commonmark-runner>

Thematic breaks do not need blank lines before or after:

<commonmark-runner examples="57"></commonmark-runner>

Thematic breaks can interrupt a paragraph:

<commonmark-runner examples="58"></commonmark-runner>

If a line of dashes that meets the above conditions for being a thematic break 
could also be interpreted as the underline of a setext heading, the 
interpretation as a setext heading takes precedence. Thus, for example, this is 
a setext heading, not a paragraph followed by a thematic break:

<commonmark-runner examples="59"></commonmark-runner>

If you want a thematic break in a list item, use a different bullet:

<commonmark-runner examples="61"></commonmark-runner>

### ATX headings

An ATX heading consists of a string of characters, parsed as inline content, 
between an opening sequence of 1–6 unescaped `#` characters and an optional 
closing sequence of any number of unescaped `#` characters. The opening sequence 
of `#` characters must be followed by spaces or tabs, or by the end of line. The 
optional closing sequence of `#`s must be preceded by spaces or tabs and may be 
followed by spaces or tabs only. The opening `#` character may be preceded by up 
to three spaces of indentation. The raw contents of the heading are stripped of 
leading and trailing space or tabs before being parsed as inline content. The 
heading level is equal to the number of `#` characters in the opening sequence.

Simple headings:

<commonmark-runner examples="62"></commonmark-runner>

More than six # characters is not a heading:

<commonmark-runner examples="63"></commonmark-runner>

At least one space or tab is required between the `#` characters and the 
heading’s contents, unless the heading is empty. Note that many implementations 
currently do not require the space. However, the space was required by the 
original ATX implementation, and it helps prevent things like the following 
from being parsed as headings:

<commonmark-runner examples="64"></commonmark-runner>

This is not a heading, because the first # is escaped:

<commonmark-runner examples="65"></commonmark-runner>

Contents are parsed as inlines:

<commonmark-runner examples="66"></commonmark-runner>

Leading and trailing spaces or tabs are ignored in parsing inline content:

<commonmark-runner examples="67"></commonmark-runner>

Up to three spaces of indentation are allowed:

<commonmark-runner examples="68"></commonmark-runner>

Four spaces of indentation is too many:

<commonmark-runner examples="69-70"></commonmark-runner>

ATX headings need not be separated from surrounding content by blank lines, and 
they can interrupt paragraphs:

<commonmark-runner examples="77-78"></commonmark-runner>

ATX headings can be empty:

<commonmark-runner examples="79"></commonmark-runner>

### Setext Headings

A setext heading consists of one or more lines of text, not interrupted by a 
blank line, of which the first line does not have more than 3 spaces of 
indentation, followed by a setext heading underline. The lines of text must be 
such that, were they not followed by the setext heading underline, they would 
be interpreted as a paragraph: they cannot be interpretable as a code fence, 
ATX heading, block quote, thematic break, list item, or HTML block.

A setext heading underline is a sequence of `=` characters or a sequence of `-` 
characters, with no more than 3 spaces of indentation and any number of 
trailing spaces or tabs.

The heading is a level 1 heading if `=` characters are used in the setext 
heading underline, and a level 2 heading if `-` characters are used. The 
contents of the heading are the result of parsing the preceding lines of text 
as CommonMark inline content.

In general, a setext heading need not be preceded or followed by a blank line. 
However, it cannot interrupt a paragraph, so when a setext heading comes after 
a paragraph, a blank line is needed between them.

Simple examples:

<commonmark-runner examples="80"></commonmark-runner>

The content of the header may span more than one line:

<commonmark-runner examples="81"></commonmark-runner>

The contents are the result of parsing the headings’s raw content as inlines. 
The heading’s raw content is formed by concatenating the lines and removing 
initial and final spaces or tabs.

<commonmark-runner examples="82"></commonmark-runner>

The underlining can be any length:

<commonmark-runner examples="83"></commonmark-runner>

The heading content can be preceded by up to three spaces of indentation, and 
need not line up with the underlining:

<commonmark-runner examples="84"></commonmark-runner>

Four spaces of indentation is too many:

<commonmark-runner examples="85"></commonmark-runner>

The setext heading underline can be preceded by up to three spaces of 
indentation, and may have trailing spaces or tabs:

<commonmark-runner examples="86"></commonmark-runner>

Four spaces of indentation is too many:

<commonmark-runner examples="87"></commonmark-runner>

The setext heading underline cannot contain internal spaces or tabs:

<commonmark-runner examples="88"></commonmark-runner>

Trailing spaces or tabs in the content line do not cause a hard line break:

<commonmark-runner examples="89"></commonmark-runner>

Nor does a backslash at the end:

<commonmark-runner examples="90"></commonmark-runner>

Since indicators of block structure take precedence over indicators of inline 
structure, the following are setext headings:

<commonmark-runner examples="91"></commonmark-runner>

A blank line is needed between a paragraph and a following setext heading, 
since otherwise the paragraph becomes part of the heading’s content:

<commonmark-runner examples="95"></commonmark-runner>

But in general a blank line is not required before or after setext headings:

<commonmark-runner examples="96"></commonmark-runner>

Setext headings cannot be empty:

<commonmark-runner examples="97"></commonmark-runner>

Setext heading text lines must not be interpretable as block constructs other 
than paragraphs. So, the line of dashes in these examples gets interpreted as 
a thematic break:

<commonmark-runner examples="98-101"></commonmark-runner>

If you want a heading with > foo as its literal text, you can use backslash 
escapes:

<commonmark-runner examples="102"></commonmark-runner>

**Compatibility note:** Most existing Markdown implementations do not allow the 
text of setext headings to span multiple lines. But there is no consensus about 
how to interpret

    Foo
    bar
    ---
    baz

One can find four different interpretations:

 1. paragraph “Foo”, heading “bar”, paragraph “baz”
 2. paragraph “Foo bar”, thematic break, paragraph “baz”
 3. paragraph “Foo bar — baz”
 4. heading “Foo bar”, paragraph “baz”

We find interpretation 4 most natural, and interpretation 4 increases the 
expressive power of CommonMark, by allowing multiline headings. Authors who 
want interpretation 1 can put a blank line after the first paragraph:

<commonmark-runner examples="103"></commonmark-runner>

Authors who want interpretation 2 can put blank lines around the thematic 
break,

<commonmark-runner examples="104"></commonmark-runner>

or use a thematic break that cannot count as a setext heading underline, such as

<commonmark-runner examples="105"></commonmark-runner>

Authors who want interpretation 3 can use backslash escapes:

<commonmark-runner examples="106"></commonmark-runner>

### Indented Code Blocks

An indented code block is composed of one or more indented chunks separated by 
blank lines. An indented chunk is a sequence of non-blank lines, each preceded 
by four or more spaces of indentation. The contents of the code block are the 
literal contents of the lines, including trailing line endings, minus four 
spaces of indentation. An indented code block has no info string.

An indented code block cannot interrupt a paragraph, so there must be a blank 
line between a paragraph and a following indented code block. (A blank line is 
not needed, however, between a code block and a following paragraph.)

<commonmark-runner examples="107"></commonmark-runner>

If there is any ambiguity between an interpretation of indentation as a code 
block and as indicating that material belongs to a list item, the list item 
interpretation takes precedence:

<commonmark-runner examples="108-109"></commonmark-runner>

The contents of a code block are literal text, and do not get parsed as 
Markdown:

<commonmark-runner examples="110"></commonmark-runner>

Here we have three chunks separated by blank lines:

<commonmark-runner examples="111"></commonmark-runner>

Any initial spaces or tabs beyond four spaces of indentation will be included 
in the content, even in interior blank lines:

<commonmark-runner examples="112"></commonmark-runner>

An indented code block cannot interrupt a paragraph. (This allows hanging 
indents and the like.)

<commonmark-runner examples="113"></commonmark-runner>

However, any non-blank line with fewer than four spaces of indentation ends the 
code block immediately. So a paragraph may occur immediately after indented 
code:

<commonmark-runner examples="114"></commonmark-runner>

And indented code can occur immediately before and after other kinds of blocks:

<commonmark-runner examples="115"></commonmark-runner>

The first line can be preceded by more than four spaces of indentation:

<commonmark-runner examples="116"></commonmark-runner>

Blank lines preceding or following an indented code block are not included in it:

<commonmark-runner examples="117"></commonmark-runner>

Trailing spaces or tabs are included in the code block’s content:

<commonmark-runner examples="118"></commonmark-runner>

### Fenced Code Blocks

A code fence is a sequence of at least three consecutive backtick characters 
`` ` `` or tildes `~`. (Tildes and backticks cannot be mixed.) A fenced code 
block begins with a code fence, preceded by up to three spaces of indentation.

The line with the opening code fence may optionally contain some text following 
the code fence; this is trimmed of leading and trailing spaces or tabs and 
called the info string. If the info string comes after a backtick fence, it may 
not contain any backtick characters. (The reason for this restriction is that 
otherwise some inline code would be incorrectly interpreted as the beginning of 
a fenced code block.)

The content of the code block consists of all subsequent lines, until a closing 
code fence of the same type as the code block began with (backticks or tildes), 
and with at least as many backticks or tildes as the opening code fence. If the 
leading code fence is preceded by N spaces of indentation, then up to N spaces 
of indentation are removed from each line of the content (if present). (If a 
content line is not indented, it is preserved unchanged. If it is indented N 
spaces or less, all of the indentation is removed.)

The closing code fence may be preceded by up to three spaces of indentation, 
and may be followed only by spaces or tabs, which are ignored. If the end of 
the containing block (or document) is reached and no closing code fence has 
been found, the code block contains all of the lines after the opening code 
fence until the end of the containing block (or document). (An alternative spec 
would require backtracking in the event that a closing code fence is not found. 
But this makes parsing much less efficient, and there seems to be no real 
downside to the behavior described here.)

A fenced code block may interrupt a paragraph, and does not require a blank 
line either before or after.

The content of a code fence is treated as literal text, not parsed as inlines. 
The first word of the info string is typically used to specify the language of 
the code sample, and rendered in the class attribute of the code tag. However, 
this spec does not mandate any particular treatment of the info string.

Here is a simple example with backticks:

<commonmark-runner examples="119"></commonmark-runner>

With tildes:

<commonmark-runner examples="120"></commonmark-runner>

Fewer than three backticks is not enough:

<commonmark-runner examples="121"></commonmark-runner>

The closing code fence must use the same character as the opening fence:

<commonmark-runner examples="122-123"></commonmark-runner>

The closing code fence must be at least as long as the opening fence:

<commonmark-runner examples="124-125"></commonmark-runner>

Unclosed code blocks are closed by the end of the document (or the enclosing 
block quote or list item):

<commonmark-runner examples="126-128"></commonmark-runner>

A code block can have all empty lines as its content:

<commonmark-runner examples="129"></commonmark-runner>

A code block can be empty:

<commonmark-runner examples="130"></commonmark-runner>

Four spaces of indentation is too many:

<commonmark-runner examples="134"></commonmark-runner>

Closing fences may be preceded by up to three spaces of indentation, and their 
indentation need not match that of the opening fence:

<commonmark-runner examples="135-136"></commonmark-runner>

This is not a closing fence, because it is indented 4 spaces:

<commonmark-runner examples="137"></commonmark-runner>

Code fences (opening and closing) cannot contain internal spaces or tabs:

<commonmark-runner examples="138-139"></commonmark-runner>

Fenced code blocks can interrupt paragraphs, and can be followed directly by 
paragraphs, without a blank line between:

<commonmark-runner examples="140"></commonmark-runner>

Other blocks can also occur before and after fenced code blocks without an 
intervening blank line:

<commonmark-runner examples="141"></commonmark-runner>

An info string can be provided after the opening code fence. Although this spec 
doesn’t mandate any particular treatment of the info string, the first word is 
typically used to specify the language of the code block. In HTML output, the 
language is normally indicated by adding a class to the code element consisting 
of language- followed by the language name.

<commonmark-runner examples="142,144"></commonmark-runner>

Info strings for backtick code blocks cannot contain backticks:

<commonmark-runner examples="145"></commonmark-runner>

Closing code fences cannot have info strings:

<commonmark-runner examples="147"></commonmark-runner>

### HTML blocks

An HTML block is a group of lines that is treated as raw HTML (and will not be 
escaped in HTML output).

There are ~~seven~~ two kinds of HTML block, which can be defined by their start 
and end conditions. The block begins with a line that meets a start condition 
(after up to three optional spaces of indentation). It ends with the first 
subsequent line that meets a matching end condition, or the last line of the 
document, or the last line of the container block containing the current HTML 
block, if no line is encountered that meets the end condition. If the first 
line meets both the start condition and the end condition, the block will 
contain just that line.

 1. Start condition: line begins with the string `<pre`, `<script`, `<style`, 
    or `<textarea` (case-insensitive), followed by a space, a tab, the string 
    `>`, or the end of the line.

    End condition: line contains an end tag `</pre>`, `</script>`, `</style>`, 
    or `</textarea>` (case-insensitive; it need not match the start tag).
    
 2. Start condition: line begins with a complete open tag (with any tag name 
    other than `pre`, `script`, `style`, or `textarea`) or a complete closing 
    tag, followed by zero or more spaces and tabs, followed by the end of the 
    line.

    End condition: line is followed by a blank line.

HTML blocks continue until they are closed by their appropriate end condition, 
or the last line of the document or other container block. This means any HTML 
within an HTML block that might otherwise be recognised as a start condition 
will be ignored by the parser and passed through as-is, without changing the 
parser’s state.

All types of HTML blocks except type 2 may interrupt a paragraph. (This 
restriction is intended to prevent unwanted interpretation of long tags inside 
a wrapped paragraph as starting HTML blocks.)

In type 2 blocks, the tag name can be anything:

<commonmark-runner examples="163-164"></commonmark-runner>

These rules are designed to allow us to work with tags that can function as 
either block-level or inline-level tags. The `<del>` tag is a nice example. We 
can surround content with `<del>` tags in three different ways. In this case, 
we get a raw HTML block, because the `<del>` tag is on a line by itself:

<commonmark-runner examples="166"></commonmark-runner>

Finally, in this case, the `<del>` tags are interpreted as raw HTML inside the 
CommonMark paragraph. (Because the tag is not on a line by itself, we get 
inline HTML rather than an HTML block.)

<commonmark-runner examples="168"></commonmark-runner>

HTML tags designed to contain literal content (`pre`, `script`, `style`, 
`textarea`), comments, processing instructions, and declarations are treated 
somewhat differently. Instead of ending at the first blank line, these blocks 
end at the first line containing a corresponding end tag. As a result, these 
blocks can contain blank lines:

A `pre` tag (type 1):

<commonmark-runner examples="169"></commonmark-runner>

A `script` tag (type 1):

<commonmark-runner examples="170"></commonmark-runner>

A `textarea` tag (type 1):

<commonmark-runner examples="171"></commonmark-runner>

A `style` tag (type 1):

<commonmark-runner examples="172"></commonmark-runner>

If there is no matching end tag, the block will end at the end of the document 
(or the enclosing block quote or list item):

<commonmark-runner examples="173"></commonmark-runner>

The end tag can occur on the same line as the start tag:

<commonmark-runner examples="176"></commonmark-runner>

Note that anything on the last line after the end tag will be included in the HTML block:

<commonmark-runner examples="178"></commonmark-runner>

HTML blocks of type 2 cannot interrupt a paragraph:

<commonmark-runner examples="187"></commonmark-runner>

### Link Reference Definitions

A link reference definition consists of a link label, optionally preceded by up 
to three spaces of indentation, followed by a colon (:), optional spaces or 
tabs (including up to one line ending), a link destination, optional spaces or 
tabs (including up to one line ending), and an optional link title, which if it 
is present must be separated from the link destination by spaces or tabs. No 
further character may occur.

A link reference definition does not correspond to a structural element of a 
document. Instead, it defines a label which can be used in reference links and 
reference-style images elsewhere in the document. Link reference definitions 
can come either before or after the links that use them.

<commonmark-runner examples="192-193"></commonmark-runner>

<commonmark-runner examples="195"></commonmark-runner>

The title may extend over multiple lines:

<commonmark-runner examples="196"></commonmark-runner>

However, it may not contain a blank line:

<commonmark-runner examples="197"></commonmark-runner>

The title may be omitted:

<commonmark-runner examples="198"></commonmark-runner>

The link destination may not be omitted:

<commonmark-runner examples="199"></commonmark-runner>

However, an empty link destination may be specified using angle brackets:

<commonmark-runner examples="200"></commonmark-runner>

The title must be separated from the link destination by spaces or tabs:

<commonmark-runner examples="201"></commonmark-runner>




<commonmark-runner examples="482"></commonmark-runner>

