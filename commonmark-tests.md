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

Both title and destination can contain backslash escapes and literal 
backslashes:

<commonmark-runner examples="202"></commonmark-runner>

A link can come before its corresponding definition:

<commonmark-runner examples="203"></commonmark-runner>

If there are several matching definitions, the first one takes precedence:

<commonmark-runner examples="204"></commonmark-runner>

As noted in the section on Links, matching of labels is case-insensitive (see 
matches).

<commonmark-runner examples="205-206"></commonmark-runner>

Whether something is a link reference definition is independent of whether the 
link reference it defines is used in the document. Thus, for example, the 
following document contains just a link reference definition, and no visible 
content:

<commonmark-runner examples="207"></commonmark-runner>

Here is another one:

<commonmark-runner examples="208"></commonmark-runner>

This is not a link reference definition, because there are characters other than 
spaces or tabs after the title:

<commonmark-runner examples="209"></commonmark-runner>

This is a link reference definition, but it has no title:

<commonmark-runner examples="210"></commonmark-runner>

This is not a link reference definition, because it is indented four spaces:

<commonmark-runner examples="211"></commonmark-runner>

This is not a link reference definition, because it occurs inside a code block:

<commonmark-runner examples="212"></commonmark-runner>

A link reference definition cannot interrupt a paragraph.

<commonmark-runner examples="213"></commonmark-runner>

However, it can directly follow other block elements, such as headings and 
thematic breaks, and it need not be followed by a blank line.

<commonmark-runner examples="214-216"></commonmark-runner>

Several link reference definitions can occur one after another, without 
intervening blank lines.

<commonmark-runner examples="217"></commonmark-runner>

Link reference definitions can occur inside block containers, like lists and 
block quotations. They affect the entire document, not just the container in 
which they are defined:

<commonmark-runner examples="218"></commonmark-runner>

### Paragraphs

A sequence of non-blank lines that cannot be interpreted as other kinds of 
blocks forms a paragraph. The contents of the paragraph are the result of 
parsing the paragraph’s raw content as inlines. The paragraph’s raw content is 
formed by concatenating the lines and removing initial and final spaces or tabs.

A simple example with two paragraphs:

<commonmark-runner examples="219"></commonmark-runner>

Paragraphs can contain multiple lines, but no blank lines:

<commonmark-runner examples="220"></commonmark-runner>

Multiple blank lines between paragraphs have no effect:

<commonmark-runner examples="221"></commonmark-runner>

Leading spaces or tabs are skipped:

<commonmark-runner examples="222"></commonmark-runner>

Lines after the first may be indented any amount, since indented code blocks 
cannot interrupt paragraphs.

<commonmark-runner examples="223"></commonmark-runner>

However, the first line may be preceded by up to three spaces of indentation. 
Four spaces of indentation is too many:

<commonmark-runner examples="224-225"></commonmark-runner>

### Blank Lines

Blank lines between block-level elements are ignored, except for the role they 
play in determining whether a list is tight or loose.

Blank lines at the beginning and end of the document are also ignored.

<commonmark-runner examples="227"></commonmark-runner>

## Container Blocks

A container block is a block that has other blocks as its contents. There are 
two basic kinds of container blocks: block quotes and list items. Lists are 
meta-containers for list items.

We define the syntax for container blocks recursively. The general form of the 
definition is:

If X is a sequence of blocks, then the result of transforming X in such-and-such 
a way is a container of type Y with these blocks as its content.

So, we explain what counts as a block quote or list item by explaining how these 
can be generated from their contents. This should suffice to define the syntax, 
although it does not give a recipe for parsing these constructions. (A recipe is 
provided below in the section entitled A parsing strategy.)

### Block Quotes

A block quote marker, optionally preceded by up to three spaces of indentation, 
consists of (a) the character `>` together with a following space of 
indentation, or (b) a single character `>` not followed by a space of 
indentation.

The following rules define block quotes:

1.  **Basic case.** If a string of lines Ls constitute a sequence of blocks 
    _Bs_, then the result of prepending a block quote marker to the beginning of 
    each line in _Ls_ is a block quote containing _Bs_.

2.  **Consecutiveness**. A document cannot contain two block quotes in a row 
    unless there is a blank line between them.

Nothing else counts as a block quote.

Here is a simple example:

<commonmark-runner examples="228"></commonmark-runner>

The space or tab after the > characters can be omitted:

<commonmark-runner examples="229"></commonmark-runner>

The > characters can be preceded by up to three spaces of indentation:

<commonmark-runner examples="230"></commonmark-runner>

Four spaces of indentation is too many:

<commonmark-runner examples="231"></commonmark-runner>

Laziness is not supported so, all the exceptions work:

<commonmark-runner examples="234-237"></commonmark-runner>

A block quote can be empty:

<commonmark-runner examples="239-240"></commonmark-runner>

A block quote can have initial or final blank lines:

<commonmark-runner examples="241"></commonmark-runner>

A blank line always separates block quotes:

<commonmark-runner examples="242"></commonmark-runner>

Consecutiveness means that if we put these block quotes together, we get a 
single block quote:

<commonmark-runner examples="243"></commonmark-runner>

To get a block quote with two paragraphs, use:

<commonmark-runner examples="244"></commonmark-runner>

Block quotes can interrupt paragraphs:

<commonmark-runner examples="245"></commonmark-runner>

In general, blank lines are not needed before or after block quotes:

<commonmark-runner examples="246"></commonmark-runner>

More laziness exceptions:

<commonmark-runner examples="248-249"></commonmark-runner>

When including an indented code block in a block quote, remember that the block 
quote marker includes both the `>` and a following space of indentation. So five 
spaces are needed after the `>`:

<commonmark-runner examples="252"></commonmark-runner>

### List Items

A list marker is a bullet list marker or an ordered list marker.

A bullet list marker is a `-`, `+`, or `*` character.

An ordered list marker is a sequence of 1–9 arabic digits (0-9), followed by 
either a `.` character or a `)` character. (The reason for the length limit is 
that with 10 digits we start seeing integer overflows in some browsers.)

The following rules define list items:

1.  **Basic case**. If a sequence of lines Ls constitute a sequence of blocks 
    _Bs_ starting with a character other than a space or tab, and _M_ is a list 
    marker of width _W_ followed by 1 ≤ _N_ ≤ 4 spaces of indentation, then the 
    result of prepending _M_ and the following spaces to the first line of _Ls_, 
    and indenting subsequent lines of _Ls_ by _W_ + _N_ spaces, is a list item 
    with _Bs_ as its contents. The type of the list item (bullet or ordered) is 
    determined by the type of its list marker. If the list item is ordered, then 
    it is also assigned a start number, based on the ordered list marker.

    Exceptions:

    1.  When the first list item in a list interrupts a paragraph—that is, when 
        it starts on a line that would otherwise count as paragraph continuation 
        text—then (a) the lines Ls must not begin with a blank line, and (b) if 
        the list item is ordered, the start number must be 1.
    2.  If any line is a thematic break then that line is not a list item.

For example, let _Ls_ be the lines

<commonmark-runner examples="253"></commonmark-runner>

And let M be the marker 1., and _N_ = 2. Then rule #1 says that the following is 
an ordered list item with start number 1, and the same contents as Ls:

<commonmark-runner examples="254"></commonmark-runner>

The most important thing to notice is that the position of the text after the 
list marker determines how much indentation is needed in subsequent blocks in 
the list item. If the list marker takes up two spaces of indentation, and there 
are three spaces between the list marker and the next character other than a 
space or tab, then blocks must be indented five spaces in order to fall under 
the list item.

Here are some examples showing how far content must be indented to be put under 
the list item:

<commonmark-runner examples="255-258"></commonmark-runner>

It is tempting to think of this in terms of columns: the continuation blocks 
must be indented at least to the column of the first character other than a 
space or tab after the list marker. However, that is not quite right. The spaces 
of indentation after the list marker determine how much relative indentation is 
needed. Which column this indentation reaches will depend on how the list item 
is embedded in other constructions, as shown by this example:

<commonmark-runner examples="259"></commonmark-runner>

Here two occurs in the same column as the list marker `1.`, but is actually 
contained in the list item, because there is sufficient indentation after the 
last containing blockquote marker.

The converse is also possible. In the following example, the word `two` occurs 
far to the right of the initial text of the list item, `one`, but it is not 
considered part of the list item, because it is not indented far enough past the 
blockquote marker:

<commonmark-runner examples="260"></commonmark-runner>

Note that at least one space or tab is needed between the list marker and any 
following content, so these are not list items:

<commonmark-runner examples="261"></commonmark-runner>

A list item may contain blocks that are separated by more than one blank line.

<commonmark-runner examples="262"></commonmark-runner>

A list item may contain any kind of block:

<commonmark-runner examples="263"></commonmark-runner>

A list item that contains an indented code block will preserve empty lines within the code block verbatim.

<commonmark-runner examples="264"></commonmark-runner>

Note that ordered list start numbers must be nine digits or less:

<commonmark-runner examples="265-266"></commonmark-runner>

A start number may begin with 0s:

<commonmark-runner examples="267-268"></commonmark-runner>

A start number may not be negative:

<commonmark-runner examples="269"></commonmark-runner>

2.  Item starting with indented code. If a sequence of lines _Ls_ constitute a 
    sequence of blocks _Bs_ starting with an indented code block, and _M_ is a 
    list marker of width _W_ followed by one space of indentation, then the 
    result of prepending _M_ and the following space to the first line of _Ls_, 
    and indenting subsequent lines of Ls by _W_ + 1 spaces, is a list item with 
    _Bs_ as its contents. If a line is empty, then it need not be indented. The 
    type of the list item (bullet or ordered) is determined by the type of its 
    list marker. If the list item is ordered, then it is also assigned a start 
    number, based on the ordered list marker.

An indented code block will have to be preceded by four spaces of indentation 
beyond the edge of the region where text will be included in the list item. In 
the following case that is 6 spaces:

<commonmark-runner examples="270"></commonmark-runner>

And in this case it is 11 spaces:

<commonmark-runner examples="271"></commonmark-runner>

If the first block in the list item is an indented code block, then by rule #2, 
the contents must be preceded by one space of indentation after the list marker:

<commonmark-runner examples="272-273"></commonmark-runner>

Note that an additional space of indentation is interpreted as space inside the 
code block:

<commonmark-runner examples="274"></commonmark-runner>

Note that rules #1 and #2 only apply to two cases: (a) cases in which the lines 
to be included in a list item begin with a character other than a space or tab, 
and (b) cases in which they begin with an indented code block. In a case like 
the following, where the first block begins with three spaces of indentation, 
the rules do not allow us to form a list item by indenting the whole thing and 
prepending a list marker:

<commonmark-runner examples="275-276"></commonmark-runner>

This is not a significant restriction, because when a block is preceded by up to 
three spaces of indentation, the indentation can always be removed without a 
change in interpretation, allowing rule #1 to be applied. So, in the above case:

<commonmark-runner examples="277"></commonmark-runner>

3.  Item starting with a blank line. If a sequence of lines _Ls_ starting with a 
    single blank line constitute a (possibly empty) sequence of blocks _Bs_, and 
    _M_ is a list marker of width _W_, then the result of prepending _M_ to the 
    first line of _Ls_, and preceding subsequent lines of _Ls_ by _W_ + 1 spaces 
    of indentation, is a list item with _Bs_ as its contents. If a line is 
    empty, then it need not be indented. The type of the list item (bullet or 
    ordered) is determined by the type of its list marker. If the list item is 
    ordered, then it is also assigned a start number, based on the ordered list 
    marker.

Here are some list items that start with a blank line but are not empty:

<commonmark-runner examples="278"></commonmark-runner>

When the list item starts with a blank line, the number of spaces following the 
list marker doesn’t change the required indentation:

<commonmark-runner examples="279"></commonmark-runner>

A list item can begin with at most one blank line. In the following example, 
`foo` is not part of the list item:

<commonmark-runner examples="280"></commonmark-runner>

Here is an empty bullet list item:

<commonmark-runner examples="281"></commonmark-runner>

It does not matter whether there are spaces or tabs following the list marker:

<commonmark-runner examples="282"></commonmark-runner>

Here is an empty ordered list item:

<commonmark-runner examples="283"></commonmark-runner>

A list may start or end with an empty list item:

<commonmark-runner examples="284"></commonmark-runner>

However, an empty list item cannot interrupt a paragraph:

<commonmark-runner examples="285"></commonmark-runner>

4.  Indentation. If a sequence of lines _Ls_ constitutes a list item according 
    to rule #1, #2, or #3, then the result of preceding each line of _Ls_ by
    up to three spaces of indentation (the same for each line) also 
    constitutes a list item with the same contents and attributes. If a line 
    is empty, then it need not be indented.

Indented one space:

<commonmark-runner examples="286"></commonmark-runner>

Indented two spaces:

<commonmark-runner examples="287"></commonmark-runner>

Indented three spaces:

<commonmark-runner examples="288"></commonmark-runner>

Four spaces indent gives a code block:

<commonmark-runner examples="289"></commonmark-runner>

5.  That’s all. Nothing that is not counted as a list item by rules #1–4 
    counts as a list item.

The rules for sublists follow from the general rules above. A sublist must be 
indented the same number of spaces of indentation a paragraph would need to be 
in order to be included in the list item.

So, in this case we need two spaces indent:

<commonmark-runner examples="294"></commonmark-runner>

Here we need four, because the list marker is wider:

<commonmark-runner examples="296"></commonmark-runner>

Three is not enough:

<commonmark-runner examples="297"></commonmark-runner>

A list may be the first block in a list item:

<commonmark-runner examples="298-299"></commonmark-runner>

### Lists

A list is a sequence of one or more list items of the same type. The list items 
may be separated by any number of blank lines.

Two list items are of the same type if they begin with a list marker of the same 
type. Two list markers are of the same type if (a) they are bullet list markers 
using the same character (`-`, `+`, or `*`) or (b) they are ordered list numbers 
with the same delimiter (either `.` or `)`).

A list is an ordered list if its constituent list items begin with ordered list 
markers, and a bullet list if its constituent list items begin with bullet list 
markers.

The start number of an ordered list is determined by the list number of its 
initial list item. The numbers of subsequent list items are disregarded.

A list is loose if any of its constituent list items are separated by blank 
lines, or if any of its constituent list items directly contain two block-level 
elements with a blank line between them. Otherwise a list is tight. (The 
difference in HTML output is that paragraphs in a loose list are wrapped in 
`<p>` tags, while paragraphs in a tight list are not.)

Changing the bullet or ordered list delimiter starts a new list:

<commonmark-runner examples="301-302"></commonmark-runner>

In CommonMark, a list can interrupt a paragraph. That is, no blank line is 
needed to separate a paragraph from a following list:

<commonmark-runner examples="303"></commonmark-runner>

Since it is well established Markdown practice to allow lists to interrupt 
paragraphs inside list items, the principle of uniformity requires us to allow 
this outside list items as well. (reStructuredText takes a different approach, 
requiring blank lines before lists even inside other list items.)

In order to solve the problem of unwanted lists in paragraphs with hard-wrapped 
numerals, we allow only lists starting with 1 to interrupt paragraphs. Thus,

<commonmark-runner examples="304"></commonmark-runner>

We may still get an unintended result in cases like

<commonmark-runner examples="305"></commonmark-runner>

but this rule should prevent most spurious list captures.

There can be any number of blank lines between items:

<commonmark-runner examples="306-307"></commonmark-runner>

This is a tight list, because the blank lines are in a code block:

<commonmark-runner examples="318"></commonmark-runner>

A single-paragraph list is tight:

<commonmark-runner examples="322-323"></commonmark-runner>

This list is loose, because of the blank line between the two block elements in 
the list item:

<commonmark-runner examples="324"></commonmark-runner>

## Inlines

Inlines are parsed sequentially from the beginning of the character stream to 
the end (left to right, in left-to-right languages). Thus, for example, in

<commonmark-runner examples="327"></commonmark-runner>

`hi` is parsed as code, leaving the backtick at the end as a literal backtick.

### Code Spans

A backtick string is a string of one or more backtick characters (`` ` ``) that 
is neither preceded nor followed by a backtick.

A code span begins with a backtick string and ends with a backtick string of 
equal length. The contents of the code span are the characters between these two 
backtick strings, normalized in the following ways:

-   First, line endings are converted to spaces.
-   If the resulting string both begins and ends with a space character, but 
    does not consist entirely of space characters, a single space character is 
    removed from the front and back. This allows you to include code that begins 
    or ends with backtick characters, which must be separated by whitespace from 
    the opening or closing backtick strings.

This is a simple code span:

<commonmark-runner examples="328"></commonmark-runner>

Here two backticks are used, because the code contains a backtick. This example 
also illustrates stripping of a single leading and trailing space:

<commonmark-runner examples="329"></commonmark-runner>

This example shows the motivation for stripping leading and trailing spaces:

<commonmark-runner examples="330"></commonmark-runner>

Note that only one space is stripped:

<commonmark-runner examples="331"></commonmark-runner>

The stripping only happens if the space is on both sides of the string:

<commonmark-runner examples="332"></commonmark-runner>

Only spaces, and not unicode whitespace in general, are stripped in this way:

<commonmark-runner examples="333"></commonmark-runner>

Line endings are treated like spaces:

<commonmark-runner examples="335-336"></commonmark-runner>

Interior spaces are not collapsed:

<commonmark-runner examples="337"></commonmark-runner>

Note that backslash escapes do not work in code spans. All backslashes are 
treated literally:

<commonmark-runner examples="338"></commonmark-runner>

Backslash escapes are never needed, because one can always choose a string of 
_n_ backtick characters as delimiters, where the code does not contain any 
strings of exactly _n_ backtick characters.

<commonmark-runner examples="339-340"></commonmark-runner>

Code span backticks have higher precedence than any other inline constructs 
except HTML tags and autolinks. Thus, for example, this is not parsed as 
emphasized text, since the second * is part of a code span:

<commonmark-runner examples="341"></commonmark-runner>

And this is not parsed as a link:

<commonmark-runner examples="342"></commonmark-runner>

Code spans, HTML tags, and autolinks have the same precedence. Thus, this is 
code:

<commonmark-runner examples="343"></commonmark-runner>

But this is an HTML tag:

<commonmark-runner examples="344"></commonmark-runner>

And this is code:

<commonmark-runner examples="345"></commonmark-runner>

But this is an autolink:

<commonmark-runner examples="346"></commonmark-runner>

When a backtick string is not closed by a matching backtick string, we just have 
literal backticks:

<commonmark-runner examples="348"></commonmark-runner>

### Emphasis and Strong Emphasis

John Gruber’s original Markdown syntax description says:

Markdown treats asterisks (`*`) and underscores (`_`) as indicators of emphasis. 
Text wrapped with one `*` or `_` will be wrapped with an HTML `<em>` tag; double 
`*`’s or `_`’s will be wrapped with an HTML `<strong>` tag.

This is enough for most users, but these rules leave much undecided, especially 
when it comes to nested emphasis. The original Markdown.pl test suite makes it 
clear that triple `***` and `___` delimiters can be used for strong emphasis, 
and most implementations have also allowed the following patterns:

    ***strong emph***
    ***strong** in emph*
    ***emph* in strong**
    **in strong *emph***
    *in emph **strong***

The following patterns are less widely supported, but the intent is clear and 
they are useful (especially in contexts like bibliography entries):

    *emph *with emph* in it*
    **strong **with strong** in it**

Many implementations have also restricted intraword emphasis to the `*` forms, 
to avoid unwanted emphasis in words containing internal underscores. (It is best 
practice to put these in code spans, but users often do not.)

    internal emphasis: foo*bar*baz
    no emphasis: foo_bar_baz

The rules given below capture all of these patterns, while allowing for 
efficient parsing strategies that do not backtrack.

First, some definitions. A delimiter run is either a sequence of one or more `*` 
characters that is not preceded or followed by a non-backslash-escaped `*` 
character, or a sequence of one or more `_` characters that is not preceded or 
followed by a non-backslash-escaped `_` character.

A left-flanking delimiter run is a delimiter run that is (1) not followed by 
Unicode whitespace, and either (2a) not followed by a Unicode punctuation 
character, or (2b) followed by a Unicode punctuation character and preceded by 
Unicode whitespace or a Unicode punctuation character. For purposes of this 
definition, the beginning and the end of the line count as Unicode whitespace.

A right-flanking delimiter run is a delimiter run that is (1) not preceded by 
Unicode whitespace, and either (2a) not preceded by a Unicode punctuation 
character, or (2b) preceded by a Unicode punctuation character and followed by 
Unicode whitespace or a Unicode punctuation character. For purposes of this 
definition, the beginning and the end of the line count as Unicode whitespace.

Here are some examples of delimiter runs.

-   left-flanking but not right-flanking:

        ***abc
        _abc
        **"abc"
        _"abc"

-   right-flanking but not left-flanking:

        abc***
        abc_
        "abc"**
        "abc"_

-   Both left and right-flanking:

        abc***def
        "abc"_"def"

-   Neither left nor right-flanking:

        abc *** def
        a _ b

(The idea of distinguishing left-flanking and right-flanking delimiter runs 
based on the character before and the character after comes from Roopesh 
Chander’s vfmd. vfmd uses the terminology “emphasis indicator string” instead 
of “delimiter run,” and its rules for distinguishing left- and right-flanking 
runs are a bit more complex than the ones given here.)

The following rules define emphasis and strong emphasis:

1.  A single `*` character can open emphasis iff (if and only if) it is part of 
    a left-flanking delimiter run.

2.  A single `_` character can open emphasis iff it is part of a left-flanking 
    delimiter run and either (a) not part of a right-flanking delimiter run or 
    (b) part of a right-flanking delimiter run preceded by a Unicode punctuation 
    character.

3.  A single `*` character can close emphasis iff it is part of a right-flanking 
    delimiter run.

4.  A single `_` character can close emphasis iff it is part of a right-flanking 
    delimiter run and either (a) not part of a left-flanking delimiter run or 
    (b) part of a left-flanking delimiter run followed by a Unicode punctuation 
    character.

5.  A double `**` can open strong emphasis iff it is part of a left-flanking 
    delimiter run.

6.  A double `__` can open strong emphasis iff it is part of a left-flanking 
    delimiter run and either (a) not part of a right-flanking delimiter run or 
    (b) part of a right-flanking delimiter run preceded by a Unicode punctuation 
    character.

7.  A double `**` can close strong emphasis iff it is part of a right-flanking 
    delimiter run.

8.  A double `__` can close strong emphasis iff it is part of a right-flanking 
    delimiter run and either (a) not part of a left-flanking delimiter run or 
    (b) part of a left-flanking delimiter run followed by a Unicode punctuation 
    character.

9.  Emphasis begins with a delimiter that can open emphasis and ends with a 
    delimiter that can close emphasis, and that uses the same character (`_` or 
    `*`) as the opening delimiter. The opening and closing delimiters must 
    belong to separate delimiter runs. If one of the delimiters can both open 
    and close emphasis, then the sum of the lengths of the delimiter runs 
    containing the opening and closing delimiters must not be a multiple of 3 
    unless both lengths are multiples of 3.

10. Strong emphasis begins with a delimiter that can open strong emphasis and 
    ends with a delimiter that can close strong emphasis, and that uses the same 
    character (`_` or `*`) as the opening delimiter. The opening and closing 
    delimiters must belong to separate delimiter runs. If one of the delimiters 
    can both open and close strong emphasis, then the sum of the lengths of the 
    delimiter runs containing the opening and closing delimiters must not be a 
    multiple of 3 unless both lengths are multiples of 3.

11. A literal `*` character cannot occur at the beginning or end of 
    `*`-delimited emphasis or `**`-delimited strong emphasis, unless it is 
    backslash-escaped.

12. A literal `_` character cannot occur at the beginning or end of 
    `_`-delimited emphasis or `__`-delimited strong emphasis, unless it is 
    backslash-escaped.

Where rules 1–12 above are compatible with multiple parsings, the following 
principles resolve ambiguity:

13. The number of nestings should be minimized. Thus, for example, an 
    interpretation `<strong>`...`</strong>` is always preferred to 
    `<em><em>`...`</em></em>`.

14. An interpretation `<em><strong>`...`</strong></em>` is always preferred to 
    `<strong><em>`...`</em></strong>`.

15. When two potential emphasis or strong emphasis spans overlap, so that the 
    second begins before the first ends and ends after the first ends, the first 
    takes precedence. Thus, for example, `*foo _bar* baz_` is parsed as 
    `<em>foo _bar</em> baz_` rather than `*foo <em>bar* baz</em>`.

16. When there are two potential emphasis or strong emphasis spans with the same 
    closing delimiter, the shorter one (the one that opens later) takes 
    precedence. Thus, for example, `**foo **bar baz**` is parsed as 
    `**foo <strong>bar baz</strong>` rather than 
    `<strong>foo **bar baz</strong>`.

17. Inline code spans, links, images, and HTML tags group more tightly than 
    emphasis. So, when there is a choice between an interpretation that contains 
    one of these elements and one that does not, the former always wins. Thus, 
    for example, `*[foo*](bar)` is parsed as `*<a href="bar">foo*</a>` rather 
    than as `<em>[foo</em>](bar)`.

These rules can be illustrated through a series of examples.

Rule 1:

<commonmark-runner examples="350"></commonmark-runner>

This is not emphasis, because the opening `*` is followed by whitespace, and 
hence not part of a left-flanking delimiter run:

<commonmark-runner examples="351"></commonmark-runner>

This is not emphasis, because the opening `*` is preceded by an alphanumeric and 
followed by punctuation, and hence not part of a left-flanking delimiter run:

<commonmark-runner examples="352"></commonmark-runner>

Unicode nonbreaking spaces count as whitespace, too:

<commonmark-runner examples="353"></commonmark-runner>

Unicode symbols count as punctuation, too:

<commonmark-runner examples="354"></commonmark-runner>

Intraword emphasis with `*` is permitted:

<commonmark-runner examples="355-356"></commonmark-runner>

Rule 2:

<commonmark-runner examples="357"></commonmark-runner>

This is not emphasis, because the opening `_` is followed by whitespace:

<commonmark-runner examples="358"></commonmark-runner>

This is not emphasis, because the opening `_` is preceded by an alphanumeric and 
followed by punctuation:

<commonmark-runner examples="359"></commonmark-runner>

Here `_` does not generate emphasis, because the first delimiter run is 
right-flanking and the second left-flanking:

<commonmark-runner examples="363"></commonmark-runner>

This is emphasis, even though the opening delimiter is both left- and 
right-flanking, because it is preceded by punctuation:

<commonmark-runner examples="364"></commonmark-runner>

Rule 3:

This is not emphasis, because the closing delimiter does not match the opening 
delimiter:

<commonmark-runner examples="365"></commonmark-runner>

This is not emphasis, because the closing `*` is preceded by whitespace:

<commonmark-runner examples="366"></commonmark-runner>

A line ending also counts as whitespace:

<commonmark-runner examples="367"></commonmark-runner>

This is not emphasis, because the second `*` is preceded by punctuation and 
followed by an alphanumeric (hence it is not part of a right-flanking delimiter 
run):

<commonmark-runner examples="368"></commonmark-runner>

Intraword emphasis with `*` is allowed:

<commonmark-runner examples="370"></commonmark-runner>

Rule 4:

This is not emphasis, because the closing `_` is preceded by whitespace:

<commonmark-runner examples="371"></commonmark-runner>

This is not emphasis, because the second `_` is preceded by punctuation and 
followed by an alphanumeric:

<commonmark-runner examples="372"></commonmark-runner>


### Links

<commonmark-runner examples="482"></commonmark-runner>

