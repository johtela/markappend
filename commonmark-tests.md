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
