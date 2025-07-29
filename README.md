---
{
    "modules": [ "./src/live-editor.ts" ]
}
---

# 🖋️ Append Markdown Directly in the DOM

**MarkAppend** is a fast, single-file Markdown parser designed for simplicity 
and performance. Unlike most Markdown parsers that generate HTML strings, 
MarkAppend modifies the DOM directly, making it ideal for dynamic web 
applications that need the best performance.

MarkAppend supports all the essential [CommonMark] features, skipping only the 
more esoteric rules that most users aren't aware of &mdash; mainly to keep the 
implementation simple(r).

## 👉 Usage

`appendMarkdown` is the only one exported function in the library. Its signature 
looks like this:
```ts
export function appendMarkdown(input: string, root: Element)
```
The `input` string parameter contains the markdown that is rendered under the 
`root` element.

## 🍰 Implementation

MarkAppend source is written in a [literate style], which hopefully makes the 
code easier to decipher. You can explore the documentation generated from the
source code [here]. The implementation uses [regular expressions] heavily and is 
very imperative in style. Special attention is paid to minimize string copying 
and unnecessary memory allocations.

As the Markdown syntax is inherently ill-defined and ambigious, parsing it
inevitably becomes a convoluted process. Lot of custom code is needed to handle 
all the special cases. So, don't expect to find a beautiful and easy-to-follow 
implementation 💩

## 🎙️ Live Editor

Test MarkAppend with the live editor below.

<live-editor></live-editor>

[CommonMark]: https://spec.commonmark.org/
[literate style]: https://en.wikipedia.org/wiki/Literate_programming
[here]: src/parser.html
[regular expressions]: https://en.wikipedia.org/wiki/Regular_expression