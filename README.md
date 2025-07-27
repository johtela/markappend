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

The parser is written in [literate style] and can be examined [here]. The 
implementation uses [regular expressions] heavily, and is very imperative in
style. Performance is the most important design criteria of the code. It tries
to avoid unnecessary string copying and memory allocations in general.

[CommonMark]: https://spec.commonmark.org/
[literate style]: https://en.wikipedia.org/wiki/Literate_programming
[here]: src/parser.html
[regular expressions]: https://en.wikipedia.org/wiki/Regular_expression