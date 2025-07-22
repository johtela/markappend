# 🥥 Append Markdown Directly in the DOM

MarkAppend is a fast, single-file Markdown parser designed for simplicity and 
performance. Unlike traditional parsers that generate HTML strings, MarkAppend 
updates the DOM directly, making it ideal for dynamic web applications. This 
also improves performance.

MarkAppend supports all the essential CommonMark features, skipping only the 
more esoteric rules that most users aren't aware of - mainly to keep the 
implementation simple(r).

With MarkAppend, you get instant rendering, minimal overhead, and an 
easy-to-integrate solution for modern projects.

## 👉 Usage

`appendMarkdown` is the only one exported function in the library. Its signature 
looks like this:
```ts
export function appendMarkdown(input: string, root: Element)
```
The `input` string contains the markdown to be rendered under the `root` 
element.
