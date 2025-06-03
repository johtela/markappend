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
 * The only function we need for the tests is below. It takes a description of
 * the test casse, an input markdown string, and the expected HTML output as
 * arguments and verifies that the input converts to correct output.
 */
function mdTest(description: string, input: string, 
    output: string) {
    let doc = document.createElement('div')
    sd.markdownToHtml(input, doc)
    let html = doc.innerHTML
    test(description, t => t.equals(html, output, 
        `Convert: "${ws(input)}"\nExpected: "${ws(output)}"\nActual: "${
            ws(html)}"`))
}
/**
 * Convert whitespace to visible characters.
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
mdTest("Example1: Tabs are treated as 4 spaces", 
    "\tfoo\tbaz\t\tbim",
    "<pre><code>foo\tbaz\t\tbim</code></pre>")
    
mdTest("Example2: Tabs are treated as 4 spaces", 
    "  \tfoo\tbaz\t\tbim",
    "<pre><code>foo\tbaz\t\tbim</code></pre>")
    
mdTest("Example3: Tabs are treated as 4 spaces", 
    "    a\ta\n    ὐ\ta",
    "<pre><code>a\ta\nὐ\ta</code></pre>")
/**
 * ## Results
 * 
 * <test-runner name="CommonMark tests"></test-runner>
 */