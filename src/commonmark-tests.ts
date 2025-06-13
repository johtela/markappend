/**
 * ---
 * {
 *  "modules": [ "./src/commonmark-tests.ts" ]
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
import { StyledElement } from 'litscript/lib/src/custom-elem'
import * as sd from './parser'
import('./spec.json').then(t => tests = t)

/**
 * ## CommonMark Test Case
 * 
 * We read the CommonMark test cases from a JSON file. Its structure is defined 
 * below (unused properties are omitted).
 */
interface CommonMarkTest {
    markdown: string
    html: string
    example: number
    section: string
}
let tests: CommonMarkTest[]
