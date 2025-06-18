/**
 * # Helper Code
 * 
 * Helper functions for creating DOM elements.
 * 
 * The `elem` function creates an HTML element of the specified `tag` type.
 * Optionally, any number of child nodes can be appended to the created element.
 * This provides a convenient way to construct DOM trees.
 */
export function elem<K extends keyof HTMLElementTagNameMap>(tag: K, 
    ...children: Node[]): HTMLElementTagNameMap[K] {
    let res = document.createElement(tag)
    if (children.length > 0)
        res.append(...children)
    return res
}
/**
 * Creates a text node containing the specified string data.
 */
export function text(data: string): Text {
    return document.createTextNode(data)
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
export function highlightWs(value: string): Node[] {
    let res: Node[] = []
    let ws = /\s/g
    let pos = 0
    let match: RegExpExecArray | null
    while (match = ws.exec(value)) {
        if (pos < match.index)
            res.push(text(value.substring(pos, match.index)))
        pos = match.index + match.length
        let span = elem('span', text(wsText(match[0])))
        span.className = "ws"
        res.push(span)
    }
    if (pos < value.length)
        res.push(text(value.slice(pos)))
    return res
}

function wsText(ch: string): string {
    let res: Element
    switch (ch) {
        case "\t": return "→"
        case "\n": return "↩\n"
        case " ": return "·"
        default: return ch
    }
}
/**
 * ## Expression Automaton
 * 
 * From [Wikipedia](
 * https://en.wikipedia.org/wiki/Generalized_nondeterministic_finite_automaton):
 * 
 * > In the theory of computation, a **generalized nondeterministic finite 
 * > automaton** (GNFA), also known as an **expression automaton** or a 
 * > **generalized nondeterministic finite state machine**, is a variation of a 
 * > nondeterministic finite automaton (NFA) where each transition is labeled 
 * > with a regular expression. The GNFA reads blocks of symbols from the input 
 * > that match the regular expression on the transition. There are several 
 * > differences between a standard finite state machine and a generalized 
 * > nondeterministic finite state machine. A GNFA must have only one start 
 * > state and one accept state, and these cannot be the same state, whereas an 
 * > NFA or DFA may have multiple accept states, and the start state can also 
 * > be an accept state. A GNFA must have only one transition between any two 
 * > states, whereas an NFA or DFA can have multiple transitions between states. 
 * > In a GNFA, each state has a single transition to every other state in the 
 * > machine, although it is common to omit transitions labeled with the empty 
 * > set when drawing generalized nondeterministic finite state machines.
 * 
 * For brevity, we refer to the GNFA as an "expression automaton" and use it to 
 * match complex regular expressions step-by-step, in phases. This approach 
 * helps us construct block parsers whose continuation regular expressions can 
 * change as more lines are read.
 * 
 * States in an expression automaton store lists of incoming and outgoing 
 * transitions.
 */
export interface State {
    next: Transition[]
}
/**
 * A transition consist of the regexp that allows it to be taken and the next 
 * state.
 */
export interface Transition {
    regexp: RegExp
    target: State 
}    
/**
 * Transitions are created in the initialization callback given as argument to
 * the AutoExp constructor. A typical usage is shown below. We create 2 states 
 * and transitions between them.
 * ```ts
 * let ea = new ExpAuto(2, null, (start, s1, s2, accept) => [
 *     transition(start, ".*", s1),
 *     transition(s1, "\\s", s2),
 *     transition(s2, "$", accept)])
 * ```
 * The first argument is the source state, then comes the regexp that allows the
 * transition, and last the target state.
 */
export function transition(source: State, regexp: string | RegExp, 
    target: State): Transition {
    let res = { source, regexp: new RegExp(regexp, "yu"), target }
    source.next.push(res)
    return res
}
/**
 * The expression automaton consists of states and transitions. The start and
 * accept states are stored here, as well as the current state.
 */
export class ExpAuto {
    readonly states: State[]
    readonly transitions: Transition[]
    readonly start: State
    readonly accept: State
    current: State
    /**
     * ### Creating an Expression Automaton
     * 
     * You can create an expression automaton with one call using the function 
     * below. You specify the number of states in the automaton and a callback
     * where you can create the transitions. The first state provided is the 
     * starting state and the last is the end state. The `count` you specify 
     * should exclude these.
     * 
     * Since expression automata always have a single entry and exit point 
     * (start & accept states), they can be chained togeter. By providing the 
     * `previous` automaton we make its accept state the start state of the 
     * created automaton.
     * 
     * The accept state can never have any outgoing transitions. This condition 
     * is validated in the constructor.
     */
    constructor(count: number, init: (...states: State[]) => Transition[]) {
        this.states = new Array<State>(count + 2).fill({ next: [] })
        this.start = this.states[0]
        this.accept = this.states[this.states.length - 1]
        this.transitions = init(...this.states)
        if (this.accept.next.length > 0)
            throw new Error("Accept state cannot have outgoing transitions")
        this.current = this.start
    }
    /**
     * To (re)initialize an automaton to its starting state, you can call the
     * `init` method.
     */
    init() {
        this.current = this.start
    }
    /**
     * ## Executing the Automaton
     * 
     * To advance the automaton, you need to feed it a string, and a starting
     * position in it. The `exec` method takes these as arguments and returns
     * `true` if the whole string was consumed; otherwise `false`. The current
     * state of the automaton changes if the input matched it. You can use the 
     * `accepted` property to check whether the automaton is in the accept 
     * state afterwards.
     */
    exec(input: string, pos: number): boolean {
        if (pos >= input.length)
            return true
        for (let i = 0; i < this.current.next.length; ++i) {
            let tr = this.current.next[i]
            tr.regexp.lastIndex = pos
            let match = tr.regexp.exec(input)
            if (match) {
                this.current = tr.target
                return this.exec(input, match.index + match.length)
            }
        }
        return false
    }
    /** 
     * Returns `true` if the automaton is in the accept state.
     */
    get accepted(): boolean {
        return this.current == this.accept
    }
}