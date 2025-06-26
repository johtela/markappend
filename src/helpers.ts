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
        pos = match.index + match[0].length
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
 * States in an expression automaton is a list of outgoing transitions.
 */
export type State = Transition[]
/**
 * A transition consist of the regexp that allows it to be taken and the next 
 * state.
 */
export interface Transition {
    regexp: RegExp
    target: State 
    group?: string
}    
/**
 * Transitions are created in the initialization callback given as argument to
 * the AutoExp constructor. 
 * 
 * The first argument is the source state, then comes the regexp that allows the
 * transition, and last the target state.
 */
export function transition(source: State, regexp: string | RegExp, 
    target: State, group?: string): Transition {
    if (source.find(tr => tr.target == target))
        throw new Error("Transition between source and target already exist")
    let res = { regexp: new RegExp(regexp, "yui"), target, group }
    source.push(res)
    return res
}
/**
 * The expression automaton consists of states and transitions. The start and
 * accept states are stored here, as well as the current state.
 */
export class ExpAuto {
    readonly states: State[]
    start: State
    accept: State
    groups: Record<string, string>
    current: State
    /**
     * ### Creating an Expression Automaton
     * 
     * This is a private constructor that is used for initializing properties.
     */
    private constructor(states: State[], start: State, 
        accept: State) {
        this.states = states
        this.start = start
        this.accept = accept
        this.groups = {}
        this.current = this.start
    }
    /**
     * You can create an expression automaton with one call using the method 
     * below. You specify the number states in the automaton and a list of 
     * triplets for transitions. The first state provided is the starting state 
     * and the last is the accept state. You should exclude them from the 
     * `count` argument.
     * 
     * The accept state can never have any outgoing transitions. This condition 
     * is validated in the constructor.
     *
     * A typical usage is shown below. We create 2 states and transitions 
     * between them.
     * ```ts
     * let ea = ExpAuto.create(2, (start, s1, s2, accept) => [
     *     transition(start, ".*", s1),
     *     transition(s1, "\\s", s2),
     *     transition(s2, "$", accept)])
     * ```     
     */
    static create(count: number, init: (...states: State[]) => 
        [State, RegExp | string, State, string?][]): ExpAuto {
        let states = Array.from({ length: count + 2 }, () => [])
        if (init.length != states.length)
            throw new Error("Wrong number of arguments for the init function")
        let transitions = init(...states)
        let start = states[0]
        let accept = states[states.length - 1]
        for (let i = 0; i < transitions.length; ++i) {
            let [source, regexp, target, group] = transitions[i]
            transition(source, regexp, target, group)
        }
        if (accept.length > 0)
            throw new Error("Accept state cannot have outgoing transitions")
        return new ExpAuto(states, start, accept)
    }
    /**
     * The simplest automaton is one with only a start and accept state, and
     * with single transition between those. The `simple` method is a shorthand 
     * for creating such automata.
     */
    static simple(regexp: RegExp | string): ExpAuto {
        return ExpAuto.create(0, (start, accept) => [[start, regexp, accept]])
    }
    /**
     * To (re)initialize an automaton to its starting state, you can call the
     * `init` method.
     */
    init() {
        this.current = this.start
        this.groups = { }
    }
    /**
     * Clone the automaton.
     */
    clone(): ExpAuto {
        let obj = structuredClone(this)
        return new ExpAuto(obj.states, obj.start, obj.accept)
    }
    /**
     * ## Executing the Automaton
     * 
     * To advance the automaton, call the `exec` method with a string and a 
     * starting position. It returns a tuple: `[accepted, newPos]`, where 
     * `accepted` is `true` if the automaton reached the accept state or the
     * whole input string was consumed. `newPos` is the position in the string 
     * after the match attempt. The automaton's current state is updated as it
     * processes the input. Use the `accepted` property to check if the 
     * automaton is in the accept state after execution.
     */
    exec(input: string, pos: number): [boolean, number] {
        if (pos >= input.length || this.current == this.accept)
            return [true, pos]
        for (let i = 0; i < this.current.length; ++i) {
            let curr = this.current
            let tr = curr[i]
            tr.regexp.lastIndex = pos
            let match = tr.regexp.exec(input)
            if (match) {
                if (tr.group) {
                    if (this.groups[tr.group]) 
                        this.groups[tr.group] += match[0]
                    else
                        this.groups[tr.group] = match[0]
                }
                this.current = tr.target
                let res = this.exec(input, match.index + match[0].length)
                if (res[0])
                    return res
            }
        }
        return [false, pos]
    }
    /** 
     * Returns `true` if the automaton is in the accept state.
     */
    get accepted(): boolean {
        return this.current == this.accept
    }
    /**
     * ## Concatenating Automata
     * 
     * Since expression automata always have single entry and exit points
     * (start & accept states), they can be chained togeter with a transition.
     * This creates a new automaton that matches both source automata in 
     * succession.
     * 
     * The `concat` method clones a list of automata, combines their states to
     * a single result automaton, and creates an empty transition from previous 
     * automaton's accept state to the following automaton's start state.
     */
    static concat(...automata: ExpAuto[]): ExpAuto {
        let res = automata[0].clone()
        let prev = res
        for (let i = 1; i < automata.length; ++i) {
            let curr = i == automata.length - 1 ? 
                automata[i] : automata[i].clone()
            let incoming = prev.incoming(prev.accept)
            for (let j = 0; j < incoming.length; ++j) {
                let [,tr] = incoming[j]
                tr.target = curr.start
            }
            res.states.pop()
            res.states.push(...curr.states)
            prev = curr
        }
        res.accept = prev.accept
        return res
    }
    /**
     * If you want to add a simple prefix to an automaton, you can use the 
     * `prepend` method. It creates a new automaton with a new starting state 
     * and adds a transition from it with the given regexp to the old starting 
     * state. Note that we don't modify `this` automaton, but return a new one
     * that is linked to it.
     */
    prepend(regexp: RegExp | string): ExpAuto {
        let newstart: State = []
        let states = [ ...this.states ]
        states[0] = newstart
        let newregexp = typeof(regexp) == 'string' ? regexp : regexp.source
        for (let i = 0; i < this.start.length; ++i) {
            let next = this.start[i]
            transition(newstart, `${newregexp}(?:${next.regexp.source})`, 
                next.target)
        }
        return new ExpAuto(states, newstart, this.accept)
    }
    /**
     * TODO: Explain.
     */
    append(regexp: RegExp | string): ExpAuto {
        let res = this.clone()
        let newregexp = typeof(regexp) == 'string' ? regexp : regexp.source
        for (let i = 0; i < res.states.length; ++i) {
            let state = res.states[i]
            for (let j = 0; j < state.length; ++j) {
                let next = state[j]
                if (next.target == res.accept)
                    next.regexp = new RegExp(
                        `(?:${next.regexp.source})${newregexp}`, "yu")
            }
        }
        return res
    }
    /**
     * ## RegExp Conversions
     * 
     * You can get the possible transitions forward from the current state as
     * a regexp. This is constructed by joining the transition regexps with a 
     * disjunction `|`.
     */
    getRegExp(state: State): string {
        return state.map(t => `(?:${t.regexp.source})`).join("|")
    }
    /**
     * Get the start regexp of the automaton.
     */
    get startRegExp() {
        return this.getRegExp(this.start)
    }
    /**
     * Get the next regexp of the automaton.
     */
    get nextRegExp() {
        return this.getRegExp(this.current)
    }
    /**
     * Incoming transitions for given state. Result is a list of 
     * (_state_, _transition_) tuples, where _state_ is the source of the
     * transition.
     */
    private incoming(state: State): [State, Transition][] {
        let res: [State, Transition][] = []
        for (let i = 0; i < this.states.length; ++i) {
            let source = this.states[i]
            if (source != state)
                for (let j = 0; j < source.length; ++j) {
                    let trans = source[j]
                    if (trans.target == state)
                        res.push([source, trans])
                }
        }
        return res
    }
}