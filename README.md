# ParseQ - Q Language to Python Translator

**An AI-Augmented Code Translation Pipeline**

ParseQ demonstrates a novel approach to program transformation: a **hybrid symbolic-neural transpilation system** that combines traditional compiler techniques with modern AI capabilities. This system showcases how to orchestrate multiple tools - AST parsers, rule-based transformers, and Large Language Models - to solve complex code translation problems that neither approach could handle alone.

## 🤯 Nested Chain of Thought Reasoning

When deployed as an MCP tool, ParseQ enables **recursive AI reasoning** - a fascinating example of collaborative AI workflows:

### **Multi-Level Reasoning Process:**
🧠 **Level 1**: Primary AI encounters complex q code: *"This is confusing, let me use the ParseQ tool..."*  
🔧 **Level 2**: ParseQ tool spins up isolated Claude session: *"I need to disambiguate these operators... looking at argument patterns... this `bang()` with positive int + table must be Enkey..."*  
💭 **Level 3**: Primary AI receives results: *"Based on the tool output, I can see this is actually a left join operation..."*

This creates **chain of thought squared** 🚀 - where one AI's reasoning process includes another AI's reasoning process, enabling:
- 🤖 **Distributed Reasoning**: Two AI instances collaborating on complex problems
- 🔄 **Recursive Problem Decomposition**: Delegating specialized analysis to expert AI sessions  
- 💡 **Meta-Cognitive Processing**: AI reasoning about AI reasoning
- 🛠️ **Collaborative AI Workflows**: Different AI instances specializing in different aspects

## What Makes This Interesting

This project represents an **AI-augmented pipeline** where:
- **Symbolic systems** handle the structured parsing and transformation
- **Neural systems** (Claude AI) resolve semantic ambiguities that are impossible to handle with pure rules
- **Tool orchestration** manages the multi-stage workflow with subprocess isolation and error handling

The result is a **hybrid symbolic-neural system** that translates q language expressions into readable, well-documented Python-like code by using AI-assisted disambiguation of heavily overloaded operators.

## Q parsing rules:

After getting the code parsed using the q code, using Python function `parseq0`, we will have an expression made of lists (enclosed in `[]`), dictionaries (format `{ key_list , value_list }`) and atoms (everything else). We can have different level nestings of lists and dicts.
The rule is: in a list, the first entry is the function to be executed (whether it's marked as Func or not; Func simply means built-in function or function-with-adverb), and all following entries are its parameters. Therefore a list with only one entry represents a lambda (lazy-evaluation). Also everything inside a dictionary, no matter what the deeper nesting is, is also a lambda. Otherwise, we are dealing with eager evaluation. 

Here are some examples of what q does when `parseq0` is called:
```
q)var2string parse "f[(min s; min t)]"
"[Symbol[f], [Func[enlist], [Func[min], Symbol[s]], [Func[min], Symbol[t]]]]"
q)var2string parse "`s`t!(min s; min t)"
"[Func[!], [LSymbol[s, t]], [Func[enlist], [Func[min], Symbol[s]], [Func[min], Symbol[t]]]]"
q)var2string parse "`s`t!((min;s); (min;t))"
"[Func[!], [LSymbol[s, t]], [Func[enlist], [Func[enlist], Func[min], Symbol[s]], [Func[enlist], Func[min], Symbol[t]]]]"
q)var2string parse "select min s, maxs t from c"
"[Func[?], Symbol[c], [], Bool[0], {LSymbol[s, t]: [[Func[min], Symbol[s]], [Func[maxs], Symbol[t]]]}]"
q)var2string parse "exec min s from c"
"[Func[?], Symbol[c], [], [], [[Func[min], Symbol[s]]]]"
q)var2string parse "-1 \"asd\""
"[Long[-1], LChar[a, s, d]]"
q)var2string parse "f[min sums c]"
"[Symbol[f], [Func[min], [Func[sums], Symbol[c]]]]"
q)var2string parse "f[min x;max y]"
"[Symbol[f], [Func[min], Symbol[x]], [Func[max], Symbol[y]]]"
q)var2string parse "select min s from c"
"[Func[?], Symbol[c], [], Bool[0], {LSymbol[s]: [[Func[min], Symbol[s]]]}]"
```

## Overview

Q language operators are heavily overloaded - a single symbol like `!` or `?` can have 6+ different meanings depending on the number and types of arguments. This makes q code very difficult for non-experts to understand.

ParseQ solves this by:
1. **Parsing** q expressions into an Abstract Syntax Tree (AST)
2. **Flattening** nested function calls into step-by-step assignments
3. **Disambiguating** overloaded operators using Claude AI + documentation
4. **Generating** readable Python-like code with explanatory comments

## Architecture

```
Q Expression → AST Parser → Flattened Python → AI Disambiguation → Documented Code
```

### Example Transformation

**Input q code:**
```q
a lj 2!select min s, maxs t from c
```

**Parsed q code:**
```
[Func[lj], Symbol[a], [Func[!], Long[2], [Func[?], Symbol[c], [], Bool[0], {LSymbol[s, t] : [[Func[min], Symbol[s]], [Func[maxs], Symbol[t]]]}]]]
```

**Intermediate flattened code:**
```python
temp1 = query(`c, [], False, {[`s, `t]: [[min, `s], [maxs, `t]]})
temp2 = bang(2, temp1)
result = lj(`a, temp2)
```

**Final disambiguated code using orchestration:**
```python
# Select/Exec - functional qSQL query with 4 args (table, conditions, groupby, aggregations)
# Queries table `c with no conditions ([]), no groupby (False), 
# and aggregations that map columns s and t to min(s) and maxs(t)
temp1 = query(`c, [], False, {[`s, `t]: [[min, `s], [maxs, `t]]})

# Enkey - makes first 2 columns the key of the table (positive integer + table pattern)
temp2 = bang(2, temp1)

# Left join - joins table `a with temp2
result = lj(`a, temp2)
```

### Multiple Statement Example

**Input q code:**
```q
f:{x+1};f[5]
```

**Parsed q code:**
```
[Char[;], [Builtin[:], Symbol[f], Lambda[[x], [Builtin[+], Symbol[x], Long[1]]]], [Symbol[f], Long[5]]]
```

**Flattened code:**
```python
def f(x):
    return x + 1
f(5)
```

## Components

### 1. AST Parser (`parseq.py`)

- **Tokenizer**: Breaks q parse output into tokens
- **Parser**: Builds AST from bracketed LISP-like syntax
- **Node Types**: Symbol, Integer, Float, Boolean, String, Function, List, Dict
- **Type Conversions**:
  - `Symbol[name]` → `name` (variables - no backticks)
  - `Func[name]` → `name` (functions are variables)
  - `LSymbol[s,t]` → `` `s``, `` `t`` (individual symbols with backticks)
  - `Int[5]`, `Long[5]` → `5`
  - `Real[3]`, `Float[3.0]` → `3.0` (ensures decimal point)
  - `Bool[0]`, `Bool[1]` → `False`, `True`
  - `LLong[1,2,3]` → `[1, 2, 3]` (typed lists)
  - `Dict[keys, values]` → `{keys: values}`

### 2. Flattening Engine

Converts deeply nested function calls into step-by-step assignments:
- Identifies nested function calls in AST
- Extracts intermediate expressions into temporary variables
- Generates linear sequence of assignment statements
- Maintains execution order and dependencies

**Smart Temporary Variable Management**:
- **Simple expressions**: `2+3` → `2 + 3` (no temp variables needed)
- **Nested expressions**: `1+2+3+4` → `temp1 = 3 + 4`, `temp2 = 2 + temp1`, `1 + temp2`
- **Final expression optimization**: Last temp variable is replaced with direct expression
- **Assignment handling**: Assignments return `None` to avoid extraneous output

**Output Examples**:
```python
# Simple arithmetic
pr('2+3')        # Output: 2 + 3

# Nested arithmetic  
pr('1+2+3+4')    # Output: temp1 = 3 + 4
                 #         temp2 = 2 + temp1
                 #         1 + temp2

# Variable assignment
pr('a:3')        # Output: a = 3

# Function definition
pr('f:{x+1}')    # Output: def f(x):
                 #             return x + 1

# Multiple statements (semicolon-separated)
pr('a:1;a+2')    # Output: a = 1
                 #         a + 2

# Function definition with call
pr('f:{x+1};f[5]')  # Output: def f(x):
                    #             return x + 1
                    #         f(5)
```

**Multiple Statement Support**: Handles Q's semicolon-separated statements:
- **Parsing**: `Char[;]` is recognized as a sequence operator in the AST
- **Execution Order**: Statements are processed sequentially, maintaining Q semantics
- **Return Value**: Returns the result of the last expression (Q semantics)
- **Lambda Sequences**: Supports multi-statement lambdas like `{a:x;x+a}`

**LSymbol Flattening**: Special handling for `LSymbol[a,b,c]` nodes:
- Creates individual symbol nodes instead of a list container
- Flattens symbols directly into parent context during parsing
- Prevents double-nesting: `f[`a`b`c]` becomes `f(`a, `b, `c)` not `f([[`a, `b, `c]])`

### 3. Glyph Translation

Maps q operator glyphs to readable names for variables (since functions are variables):
```python
glyph_map = {
    '@': 'at', '!': 'bang', ':': 'colon', '::': 'colon_colon',
    '-': 'dash', '.': 'dot', '$': 'dollar', '#': 'hash', 
    '?': 'query', '_': 'underscore', ',': 'comma'
}
```

Examples:
- `#` becomes `hash`
- `,` becomes `comma`
- `!` becomes `bang`

### 4. Claude AI Integration (`callclaude.py`)

- **Isolation**: Creates separate subdirectories for each Claude session
- **Process Management**: Runs `claude -p` in print mode to avoid interactive sessions
- **Error Handling**: Captures timeouts, command errors, and subprocess failures
- **Session Management**: Uses unique directory names to prevent conflicts

### 5. Disambiguation System (`disambiguate.py`)

- **Reference Documentation**: Uses `q_operators.md` as context for AI disambiguation
- **Prompt Engineering**: Constructs detailed prompts with code + operator reference
- **AI Processing**: Leverages Claude's understanding to resolve operator ambiguity
- **Comment Generation**: Produces explanatory comments for each operation

### 6. Operator Reference (`q_operators.md`)

Comprehensive documentation of q operator overloading patterns:
- **Arity-based disambiguation**: Different meanings by argument count
- **Type-based disambiguation**: Different meanings by argument types
- **Pattern recognition**: Specific argument patterns (e.g., `0` vs positive integer)
- **Functional qSQL**: Complex 4-6 argument query operations
- **Context clues**: Usage patterns that indicate specific variants

## Recent Enhancements

### Partial Application Detection
ParseQ now intelligently detects and handles Q's partial application semantics:

- **Built-in Glyph Rules**: Variable-arity operators like `at()`, `bang()`, `query()` dispatch immediately when enough arguments are provided, but create partials only when arguments are below minimum arity
- **User Function Rules**: Fixed-arity functions create partials whenever fewer arguments than expected are provided  
- **Syntax Awareness**: Distinguishes between function calls (`func(args)`) and function object assignments (`result = func`)
- **Arity Documentation**: Comments explain the before/after arity relationships when partials are created

Example transformation:
```python
# Before partial analysis:
temp4 = func2(x)  # func2 defined with 2 parameters

# After partial analysis:  
# Partial application: func2 expects 2 args but only 1 provided
temp4 = partial(func2, x)
```

### Enhanced Function Disambiguation  
Added comprehensive support for additional Q operators:

- **`slash()` Functional**: Now disambiguates to `converge()`, `do()`, `while()`, or `reduce()` based on input function arity and usage context
- **Three-Level Arity Analysis**: Handles the complex arity relationships in functionals (slash arity, input function arity, output function arity)
- **Mandatory Replacement**: Ensures no ambiguous function names remain in output
- **Consistent Application**: Improved prompt engineering for reliable disambiguation

Example `slash()` disambiguation:
```python
# Before: ambiguous functional
temp5 = slash(temp4)

# After: specific variant based on arity analysis  
# Disambiguation: slash(unary_function) called with 1 arg → Converge
temp5 = converge(temp4)
```

### Q Assignment Support
ParseQ now properly handles Q's unified assignment syntax for both variables and functions:

- **Variable Assignments**: `x:3` → `x = 3`
- **Function Assignments**: `f:{x+1}` → `def f(x): return x + 1`
- **Arity Detection**: Only colon operations with exactly 2 arguments are treated as assignments
- **Lambda Integration**: Function assignments use the variable name as the function name instead of auto-generated names
- **Clean Output**: Eliminates redundant assignment statements for function definitions

**Key Implementation Features:**
- **Pre-flattening Detection**: Checks AST node types before flattening to identify lambda assignments
- **Extensible kwargs System**: `flatten_ast` accepts kwargs for passing context (e.g., `func_name`)
- **Duplicate Prevention**: Processes lambda assignments once instead of twice
- **Glyph Mapping**: Removed `:` from glyph translation to enable proper assignment handling

Example transformations:
```python
# Variable assignment
q_code: x:3
Output: x = 3

# Function assignment  
q_code: f:{x+1}
Output: def f(x):
            return x + 1

# Before assignment support:
# result = colon(f, func1)
# After assignment support:
# def f(x): return x + 1
```

### Lambda Return Optimization
Enhanced lambda code generation to eliminate unnecessary temporary variables:

- **Smart Return Statements**: Replaces `temp{n} = expr` with `return expr` when possible
- **Consistent Pattern**: Mirrors the expression-level optimization for `result =` assignments
- **Multi-line Support**: Handles both simple and complex lambda bodies

Example optimization:
```python
# Before optimization:
def func1(x):
    temp1 = x + 1
    temp2 = temp1 * 2
    temp3 = temp2 / 3
    return temp3

# After optimization:
def func1(x):
    temp1 = x + 1
    temp2 = temp1 * 2
    return temp2 / 3
```

### Standalone Function Output
Improved output formatting for standalone lambda expressions:

- **Clean Termination**: Eliminates confusing `result = func{n}` lines for standalone functions
- **Context Awareness**: Detects when final expression is a function definition
- **Consistent Behavior**: Maintains `result =` for non-function expressions

## Key Features

### Variable vs Symbol Distinction
ParseQ distinguishes between two fundamental Q concepts:
- **Variables**: Named references (functions, variables) → `name` (no backticks)
  - `Symbol[f]` becomes `f` 
  - `Func[min]` becomes `min`
- **Symbols**: Literal symbol values → `` `symbol`` (with backticks)
  - `LSymbol[a,b,c]` becomes `` `a``, `` `b``, `` `c``

This preserves Q's semantic distinction where `f` references a variable but `` `f`` is a symbol literal.

### Step-by-Step Execution
Instead of nested function calls, ParseQ generates linear assignment sequences that are:
- **Debuggable**: Can inspect intermediate values
- **Readable**: Clear data flow from top to bottom
- **Modifiable**: Easy to rearrange or comment individual steps

### AI-Powered Disambiguation
Uses Claude AI to resolve operator ambiguity by:
- Analyzing argument patterns and types
- Consulting comprehensive operator documentation
- Generating contextual explanations
- Adding semantic comments explaining actual operations

## Usage

### Basic Parsing
```python
from parseq import parseq0, convert_lisp_to_flat_statements

# Get raw q parse output
raw = parseq0('a lj 2!select min s, maxs t from c')

# Convert to flattened Python-like code
flattened = convert_lisp_to_flat_statements(raw)
print(flattened)
```

### Full Disambiguation
```python
from disambiguate import disambiguate_q_code

# Full pipeline: parse + flatten + disambiguate
result = disambiguate_q_code('a lj 2!select min s, maxs t from c')
print(result)
```

### Manual Claude Queries
```python
from callclaude import ask_claude

# Ask Claude questions about q operations
response = ask_claude("What does the q operator ! do with 2 arguments?")
print(response)
```

## Files

- **`parseq.py`**: Core AST parser, tokenizer, and flattening engine
- **`parseq.q`**: Q initialization script that creates function mappings, type definitions, and AST serialization utilities for converting q parse trees into string representations
- **`callclaude.py`**: Claude AI integration for isolated subprocess calls
- **`disambiguate.py`**: Main disambiguation pipeline
- **`q_operators.md`**: Comprehensive q operator reference documentation
- **`README.md`**: This documentation

## Dependencies

- **Python 3.8+**: For dataclasses and type hints
- **Claude CLI**: Must have `claude` command available in PATH
- **Q/KDB+**: For generating parse trees (via qmcp connection)

## Limitations

- Requires q connection for parsing (uses q's built-in parser)
- Claude API calls can be slow (several seconds per disambiguation)
- Currently handles expressions, not full q scripts
- Some advanced q constructs may not be fully supported

## Future Enhancements

- **Caching**: Store disambiguation results to avoid repeated AI calls
- **Batch Processing**: Handle multiple expressions in single Claude call
- **Type Inference**: Better argument type detection for disambiguation
- **Full Script Support**: Handle q scripts with multiple statements
- **Optimization**: Reduce temporary variables where possible
- **Syntax Highlighting**: Visual indication of operator types in output

## Contributing

The system is modular and extensible:
1. **Parser Extensions**: Add new AST node types in `parseq.py`
2. **Operator Support**: Update `q_operators.md` with new operator documentation
3. **AI Prompting**: Improve disambiguation prompts in `disambiguate.py`
4. **Output Formats**: Add different code generation targets

## Examples

See the main functions for working examples:
- Basic parsing and flattening
- AI-powered disambiguation
- Symbol conversion and glyph mapping
- Step-by-step code generation