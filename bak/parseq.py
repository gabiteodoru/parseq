from qmcp.qlib import connect_to_q
import re
from dataclasses import dataclass
from typing import List as ListType, Union, Any, Tuple

# AST Node classes
class ASTNode:
    def __init__(self, node_type, children = None, eager = False):
        self.node_type = node_type
        self.children = children
        self.eager = eager

# remove from here ...
@dataclass
class Symbol(ASTNode):
    def __init__(self, eager = False):
    name: str = ""

@dataclass
class Integer(ASTNode):
    value: int = 0

@dataclass
class Float(ASTNode):
    value: float = 0.0

@dataclass
class Boolean(ASTNode):
    value: bool = False

@dataclass
class String(ASTNode):
    value: str = ""

@dataclass
class Func(ASTNode):
    name: str = ""
    args: ListType[ASTNode] = None

@dataclass
class Function(ASTNode):
    name: str = ""
    args: ListType[ASTNode] = None

@dataclass
class List(ASTNode):
    elements: ListType[ASTNode] = None

@dataclass
class Dict(ASTNode):
    keys: ASTNode = None
    values: ASTNode = None
# to here


# Tokenizer
def tokenize(text: str) -> ListType[str]:
    """Tokenize the LISP-like input into a list of tokens"""
    tokens = []
    i = 0
    while i < len(text):
        char = text[i]
        if char.isspace():
            i += 1
        elif char in '[](),{}':
            tokens.append(char)
            i += 1
        elif char == ':':
            # Handle dict syntax like LSymbol[s, t]:
            tokens.append(char)
            i += 1
        else:
            # Read a word/identifier
            start = i
            while i < len(text) and text[i] not in '[](){},:' and not text[i].isspace():
                i += 1
            tokens.append(text[start:i])
    return tokens

# Parser
class Parser:
    def __init__(self, tokens: ListType[str]):
        self.tokens = tokens
        self.pos = 0
    
    def current_token(self) -> str:
        if self.pos >= len(self.tokens):
            return None
        return self.tokens[self.pos]
    
    def consume(self) -> str:
        token = self.current_token()
        self.pos += 1
        return token
    
    def parse(self) -> ASTNode:
        """Parse the tokens into an AST"""
        return self.parse_expression()
    
    def parse_expression(self, idx = 0, eager=True) -> ASTNode:
        token = self.current_token()
        
        if token == '[':
            return self.parse_list(eager)
        elif token == '{':
            return self.parse_dict()
        elif token and token[0].isupper():
            # Type constructor like Symbol[name], Int[5], etc.
            return self.parse_type_constructor()
        else:
            # Simple token - consume it and parse as simple token
            token = self.consume()
            return self.parse_simple_token(token)
    
    def parse_list(self, eager) -> List:
        """Parse a bracketed list [...]"""
        self.consume()  # consume '['
        elements = []
        idx = 0
        while self.current_token() != ']':
            if self.current_token() == ',':
                self.consume()  # skip comma
                continue
            elements.append(self.parse_expression(idx, eager))
            idx += 1
        
        self.consume()  # consume ']'
        return List(elements)
    
    def parse_dict(self) -> Dict:
        """Parse a dictionary {...}"""
        self.consume()  # consume '{'
        
        # Parse keys
        keys = self.parse_expression(0, False)  # Always eager=False in dict
        
        # Expect ',' (changed from ':')
        if self.current_token() != ',':
            raise ValueError(f"Expected ',' in dict, got {self.current_token()}")
        self.consume()  # consume ','
        
        # Parse values
        values = self.parse_expression(0, False)  # Always eager=False in dict
        
        # Expect '}'
        if self.current_token() != '}':
            raise ValueError(f"Expected '}}' in dict, got {self.current_token()}")
        self.consume()  # consume '}'
        
        return Dict(keys, values)
    
    def parse_type_constructor(self) -> ASTNode:
        """Parse type constructors like Symbol[name], Int[5], Bool[0]"""
        type_name = self.consume()
        
        if self.current_token() != '[':
            # Just a plain identifier
            return String(type_name)
        
        self.consume()  # consume '['
        
        if type_name == 'Symbol':
            name = self.consume()
            self.consume()  # consume ']'
            return Symbol(name)
        elif type_name in ['Int', 'Long']:
            value = int(self.consume())
            self.consume()  # consume ']'
            return Integer(value)
        elif type_name in ['Real', 'Float']:
            value = float(self.consume())
            self.consume()  # consume ']'
            return Float(value)
        elif type_name == 'Bool':
            value = self.consume() == '1'
            self.consume()  # consume ']'
            return Boolean(value)
        elif type_name == 'Func':
            func_name = self.consume()
            self.consume()  # consume ']'
            return Function(func_name, [])
        elif type_name == 'LSymbol':
            # Parse LSymbol[s, t] as a list of symbols
            symbols = []
            while self.current_token() != ']':
                if self.current_token() == ',':
                    self.consume()  # skip comma
                    continue
                symbols.append(Symbol(self.consume()))
            self.consume()  # consume ']'
            return List(symbols)
        else:
            # Unknown type, parse contents as string
            content = self.consume()
            self.consume()  # consume ']'
            return String(f"{type_name}[{content}]")
    
    def parse_simple_token(self, token: str) -> ASTNode:
        """Parse simple tokens that aren't type constructors"""
        if token.isdigit():
            return Integer(int(token))
        elif token.replace('.', '').isdigit() and '.' in token:
            return Float(float(token))
        else:
            return String(token)

# AST Flattener - converts nested calls to step-by-step assignments
class Flattener:
    def flatten(self, node: ASTNode) -> tuple[ListType[str], str]:
        self.temp_counter = 0
        return self.flatten_ast(node)
    def flatten_ast(self, node: ASTNode) -> tuple[ListType[str], str]:
        """
        Flatten nested function calls into step-by-step assignments.
        Returns (statements, final_expression)
        """
        statements = []
        
        if isinstance(node, Symbol):
            return statements, f"`{node.name}"
        elif isinstance(node, Integer):
            return statements, str(node.value)
        elif isinstance(node, Float):
            value_str = str(node.value)
            if '.' not in value_str:
                value_str += '.0'
            return statements, value_str
        elif isinstance(node, Boolean):
            return statements, 'True' if node.value else 'False'
        elif isinstance(node, String):
            return statements, node.value
        elif isinstance(node, Function):
            # Replace glyphs with readable names
            glyph_map = {
                '@': 'at', '!': 'bang', ':': 'colon', '::': 'colon_colon',
                '-': 'dash', '.': 'dot', '$': 'dollar', '#': 'hash', '?': 'query',
                '_': 'underscore'
            }
            func_name = glyph_map.get(node.name, node.name)
            
            # Flatten arguments first
            arg_exprs = []
            for arg in node.args:
                arg_stmts, arg_expr = self.flatten_ast(arg)
                statements.extend(arg_stmts)
                arg_exprs.append(arg_expr)
            
            args_str = ', '.join(arg_exprs)
            
            # Always create temp variables for function calls to flatten everything
            self.temp_counter += 1
            temp_name = f"temp{self.temp_counter}"
            statements.append(f"{temp_name} = {func_name}({args_str})")
            return statements, temp_name
            
        elif isinstance(node, List):
            if node.elements and isinstance(node.elements[0], Function):
                # Function call: [Func[name], arg1, arg2, ...]
                func = node.elements[0]
                func.args = node.elements[1:]
                return self.flatten_ast(func)
            else:
                # Regular list
                elem_exprs = []
                for elem in node.elements:
                    elem_stmts, elem_expr = self.flatten_ast(elem)
                    statements.extend(elem_stmts)
                    elem_exprs.append(elem_expr)
                elements_str = ', '.join(elem_exprs)
                return statements, f"[{elements_str}]"
                
        elif isinstance(node, Dict):
            # Flatten dictionary
            k_stmts, k_expr = self.flatten_ast(node.keys)
            v_stmts, v_expr = self.flatten_ast(node.values)
            statements.extend(k_stmts)
            statements.extend(v_stmts)
            return statements, f"{{{k_expr}: {v_expr}}}"
        else:
            return statements, str(node)

# AST Transformer
def transform_ast(node: ASTNode) -> str:
    """Transform AST nodes to Python-like syntax"""
    if isinstance(node, Symbol):
        return f"`{node.name}"
    elif isinstance(node, Integer):
        return str(node.value)
    elif isinstance(node, Float):
        # Ensure float has decimal point
        value_str = str(node.value)
        if '.' not in value_str:
            value_str += '.0'
        return value_str
    elif isinstance(node, Boolean):
        return 'True' if node.value else 'False'
    elif isinstance(node, String):
        return node.value
    elif isinstance(node, Function):
        # Convert function calls from LISP [Func[name], arg1, arg2] to name(arg1, arg2)
        # Replace glyphs with readable names
        glyph_map = {
            '@': 'at',
            '!': 'bang',
            ':': 'colon',
            '::': 'colon_colon',
            '-': 'dash',
            '.': 'dot',
            '$': 'dollar',
            '#': 'hash',
            '?': 'query'
        }
        func_name = glyph_map.get(node.name, node.name)
        args_str = ', '.join(transform_ast(arg) for arg in node.args)
        return f"{func_name}({args_str})"
    elif isinstance(node, List):
        # Check if this is a function call (first element is Function)
        if node.elements and isinstance(node.elements[0], Function):
            func = node.elements[0]
            args = node.elements[1:]
            func.args = args  # Set the arguments
            return transform_ast(func)
        else:
            # Regular list
            elements_str = ', '.join(transform_ast(elem) for elem in node.elements)
            return f"[{elements_str}]"
    elif isinstance(node, Dict):
        # Transform Dict to Python dict syntax
        k_str = transform_ast(node.keys)
        v_str = transform_ast(node.values)
        return f"{{{k_str}: {v_str}}}"
    else:
        return str(node)

def convert_lisp_to_function_calls(parsed_str):
    """Convert LISP-like representation to function call syntax using AST"""
    # First decode bytes to string if needed
    if isinstance(parsed_str, bytes):
        parsed_str = parsed_str.decode('utf-8')
    
    # Tokenize and parse into AST
    tokens = tokenize(parsed_str)
    parser = Parser(tokens)
    ast = parser.parse()
    
    # Transform AST to Python-like syntax
    return transform_ast(ast)

def convert_lisp_to_flat_statements(parsed_str):
    """Convert LISP-like representation to step-by-step Python statements"""
    # First decode bytes to string if needed
    if isinstance(parsed_str, bytes):
        parsed_str = parsed_str.decode('utf-8')
    
    # Tokenize and parse into AST
    tokens = tokenize(parsed_str)
    parser = Parser(tokens)
    ast = parser.parse()
    
    # Flatten AST to step-by-step assignments
    f = Flattener()
    statements, final_expr = f.flatten(ast)
    
    # Format output
    if statements:
        return '\n'.join(statements) + '\nresult = ' + final_expr
    else:
        return 'result = ' + final_expr


q = connect_to_q(5001)
def parseq0(s):
    return q('var2string parse "'+s+'"')
def parseq(s):
    return convert_lisp_to_function_calls(q('var2string parse "'+s+'"'))
def pr(s):
    print(convert_lisp_to_flat_statements(parseq0(s)))
pr('f[min]')
pr('a lj 2!select min s, maxs t from c')
