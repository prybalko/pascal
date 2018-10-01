""" SPI - Simple Pascal Interpreter """

###############################################################################
#                                                                             #
#  LEXER                                                                      #
#                                                                             #
###############################################################################

# Token types
#
# EOF (end-of-file) token is used to indicate that
# there is no more input left for lexical analysis
from collections import OrderedDict

INTEGER       = 'INTEGER'
REAL          = 'REAL'
INTEGER_CONST = 'INTEGER_CONST'
REAL_CONST    = 'REAL_CONST'
PLUS          = 'PLUS'
MINUS         = 'MINUS'
MUL           = 'MUL'
INTEGER_DIV   = 'INTEGER_DIV'
FLOAT_DIV     = 'FLOAT_DIV'
LPAREN        = 'LPAREN'
RPAREN        = 'RPAREN'
ID            = 'ID'
ASSIGN        = 'ASSIGN'
BEGIN         = 'BEGIN'
END           = 'END'
SEMI          = 'SEMI'
DOT           = 'DOT'
PROGRAM       = 'PROGRAM'
PROCEDURE     = 'PROCEDURE'
VAR           = 'VAR'
COLON         = 'COLON'
COMMA         = 'COMMA'
EOF           = 'EOF'


class Token(object):
    def __init__(self, type, value=None):
        self.type = type
        self.value = value

    def __repr__(self):
        return '{}({}, {})'.format(self.__class__.__name__, self.type, self.value)


RESERVED_KEYWORDS = {
    'PROGRAM': Token(PROGRAM, 'PROGRAM'),
    'VAR': Token(VAR, 'VAR'),
    'DIV': Token(INTEGER_DIV, 'DIV'),
    'INTEGER': Token(INTEGER, 'INTEGER'),
    'REAL': Token(REAL, 'REAL'),
    'BEGIN': Token(BEGIN, 'BEGIN'),
    'END': Token(END, 'END'),
    'PROCEDURE': Token('PROCEDURE', 'PROCEDURE'),
}

GLOBAL_SCOPE = {}


class Lexer(object):
    def __init__(self, text):
        self.text = text.strip()
        self.current_pos = 0

    def advance(self):
        self.current_pos += 1

    def parse(self):
        tokens = []
        for _ in range(len(text)):
            tokens.append(self.get_next_token())
            if tokens[-1].type == EOF:
                break
        return tokens

    def peek(self):
        peek_pos = self.current_pos + 1
        if peek_pos < len(self.text):
            return self.text[peek_pos]

    def skip_whitespace(self):
        while self.current_pos < len(self.text) and self.text[self.current_pos].isspace():
            self.advance()

    def skip_comment(self):
        while not self.text[self.current_pos] == '}':
            self.advance()
        self.advance()  # the closing curly brace

    def number(self):
        """Return a (multidigit) integer or float consumed from the input."""
        result = ''
        while self.current_pos < len(self.text) and self.text[self.current_pos].isdigit():
            result += self.text[self.current_pos]
            self.advance()
        if not self.text[self.current_pos] == '.':
            return Token(INTEGER_CONST, int(result))

        result += '.'
        self.advance()
        while self.current_pos < len(self.text) and self.text[self.current_pos].isdigit():
            result += self.text[self.current_pos]
            self.advance()
        return Token(REAL_CONST, float(result))

    def _id(self):
        """Handle identifiers and reserved keywords"""
        result = ''
        while self.current_pos < len(self.text) and self.text[self.current_pos].isalnum():
            result += self.text[self.current_pos].upper()
            self.advance()
        return RESERVED_KEYWORDS.get(result, Token(ID, result))

    def get_next_token(self):
        if not self.current_pos < len(self.text):
            return Token(EOF)

        char = self.text[self.current_pos]

        if char.isspace():
            self.skip_whitespace()
            return self.get_next_token()

        if char == '{':
            self.skip_comment()
            return self.get_next_token()

        if char.isalpha():
            return self._id()

        if char.isdigit():
            return self.number()

        if char == '+':
            self.advance()
            return Token(PLUS, '+')

        if char == '-':
            self.advance()
            return Token(MINUS, '-')

        if char == '*':
            self.advance()
            return Token(MUL, '*')

        if char == '/':
            self.advance()
            return Token(FLOAT_DIV, '/')

        if char == '(':
            self.advance()
            return Token(LPAREN, '(')

        if char == ')':
            self.advance()
            return Token(RPAREN, '(')

        if char == ':' and self.peek() == '=':
            self.advance()
            self.advance()
            return Token(ASSIGN, ':=')

        if char == ':':
            self.advance()
            return Token(COLON, ':')

        if char == ';':
            self.advance()
            return Token(SEMI, ';')

        if char == '.':
            self.advance()
            return Token(DOT, '.')

        if char == ',':
            self.advance()
            return Token(COMMA, ',')

        raise SyntaxError('Symbol "{}" is not supported'.format(char))


###############################################################################
#                                                                             #
#  PARSER                                                                     #
#                                                                             #
###############################################################################


class AstNode(object):
    def visit(self):
        raise NotImplementedError

    def build(self, symtab):
        raise NotImplementedError

    def __repr__(self):
        return '{}({})'.format(self.__class__.__name__,
                               ', '.join([attr for attr in dir() if not attr == 'self']))


class ProgramNode(AstNode):
    def __init__(self, name, block):
        self.name = name
        self.block = block

    def visit(self):
        self.block.visit()

    def build(self, symtab):
        self.block.build(symtab)


class ProcedureDectNode(AstNode):
    def __init__(self, proc_name, block_node):
        self.proc_name = proc_name
        self.block_node = block_node

    def visit(self):
        pass

    def build(self, symtab):
        pass


class BlockNode(AstNode):
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement

    def visit(self):
        for declaration in self.declarations:
            declaration.visit()
        self.compound_statement.visit()

    def build(self, symtab):
        for declaration in self.declarations:
            declaration.build(symtab)
        self.compound_statement.build(symtab)


class VarDeclNode(AstNode):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

    def visit(self):
        pass

    def build(self, symtab):
        type_name = self.type_node.value
        type_symbol = symtab.lookup(type_name)
        var_name = self.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)
        symtab.define(var_symbol)


class BinOp(AstNode):
    def __init__(self, op, left, right):
        self.left = left
        self.op = op
        self.right = right

    def visit(self):
        left = self.left.visit()
        right = self.right.visit()
        if self.op.type == PLUS:
            return left + right
        if self.op.type == MINUS:
            return left - right
        if self.op.type == MUL:
            return left * right
        if self.op.type == INTEGER_DIV:
            return left // right
        if self.op.type == FLOAT_DIV:
            return float(left) / right

    def build(self, symtab):
        self.left.build(symtab)
        self.right.build(symtab)


class UnaryOp(AstNode):
    def __init__(self, op, left):
        self.left = left
        self.op = op

    def visit(self):
        value = self.left.visit()
        if self.op.type == MINUS:
            return -1 * value
        if self.op.type == PLUS:
            return value
        raise Exception('Unsupported unary operation')

    def build(self, symtab):
        self.left.build(symtab)


class NumNode(AstNode):
    def __init__(self, token):
        self.token = token

    def visit(self):
        return self.token.value

    def build(self, symtab):
        pass


class Compound(AstNode):
    """Represents a 'BEGIN ... END' block"""

    def __init__(self, children):
        self.children = children

    def visit(self):
        for child in self.children:
            child.visit()

    def build(self, symtab):
        for child in self.children:
            child.build(symtab)


class Assign(AstNode):

    def __init__(self, var, value):
        self.var = var
        self.value = value

    def visit(self):
        GLOBAL_SCOPE[self.var.name] = self.value.visit()

    def build(self, symtab):
        var_name = self.var.name
        var_symbol = symtab.lookup(var_name)
        if var_symbol is None:
            raise NameError(repr(var_name))

        self.value.build(symtab)


class Var(AstNode):
    """The Var node is constructed out of ID token."""

    def __init__(self, id_token):
        self.id_token = id_token
        self.name = id_token.value

    def visit(self):
        return GLOBAL_SCOPE[self.id_token.value]

    def build(self, symtab):
        var_name = self.id_token.value
        var_symbol = symtab.lookup(var_name)

        if var_symbol is None:
            raise NameError(repr(var_name))


class EmptyNode(AstNode):
    def visit(self):
        pass

    def build(self, symtab):
        pass


class Symbol(object):
    def __init__(self, name, type=None):
        self.name = name
        self.type = type


class BuiltinTypeSymbol(Symbol):

    def __str__(self):
        return self.name


class VarSymbol(Symbol):

    def __str__(self):
        return '<{name}:{type}>'.format(name=self.name, type=self.type)


class SymbolTable(object):
    def __init__(self):
        self._symbols = OrderedDict()
        self._init_builtins()

    def _init_builtins(self):
        self.define(BuiltinTypeSymbol('INTEGER'))
        self.define(BuiltinTypeSymbol('REAL'))

    def __str__(self):
        return 'Symbols: {symbols}'.format(symbols=[value for value in self._symbols.values()])

    def define(self, symbol):
        print('Define: %s' % symbol)
        self._symbols[symbol.name] = symbol

    def lookup(self, name):
        print('Lookup: %s' % name)
        symbol = self._symbols.get(name)
        # 'symbol' is either an instance of the Symbol class or 'None'
        return symbol


class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def eat(self, expected_type):
        if not self.current_token.type == expected_type:
            raise ValueError('Unexpected token. Expected {}, got {}'.format(expected_type, self.current_token.type))
        self.current_token = self.lexer.get_next_token()

    def program(self):
        """program : PROGRAM variable SEMI block DOT"""
        self.eat(PROGRAM)
        name = self.current_token.value
        self.eat(ID)
        self.eat(SEMI)
        block = self.block()
        self.eat(DOT)
        return ProgramNode(name, block)

    def block(self):
        """block : declarations compound_statement"""
        return BlockNode(self.declarations(), self.compound_statement())

    def declarations(self):
        """declarations : VAR (variable_declaration SEMI)+
                        | (PROCEDURE ID SEMI block SEMI)*
                        | empty
        """
        declarations = []
        if self.current_token.type == VAR:
            self.eat(VAR)
            while self.current_token.type == ID:
                declarations += self.variable_declaration()
                self.eat(SEMI)

        while self.current_token.type == PROCEDURE:
            self.eat(PROCEDURE)
            name = self.current_token
            self.eat(ID)
            self.eat(SEMI)
            declarations.append(ProcedureDectNode(name, self.block()))
            self.eat(SEMI)

        if not declarations:
            return self.empty()

        return declarations

    def variable_declaration(self):
        """variable_declaration : ID (COMMA ID)* COLON type_spec"""
        var_names = [self.current_token]
        self.eat(ID)
        while self.current_token.type == COMMA:
            self.eat(COMMA)
            var_names.append(self.current_token)
            self.eat(ID)
        self.eat(COLON)
        type_spec = self.current_token
        self.eat(type_spec.type)
        return [VarDeclNode(name, type_spec) for name in var_names]

    def compound_statement(self):
        """
        compound_statement: BEGIN statement_list END
        """
        self.eat(BEGIN)
        statements = self.statement_list()
        self.eat(END)
        return Compound(statements)

    def statement_list(self):
        """
        statement_list : statement
                       | statement SEMI statement_list
        """
        statements = [self.statement()]
        while self.current_token.type == SEMI:
            self.eat(SEMI)
            statements.append(self.statement())
        return statements

    def statement(self):
        """
        statement : compound_statement
                  | assignment_statement
                  | empty
        """
        token = self.current_token
        if token.type == BEGIN:
            return self.compound_statement()
        if token.type == ID:
            return self.assign_statement()
        if token.type == END:
            return self.empty()
        raise SyntaxError('Unexpected token type. Got {}'.format(self.current_token.type))

    def assign_statement(self):
        """
        assignment_statement : variable ASSIGN expr
        """
        var = self.variable()
        self.eat(ASSIGN)
        expr = self.expr()
        return Assign(var, expr)

    def variable(self):
        """
        variable : ID
        """
        var_name = self.current_token
        self.eat(ID)
        return Var(var_name)

    def empty(self):
        return EmptyNode()

    def expr(self):
        """
            expr   : term ((PLUS | MINUS) term)*
            term   : factor ((MUL | DIV) factor)*
            factor : INTEGER | LPAREN expr RPAREN
        """
        result = self.term()
        while self.current_token.type in (PLUS, MINUS):
            op = self.current_token
            self.eat(self.current_token.type)
            right = self.term()
            result = BinOp(op, result, right)
        return result

    def term(self):
        """term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*"""
        result = self.factor()
        while self.current_token.type in (MUL, INTEGER_DIV, FLOAT_DIV):
            op = self.current_token
            self.eat(self.current_token.type)
            right = self.factor()
            result = BinOp(op, result, right)
        return result

    def factor(self):
        """factor : PLUS factor
                   | MINUS factor
                   | INTEGER_CONST
                   | REAL_CONST
                   | LPAREN expr RPAREN
                   | variable
        """
        token = self.current_token
        if token.type in (INTEGER_CONST, REAL_CONST):
            self.eat(token.type)
            return NumNode(token)

        if token.type == LPAREN:
            self.eat(LPAREN)
            result = self.expr()
            self.eat(RPAREN)
            return result

        if token.type in (PLUS, MINUS):
            self.eat(token.type)
            operand = self.factor()
            return UnaryOp(token, operand)

        if token.type == ID:
            return self.variable()
        raise SyntaxError('Unexpected token type. Got {}'.format(token))


###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################


class Interpreter(object):
    def __init__(self, tree):
        self.tree = tree

    def execute(self):
        return self.tree.visit()


class SymbolTableBuilder(object):
    def __init__(self, tree):
        self.symtab = SymbolTable()
        self.tree = tree

    def build(self):
        return self.tree.build(self.symtab)


text = """
PROGRAM Part12;
VAR
   a : INTEGER;

PROCEDURE P1;
VAR
   a : REAL;
   k : INTEGER;

   PROCEDURE P2;
   VAR
      a, z : INTEGER;
   BEGIN {P2}
      z := 777;
   END;  {P2}

BEGIN {P1}

END;  {P1}

BEGIN {Part12}
   a := 10;
END.  {Part12}
"""

if __name__ == '__main__':
    lexer = Lexer(text)
    parser = Parser(lexer)
    root_node = parser.program()

    builder = SymbolTableBuilder(root_node)
    builder.build()

    interpreter = Interpreter(root_node)
    interpreter.execute()

    print GLOBAL_SCOPE
