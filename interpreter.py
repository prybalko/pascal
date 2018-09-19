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

INTEGER = 'INTEGER'
PLUS = 'PLUS'
MINUS = 'MINUS'
MUL = 'MUL'
DIV = 'DIV'
LPAREN = 'LPAREN'
RPAREN = 'RPAREN'
EOF = 'EOF'

BEGIN = 'BEGIN'
END = 'END'
DOT = 'DOT'
ID = 'ID'
ASSIGN = 'ASSIGN'
SEMI = 'SEMI'


class Token(object):
    def __init__(self, type, value=None):
        self.type = type
        self.value = value

    def __repr__(self):
        return '{}({}, {})'.format(self.__class__.__name__, self.type, self.value)


RESERVED_KEYWORDS = {
    'BEGIN': Token('BEGIN'),
    'END': Token('END'),
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

    def number(self):
        num = ''
        while self.current_pos < len(self.text) and self.text[self.current_pos].isdigit():
            num += self.text[self.current_pos]
            self.advance()
        return num

    def _id(self):
        """Handle identifiers and reserved keywords"""
        result = ''
        while self.current_pos < len(self.text) and self.text[self.current_pos].isalnum():
            result += self.text[self.current_pos]
            self.advance()
        return RESERVED_KEYWORDS.get(result, Token(ID, result))

    def get_next_token(self):
        if not self.current_pos < len(self.text):
            return Token(EOF)

        if self.text[self.current_pos].isspace():
            self.skip_whitespace()

        char = self.text[self.current_pos]

        if char.isalpha():
            return self._id()

        if char.isdigit():
            num = self.number()
            return Token(INTEGER, int(num))

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
            return Token(DIV, '/')

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

        if char == ';':
            self.advance()
            return Token(SEMI, ';')

        if char == '.':
            self.advance()
            return Token(DOT, '.')

        raise SyntaxError('Symbol "{}" is not supported'.format(char))


###############################################################################
#                                                                             #
#  PARSER                                                                     #
#                                                                             #
###############################################################################


class AstNode(object):
    def visit(self):
        raise NotImplementedError

    def __repr__(self):
        return '{}({})'.format(self.__class__.__name__,
                               ', '.join([attr for attr in dir() if not attr == 'self']))


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
        if self.op.type == DIV:
            return left / right


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


class NumNode(AstNode):
    def __init__(self, token):
        self.token = token

    def visit(self):
        return self.token.value


class Compound(AstNode):
    """Represents a 'BEGIN ... END' block"""

    def __init__(self, children):
        self.children = children

    def visit(self):
        for child in self.children:
            child.visit()


class Assign(AstNode):

    def __init__(self, var, value):
        self.var = var
        self.value = value

    def visit(self):
        GLOBAL_SCOPE[self.var.name] = self.value.visit()


class Var(AstNode):
    """The Var node is constructed out of ID token."""

    def __init__(self, id_token):
        self.id_token = id_token
        self.name = id_token.value

    def visit(self):
        return GLOBAL_SCOPE[self.id_token.value]


class EmptyNode(AstNode):
    def visit(self):
        pass


class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def eat(self, expected_type):
        if not self.current_token.type == expected_type:
            raise ValueError('Unexpected token. Expected {}, got {}'.format(expected_type, self.current_token.type))
        self.current_token = self.lexer.get_next_token()

    def program(self):
        """program : compound_statement DOT"""
        compound = self.compound_statement()
        self.eat(DOT)
        return compound

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
        result = self.factor()
        while self.current_token.type in (MUL, DIV):
            op = self.current_token
            self.eat(self.current_token.type)
            right = self.factor()
            result = BinOp(op, result, right)
        return result

    def factor(self):
        """factor : PLUS  factor
                  | MINUS factor
                  | INTEGER
                  | LPAREN expr RPAREN
                  | variable
        """
        token = self.current_token
        if token.type == INTEGER:
            self.eat(INTEGER)
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
    def __init__(self, lexer):
        self.lexer = lexer

    def execute(self):
        root_node = self.lexer.program()
        return root_node.visit()


text = """
BEGIN
    BEGIN
        number := 2;
        a := number;
        b := 10 * a + 10 * number / 4;
        c := a - - b
    END;
    x := 11;
END.
"""

if __name__ == '__main__':
    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    interpreter.execute()
    print GLOBAL_SCOPE
