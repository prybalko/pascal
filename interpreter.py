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


class Token(object):
    def __init__(self, type, value=None):
        self.type = type
        self.value = value

    def __repr__(self):
        return '{}({}, {})'.format(self.__class__.__name__, self.type, self.value)


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
        return tokens

    def skip_whitespace(self):
        while self.current_pos < len(self.text) and self.text[self.current_pos].isspace():
            self.advance()

    def digit(self):
        num = ''
        while self.current_pos < len(self.text) and self.text[self.current_pos].isdigit():
            num += self.text[self.current_pos]
            self.advance()
        return num

    def get_next_token(self):
        if not self.current_pos < len(self.text):
            return Token(EOF)

        if self.text[self.current_pos].isspace():
            self.skip_whitespace()

        char = self.text[self.current_pos]

        if char.isdigit():
            num = self.digit()
            return Token(INTEGER, int(num))

        if char == '+':
            self.advance()
            return Token(PLUS)

        if char == '-':
            self.advance()
            return Token(MINUS)

        if char == '*':
            self.advance()
            return Token(MUL)

        if char == '/':
            self.advance()
            return Token(DIV)

        if char == '(':
            self.advance()
            return Token(LPAREN)

        if char == ')':
            self.advance()
            return Token(RPAREN)

        raise SyntaxError('Symbol "{}" is not supported'.format(char))


###############################################################################
#                                                                             #
#  PARSER                                                                     #
#                                                                             #
###############################################################################


class AstNode(object):
    def __init__(self, op, left=None, right=None):
        self.left = left
        self.op = self.value_token = op
        self.right = right

    def visit(self):
        raise NotImplementedError

    def __repr__(self):
        return '{}({}, {}, {})'.format(self.__class__.__name__,
                                       self.op.type,
                                       self.left.__class__.__name__,
                                       self.right.__class__.__name__)


class BinOp(AstNode):
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
    def visit(self):
        value = self.left.visit()
        if self.op.type == MINUS:
            return -1 * value
        if self.op.type == PLUS:
            return value
        raise Exception('Unsupported unary operation')


class NumNode(AstNode):
    def visit(self):
        return self.value_token.value


class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def eat(self, expected_type):
        if not self.current_token.type == expected_type:
            raise ValueError('Unexpected token. Expected {}, got {}'.format(expected_type, self.current_token.type))
        self.current_token = self.lexer.get_next_token()

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
        raise ValueError('Unexpected token type. Got {}'.format(token))


###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################


class Interpreter(object):
    def __init__(self, lexer):
        self.lexer = lexer

    def execute(self):
        root_node = self.lexer.expr()
        return root_node.visit()


if __name__ == '__main__':
    while True:
        text = raw_input('calc> ')
        lexer = Lexer(text)
        parser = Parser(lexer)
        interpreter = Interpreter(parser)
        print interpreter.execute()
