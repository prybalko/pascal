# Tokens

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


class Parser(object):
    def __init__(self, text):
        self.text = text
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

    def get_next_token(self):
        if not self.current_pos < len(self.text):
            return Token(EOF)

        if self.text[self.current_pos].isspace():
            self.skip_whitespace()

        char = self.text[self.current_pos]

        if char.isdigit():
            self.advance()
            return Token(INTEGER, int(char))

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

        raise SyntaxError('Symbol "{}" is not supported'.format(char))


class AstNode(object):
    def __init__(self, op, left=None, right=None):
        self.left = left
        self.op = self.value_token = op
        self.right = right

    def visit(self):
        method_name = 'visit_{}'.format(self.__class__.__name__)
        return getattr(self, method_name)()

    def __repr__(self):
        return '{}({}, {}, {})'.format(self.__class__.__name__,
                                       self.op.type,
                                       self.left.__class__.__name__,
                                       self.right.__class__.__name__)


class BinOp(AstNode):
    def visit_BinOp(self):
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


class NumNode(AstNode):
    def visit_NumNode(self):
        return self.value_token.value


class Lexer(object):
    def __init__(self, parser):
        self.parser = parser
        self.current_token = self.parser.get_next_token()

    def eat(self, expected_type):
        if not self.current_token.type == expected_type:
            raise ValueError('Unexpected token. Expected {}, got {}'.format(expected_type, self.current_token.type))
        self.current_token = self.parser.get_next_token()

    def expr(self):
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
        raise ValueError('Unsupported token type. Got {}'.format(token))


class Interpreter(object):
    def __init__(self, lexer):
        self.lexer = lexer

    def execute(self):
        root_node = self.lexer.expr()
        return root_node


if __name__ == '__main__':
    while True:
        text = raw_input('calc>')
        parser = Parser(text)
        lexer = Lexer(parser)
        interpreter = Interpreter(lexer)
        print interpreter.execute()
