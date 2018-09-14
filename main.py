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




class Lexer(object):
    pass


class Interpreter(object):
    pass


if __name__ == '__main__':
    while True:
        text = raw_input('calc>')
        parser = Parser(text)
        print parser.parse()
