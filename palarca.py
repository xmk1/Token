# Token types 
# 
# EOF (end-of-file) token is used to indicate that 
# there is no more input left for lexical analysis
INTEGER, MUL, DIV, PLUS, MINUS, EOF = 'INTEGER', 'MUL', 'DIV', 'PLUS', 'MINUS', 'EOF'


class Token(object):
    def __init__(self, type, value):
        # token type: INTEGER, MUL, DIV, PLUS, MINUS or EOF
        self.type = type
        # token value: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,'+', or None
        self.value = value

    def __str__(self):
        """String representation of the class instance.

        Examples:

        Token(INTEGER, 3)
        Token(PLUS'+')
        ________________
        Token(INTEGER, 3)
        Token(MUL'*')
        ________________
        Token(INTEGER, 3)
        Token(DIV'/')
        ________________
        Token(INTEGER, 3)
        Token(MINUS'-')
        ________________
        """

        return 'Token({type}, {value})'.format(
            type = self.type,
            value = repr(self.value)
        )

    def __repr__(self):
        return self.__str__()
        
class Lexer(object):
    def __init__(self, text):
        #client string input, e.g    "3 * 5", "12 / 3 * 4", etc
        self.text = text
        #self.pos is index into self.text
        self.pos = 0
        self.current_char = self.text[self.pos]
        
    def erorr(self):
        raise Exception('Invalid character')
        
    def advance(self):
        """Advance the 'pos' pointer and set the 'current_char' variable."""
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None # Indicates end of input
        else:
            self.current_char = self.text[self.pos]
            
    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
                self.advance()
                
    def integer(self):
        """Return a (multidigit) integer consumed from the input. """
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        return int(result)
        
    def get_next_token(self):
        """ Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence apart into tokens. One token at a time.
        """
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            
            if self.current_char.isdigit():
                return Token(INTEGER, self.integer())
                
            if self.current_char == '+':
                self.advance()
                return Token(PLUS, '+')
                
            if self.current_char == '-':
                self.advance()
                return Token(MINUS, '-')
                
            if self.current_char == '*':
                self.advance()
                return Token(MUL, '*')
            
            if self.current_char == '/':
                self.advance()
                return Token(DIV, '/')
                
            self.error()
            
        return Token(EOF, None)
            
            

class Interpreter(object):
    def __init__(self, text):
        self.lexer = Lexer(text)  # Create Lexer instance
        self.current_token = self.lexer.get_next_token()
    
    def error(self):
        raise Exception('Invalid syntax')
        
    def eat(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()
    
    def term(self):
        token = self.current_token
        self.eat(INTEGER)
        return token.value

    def expr(self):
        result = self.term()
        while self.current_token.type in (PLUS, MINUS, MUL, DIV):
            token = self.current_token
            if token.type == PLUS:
                self.eat(PLUS)
                result = result + self.term()
            elif token.type == MINUS:
                self.eat(MINUS)
                result = result - self.term()
            elif token.type == MUL:
                self.eat(MUL)
                result = result * self.term()
            elif token.type == DIV:
                self.eat(DIV)
                result = result / self.term()
                
        return result


    
def main():
    while True:
        try: 
            text = input('calc> ')
        except EOFError:
            break

        if not text:
            continue

        interpreter = Interpreter(text)  # Pass the input text to the Interpreter
        result = interpreter.expr()

        print(result)


if __name__ == '__main__':
    main()
