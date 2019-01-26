#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

impl Position {
    fn forward(self: &mut Self, n: usize) {
        self.offset += n;
        self.column += n;
    }
}

pub enum Symbol {
    OpenParen,
    CloseParen,
    Comma,
    HyphenGreater,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    Bar,
    BarBar,
    LessHyphen,
    Semicolon,
    Colon,
    Numbersign,
    Dot,
    Asterisk,
    Slash,
    Plus,
    Hyphen,
    PlusPlus,
    HyphenHyphen,
    EqualEqual,
    SlashEqual,
    EqualLess,
    Less,
    GreaterEqual,
    Greater,
    EqualColonEqual,
    EqualSlashEqual,
    LessEqual,
    EqualGreater,
    ColonEqual,
    LessLess,
    GreaterGreater,
    Exclamation,
    Equal,
    ColonColon,
    DotDot,
    DotDotDot,
    DotNewLine,
    Question,
}

pub enum Keyword {
    After,
    Begin,
    Case,
    Try,
    Catch,
    End,
    Fun,
    If,
    Of,
    Receive,
    When,
    Andalso,
    Orelse,
    Bnot,
    Not,
    Div,
    Rem,
    Band,
    And,
    Bor,
    Bxor,
    Bsl,
    Bsr,
    Or,
    Xor,
}

pub enum TokenType {
    Integer,
    Float,
    Atom,
    String,
    Variable,
    Symbol(Symbol),
    Keyword(Keyword),
    Comment,
    Whitespace,
}

pub struct Token<'a> {
    pub text: &'a [u8],
    pub position: Position,
    pub token_type: TokenType,
}

pub enum Error {}

pub struct Tokenizer<'a> {
    src: &'a [u8],
    current_position: Position,
}

impl<'a> Tokenizer<'a> {
    pub fn new(src: &'a [u8]) -> Self {
        Tokenizer {
            src,
            current_position: Position { offset: 0, line: 1, column: 1 },
        }
    }

    fn tokenize_symbol(&mut self, length: usize, symbol: Symbol, rest: &'a [u8]) -> Option<Result<Token<'a>, Error>>{
        let position = self.current_position;
        self.src = rest;
        self.current_position.forward(length);
        Some(Ok(Token { text: &self.src[0..length], position, token_type: TokenType::Symbol(symbol) }))
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.src {
            &[] => None,

            &[b'=', b':', b'=', ref rest..] => self.tokenize_symbol(3, Symbol::EqualColonEqual, rest),
            &[b'=', b'/', b'=', ref rest..] => self.tokenize_symbol(3, Symbol::EqualSlashEqual, rest),
            &[b'.', b'.', b'.', ref rest..] => self.tokenize_symbol(3, Symbol::DotDotDot, rest),

            &[b'-', b'>', ref rest..] => self.tokenize_symbol(2, Symbol::HyphenGreater, rest),
            &[b'|', b'|', ref rest..] => self.tokenize_symbol(2, Symbol::BarBar, rest),
            &[b'<', b'-', ref rest..] => self.tokenize_symbol(2, Symbol::LessHyphen, rest),
            &[b'+', b'+', ref rest..] => self.tokenize_symbol(2, Symbol::PlusPlus, rest),
            &[b'-', b'-', ref rest..] => self.tokenize_symbol(2, Symbol::HyphenHyphen, rest),
            &[b'=', b'=', ref rest..] => self.tokenize_symbol(2, Symbol::EqualEqual, rest),
            &[b'/', b'=', ref rest..] => self.tokenize_symbol(2, Symbol::SlashEqual, rest),
            &[b'=', b'<', ref rest..] => self.tokenize_symbol(2, Symbol::SlashEqual, rest),
            &[b'>', b'=', ref rest..] => self.tokenize_symbol(2, Symbol::GreaterEqual, rest),
            &[b'<', b'=', ref rest..] => self.tokenize_symbol(2, Symbol::LessEqual, rest),
            &[b'=', b'>', ref rest..] => self.tokenize_symbol(2, Symbol::EqualGreater, rest),
            &[b':', b'=', ref rest..] => self.tokenize_symbol(2, Symbol::ColonEqual, rest),
            &[b'.', b'.', ref rest..] => self.tokenize_symbol(2, Symbol::DotDot, rest),

            &[b'(', ref rest..] => self.tokenize_symbol(1, Symbol::OpenParen, rest),
            &[b')', ref rest..] => self.tokenize_symbol(1, Symbol::CloseParen, rest),
            &[b',', ref rest..] => self.tokenize_symbol(1, Symbol::Comma, rest),
            &[b'{', ref rest..] => self.tokenize_symbol(1, Symbol::OpenBrace, rest),
            &[b'}', ref rest..] => self.tokenize_symbol(1, Symbol::CloseBrace, rest),
            &[b'[', ref rest..] => self.tokenize_symbol(1, Symbol::OpenBracket, rest),
            &[b']', ref rest..] => self.tokenize_symbol(1, Symbol::CloseBracket, rest),
            &[b'|', ref rest..] => self.tokenize_symbol(1, Symbol::Bar, rest),
            &[b';', ref rest..] => self.tokenize_symbol(1, Symbol::Semicolon, rest),
            &[b':', ref rest..] => self.tokenize_symbol(1, Symbol::Colon, rest),
            &[b'#', ref rest..] => self.tokenize_symbol(1, Symbol::Numbersign, rest),
            &[b'.', ref rest..] => self.tokenize_symbol(1, Symbol::Dot, rest),
            &[b'*', ref rest..] => self.tokenize_symbol(1, Symbol::Asterisk, rest),
            &[b'/', ref rest..] => self.tokenize_symbol(1, Symbol::Slash, rest),
            &[b'+', ref rest..] => self.tokenize_symbol(1, Symbol::Plus, rest),
            &[b'-', ref rest..] => self.tokenize_symbol(1, Symbol::Hyphen, rest),
            &[b'!', ref rest..] => self.tokenize_symbol(1, Symbol::Exclamation, rest),
            &[b'=', ref rest..] => self.tokenize_symbol(1, Symbol::Equal, rest),
            &[b'?', ref rest..] => self.tokenize_symbol(1, Symbol::Question, rest),

            // TODO: handle utf8 correctly
            &[b'$', b'\\', _, ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(3); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Integer })) },
            &[b'$', _, ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Integer })) },

            &[b'0'..=b'9', ref rest..] => {
                let mut size: usize = 1;
                let mut next = rest;

                while let &[b'0'..=b'9', ref rest..] = next {
                    size += 1;
                    next = rest;
                }

                match next {
                    &[b'#', ref rest..] => {
                        size += 1;
                        next = rest;

                        while let &[b'0'..=b'9', ref rest..] = next {
                            size += 1;
                            next = rest;
                        }


                        let text = &self.src[0..size];
                        let position = self.current_position;

                        self.src = next;
                        self.current_position.forward(size);

                        Some(Ok(Token { text, position, token_type: TokenType::Integer }))
                    },
                    _ => panic!("")
                }
            },

            &[b'a'..=b'z', ..] | &[b'_', ..] => {
                let mut size: usize = 0;
                let mut next = self.src;

                while let &[b'A'..=b'Z', ref rest..] | &[b'a'..=b'z', ref rest..] | &[b'_', ref rest..] = next {
                    size += 1;
                    next = rest;
                }

                let text = &self.src[0..size];
                let position = self.current_position;

                self.src = next;
                self.current_position.forward(size);

                Some(Ok(Token { text, position, token_type: TokenType::Atom }))
            }

            &[b'A'..=b'Z', ..] => {
                let mut size: usize = 0;
                let mut next = self.src;

                while let &[b'A'..=b'Z', ref rest..] | &[b'a'..=b'z', ref rest..] | &[b'_', ref rest..] = next {
                    size += 1;
                    next = rest;
                }

                let text = &self.src[0..size];
                let position = self.current_position;

                self.src = next;
                self.current_position.forward(size);

                Some(Ok(Token { text, position, token_type: TokenType::Variable }))
            }

            _ => None
        }
    }
}
