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
    VerticalBar,
    DoubleVerticalBar,
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
    fn new(src: &'a [u8]) -> Self {
        Tokenizer {
            src,
            current_position: Position { offset: 0, line: 1, column: 1 },
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        #[rustfmt::skip]
        match self.src {
            &[] => None,

            &[b'=', b':', b'=', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(3); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::EqualColonEqual) })) },
            &[b'=', b'/', b'=', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(3); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::EqualSlashEqual) })) },
            &[b'.', b'.', b'.', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(3); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::DotDotDot) })) },

            &[b'-', b'>', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::HyphenGreater) })) },
            &[b'|', b'|', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::DoubleVerticalBar) })) },
            &[b'<', b'-', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::LessHyphen) })) },
            &[b'+', b'+', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::PlusPlus) })) },
            &[b'-', b'-', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::HyphenHyphen) })) },
            &[b'=', b'=', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::EqualEqual) })) },
            &[b'/', b'=', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::SlashEqual) })) },
            &[b'=', b'<', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::SlashEqual) })) },
            &[b'>', b'=', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::GreaterEqual) })) },
            &[b'<', b'=', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::LessEqual) })) },
            &[b'=', b'>', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::EqualGreater) })) },
            &[b':', b'=', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::ColonEqual) })) },
            &[b'.', b'.', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(2); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::DotDot) })) },

            &[b'(', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::OpenParen) })) },
            &[b')', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::CloseParen) })) },
            &[b',', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::Comma) })) },
            &[b'{', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::OpenBrace) })) },
            &[b'}', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::CloseBrace) })) },
            &[b'[', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::OpenBracket) })) },
            &[b']', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::CloseBracket) })) },
            &[b'|', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::VerticalBar) })) },
            &[b';', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::Semicolon) })) },
            &[b':', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::Colon) })) },
            &[b'#', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::Numbersign) })) },
            &[b'.', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::Dot) })) },
            &[b'*', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::Asterisk) })) },
            &[b'/', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::Slash) })) },
            &[b'+', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::Plus) })) },
            &[b'-', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::Hyphen) })) },
            &[b'!', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::Exclamation) })) },
            &[b'=', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::Equal) })) },
            &[b'?', ref rest..] => { let position = self.current_position; self.src = rest; self.current_position.forward(1); Some(Ok(Token { text: &self.src[0..1], position, token_type: TokenType::Symbol(Symbol::Question) })) },

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
                    }
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
