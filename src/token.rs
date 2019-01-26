#[cfg(test)]
use insta::assert_debug_snapshot_matches;

#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

impl Position {
    fn forward(&mut self, n: usize) {
        self.offset += n;
        self.column += n;
    }

    fn break_line(&mut self) {
        self.column = 1;
        self.line += 1;
    }
}

#[derive(Debug)]
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
    NumberSign,
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
    Question,
    NewLine,
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Token<'a> {
    pub text: &'a [u8],
    pub position: Position,
    pub token_type: TokenType,
}

#[derive(Debug)]
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

    fn tokenize_symbol(&mut self, length: usize, symbol: Symbol, rest: &'a [u8]) -> Option<Result<Token<'a>, Error>> {
        let src = self.src;
        let position = self.current_position;

        self.src = rest;
        self.current_position.forward(length);
        if let Symbol::NewLine = symbol {
            self.current_position.break_line();
        }

        Some(Ok(Token {
            text: &src[0..length],
            position,
            token_type: TokenType::Symbol(symbol),
        }))
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.src {
                &[] => return None,

                &[b'=', b':', b'=', ref rest..] => return self.tokenize_symbol(3, Symbol::EqualColonEqual, rest),
                &[b'=', b'/', b'=', ref rest..] => return self.tokenize_symbol(3, Symbol::EqualSlashEqual, rest),
                &[b'.', b'.', b'.', ref rest..] => return self.tokenize_symbol(3, Symbol::DotDotDot, rest),

                &[b'-', b'>', ref rest..] => return self.tokenize_symbol(2, Symbol::HyphenGreater, rest),
                &[b'|', b'|', ref rest..] => return self.tokenize_symbol(2, Symbol::BarBar, rest),
                &[b'<', b'-', ref rest..] => return self.tokenize_symbol(2, Symbol::LessHyphen, rest),
                &[b'+', b'+', ref rest..] => return self.tokenize_symbol(2, Symbol::PlusPlus, rest),
                &[b'-', b'-', ref rest..] => return self.tokenize_symbol(2, Symbol::HyphenHyphen, rest),
                &[b'=', b'=', ref rest..] => return self.tokenize_symbol(2, Symbol::EqualEqual, rest),
                &[b'/', b'=', ref rest..] => return self.tokenize_symbol(2, Symbol::SlashEqual, rest),
                &[b'=', b'<', ref rest..] => return self.tokenize_symbol(2, Symbol::EqualLess, rest),
                &[b'>', b'=', ref rest..] => return self.tokenize_symbol(2, Symbol::GreaterEqual, rest),
                &[b'<', b'=', ref rest..] => return self.tokenize_symbol(2, Symbol::LessEqual, rest),
                &[b'=', b'>', ref rest..] => return self.tokenize_symbol(2, Symbol::EqualGreater, rest),
                &[b':', b'=', ref rest..] => return self.tokenize_symbol(2, Symbol::ColonEqual, rest),
                &[b'<', b'<', ref rest..] => return self.tokenize_symbol(2, Symbol::LessLess, rest),
                &[b'>', b'>', ref rest..] => return self.tokenize_symbol(2, Symbol::GreaterGreater, rest),
                &[b':', b':', ref rest..] => return self.tokenize_symbol(2, Symbol::ColonColon, rest),
                &[b'.', b'.', ref rest..] => return self.tokenize_symbol(2, Symbol::DotDot, rest),
                &[b'\r', b'\n', ref rest..] => return self.tokenize_symbol(2, Symbol::NewLine, rest),

                &[b'(', ref rest..] => return self.tokenize_symbol(1, Symbol::OpenParen, rest),
                &[b')', ref rest..] => return self.tokenize_symbol(1, Symbol::CloseParen, rest),
                &[b',', ref rest..] => return self.tokenize_symbol(1, Symbol::Comma, rest),
                &[b'{', ref rest..] => return self.tokenize_symbol(1, Symbol::OpenBrace, rest),
                &[b'}', ref rest..] => return self.tokenize_symbol(1, Symbol::CloseBrace, rest),
                &[b'[', ref rest..] => return self.tokenize_symbol(1, Symbol::OpenBracket, rest),
                &[b']', ref rest..] => return self.tokenize_symbol(1, Symbol::CloseBracket, rest),
                &[b'|', ref rest..] => return self.tokenize_symbol(1, Symbol::Bar, rest),
                &[b';', ref rest..] => return self.tokenize_symbol(1, Symbol::Semicolon, rest),
                &[b':', ref rest..] => return self.tokenize_symbol(1, Symbol::Colon, rest),
                &[b'#', ref rest..] => return self.tokenize_symbol(1, Symbol::NumberSign, rest),
                &[b'.', ref rest..] => return self.tokenize_symbol(1, Symbol::Dot, rest),
                &[b'*', ref rest..] => return self.tokenize_symbol(1, Symbol::Asterisk, rest),
                &[b'/', ref rest..] => return self.tokenize_symbol(1, Symbol::Slash, rest),
                &[b'+', ref rest..] => return self.tokenize_symbol(1, Symbol::Plus, rest),
                &[b'-', ref rest..] => return self.tokenize_symbol(1, Symbol::Hyphen, rest),
                &[b'<', ref rest..] => return self.tokenize_symbol(1, Symbol::Less, rest),
                &[b'>', ref rest..] => return self.tokenize_symbol(1, Symbol::Greater, rest),
                &[b'!', ref rest..] => return self.tokenize_symbol(1, Symbol::Exclamation, rest),
                &[b'=', ref rest..] => return self.tokenize_symbol(1, Symbol::Equal, rest),
                &[b'?', ref rest..] => return self.tokenize_symbol(1, Symbol::Question, rest),
                &[b'\n', ref rest..] => return self.tokenize_symbol(1, Symbol::NewLine, rest),

                // TODO: handle utf8 correctly
                &[b'$', b'\\', _, ref rest..] => {
                    let position = self.current_position;
                    self.src = rest;
                    self.current_position.forward(3);
                    return Some(Ok(Token {
                        text: &self.src[0..2],
                        position,
                        token_type: TokenType::Integer,
                    }));
                }
                &[b'$', _, ref rest..] => {
                    let position = self.current_position;
                    self.src = rest;
                    self.current_position.forward(2);
                    return Some(Ok(Token {
                        text: &self.src[0..1],
                        position,
                        token_type: TokenType::Integer,
                    }));
                }

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

                            return Some(Ok(Token {
                                text,
                                position,
                                token_type: TokenType::Integer,
                            }));
                        }
                        _ => panic!(""),
                    }
                }

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

                    return Some(Ok(Token {
                        text,
                        position,
                        token_type: TokenType::Atom,
                    }));
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

                    return Some(Ok(Token {
                        text,
                        position,
                        token_type: TokenType::Variable,
                    }));
                }

                &[b' ', ref rest..] | &[b'\t', ref rest..] => {
                    self.src = rest;
                    self.current_position.forward(1);
                    continue;
                }

                _ => return None,
            }
        }
    }
}

#[cfg(test)]
#[test]
fn tokenizer_test() {
    assert_debug_snapshot_matches!(
        "tokenize symbols",
        Tokenizer::new(b"( ) , -> { } [ ] | || <- ; : # . \n / + - ++ -- == /= =< < >= \r\n > =:= =/= <= => := << >> ! = :: .. ...")
            .collect::<Result<Vec<_>, _>>()
    );
}
