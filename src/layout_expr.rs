use erl_tokenize::values::Keyword;
use erl_tokenize::values::Symbol;
use erl_tokenize::Token;
use std::rc::Rc;

type VarId = u32;

#[derive(Debug)]
pub enum LayoutExpr<'a> {
    Empty,
    Text(&'a str),
    Juxtaposition(Vec<Rc<LayoutExpr<'a>>>),
    Stacking(Vec<Rc<LayoutExpr<'a>>>),
    Choice(Rc<LayoutExpr<'a>>, Rc<LayoutExpr<'a>>),
    Wrap(Vec<Rc<LayoutExpr<'a>>>),
    Indent(Rc<LayoutExpr<'a>>),
    HalfIndent(Rc<LayoutExpr<'a>>),
    Bind {
        var_id: VarId,
        var_expr: Rc<LayoutExpr<'a>>,
        body_expr: Rc<LayoutExpr<'a>>,
    },
    Var(VarId),
}

impl<'a> LayoutExpr<'a> {
    fn empty() -> Rc<LayoutExpr<'a>> {
        Rc::new(LayoutExpr::Empty)
    }

    fn text(t: &'a str) -> Rc<LayoutExpr<'a>> {
        Rc::new(LayoutExpr::Text(t))
    }

    fn juxtaposition(exprs: Vec<Rc<LayoutExpr<'a>>>) -> Rc<LayoutExpr<'a>> {
        Rc::new(LayoutExpr::Juxtaposition(exprs))
    }

    fn stacking(exprs: Vec<Rc<LayoutExpr<'a>>>) -> Rc<LayoutExpr<'a>> {
        Rc::new(LayoutExpr::Stacking(exprs))
    }

    fn choice(lhs: Rc<LayoutExpr<'a>>, rhs: Rc<LayoutExpr<'a>>) -> Rc<LayoutExpr<'a>> {
        Rc::new(LayoutExpr::Choice(lhs, rhs))
    }

    fn wrap(exprs: Vec<Rc<LayoutExpr<'a>>>) -> Rc<LayoutExpr<'a>> {
        Rc::new(LayoutExpr::Wrap(exprs))
    }

    fn indent(expr: Rc<LayoutExpr<'a>>) -> Rc<LayoutExpr<'a>> {
        Rc::new(LayoutExpr::Indent(expr))
    }

    fn half_indent(expr: Rc<LayoutExpr<'a>>) -> Rc<LayoutExpr<'a>> {
        Rc::new(LayoutExpr::HalfIndent(expr))
    }

    fn bind(var_id: VarId, var_expr: Rc<LayoutExpr<'a>>, body_expr: Rc<LayoutExpr<'a>>) -> Rc<LayoutExpr<'a>> {
        Rc::new(LayoutExpr::Bind { var_id, var_expr, body_expr })
    }

    fn var(var_id: VarId) -> Rc<LayoutExpr<'a>> {
        Rc::new(LayoutExpr::Var(var_id))
    }
}

pub fn parse(tokens: &[Token]) -> Rc<LayoutExpr> {
    let (layout_expr, _) = parse_forms(tokens);
    layout_expr
}

fn parse_forms(mut tokens: &[Token]) -> (Rc<LayoutExpr>, &[Token]) {
    let mut forms = vec![];

    while tokens.len() > 1 {
        let (stack, rest_tokens) = parse_stack(tokens);

        forms.push(stack);

        tokens = rest_tokens;
    }

    (LayoutExpr::stacking(forms), tokens)
}

fn parse_stack(mut tokens: &[Token]) -> (Rc<LayoutExpr>, &[Token]) {
    let mut lines = vec![];

    while tokens.len() > 1 {
        println!("parse_stack {:?}", tokens[0].text());

        let (line_expr, terminator, rest_tokens) = parse_juxtaposition(tokens);

        match terminator.as_symbol_token() {
            Some(symbol_token) => match symbol_token.value() {
                Symbol::Dot | Symbol::Semicolon => {
                    lines.push(line_expr);
                    return (LayoutExpr::stacking(lines), rest_tokens);
                }
                Symbol::RightArrow => {
                    let (stack_expr, rest_tokens) = parse_stack(rest_tokens);

                    let stacking_choice = LayoutExpr::stacking(vec![line_expr.clone(), LayoutExpr::indent(stack_expr.clone())]);
                    let juxtapositions_choice = LayoutExpr::juxtaposition(vec![line_expr.clone(), stack_expr.clone()]);

                    lines.push(LayoutExpr::choice(stacking_choice, juxtapositions_choice));

                    tokens = rest_tokens;
                }
                _ => {
                    lines.push(line_expr);
                    tokens = rest_tokens;
                }
            },
            None => match terminator.as_keyword_token() {
                Some(keyword_token) => match keyword_token.value() {
                    Keyword::End => {
                        lines.push(line_expr);
                        return (LayoutExpr::stacking(lines), rest_tokens);
                    }
                    _ => {
                        lines.push(line_expr);
                        tokens = rest_tokens;
                    }
                },
                None => {
                    lines.push(line_expr);
                    tokens = rest_tokens;
                }
            },
        }
    }

    (LayoutExpr::stacking(lines), tokens)
}

fn parse_juxtaposition(mut tokens: &[Token]) -> (Rc<LayoutExpr>, &Token, &[Token]) {
    let mut line = vec![];

    while tokens.len() > 1 {
        println!("parse_juxtaposition {:?}", tokens[0].text());

        match tokens[0].as_symbol_token() {
            Some(symbol_token) => match symbol_token.value() {
                Symbol::Dot | Symbol::Semicolon | Symbol::RightArrow => {
                    line.push(LayoutExpr::text(tokens[0].text()));
                    return (LayoutExpr::juxtaposition(line), &tokens[0], &tokens[1..]);
                }
                Symbol::CloseParen | Symbol::CloseBrace | Symbol::CloseSquare => {
                    if line.is_empty() {
                        return (LayoutExpr::empty(), &tokens[0], tokens);
                    } else {
                        return (LayoutExpr::juxtaposition(line), &tokens[0], tokens);
                    }
                }
                Symbol::OpenParen | Symbol::OpenBrace | Symbol::OpenSquare => {
                    let (elements, rest_tokens) = parse_elements(&tokens[1..]);

                    if elements.is_empty() {
                        line.push(LayoutExpr::text(tokens[0].text()));
                        line.push(LayoutExpr::text(rest_tokens[0].text()));
                    } else {
                        let open_text = LayoutExpr::text(tokens[0].text());
                        let wrap_expr = LayoutExpr::wrap(elements);
                        let close_text = LayoutExpr::text(rest_tokens[0].text());

                        let stacking_coice = LayoutExpr::stacking(vec![
                            open_text.clone(),
                            LayoutExpr::half_indent(LayoutExpr::juxtaposition(vec![wrap_expr.clone(), close_text.clone()])),
                        ]);
                        let juxtapositions_choice = LayoutExpr::juxtaposition(vec![open_text.clone(), wrap_expr.clone(), close_text.clone()]);

                        line.push(LayoutExpr::choice(stacking_coice, juxtapositions_choice));
                    }

                    tokens = &rest_tokens[1..];
                }
                _ => {
                    line.push(LayoutExpr::text(tokens[0].text()));
                    tokens = &tokens[1..];
                }
            },
            None => match tokens[0].as_keyword_token() {
                Some(keyword_token) => match keyword_token.value() {
                    Keyword::Of => {
                        line.push(LayoutExpr::text(tokens[0].text()));
                        return (LayoutExpr::juxtaposition(line), &tokens[0], &tokens[1..]);
                    }
                    Keyword::End => {
                        return (LayoutExpr::juxtaposition(line), &tokens[0], tokens);
                    }
                    Keyword::Fun => {
                        let (stack_expr, rest_tokens) = parse_stack(&tokens[1..]);
                        let fun_expr = LayoutExpr::juxtaposition(vec![LayoutExpr::text(tokens[0].text()), stack_expr]);
                        let end_text = LayoutExpr::text(rest_tokens[0].text());

                        let stacking_coice = LayoutExpr::stacking(vec![fun_expr.clone(), end_text.clone()]);
                        let juxtapositions_choice = LayoutExpr::juxtaposition(vec![fun_expr.clone(), end_text.clone()]);

                        line.push(LayoutExpr::choice(stacking_coice, juxtapositions_choice));

                        tokens = &rest_tokens[1..];
                    }
                    Keyword::Case => {
                        let case_text = LayoutExpr::text(tokens[0].text());
                        let (case_line, _of_tokeen, rest_tokens) = parse_juxtaposition(&tokens[1..]);
                        let (case_stack, rest_tokens) = parse_stack(rest_tokens);
                        let end_text = LayoutExpr::text(rest_tokens[0].text());

                        line.push(LayoutExpr::stacking(vec![
                            LayoutExpr::juxtaposition(vec![case_text, case_line]),
                            LayoutExpr::indent(case_stack),
                            end_text,
                        ]));

                        tokens = &rest_tokens[1..];
                    }
                    Keyword::If => {
                        let if_text = LayoutExpr::text(tokens[0].text());
                        let (if_stack, rest_tokens) = parse_stack(&tokens[1..]);
                        let end_text = LayoutExpr::text(rest_tokens[0].text());

                        line.push(LayoutExpr::stacking(vec![if_text, LayoutExpr::indent(if_stack), end_text]));

                        tokens = &rest_tokens[1..];
                    }
                    _ => {
                        line.push(LayoutExpr::text(tokens[0].text()));
                        tokens = &tokens[1..];
                    }
                },
                None => {
                    line.push(LayoutExpr::text(tokens[0].text()));
                    tokens = &tokens[1..];
                }
            },
        }
    }

    (LayoutExpr::juxtaposition(line), &tokens[0], tokens)
}

fn parse_elements(mut tokens: &[Token]) -> (Vec<Rc<LayoutExpr>>, &[Token]) {
    let mut elements = vec![];

    while tokens.len() > 1 {
        println!("parse_elements {:?}", tokens[0].text());

        let (element, terminator, rest_tokens) = parse_juxtaposition(tokens);

        match terminator.as_symbol_token() {
            Some(symbol_token) => match symbol_token.value() {
                Symbol::CloseParen | Symbol::CloseBrace | Symbol::CloseSquare => {
                    match *element {
                        LayoutExpr::Empty => {}
                        _ => elements.push(element),
                    }

                    return (elements, rest_tokens);
                }
                Symbol::Comma => match *element {
                    LayoutExpr::Empty => {}
                    _ => elements.push(element),
                },
                _ => panic!("close expected"),
            },
            None => panic!("close expected"),
        }

        tokens = rest_tokens;
    }

    panic!("close expected")
}
