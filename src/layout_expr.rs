use std::iter::repeat;
use std::rc::Rc;

#[derive(PartialEq, Clone, Debug)]
pub enum LayoutExpr<'a> {
    Unit,
    Text(&'a str),
    Stack(Rc<LayoutExpr<'a>>, Rc<LayoutExpr<'a>>),
    Apposition(Rc<LayoutExpr<'a>>, Rc<LayoutExpr<'a>>),
    Choice(Rc<LayoutExpr<'a>>, Rc<LayoutExpr<'a>>),
    MultiLineCost(Rc<LayoutExpr<'a>>),
}

impl<'a> LayoutExpr<'a> {
    pub fn format(&self, indent: usize, indented: bool) -> (String, usize, bool) {
        match self {
            LayoutExpr::Unit => panic!("Unit should not be occured"),

            LayoutExpr::Text("") => ("".to_string(), indent, indented),

            LayoutExpr::Text(text) => {
                if indented {
                    (text.to_string(), indent + text.len(), true)
                } else {
                    (
                        repeat(' ').take(indent).collect::<String>() + text,
                        indent + text.len(),
                        true,
                    )
                }
            }

            LayoutExpr::Stack(lhs, rhs) => {
                let (lhs_text, _, _) = lhs.format(indent, indented);
                let (rhs_text, rhs_width, indented) = rhs.format(indent, false);

                (format!("{}\n{}", lhs_text, rhs_text), rhs_width, indented)
            }

            LayoutExpr::Apposition(lhs, rhs) => {
                let (lhs_text, lhs_width, indented) = lhs.format(indent, indented);
                let (rhs_text, rhs_width, indented) = rhs.format(lhs_width, indented);
                (format!("{}{}", lhs_text, rhs_text), rhs_width, indented)
            }

            LayoutExpr::Choice(lhs, _) => lhs.format(indent, indented),

            LayoutExpr::MultiLineCost(expr) => expr.format(indent, indented),
        }
    }

    pub fn is_unit(&self) -> bool {
        match self {
            LayoutExpr::Unit => true,
            _ => false,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            LayoutExpr::Text("") => true,
            _ => false,
        }
    }
}

macro layout_expr_helper {
    ($ctor:ident, $f:ident($($args:tt)*) $(, $($y:tt $($ys:tt)*)?)?) => {{
        let result = layout_expr!($f($($args)*));
        $(
            $(
                let result = Rc::new(LayoutExpr::$ctor(result, layout_expr_helper!($ctor, $y $($ys)*)));
            )?
        )?
        result
    }},

    ($ctor:ident, $x:expr $(, $($y:tt $($ys:tt)*)?)?) => {{
        let result = layout_expr!($x);
        $(
            $(
                let result = Rc::new(LayoutExpr::$ctor(result, layout_expr_helper!($ctor, $y $($ys)*)));
            )?
        )?
        result
    }},
}

pub macro layout_expr {
    ($x:literal) => {
        Rc::new(LayoutExpr::Text($x))
    },

    (stack ($x:tt $($xs:tt)*) $(,)?) => {
        layout_expr_helper!(Stack, $x $($xs)*)
    },

    (apposition ($x:tt $($xs:tt)*) $(,)?) => {
        layout_expr_helper!(Apposition, $x $($xs)*)
    },

    (choice ($x:tt $($xs:tt)*) $(,)?) => {
        layout_expr_helper!(Choice, $x $($xs)*)
    },

    (multi_line_cost ($x:tt $($xs:tt)*) $(,)?) => {
        layout_expr_helper!(MultiLineCost, $x $($xs)*)
    },

    ($x:expr) => {
        $x
    },
}

pub macro unit() {
    Rc::new(LayoutExpr::Unit)
}

pub macro text($x:expr) {
    Rc::new(LayoutExpr::Text($x))
}

pub macro stack {
    () => {
        unit!()
    },

    ($head:expr $(, $($tail:expr),* $(,)?)?) => {{
        #[allow(unused_mut)]
        let mut result = $head;

        if result.is_unit() {
            stack!($($($tail),*)?)
        } else {
            $(
                $(
                    let tail = $tail;

                    if !tail.is_unit() {
                        result = Rc::new(LayoutExpr::Stack(result, tail));
                    }
                 )*
             )?

            result
        }
    }},
}

#[test]
fn stack_test() {
    let text = text!("text");

    assert_eq!(unit!(), stack!(unit!()));
    assert_eq!(unit!(), stack!(unit!(), unit!()));
    assert_eq!(text.clone(), stack!(text.clone()));
    assert_eq!(text.clone(), stack!(unit!(), text.clone()));
    assert_eq!(text.clone(), stack!(unit!(), text.clone(), unit!()));
    assert_eq!(
        stack!(text.clone(), text.clone()),
        stack!(unit!(), text.clone(), unit!(), text.clone())
    );
}

pub macro apposition {
    () => {
        unit!()
    },

    ($head:expr $(, $($tail:expr),* $(,)?)?) => {{
        #[allow(unused_mut)]
        let mut result = $head;

        if result.is_unit() {
            apposition!($($($tail),*)?)
        } else {
            $(
                $(
                    let tail = $tail;

                    if !tail.is_unit() {
                        result = Rc::new(LayoutExpr::Apposition(result, tail));
                    }
                 )*
             )?

            result
        }
    }},
}

#[test]
fn apposition_test() {
    let text = text!("text");

    assert_eq!(unit!(), apposition!(unit!()));
    assert_eq!(unit!(), apposition!(unit!(), unit!()));
    assert_eq!(text.clone(), apposition!(text.clone()));
    assert_eq!(text.clone(), apposition!(unit!(), text.clone()));
    assert_eq!(text.clone(), apposition!(unit!(), text.clone(), unit!()));
    assert_eq!(
        apposition!(text.clone(), text.clone()),
        apposition!(unit!(), text.clone(), unit!(), text.clone())
    );
}

pub macro apposition_sep {
    ($separator:expr,) => {
        unit!()
    },

    ($separator:expr, $head:expr $(, $($tail:expr),* $(,)?)?) => {{
        #[allow(unused)]
        let separator = $separator;
        #[allow(unused_mut)]
        let mut result = $head;

        if result.is_unit() {
            apposition_sep!(separator, $($($tail),*)?)
        } else {
            $(
                $(
                    let tail = $tail;

                    if !tail.is_unit() {
                        result = Rc::new(LayoutExpr::Apposition(result, separator.clone()));
                        result = Rc::new(LayoutExpr::Apposition(result, tail));
                    }
                 )*
             )?

            result
        }
    }},
}

#[test]
fn apposition_sep_test() {
    let text = text!("text");
    let sep = text!("sep");

    assert_eq!(unit!(), apposition_sep!(sep.clone(), unit!()));
    assert_eq!(unit!(), apposition_sep!(sep.clone(), unit!(), unit!()));
    assert_eq!(text.clone(), apposition_sep!(sep.clone(), text.clone()));
    assert_eq!(
        text.clone(),
        apposition_sep!(sep.clone(), unit!(), text.clone())
    );
    assert_eq!(
        text.clone(),
        apposition_sep!(sep.clone(), unit!(), text.clone(), unit!())
    );
    assert_eq!(
        apposition!(text.clone(), sep.clone(), text.clone()),
        apposition_sep!(sep.clone(), unit!(), text.clone(), unit!(), text.clone())
    );
    assert_eq!(
        apposition!(
            text.clone(),
            sep.clone(),
            text.clone(),
            sep.clone(),
            text.clone()
        ),
        apposition_sep!(sep.clone(), text.clone(), text.clone(), text.clone())
    );
}

pub macro choice {
    () => {
        unit!()
    },

    ($head:expr $(, $($tail:expr),* $(,)?)?) => {{
        #[allow(unused_mut)]
        let mut result = $head;

        if result.is_unit() {
            choice!($($($tail),*)?)
        } else {
            $(
                $(
                    let tail = $tail;

                    if !tail.is_unit() {
                        result = Rc::new(LayoutExpr::Choice(result, tail));
                    }
                 )*
             )?

            result
        }
    }},
}

#[test]
fn choice_test() {
    let text = text!("text");

    assert_eq!(unit!(), choice!(unit!()));
    assert_eq!(unit!(), choice!(unit!(), unit!()));
    assert_eq!(text.clone(), choice!(text.clone()));
    assert_eq!(text.clone(), choice!(unit!(), text.clone()));
    assert_eq!(text.clone(), choice!(unit!(), text.clone(), unit!()));
    assert_eq!(
        choice!(text.clone(), text.clone()),
        choice!(unit!(), text.clone(), unit!(), text.clone())
    );
}

pub macro multi_line_cost($expr:expr) {{
    let expr = $expr;

    if expr.is_unit() {
        expr
    } else {
        Rc::new(LayoutExpr::MultiLineCost(expr))
    }
}}
