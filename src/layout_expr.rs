use std::iter::repeat;
use std::rc::Rc;

pub type Variable = usize;

#[derive(PartialEq, Clone, Debug)]
pub enum LayoutExpr<'a> {
    Unit,
    Text(&'a str),
    Stack(Rc<LayoutExpr<'a>>, Rc<LayoutExpr<'a>>),
    Apposition(Rc<LayoutExpr<'a>>, Rc<LayoutExpr<'a>>),
    Choice(Rc<LayoutExpr<'a>>, Rc<LayoutExpr<'a>>),
    HeightCost(Rc<LayoutExpr<'a>>),
    Let(Variable, Rc<LayoutExpr<'a>>, Rc<LayoutExpr<'a>>),
    Var(Variable),
}

impl<'a> LayoutExpr<'a> {
    pub fn print(&self, indent: usize) -> usize {
        match self {
            LayoutExpr::Unit => panic!("Unit should not be occured"),

            LayoutExpr::Text(text) => {
                print!("{}", text);
                indent + text.len()
            }

            LayoutExpr::Stack(lhs, rhs) => {
                lhs.print(indent);
                print!("\n{}", repeat(' ').take(indent).collect::<String>());
                rhs.print(indent)
            }

            LayoutExpr::Apposition(lhs, rhs) => {
                let width = lhs.print(indent);
                rhs.print(width)
            }

            LayoutExpr::Choice(_, _) => panic!("Choice should no be occered: {:?}", self),
            LayoutExpr::HeightCost(_) => panic!("HeightCost should no be occered: {:?}", self),
            LayoutExpr::Let(_, _, _) => panic!("Let should no be occered: {:?}", self),
            LayoutExpr::Var(_) => panic!("Var should no be occered: {:?}", self),
        }
    }

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
                let (rhs_text, rhs_width, contents_indent) = rhs.format(lhs_width, indented);
                (format!("{}{}", lhs_text, rhs_text), rhs_width, indented)
            }

            LayoutExpr::Choice(lhs, _) => lhs.format(indent, indented),

            LayoutExpr::HeightCost(expr) => expr.format(indent, indented),

            LayoutExpr::Let(_, _, _) => panic!("Let should no be occered: {:?}", self),
            LayoutExpr::Var(_) => panic!("Var should no be occered: {:?}", self),
        }
    }
}

macro_rules! layout_expr_helper {
    ($ctor:ident, $f:ident($($args:tt)*) $(, $($y:tt $($ys:tt)*)?)?) => {
        {
            let result = layout_expr!($f($($args)*));
            $(
                $(
                    let result = std::rc::Rc::new($crate::layout_expr::LayoutExpr::$ctor(result, layout_expr_helper!($ctor, $y $($ys)*)));
                )?
            )?
            result
        }
    };

    ($ctor:ident, $x:expr $(, $($y:tt $($ys:tt)*)?)?) => {
        {
            let result = layout_expr!($x);
            $(
                $(
                    let result = std::rc::Rc::new($crate::layout_expr::LayoutExpr::$ctor(result, layout_expr_helper!($ctor, $y $($ys)*)));
                )?
            )?
            result
        }
    };
}

macro_rules! layout_expr {
    ($x:literal) => {
        std::rc::Rc::new($crate::layout_expr::LayoutExpr::Text($x))
    };

    (stack ($x:tt $($xs:tt)*) $(,)?) => {
        layout_expr_helper!(Stack, $x $($xs)*)
    };

    (apposition ($x:tt $($xs:tt)*) $(,)?) => {
        layout_expr_helper!(Apposition, $x $($xs)*)
    };

    (choice ($x:tt $($xs:tt)*) $(,)?) => {
        layout_expr_helper!(Choice, $x $($xs)*)
    };

    (height_cost ($x:tt $($xs:tt)*) $(,)?) => {
        layout_expr_helper!(HeightCost, $x $($xs)*)
    };

    ($x:expr) => {
        $x
    }
}

macro_rules! unit {
    () => {
        std::rc::Rc::new($crate::layout_expr::LayoutExpr::Unit)
    };
}

macro_rules! text {
    ($x:expr) => {
        std::rc::Rc::new($crate::layout_expr::LayoutExpr::Text($x))
    };
}

macro_rules! stack {
    () => {
        unit!()
    };

    ($head:expr $(, $($tail:expr),* $(,)?)?) => {
        {
            use $crate::layout_expr::LayoutExpr;

            #[allow(unused_mut)]
            let mut result = $head;

            match &*result {
                LayoutExpr::Unit => {
                    stack!($($($tail),*)?)
                }

                _ => {
                    $(
                        $(
                            let tail = $tail;
                            match &*tail {
                                LayoutExpr::Unit => {}
                                _ => {
                                    result = std::rc::Rc::new(LayoutExpr::Stack(result, tail));
                                }
                            }
                         )*
                     )?

                    result
                }
            }
        }
    }
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

macro_rules! apposition {
    () => {
        unit!()
    };

    ($head:expr $(, $($tail:expr),* $(,)?)?) => {
        {
            use $crate::layout_expr::LayoutExpr;

            #[allow(unused_mut)]
            let mut result = $head;

            match &*result {
                LayoutExpr::Unit => {
                    apposition!($($($tail),*)?)
                }

                _ => {
                    $(
                        $(
                            let tail = $tail;
                            match &*tail {
                                LayoutExpr::Unit => {}
                                _ => {
                                    result = std::rc::Rc::new(LayoutExpr::Apposition(result, tail));
                                }
                            }
                         )*
                     )?

                    result
                }
            }
        }
    }
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

macro_rules! choice {
    () => {
        unit!()
    };

    ($head:expr $(, $($tail:expr),* $(,)?)?) => {
        {
            use $crate::layout_expr::LayoutExpr;

            #[allow(unused_mut)]
            let mut result = $head;

            match &*result {
                LayoutExpr::Unit => {
                    choice!($($($tail),*)?)
                }

                _ => {
                    $(
                        $(
                            let tail = $tail;
                            match &*tail {
                                LayoutExpr::Unit => {}
                                _ => {
                                    result = std::rc::Rc::new(LayoutExpr::Choice(result, tail));
                                }
                            }
                         )*
                     )?

                    result
                }
            }
        }
    }
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

macro_rules! height_cost {
    ($x:expr) => {
        std::rc::Rc::new($crate::layout_expr::LayoutExpr::HeightCost($x))
    };
}
