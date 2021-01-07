#[cfg(test)]
use insta::assert_debug_snapshot;

use std::cmp::max;
use std::collections::{BTreeSet, HashSet};
use std::rc::Rc;

use itertools::Itertools;
use rpds::HashTrieMap;

use crate::avltree::AvlTree;
use crate::layout_expr::{LayoutExpr, Variable};

#[derive(Clone, Copy, Debug)]
pub struct Config {
    pub right_margin: i32,
    pub newline_cost: i32,
    pub beyond_right_margin_cost: i32,
}

type Position = i32;

#[derive(PartialEq, Debug)]
pub struct Layout<'a> {
    pub layout_expr: LayoutExpr<'a>,
    pub span: i32,
    pub cost: i32,
    pub cost_gradient: i32,
}

#[derive(PartialEq, Clone, Debug)]
pub enum LayoutFun<'a> {
    Unit,
    Fun(Rc<AvlTree<Position, Layout<'a>>>),
}

impl<'a> LayoutFun<'a> {
    pub fn at(&self, position: Position) -> Layout<'a> {
        match self {
            LayoutFun::Unit => panic!(),
            LayoutFun::Fun(tree) => match tree.get_previous(position, true) {
                Some((
                    knot,
                    Layout {
                        layout_expr,
                        span,
                        cost,
                        cost_gradient,
                    },
                )) => Layout {
                    layout_expr: layout_expr.clone(),
                    span: *span,
                    cost: cost + cost_gradient * (position - knot),
                    cost_gradient: *cost_gradient,
                },
                None => panic!(),
            },
        }
    }

    pub fn from_layout_expr(layout_expr: &LayoutExpr<'a>, config: &Config) -> Self {
        LayoutFun::from_layout_expr_with_trailing(
            layout_expr,
            LayoutFun::Unit,
            &HashTrieMap::new(),
            config,
        )
    }

    fn from_layout_expr_with_trailing(
        layout_expr: &LayoutExpr<'a>,
        trailing_layout_fun: Self,
        env: &HashTrieMap<Variable, Self>,
        config: &Config,
    ) -> Self {
        match layout_expr {
            LayoutExpr::Unit => Self::Unit,
            LayoutExpr::Text(text) => Self::apposition(
                Self::text(text, config),
                trailing_layout_fun.clone(),
                config,
            ),
            LayoutExpr::Stack(lhs, rhs) => Self::stack(
                Self::from_layout_expr_with_trailing(lhs, Self::Unit, env, config),
                Self::from_layout_expr_with_trailing(rhs, trailing_layout_fun, env, config),
                config,
            ),
            LayoutExpr::Apposition(lhs, rhs) => Self::from_layout_expr_with_trailing(
                lhs,
                Self::from_layout_expr_with_trailing(rhs, trailing_layout_fun, env, config),
                env,
                config,
            ),
            LayoutExpr::Choice(lhs, rhs) => Self::choice(
                Self::from_layout_expr_with_trailing(lhs, trailing_layout_fun.clone(), env, config),
                Self::from_layout_expr_with_trailing(rhs, trailing_layout_fun, env, config),
            ),
            LayoutExpr::Let(var, def, body) => {
                let def_fun =
                    Self::from_layout_expr_with_trailing(def, LayoutFun::Unit, env, config);

                Self::from_layout_expr_with_trailing(
                    body,
                    trailing_layout_fun,
                    &env.insert(*var, def_fun),
                    config,
                )
            }
            LayoutExpr::Var(var) => Self::apposition(
                env.get(var).unwrap().clone(),
                trailing_layout_fun.clone(),
                config,
            ),
        }
    }

    fn text(text: &'a str, config: &Config) -> Self {
        let span = text.len() as i32;

        if span < config.right_margin {
            let layout_expr = LayoutExpr::Text(text);

            LayoutFun::Fun(Rc::new(
                AvlTree::new()
                    .insert(
                        0,
                        Layout {
                            layout_expr: layout_expr.clone(),
                            span,
                            cost: 0,
                            cost_gradient: 0,
                        },
                    )
                    .insert(
                        config.right_margin - span,
                        Layout {
                            layout_expr,
                            span,
                            cost: 0,
                            cost_gradient: config.beyond_right_margin_cost,
                        },
                    ),
            ))
        } else {
            LayoutFun::Fun(Rc::new(AvlTree::new().insert(
                0,
                Layout {
                    layout_expr: LayoutExpr::Text(text),
                    span,
                    cost: (span - config.right_margin),
                    cost_gradient: config.beyond_right_margin_cost,
                },
            )))
        }
    }

    fn stack(lhs: Self, rhs: Self, config: &Config) -> Self {
        match (&lhs, &rhs) {
            (LayoutFun::Unit, _) => rhs,
            (_, LayoutFun::Unit) => lhs,
            (LayoutFun::Fun(lhs_tree), LayoutFun::Fun(rhs_tree)) => {
                let lhs_knots: HashSet<_> = lhs_tree.keys().collect();
                let rhs_knots: HashSet<_> = rhs_tree.keys().collect();
                let knots = lhs_knots.union(&rhs_knots).cloned();

                let mut new_layout_fun = AvlTree::Empty;

                for knot in knots {
                    let lhs_layout = lhs.at(knot);
                    let rhs_layout = rhs.at(knot);

                    new_layout_fun = new_layout_fun.insert(
                        knot,
                        Layout {
                            layout_expr: LayoutExpr::Stack(
                                Rc::new(lhs_layout.layout_expr),
                                Rc::new(rhs_layout.layout_expr),
                            ),
                            span: rhs_layout.span,
                            cost: lhs_layout.cost + rhs_layout.cost + config.newline_cost,
                            cost_gradient: lhs_layout.cost_gradient + rhs_layout.cost_gradient,
                        },
                    )
                }

                LayoutFun::Fun(Rc::new(new_layout_fun))
            }
        }
    }

    fn apposition(lhs: Self, rhs: Self, config: &Config) -> Self {
        match (&lhs, &rhs) {
            (LayoutFun::Unit, _) => rhs,
            (_, LayoutFun::Unit) => lhs,
            (LayoutFun::Fun(lhs_tree), LayoutFun::Fun(rhs_tree)) => {
                let mut knots: HashSet<Position> = lhs_tree.keys().collect();

                for rhs_knot in rhs_tree.keys() {
                    for (lhs_knot, Layout { span, .. }) in lhs_tree.iter() {
                        if lhs_knot >= rhs_knot {
                            continue;
                        }

                        let knot_candidate = rhs_knot - span;

                        match lhs_tree.get_next(lhs_knot, false) {
                            None => {
                                if lhs_knot <= knot_candidate {
                                    knots.insert(knot_candidate);
                                }
                            }

                            Some((next_lhs_knot, _)) => {
                                if lhs_knot <= knot_candidate && knot_candidate < next_lhs_knot {
                                    knots.insert(knot_candidate);
                                }
                            }
                        }
                    }
                }

                let mut new_layout_fun = AvlTree::Empty;

                for knot in knots.into_iter() {
                    let lhs_layout = lhs.at(knot);
                    let rhs_position = knot + lhs_layout.span;
                    let rhs_layout = rhs.at(rhs_position);

                    new_layout_fun = new_layout_fun.insert(
                        knot,
                        Layout {
                            layout_expr: LayoutExpr::Apposition(
                                Rc::new(lhs_layout.layout_expr),
                                Rc::new(rhs_layout.layout_expr),
                            ),
                            span: lhs_layout.span + rhs_layout.span,
                            cost: lhs_layout.cost + rhs_layout.cost
                                - config.beyond_right_margin_cost
                                    * max(rhs_position - config.right_margin, 0),
                            cost_gradient: lhs_layout.cost_gradient + rhs_layout.cost_gradient
                                - config.beyond_right_margin_cost
                                    * (rhs_position >= config.right_margin) as Position,
                        },
                    )
                }

                LayoutFun::Fun(Rc::new(new_layout_fun))
            }
        }
    }

    fn choice(lhs: Self, rhs: Self) -> Self {
        match (&lhs, &rhs) {
            (LayoutFun::Unit, _) => rhs,
            (_, LayoutFun::Unit) => lhs,
            (LayoutFun::Fun(lhs_tree), LayoutFun::Fun(rhs_tree)) => {
                let lhs_knots: BTreeSet<_> = lhs_tree.keys().collect();
                let rhs_knots: BTreeSet<_> = rhs_tree.keys().collect();
                let knots: BTreeSet<_> = lhs_knots.union(&rhs_knots).cloned().collect();

                let mut adding_knots: BTreeSet<Position> = BTreeSet::new();

                for (knot, next_knot) in knots.iter().tuple_windows() {
                    match intersection_point(*knot, &lhs, &rhs) {
                        Some(intersection_point) => {
                            if intersection_point < *next_knot {
                                adding_knots.insert(intersection_point);
                            }
                        }
                        None => {}
                    }
                }

                let mut new_layout_fun = AvlTree::Empty;
                let mut previous: Option<(Position, i32, i32)> = None;

                for knot in knots.union(&adding_knots).cloned() {
                    let lhs_layout = lhs.at(knot);
                    let rhs_layout = rhs.at(knot);

                    let layout = if lhs_layout.cost < rhs_layout.cost
                        || (lhs_layout.cost == rhs_layout.cost
                            && lhs_layout.cost_gradient <= rhs_layout.cost_gradient)
                    {
                        lhs_layout
                    } else {
                        rhs_layout
                    };

                    let cost = layout.cost;
                    let cost_gradient = layout.cost_gradient;

                    match previous {
                        Some((previous_knot, previous_cost, previous_cost_gradient)) => {
                            if previous_cost_gradient != layout.cost_gradient
                                || previous_cost + (knot - previous_knot) * previous_cost_gradient
                                    != layout.cost
                            {
                                new_layout_fun = new_layout_fun.insert(knot, layout)
                            }
                        }
                        None => new_layout_fun = new_layout_fun.insert(knot, layout),
                    }

                    previous = Some((knot, cost, cost_gradient))
                }

                LayoutFun::Fun(Rc::new(new_layout_fun))
            }
        }
    }

    fn to_vec(&self) -> Vec<(Position, &Layout<'a>)> {
        match self {
            LayoutFun::Unit => vec![],
            LayoutFun::Fun(tree) => tree.iter().collect(),
        }
    }
}

#[test]
fn test_text() {
    /*
     *  01234
     * |foo･･|
     * |-----|
     * |･･foo|
     *
     * 0 -> {"foo", 3, 0, 0}
     * 2 -> {"foo", 3, 0, 100}
     *
     * snapshots/fermat__layout_fun__text.snap
     */

    assert_debug_snapshot!(LayoutFun::text(
        "foo",
        &Config {
            right_margin: 5,
            newline_cost: 1,
            beyond_right_margin_cost: 100,
        }
    )
    .to_vec())
}

#[test]
fn test_stack() {
    /*
     *  01234567
     * |foobar･･|･･･
     * |baz･････|･･･
     * |--------|---
     * |･･foobar|･･･
     * |･･baz･･･|･･･
     * |--------|---
     * |･････foo|bar
     * |･････baz|･･･
     *
     * 0 -> {"foobar\nbaz", 3, 1, 0}
     * 2 -> {"foobar\nbaz", 3, 1, 100}
     * 5 -> {"foobar\nbaz", 3, 301, 200}
     *
     * snapshots/fermat__layout_fun__stack.snap
     */

    let config = &Config {
        right_margin: 8,
        newline_cost: 1,
        beyond_right_margin_cost: 100,
    };

    assert_debug_snapshot!(LayoutFun::stack(
        LayoutFun::text("foobar", config),
        LayoutFun::text("baz", config),
        config
    )
    .to_vec())
}

#[test]
fn test_apposition() {
    /*
     *  01234567
     * |foobar･･|･
     * |--------|-
     * |･･foobar|･
     * |--------|-
     * |･････foo|bar
     *
     * 0 -> {"foobar", 6, 0, 0}
     * 2 -> {"foobar", 6, 0, 100}
     * 5 -> {"foobar", 6, 300, 100}
     *
     * snapshots/fermat__layout_fun__line.snap
     */

    let config = &Config {
        right_margin: 8,
        newline_cost: 1,
        beyond_right_margin_cost: 100,
    };

    assert_debug_snapshot!(LayoutFun::apposition(
        LayoutFun::text("foo", config),
        LayoutFun::text("bar", config),
        config
    )
    .to_vec())
}

#[test]
fn test_choice() {
    /*
     *  0123456789ABCDEF
     * |if(foo) bar();･･|･･･
     * |----------------|---
     * |･･if(foo) bar();|･･･
     * |----------------|---
     * |･･･if(foo)･･････|･･･
     * |･･･    bar();･･･|･･･
     * |----------------|---
     * |･･････if(foo)･･･|･･･
     * |･･････    bar();|･･･
     * |----------------|---
     * |･････････if(foo)|･･･
     * |･････････    bar|();
     *
     * 0 -> {"if(foo)･bar();", 14, 0, 0}
     * 2 -> {"if(foo)･bar();", 14, 0, 100}
     * 3 -> {"if(foo)\n    bar();", 10, 1, 0}
     * 6 -> {"if(foo)\n    bar();", 10, 1, 100}
     * 9 -> {"if(foo)\n    bar();", 10, 301, 200}
     *
     * snapshots/fermat__layout_fun__choice.snap
     */

    let config = &Config {
        right_margin: 16,
        newline_cost: 1,
        beyond_right_margin_cost: 100,
    };

    assert_debug_snapshot!(LayoutFun::choice(
        LayoutFun::text("if(foo) bar();", config),
        LayoutFun::stack(
            LayoutFun::text("if(foo)", config),
            LayoutFun::text("    bar();", config),
            config,
        ),
    )
    .to_vec());
}

#[test]
fn test_from_layout_expr_record() {
    /*  0123456789ABCDEFGHIJKLMNOPQRSTUV
     * |Foobarbaz = #record{field = 1}･･|･････････
     * |--------------------------------|---------
     * |･･Foobarbaz = #record{field = 1}|･････････
     * |--------------------------------|---------
     * |･･･Foobarbaz =･･････････････････|･････････
     * |･･･    #record{field = 1}･･･････|･････････
     * |--------------------------------|---------
     * |･･････････Foobarbaz =･･･････････|･････････
     * |･･････････    #record{field = 1}|･････････
     * |--------------------------------|---------
     * |･･･････････Foobarbaz =･･････････|･････････
     * |･･･････････    #record{･････････|･････････
     * |･･･････････       field = 1･････|･････････
     * |･･･････････      }･･････････････|･････････
     * |--------------------------------|---------
     * |････････････････Foobarbaz =･････|･････････
     * |････････････････    #record{････|･････････
     * |････････････････       field = 1|･････････
     * |････････････････      }･････････|･････････
     * |--------------------------------|---------
     * |････････････････････Foobarbaz =･|･････････
     * |････････････････････    #record{|･････････
     * |････････････････････       field| = 1･････
     * |････････････････････      }･････|･････････
     * |--------------------------------|---------
     * |･････････････････････Foobarbaz =|･････････
     * |･････････････････････    #record|{････････
     * |･････････････････････       fiel|d = 1････
     * |･････････････････････      }････|･････････
     * |--------------------------------|---------
     * |･････････････････････････Foobarb|az =･････
     * |･････････････････････････    #re|cord{････
     * |･････････････････････････       |field = 1
     * |･････････････････････････      }|･････････
     * |--------------------------------|---------
     *
     * 0 ->  {"Foobarbaz = #record{field = 1}", 30, 0, 0}
     * 2 ->  {"Foobarbaz = #record{field = 1}", 30, 0, 100}
     * 3 ->  {"Foobarbaz =\n    #record{field = 1}", 22, 1, 0}
     * 10 -> {"Foobarbaz =\n    #record{field = 1}", 22, 1, 100}
     * 11 -> {"Foobarbaz =\n    #record{\n       field = 1\n      }", 7, 3, 0}
     * 16 -> {"Foobarbaz =\n    #record{\n       field = 1\n      }", 7, 3, 100}
     * 20 -> {"Foobarbaz =\n    #record{\n       field = 1\n      }", 7, 403, 200}
     * 21 -> {"Foobarbaz =\n    #record{\n       field = 1\n      }", 7, 603, 300}
     * 25 -> {"Foobarbaz =\n    #record{\n       field = 1\n      }", 7, 1803, 400}
     *
     * snapshots/fermat__layout_fun__from_layout_expr_record.snap
     */

    let config = &Config {
        right_margin: 32,
        newline_cost: 1,
        beyond_right_margin_cost: 100,
    };

    /*
    assert_debug_snapshot!(LayoutFun::from_layout_expr(
        &*apposition!(
            choice!(
                apposition!(text!("Foobarbaz ="), text!(" ")),
                stack!(text!("Foobarbaz ="), text!("    "))
            ),
            choice!(
                apposition!(text!("#record{"), text!("field = 1"), text!("}")),
                stack!(
                    text!("#record{"),
                    apposition!(text!("   "), text!("field = 1")),
                    apposition!(text!("  "), text!("}")),
                ),
            ),
        ),
        config
    )
    .to_vec())
    */

    assert_debug_snapshot!(LayoutFun::from_layout_expr(
        &*layout_expr!(apposition(
            choice(apposition("Foobarbaz =", " "), stack("Foobarbaz =", "    "),),
            choice(
                apposition("#record{", "field = 1", "}"),
                stack(
                    "#record{",
                    apposition("   ", "field = 1"),
                    apposition("  ", "}"),
                ),
            ),
        )),
        config
    )
    .to_vec())
}

fn intersection_point<'a>(
    position: Position,
    lhs: &LayoutFun<'a>,
    rhs: &LayoutFun<'a>,
) -> Option<Position> {
    let lhs_layout = lhs.at(position);
    let rhs_layout = rhs.at(position);

    if lhs_layout.cost_gradient == rhs_layout.cost_gradient {
        None // Parallel
    } else {
        let intersection_point = ((rhs_layout.cost - lhs_layout.cost) as f64
            / (lhs_layout.cost_gradient - rhs_layout.cost_gradient) as f64)
            .ceil() as i32;

        if intersection_point < 0 {
            None
        } else {
            Some(position + intersection_point)
        }
    }
}
