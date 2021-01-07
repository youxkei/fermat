#[cfg(test)]
use insta::assert_debug_snapshot;

use std::cmp::max;
use std::iter::FromIterator;

#[derive(PartialEq, Debug)]
pub struct Node<K, V> {
    key: K,
    value: V,
    height: i32,
    min_key: K,
    max_key: K,

    left: Box<AvlTree<K, V>>,
    right: Box<AvlTree<K, V>>,
}

#[derive(PartialEq, Debug)]
pub enum AvlTree<K, V> {
    Empty,
    Node(Node<K, V>),
}

impl<K: Copy + Ord, V> AvlTree<K, V> {
    pub fn new() -> AvlTree<K, V> {
        AvlTree::Empty
    }

    pub fn insert(self, key: K, value: V) -> AvlTree<K, V> {
        match self {
            AvlTree::Empty => AvlTree::Node(Node {
                key: key,
                value,
                height: 1,
                min_key: key,
                max_key: key,
                left: box AvlTree::Empty,
                right: box AvlTree::Empty,
            }),

            AvlTree::Node(node) => {
                if key == node.key {
                    AvlTree::Node(Node { value, ..node })
                } else if key < node.key {
                    *balance(
                        Operation::InsertLeft,
                        key,
                        Node {
                            left: box node.left.insert(key, value),
                            ..node
                        },
                    )
                } else if key > node.key {
                    *balance(
                        Operation::InsertRight,
                        key,
                        Node {
                            right: box node.right.insert(key, value),
                            ..node
                        },
                    )
                } else {
                    panic!()
                }
            }
        }
    }

    pub fn get(&self, key: K) -> Option<&V> {
        match self {
            AvlTree::Empty => None,
            AvlTree::Node(node) => {
                if key == node.key {
                    Some(&node.value)
                } else if key < node.key {
                    node.left.get(key)
                } else if key > node.key {
                    node.right.get(key)
                } else {
                    panic!()
                }
            }
        }
    }

    pub fn get_previous(&self, key: K, inclusive: bool) -> Option<(K, &V)> {
        match self {
            AvlTree::Empty => None,
            AvlTree::Node(node) => {
                if key == node.key {
                    if inclusive {
                        Some((node.key, &node.value))
                    } else {
                        node.left.get_previous(key, false)
                    }
                } else if key < node.key {
                    node.left.get_previous(key, inclusive)
                } else if key > node.key {
                    match &node.right {
                        box AvlTree::Empty => Some((node.key, &node.value)),
                        box AvlTree::Node(right) => {
                            if (inclusive && key < right.min_key)
                                || (!inclusive && key <= right.min_key)
                            {
                                Some((node.key, &node.value))
                            } else {
                                node.right.get_previous(key, inclusive)
                            }
                        }
                    }
                } else {
                    panic!()
                }
            }
        }
    }

    pub fn get_next(&self, key: K, inclusive: bool) -> Option<(K, &V)> {
        match self {
            AvlTree::Empty => None,
            AvlTree::Node(node) => {
                if key == node.key {
                    if inclusive {
                        Some((node.key, &node.value))
                    } else {
                        node.right.get_next(key, false)
                    }
                } else if key < node.key {
                    match &node.left {
                        box AvlTree::Empty => Some((node.key, &node.value)),
                        box AvlTree::Node(left) => {
                            if (inclusive && key > left.max_key)
                                || (!inclusive && key >= left.max_key)
                            {
                                Some((node.key, &node.value))
                            } else {
                                node.left.get_next(key, inclusive)
                            }
                        }
                    }
                } else if key > node.key {
                    node.right.get_next(key, inclusive)
                } else {
                    panic!()
                }
            }
        }
    }

    pub fn iter<'a>(&'a self) -> Iter<'a, K, V> {
        Iter {
            tree: self,
            current_key: None,
        }
    }

    pub fn keys<'a>(&'a self) -> Keys<'a, K, V> {
        Keys {
            tree: self,
            current_key: None,
        }
    }
}

#[test]
fn test_insert() {
    // snapshots/fermat__avltree__insert.snap
    assert_debug_snapshot!(AvlTree::new().insert(1, '1').insert(2, '2').insert(3, '3'))
}

#[test]
fn test_get() {
    let tree: AvlTree<u8, char> = vec![(1, '1'), (2, '2'), (3, '3')].into_iter().collect();

    assert_eq!(None, tree.get(0));
    assert_eq!(Some(&'1'), tree.get(1));
    assert_eq!(Some(&'2'), tree.get(2));
    assert_eq!(Some(&'3'), tree.get(3));
    assert_eq!(None, tree.get(4));
}

#[test]
fn test_get_previous() {
    let tree: AvlTree<u8, char> = vec![(1, '1'), (3, '3'), (5, '5')].into_iter().collect();

    assert_eq!(None, tree.get_previous(0, true));
    assert_eq!(Some((1, &'1')), tree.get_previous(1, true));
    assert_eq!(Some((1, &'1')), tree.get_previous(2, true));
    assert_eq!(Some((3, &'3')), tree.get_previous(3, true));
    assert_eq!(Some((3, &'3')), tree.get_previous(4, true));
    assert_eq!(Some((5, &'5')), tree.get_previous(5, true));
    assert_eq!(Some((5, &'5')), tree.get_previous(6, true));

    assert_eq!(None, tree.get_previous(0, false));
    assert_eq!(None, tree.get_previous(1, false));
    assert_eq!(Some((1, &'1')), tree.get_previous(2, false));
    assert_eq!(Some((1, &'1')), tree.get_previous(3, false));
    assert_eq!(Some((3, &'3')), tree.get_previous(4, false));
    assert_eq!(Some((3, &'3')), tree.get_previous(5, false));
    assert_eq!(Some((5, &'5')), tree.get_previous(6, false));
}

#[test]
fn test_get_next() {
    let tree: AvlTree<u8, char> = vec![(1, '1'), (3, '3'), (5, '5')].into_iter().collect();

    assert_eq!(Some((1, &'1')), tree.get_next(0, true));
    assert_eq!(Some((1, &'1')), tree.get_next(1, true));
    assert_eq!(Some((3, &'3')), tree.get_next(2, true));
    assert_eq!(Some((3, &'3')), tree.get_next(3, true));
    assert_eq!(Some((5, &'5')), tree.get_next(4, true));
    assert_eq!(Some((5, &'5')), tree.get_next(5, true));
    assert_eq!(None, tree.get_next(6, true));

    assert_eq!(Some((1, &'1')), tree.get_next(0, false));
    assert_eq!(Some((3, &'3')), tree.get_next(1, false));
    assert_eq!(Some((3, &'3')), tree.get_next(2, false));
    assert_eq!(Some((5, &'5')), tree.get_next(3, false));
    assert_eq!(Some((5, &'5')), tree.get_next(4, false));
    assert_eq!(None, tree.get_next(5, false));
    assert_eq!(None, tree.get_next(6, false));
}

impl<'a, K: Copy + Ord, V> FromIterator<(K, V)> for AvlTree<K, V> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (K, V)>,
    {
        let mut tree = AvlTree::new();

        for (key, value) in iter.into_iter() {
            tree = tree.insert(key, value);
        }

        tree
    }
}

#[test]
fn test_from_iter() {
    // snapshots/fermat__avltree__from_iter.snap
    let tree: AvlTree<u8, char> = vec![
        (2, '2'),
        (4, '4'),
        (7, '7'),
        (3, '3'),
        (1, '1'),
        (5, '5'),
        (6, '6'),
    ]
    .into_iter()
    .collect();
    assert_debug_snapshot!(tree);
}

pub struct Iter<'a, K, V> {
    tree: &'a AvlTree<K, V>,
    current_key: Option<K>,
}

impl<'a, K: Copy + Ord, V> Iterator for Iter<'a, K, V> {
    type Item = (K, &'a V);

    fn next(&mut self) -> Option<(K, &'a V)> {
        match self.current_key {
            None => match self.tree {
                AvlTree::Empty => None,
                AvlTree::Node(Node { min_key, .. }) => {
                    self.current_key = Some(*min_key);
                    Some((*min_key, self.tree.get(*min_key).unwrap()))
                }
            },
            Some(current_key) => match self.tree.get_next(current_key, false) {
                None => None,
                next @ Some((next_key, _)) => {
                    self.current_key = Some(next_key);
                    next
                }
            },
        }
    }
}

#[test]
fn test_iter() {
    let tree: AvlTree<u8, char> = vec![(1, '1'), (2, '2'), (3, '3')].into_iter().collect();
    let vec: Vec<(u8, &char)> = tree.iter().collect();

    assert_eq!(vec![(1, &'1'), (2, &'2'), (3, &'3')], vec);
}

pub struct Keys<'a, K, V> {
    tree: &'a AvlTree<K, V>,
    current_key: Option<K>,
}

impl<'a, K: Copy + Ord, V> Iterator for Keys<'a, K, V> {
    type Item = K;

    fn next(&mut self) -> Option<K> {
        match self.current_key {
            None => match self.tree {
                AvlTree::Empty => None,
                AvlTree::Node(Node { min_key, .. }) => {
                    self.current_key = Some(*min_key);
                    Some(*min_key)
                }
            },
            Some(current_key) => match self.tree.get_next(current_key, false) {
                None => None,
                Some((next_key, _)) => {
                    self.current_key = Some(next_key);
                    Some(next_key)
                }
            },
        }
    }
}

#[test]
fn test_keys() {
    let tree: AvlTree<u8, char> = vec![(1, '1'), (2, '2'), (3, '3')].into_iter().collect();
    let vec: Vec<u8> = tree.keys().collect();

    assert_eq!(vec![1, 2, 3], vec);
}

enum Operation {
    InsertLeft,
    InsertRight,
}

fn balance<K: Copy + Ord, V>(
    operation: Operation,
    inserting_key: K,
    node: Node<K, V>,
) -> Box<AvlTree<K, V>> {
    match operation {
        Operation::InsertLeft => match node.left {
            box AvlTree::Empty => panic!(),
            box AvlTree::Node(Node { key: left_key, .. }) => {
                if height(&node.left) - height(&node.right) == 2 {
                    if inserting_key < left_key {
                        left_left_rotation(node)
                    } else {
                        left_right_rotation(node)
                    }
                } else {
                    update(node)
                }
            }
        },

        Operation::InsertRight => match node.right {
            box AvlTree::Empty => panic!(),
            box AvlTree::Node(Node { key: right_key, .. }) => {
                if height(&node.right) - height(&node.left) == 2 {
                    if inserting_key < right_key {
                        right_left_rotation(node)
                    } else {
                        right_right_rotation(node)
                    }
                } else {
                    update(node)
                }
            }
        },
    }
}

fn left_left_rotation<K: Copy, V>(node: Node<K, V>) -> Box<AvlTree<K, V>> {
    match node.left {
        box AvlTree::Empty => panic!(),
        box AvlTree::Node(left) => update(Node {
            right: update(Node {
                left: left.right,
                ..node
            }),
            ..left
        }),
    }
}

#[test]
fn test_left_left_rotation() {
    /*
     *
     *         6             4
     *        / \           / \
     *       4   7         /   \
     *      / \     ->    2     6
     *     3   5         / \   / \
     *    / \           1   3 5   7
     *   1   2
     *
     * snapshots/fermat__avltree__left_left_rotation.snap
     */
    assert_debug_snapshot!(left_left_rotation(Node {
        key: 6,
        value: 6,
        min_key: 1,
        max_key: 7,
        height: 4,
        left: box AvlTree::Node(Node {
            key: 4,
            value: 4,
            min_key: 1,
            max_key: 5,
            height: 3,
            left: box AvlTree::Node(Node {
                key: 2,
                value: 2,
                min_key: 1,
                max_key: 3,
                height: 2,
                left: box AvlTree::Empty.insert(1, 1),
                right: box AvlTree::Empty.insert(3, 3),
            }),
            right: box AvlTree::Empty.insert(5, 5),
        }),
        right: box AvlTree::Empty.insert(7, 7)
    }))
}

fn left_right_rotation<K: Copy, V>(node: Node<K, V>) -> Box<AvlTree<K, V>> {
    match node.left {
        box AvlTree::Empty => panic!(),
        box AvlTree::Node(left) => match left.right {
            box AvlTree::Empty => panic!(),
            box AvlTree::Node(left_right) => update(Node {
                left: update(Node {
                    right: left_right.left,
                    ..left
                }),
                right: update(Node {
                    left: left_right.right,
                    ..node
                }),
                ..left_right
            }),
        },
    }
}

#[test]
fn test_left_right_rotation() {
    /*
     *
     *       6             4
     *      / \           / \
     *     2   7         /   \
     *    / \     ->    2     6
     *   1   4         / \   / \
     *      / \       1   3 5   7
     *     3   5
     *
     * snapshots/fermat__avltree__left_right_rotation.snap
     */
    assert_debug_snapshot!(left_right_rotation(Node {
        key: 6,
        value: 6,
        min_key: 1,
        max_key: 7,
        height: 4,
        left: box AvlTree::Node(Node {
            key: 2,
            value: 2,
            min_key: 1,
            max_key: 5,
            height: 3,
            left: box AvlTree::Empty.insert(1, 1),
            right: box AvlTree::Node(Node {
                key: 4,
                value: 4,
                min_key: 3,
                max_key: 5,
                height: 2,
                left: box AvlTree::Empty.insert(3, 3),
                right: box AvlTree::Empty.insert(5, 5),
            })
        }),
        right: box AvlTree::Empty.insert(7, 7),
    }))
}

fn right_right_rotation<K: Copy, V>(node: Node<K, V>) -> Box<AvlTree<K, V>> {
    match node.right {
        box AvlTree::Empty => panic!(),
        box AvlTree::Node(right) => update(Node {
            left: update(Node {
                right: right.left,
                ..node
            }),
            ..right
        }),
    }
}

#[test]
fn test_right_right_rotation() {
    /*
     *
     *     2                 4
     *    / \               / \
     *   1   4             /   \
     *      / \     ->    2     6
     *     3   6         / \   / \
     *        / \       1   3 5   7
     *       5   7
     *
     * snapshots/fermat__avltree__right_right_rotation.snap
     */
    assert_debug_snapshot!(right_right_rotation(Node {
        key: 2,
        value: 2,
        min_key: 1,
        max_key: 7,
        height: 4,
        left: box AvlTree::Empty.insert(1, 1),
        right: box AvlTree::Node(Node {
            key: 4,
            value: 4,
            min_key: 3,
            max_key: 7,
            height: 3,
            left: box AvlTree::Empty.insert(3, 3),
            right: box AvlTree::Node(Node {
                key: 6,
                value: 6,
                min_key: 5,
                max_key: 7,
                height: 2,
                left: box AvlTree::Empty.insert(5, 5),
                right: box AvlTree::Empty.insert(7, 7),
            })
        })
    }))
}

fn right_left_rotation<K: Copy, V>(node: Node<K, V>) -> Box<AvlTree<K, V>> {
    match node.right {
        box AvlTree::Empty => panic!(),
        box AvlTree::Node(right) => match right.left {
            box AvlTree::Empty => panic!(),
            box AvlTree::Node(right_left) => update(Node {
                left: update(Node {
                    right: right_left.left,
                    ..node
                }),
                right: update(Node {
                    left: right_left.right,
                    ..right
                }),
                ..right_left
            }),
        },
    }
}

#[test]
fn test_right_left_rotation() {
    /*
     *
     *     2               4
     *    / \             / \
     *   1   6           /   \
     *      / \   ->    2     6
     *     4   7       / \   / \
     *    / \         1   3 5   7
     *   3   5
     *
     * snapshots/fermat__avltree__right_left_rotation.snap
     */
    assert_debug_snapshot!(right_left_rotation(Node {
        key: 2,
        value: 2,
        min_key: 1,
        max_key: 7,
        height: 4,
        left: box AvlTree::Empty.insert(1, 1),
        right: box AvlTree::Node(Node {
            key: 6,
            value: 6,
            min_key: 3,
            max_key: 7,
            height: 3,
            left: box AvlTree::Node(Node {
                key: 4,
                value: 4,
                min_key: 3,
                max_key: 5,
                height: 2,
                left: box AvlTree::Empty.insert(3, 3),
                right: box AvlTree::Empty.insert(5, 5),
            }),
            right: box AvlTree::Empty.insert(7, 7),
        })
    }))
}

fn update<K: Copy, V>(node: Node<K, V>) -> Box<AvlTree<K, V>> {
    box AvlTree::Node(Node {
        height: max(height(&node.left), height(&node.right)) + 1,
        min_key: match node.left {
            box AvlTree::Empty => node.key,
            box AvlTree::Node(Node { min_key, .. }) => min_key,
        },
        max_key: match node.right {
            box AvlTree::Empty => node.key,
            box AvlTree::Node(Node { max_key, .. }) => max_key,
        },
        ..node
    })
}

fn height<K, V>(tree: &Box<AvlTree<K, V>>) -> i32 {
    match tree {
        box AvlTree::Empty => 0,
        box AvlTree::Node(Node { height, .. }) => *height,
    }
}
