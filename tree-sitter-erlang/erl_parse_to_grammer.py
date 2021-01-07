from enum import Enum, auto
from typing import Set, Dict, List

def normalize_token(token: str) -> str:
    if token == 'function':
        return 'function_'
    elif token == 'char':
        return 'char_'
    elif token == 'float':
        return 'float_'
    elif token == 'var':
        return 'var_'
    else:
        return token

def normalize_tokens(tokens: List[str]) -> List[str]:
    normalized_tokens: List[str] = []

    for token in tokens:
        normalized_token = normalize_token(token)

        if token == ':':
            break

        elif token.endswith(':'):
            normalized_tokens.append(normalized_token[:-1])
            break

        else:
            normalized_tokens.append(normalized_token)

    return normalized_tokens

def read_rules():
    class Mode(Enum):
        RULES = auto()
        NONTERMINALS = auto()

    mode: Mode = Mode.RULES
    nonterminals: Set[str] = set()
    rules: Dict[str, List[List[str]]] = {}

    with open('erl_parse.yrl') as f:
        for line in f.read().splitlines():
            if mode == Mode.RULES:
                if line == 'Nonterminals':
                    mode = Mode.NONTERMINALS

                elif line == 'Erlang code.':
                    break

                else:
                    tokens = normalize_tokens(line.split())

                    if len(tokens) >= 2 and tokens[0] in nonterminals and tokens[1] == '->':
                        if tokens[0] not in rules:
                            rules[tokens[0]] = []

                        rule: List[str] = []
                        for token in tokens[2:]:
                            if token == ':':
                                break

                            rule.append(token)

                        rules[tokens[0]].append(rule)

            elif mode == Mode.NONTERMINALS:
                tokens = normalize_tokens(line.split())

                for token in tokens:
                    if token.endswith('.'):
                        nonterminals.add(token[:-1])
                        mode = Mode.RULES
                        break

                    else:
                        nonterminals.add(token)

    return rules

for nonterminal, rule in read_rules().items():
    indent: str = '    '

    print('{}{}: $ => '.format(indent, nonterminal), end='')

    if len(rule) > 1:
        print('choice(')
    else:
        print('')

    indent += '  '

    for rhs in rule:
        print(indent, end='')

        if len(rhs) > 1:
            print('seq(', end='')

        for i, token in enumerate(rhs):
            if i > 0:
                print(', ', end='')

            if token[0] == "'":
                print(token, end='')

            else:
                print('$.{}'.format(token), end='')

        if len(rhs) > 1:
            print(')', end='')

        print(',')

    if len(rule) > 1:
        indent = indent[2:]
        print('{}),'.format(indent))

    print('')
