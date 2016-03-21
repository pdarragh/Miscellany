#!/usr/bin/env python

# A Lisp-type interpreter in Python.
#
# Based on the information from: http://norvig.com/lispy.html

Env = dict

def standard_env():
    import math
    import operator as op
    env = Env()
    env.update(vars(math))
    env.update({
        '+': op.add, '-': op.sub, '*': op.mul, '/': op.div,
        '>': op.gt, '<': op.lt, '>=': op.ge, '<=': op.le, '=': op.eq,
        'abs'       : abs,
        'append'    : op.add,
        'apply'     : apply,
        'begin'     : lambda *x: x[-1],
        'car'       : lambda x: x[0],
        'cdr'       : lambda x: x[1:],
        'cons'      : lambda x,y: [x] + y,
        'eq?'       : op.is_,
        'equal?'    : op.eq,
        'list'      : lambda *x: list(x),
        'list?'     : lambda x: isinstance(x, list),
        'map'       : map,
        'max'       : max,
        'min'       : min,
        'not'       : op.not_,
        'null?'     : lambda x: x == [],
        'number?'   : lambda x: (isinstance(x, int) or isinstance(x, float)),
        'procedure?': callable,
        'round'     : round,
        'symbol?'   : lambda x: isinstance(x, str),
    })
    return env

global_env = standard_env()

def parse(program):
    return read_from_tokens(tokenize(program))

def tokenize(chars):
    return chars.replace('(', ' ( ').replace(')', ' ) ').split()

def read_from_tokens(tokens):
    if not tokens:
        raise SyntaxError("Unexpectedly hit the end!")
    token = tokens.pop(0)
    if token == '(':
        l = []
        while tokens[0] != ')':
            l.append(read_from_tokens(tokens))
        tokens.pop(0)
        return l
    elif token == ')':
        raise SyntaxError("Unexpected ).")
    else:
        return atomize(token)

def atomize(token):
    try:
        return int(token)
    except ValueError:
        try:
            return float(token)
        except ValueError:
            return str(token)

def eval(x, env=global_env):
    if isinstance(x, str):
        return env[x]
    elif not isinstance(x, list):
        return x
    elif x[0] == 'quote':
        (_, exp) = x
        return exp
    elif x[0] == 'if':
        (_, test, true_branch, false_branch) = x
        exp = (true_branch if eval(test, env) else false_branch)
        return eval(exp, env)
    elif x[0] == 'define':
        (_, var, exp) = x
        env[var] = eval(exp, env)
    else:
        proc = eval(x[0], env)
        args = [eval(arg, env) for arg in x[1:]]
        return proc(*args)

def repl(prompt="lis.py> "):
    while True:
        val = eval(parse(raw_input(prompt)))
        if val is not None:
            print(schemestr(val))

def schemestr(exp):
    if isinstance(exp, list):
        return '(' + ' '.join(map(schemestr, exp)) + ')'
    else:
        return str(exp)

if __name__ == '__main__':
    repl()
