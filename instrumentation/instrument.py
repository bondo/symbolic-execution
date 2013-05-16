import imp
import builtins

def symbolic_scope(*vars):
    print('symbolic_scope:', vars)
builtins.symbolic_scope = symbolic_scope

def symbolic_assert(var):
    print('symbolic_assert:', var)
builtins.symbolic_assert = symbolic_assert

def symbolic_refute(var):
    print('symbolic_refute:', var)
builtins.symbolic_refute = symbolic_refute

def symbolic_call(fun, *vars):
    print('symbolic_call:', fun, '<-', vars)
builtins.symbolic_call = symbolic_call

def symbolic_assign_literal(target, literal):
    print('symbolic_assign_literal:', target, '=', literal)
builtins.symbolic_assign_literal = symbolic_assign_literal

def symbolic_assign_binop(target, op, left, right):
    print('symbolic_assign_binop:', target, '=', left, op, right)
builtins.symbolic_assign_binop = symbolic_assign_binop

bar = imp.load_source('module.name', '../transformer/tests/simple3.out.py')
x = 8
try:
    bar.foo(x)
except ZeroDivisionError:
    print('When x is', x, 'a division by zero error occurs.')
except:
    print('When x is', x, 'an unknown error occurs.')
else:
    print('No error occurs when x is', x)
