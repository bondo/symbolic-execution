import imp
import builtins

def symbolic_scope(*vars):
    print('symbolic_scope: ', vars)
builtins.symbolic_scope = symbolic_scope

def symbolic_assert(*vars):
    print('symbolic_assert: ', vars)
builtins.symbolic_assert = symbolic_assert

def symbolic_refute(*vars):
    print('symbolic_refute: ', vars)
builtins.symbolic_refute = symbolic_refute

def symbolic_call(*vars):
    print('symbolic_call: ', vars)
builtins.symbolic_call = symbolic_call

def symbolic_assign_literal(*vars):
    print('symbolic_assign_literal: ', vars)
builtins.symbolic_assign_literal = symbolic_assign_literal

def symbolic_assign_binop(*vars):
    print('symbolic_assign_binop: ', vars)
builtins.symbolic_assign_binop = symbolic_assign_binop

bar = imp.load_source('module.name', '../transformer/tests/simple3.out.py')
x = 3
try:
    bar.foo(x)
except ZeroDivisionError:
    print('When x is', x, 'a division by zero error occurs.')
except:
    print('When x is', x, 'an unknown error occurs.')
