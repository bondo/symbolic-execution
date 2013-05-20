import imp
import builtins
import os.path
from subprocess import call
from sys import exc_info
from traceback import extract_tb

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


def get_transformed_path(path):
    realpath = os.path.realpath(path)
    prefix, ext = os.path.splitext(realpath)
    outpath = prefix + '.out' + ext

    transform = True
    try:
        before = os.path.getmtime(realpath)
        after = os.path.getmtime(outpath)
        if before < after:
            transform = False
    except: pass
	
    if transform:
        print('Transform %s.' % realpath)
        if call(['../transformer/Main', realpath, outpath]):
            print('Transformation failed.')
            return None
        print('Transformation successful.')
    else:
        print("Don't transform %s." % realpath)

    return outpath

def main():
    path = get_transformed_path('../transformer/tests/simple3.py')
    if path is None: return

    bar = imp.load_source('module.name', path)
    x = 3
    try:
        bar.foo(x)
    except:
        type, e, trace = exc_info()
        trace = '\n'.join(['%s line %d in function %s:\n  %s' % info for info in extract_tb(trace)[1:]])
        print('When x is %s a %s exception is raised: %s\nTraceback:\n %s' % (str(x), type.__name__, e, trace))
    else:
        print('No error occurs when x is %s.' % str(x))

if __name__ == '__main__': main()
