import imp
import builtins
import os.path
from subprocess import call
from sys import exc_info
from traceback import extract_tb

from z3 import *

class InstrumentationException(Exception): pass

class SymbolicEnvironmentException(InstrumentationException): pass

class SymbolicEnvironment:
    def __init__(self):
        self.env = []
        self.formals = []

    def new_scope(self, formals):
        if len(self.env) > 0:
            raise SymbolicEnvironmentException('Only simple environments supported')
        if len(self.env) != len(self.formals):
            raise SymbolicEnvironmentException('Arguments have not been bound to formals')

        self.env.append({})
        self.formals.append(formals)

    def get_formals(self):
        if len(self.formals) == 0:
            raise SymbolicEnvironmentException('No formals are bound')

        return self.formals[-1]

    def end_scope(self):
        raise SymbolicEnvironmentException('Exiting scope not implemented')

    def set(self, var, value):
        if len(self.env) == 0:
            raise SymbolicEnvironmentException('No scope')
        if len(self.env) != 1:
            raise SymbolicEnvironmentException('Only simple environments supported')

        self.env[-1][var] = value

    def get(self, var):
        if len(self.env) == 0:
            raise SymbolicEnvironmentException('No scope')
        if len(self.env) != 1:
            raise SymbolicEnvironmentException('Only simple environments supported')

        value = self.env[-1].get(var)

        if value is None:
            raise SymbolicEnvironmentException('Variable not found: ' + var)

        return value

class PathCondition:
    pc = []
    seen = set()
    def add(self, c, name, is_assert):
        self.pc.append((c, name, is_assert))
    def clear(self):
        self.pc = []
    def path(self):
        res = ()
        for c, name, is_assert in self.pc:
            res += ((name, is_assert),)
        return res
    def see(self):
        path = ()
        for c, name, is_assert in self.pc:
            path += ((name, is_assert),)
            self.seen.add(path)
    def next(self):
        if len(self.pc) == 0: return (None, None)

        # Flip the top condition
        c, name, is_assert = self.pc.pop()
        self.pc.append((Not(c), name, not is_assert))

        path = self.path()
        if path in self.seen:
            self.pc.pop()
            return self.next()
        else:
            cond = self.pc.pop()[0]
            for c, _, _ in self.pc: cond = And(cond, c)
            return (cond, path)


env = None
pc = PathCondition()
lits = set()

def symbolic_scope(*vars):
    global env
#   print('symbolic_scope:', vars)
    env.new_scope(vars)
    [env.set(var, Int(var)) for var in vars]
builtins.symbolic_scope = symbolic_scope

def symbolic_assert(var):
    global env, pc
#   print('symbolic_assert:', var)
    pc.add(env.get(var), var, True)
builtins.symbolic_assert = symbolic_assert

def symbolic_refute(var):
    global env, pc
#   print('symbolic_refute:', var)
    pc.add(Not(env.get(var)), var, False)
builtins.symbolic_refute = symbolic_refute

def symbolic_call(fun, *vars):
    global env, pc
#   print('symbolic_call:', fun, '<-', vars)
builtins.symbolic_call = symbolic_call

def symbolic_assign_call(target, fun, *vars):
    global env, pc
#   print('symbolic_assign_call:', target, '=', fun, '<-', vars)
    raise InstrumentationException('symbolic_assign_call not implemented')
builtins.symbolic_call = symbolic_call

def symbolic_assign_literal(target, literal):
    global env, pc, lits
#   print('symbolic_assign_literal:', target, '=', literal)
    if not isinstance(literal, int):
        raise InstrumentationException('Only integer literals are supported')
    lit = Int(str(literal))
    lits.add(literal)
    env.set(target, lit)
builtins.symbolic_assign_literal = symbolic_assign_literal

def symbolic_assign_binop(target, op, left, right):
    global env, pc
#   print('symbolic_assign_binop:', target, '=', left, op, right)
    l = env.get(left)
    r = env.get(right)
    if op == '<': env.set(target, l < r)
    elif op == '>': env.set(target, l > r)
    elif op == '==': env.set(target, l == r)
    elif op == '<=': env.set(target, l <= r)
    elif op == 'and': env.set(target, And(l, r))
    elif op == '+': env.set(target, l + r)
    elif op == '-': env.set(target, l - r)
    elif op == '/': env.set(target, l / r)
    elif op == '*': env.set(target, l * r)
    else: raise InstrumentationException('Unsupported binop: ' + op)
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

def solver_with_lits():
    global lits
    solver = Solver()
    for literal in lits:
        lit = Int(str(literal))
        solver.add(lit == literal)
    return solver

def model_next():
    global pc

    cond, nextpath = pc.next()
    if nextpath is None:
        print('No more execution paths found')
        return None
    print('Next execution path:', nextpath)

    solver = solver_with_lits()
    solver.add(cond)
    print('Corresponding solver:', solver)

    sat = str(solver.check())
    if sat is 'unsat':
        print('Constraints cannot be satisfied')
        return model_next()
    elif sat is 'unknown':
        print('Constraint satisfiability unknown')
        return model_next()
    elif sat is 'sat':
        print('Constraints are satisfiable')
        try: return solver.model()
        except Z3Exception:
            print('Model not found')
            return model_next()

def main():
    global env, pc

    num_args = 3
    path = get_transformed_path('../transformer/tests/pythagoras.py')
    if path is None: return

    bar = imp.load_source('module.name', path)
    arg_vals = (0,) * num_args
    while True:
        print('')
        pc.clear()
        env = SymbolicEnvironment()

        trace = None

        try:
            bar.foo(*arg_vals)
        except Z3Exception as ex:
            print("Z3 exception:", ex)
            break
        except InstrumentationException as ex:
            print("InstrumentationException:", ex)
            break
        except:
            type, e, trace = exc_info()
            trace = '\n  '.join(['%s line %d in function %s:\n    %s' % info for info in extract_tb(trace)[1:]])

        formals = env.get_formals()
        if num_args != len(formals):
            raise InstrumentationException('Something went bad: num_args != len(formals)')

        if trace is None: print('No error occurs when %s = %s.' % (str(formals), str(arg_vals)))
        else: print('When %s = %s a %s exception is raised: %s\nTraceback:\n  %s' % \
                        (str(formals), str(arg_vals), type.__name__, e, trace))

        print('Path just executed: ', pc.path())
        pc.see()
        model = model_next()
        if model is None: break
        else:
            print('Model:', model, '\n')
            arg_vals = tuple([model[Int(arg)].as_long() for arg in formals])

if __name__ == '__main__': main()
