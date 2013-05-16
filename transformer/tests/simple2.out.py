def foo(x):
    symbolic_scope('x')
    symbolic_assign_literal('tmp_1', 2)
    tmp_1 = 2
    symbolic_assign_binop('tmp_2', '*', 'tmp_1', 'x')
    tmp_2 = tmp_1 * x
    symbolic_return('tmp_2')
    return tmp_2