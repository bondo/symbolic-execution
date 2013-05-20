def foo(x, y):
    symbolic_scope('x', 'y')
    symbolic_assign_literal('tmp_1', 2)
    tmp_1 = 2
    symbolic_assign_binop('tmp_2', '*', 'tmp_1', 'y')
    tmp_2 = tmp_1 * y
    symbolic_assign_binop('tmp_3', '==', 'x', 'tmp_2')
    tmp_3 = x == tmp_2
    if tmp_3:
        symbolic_assert('tmp_3')
        symbolic_assign_literal('tmp_4', 10)
        tmp_4 = 10
        symbolic_assign_binop('tmp_5', '+', 'y', 'tmp_4')
        tmp_5 = y + tmp_4
        symbolic_assign_binop('tmp_6', '>', 'x', 'tmp_5')
        tmp_6 = x > tmp_5
        if tmp_6:
            symbolic_assert('tmp_6')
            symbolic_assign_literal('tmp_7', 0)
            tmp_7 = 0
            symbolic_assign_binop('x', '/', 'x', 'tmp_7')
            x = x / tmp_7
        else:
            symbolic_refute('tmp_6')
    else:
        symbolic_refute('tmp_3')