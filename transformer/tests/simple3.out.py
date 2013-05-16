def foo(x):
    symbolic_scope('x')
    symbolic_assign_literal('tmp_1', 5)
    tmp_1 = 5
    symbolic_assign_binop('tmp_2', '>', 'x', 'tmp_1')
    tmp_2 = x > tmp_1
    if tmp_2:
        symbolic_assert('tmp_2')
        symbolic_assign_literal('tmp_3', 2)
        tmp_3 = 2
        symbolic_assign_binop('tmp_4', '*', 'tmp_3', 'x')
        tmp_4 = tmp_3 * x
        symbolic_call('print', 'tmp_4', tmp_4)
        print(tmp_4)
    else:
        symbolic_refute('tmp_2')
        symbolic_assign_literal('tmp_5', 0)
        tmp_5 = 0
        symbolic_assign_binop('tmp_6', '<=', 'x', 'tmp_5')
        tmp_6 = x <= tmp_5
        if tmp_6:
            symbolic_assert('tmp_6')
            symbolic_call('print', 'x', x)
            print(x)
        else:
            symbolic_refute('tmp_6')
            symbolic_assign_literal('tmp_7', 0)
            tmp_7 = 0
            symbolic_assign_binop('tmp_8', '/', 'x', 'tmp_7')
            tmp_8 = x / tmp_7
            symbolic_call('print', 'tmp_8', tmp_8)
            print(tmp_8)