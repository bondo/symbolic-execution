def foo(a, b, c):
    symbolic_scope('a', 'b', 'c')
    symbolic_assign_binop('tmp_1', '*', 'a', 'a')
    tmp_1 = a * a
    symbolic_assign_binop('tmp_2', '*', 'b', 'b')
    tmp_2 = b * b
    symbolic_assign_binop('tmp_3', '+', 'tmp_1', 'tmp_2')
    tmp_3 = tmp_1 + tmp_2
    symbolic_assign_binop('tmp_4', '*', 'c', 'c')
    tmp_4 = c * c
    symbolic_assign_binop('tmp_5', '==', 'tmp_3', 'tmp_4')
    tmp_5 = tmp_3 == tmp_4
    symbolic_assign_literal('tmp_6', 0)
    tmp_6 = 0
    symbolic_assign_binop('tmp_7', '>', 'a', 'tmp_6')
    tmp_7 = a > tmp_6
    symbolic_assign_binop('tmp_8', 'and', 'tmp_5', 'tmp_7')
    tmp_8 = tmp_5 and tmp_7
    if tmp_8:
        symbolic_assert('tmp_8')
        symbolic_assign_binop('tmp_9', '+', 'a', 'b')
        tmp_9 = a + b
        symbolic_assign_binop('sum', '+', 'tmp_9', 'c')
        sum = tmp_9 + c
        symbolic_assign_literal('tmp_10', 0)
        tmp_10 = 0
        symbolic_assign_binop('tmp_11', '<', 'tmp_10', 'sum')
        tmp_11 = tmp_10 < sum
        symbolic_assign_literal('tmp_12', 15)
        tmp_12 = 15
        symbolic_assign_binop('tmp_13', '<', 'sum', 'tmp_12')
        tmp_13 = sum < tmp_12
        symbolic_assign_binop('tmp_14', 'and', 'tmp_11', 'tmp_13')
        tmp_14 = tmp_11 and tmp_13
        symbolic_assign_binop('tmp_15', '<', 'a', 'b')
        tmp_15 = a < b
        symbolic_assign_binop('tmp_16', 'and', 'tmp_14', 'tmp_15')
        tmp_16 = tmp_14 and tmp_15
        symbolic_assign_binop('tmp_17', '<', 'b', 'c')
        tmp_17 = b < c
        symbolic_assign_binop('tmp_18', 'and', 'tmp_16', 'tmp_17')
        tmp_18 = tmp_16 and tmp_17
        if tmp_18:
            symbolic_assert('tmp_18')
            symbolic_assign_binop('tmp_19', '-', 'c', 'b')
            tmp_19 = c - b
            symbolic_assign_binop('tmp_20', '-', 'tmp_19', 'a')
            tmp_20 = tmp_19 - a
            symbolic_assign_literal('tmp_21', 2)
            tmp_21 = 2
            symbolic_assign_binop('tmp_22', '+', 'tmp_20', 'tmp_21')
            tmp_22 = tmp_20 + tmp_21
            symbolic_assign_binop('res', '/', 'sum', 'tmp_22')
            res = sum / tmp_22
        else:
            symbolic_refute('tmp_18')
    else:
        symbolic_refute('tmp_8')