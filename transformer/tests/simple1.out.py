symbolic_assign_literal('tmp_1', 42)
tmp_1 = 42
if tmp_1:
    symbolic_assert('tmp_1')
    pass
else:
    symbolic_refute('tmp_1')