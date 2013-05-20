def foo(a, b, c):
    if a*a + b*b == c*c and a > 0:
        sum = a+b+c
        if 0 < sum and sum < 15 and a < b and b < c:
            res = sum / (c - b - a + 2)
