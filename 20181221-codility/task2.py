# you can write to stdout for debugging purposes, e.g.
# print("this is a debug message")

def solution(S):
    # write your code in Python 3.6
    s_array = S.split(' ')
    A = []

    try:
        for v in s_array:        
            if v.isdigit():
                v = int(v)
                A.append(v)
            elif v == 'DUP':
                A.append(A[-1])
            elif v == 'POP':
                A.pop()
            elif v == '+':
                val = sum(A[-2:])
                A.pop()
                A.pop()
                A.append(val)
            elif v == '-':
                val = A[-1] - A[-2]
                A.pop()
                A.pop()
                A.append(val)
        return A[-1]

    except IndexError:
        return -1


# print(solution('13 DUP 4 POP 5 DUP + DUP + -'))
# print(solution('5 6 + -'))
# print(solution('3 DUP - -'))


test_cases = [
    # '102 DUP DUP DUP',
    # '2000 11 -',
    # '141 22 +',
    # '1000 200 POP',
    # '200 2000 DUP + 192 - DUP DUP',
    # '500 100 200 400 POP POP POP',
    '500 100 200 400 POP POP POP POP',
    # '3 DUP - +'
]

for i in test_cases:
    print(solution(i))