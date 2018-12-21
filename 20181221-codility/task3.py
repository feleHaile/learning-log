# you can write to stdout for debugging purposes, e.g.
# print("this is a debug message")

def solution(A):
    # write your code in Python 3.6

    li_li = []

    def tabulate(v1, v2, i, j):
        return [(i, j), abs(i-j)]
    
    # check if adjacent values
    for i, v1 in enumerate(A):
        li = [tabulate(v1, v2, i, j) for j, v2 in enumerate(A) if abs(v2-v1) == 1]
        if len(li) > 0:
            li_li += li

    if len(li_li) > 0:
        d = dict(li_li)
        return min(d.values())
    else:
        return -1

print(solution([1, 4, 7, 3, 3, 5]))

test_cases = [
    [100, 1, 3, 5, 2, 1],
    [1, 1, 1, 1, 1, 1],
    [1, 2, 1, 1, 1, 1],
    [1, 1, 2, 1, 1, 2],
    [2, 5, 5, 1, 5, 5],
]

for i in test_cases:
    print(solution(i))

