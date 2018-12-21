# you can write to stdout for debugging purposes, e.g.
# print("this is a debug message")

def solution(A):
    # write your code in Python 3.6

    counter = 0

    # loop through list
    for i, v in enumerate(A):

        # remove char
        # print(f'Removing {v} at {i} position.')
        new_A = A[:i] + A[i+1:]

        # check if new_A is ascending
        if new_A == sorted(new_A):
            counter += 1
    
    return counter


# Test cases:

test_cases = [
    [1, 10, 100, 150, 190],
    [190, 170, 140, 100, 40],
    [200, 1, 2, 5],
    [1, 2, 105, 99],
    [2, 99, 99, 99],
    [99, 99, 99, 2],
    list(range(0, 100)),
]

for i in test_cases:
    print(solution(i))

# print(solution([1, 2, 3, 4]))
# print(solution([1, 2, 3, 3, 5, 6, 7]))
# print(solution([4, 5, 2, 3, 4]))