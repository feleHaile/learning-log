# function writing

# 3 parts of a function
add <- function (x, y) {
  x+y
}

formals(add)
body(add)
environment(add)

# the last evaluated function will be returned in a function

# functions are objects

# anonymous functions = functions without a name

# if a name isn't defined in the local environment, it will 
# look one level up

# functions should only depend on the arguments

# new environment is created each time 



## data structure

# atomic vectors
# list in lists

# NULL = absence of a vector
# NA = absence of a value in a vector
# missing values are contagious


## for loop

# seq_along
# writing to a for loop

# Create new double vector: output
output <- vector("double", ncol(df))

# Alter the loop
for (i in seq_along(df)) {
  # Change code to store result in output
  print(median(df[[i]]))
  output[[i]] <- median(df[[i]])
}

# print output
print(output)

## when you should you write a function
# once you have to copy and paste twice

## steps on writing a function
# 1 start with a problem
# 2 get a working copy of the snippet
# 3 rewrite to use temporary variables [remove duplication]
# 4 turn it into a function, with the function format
# 5 test function
# 6 test function on complicated matters

# a good function is:
# correct, understandable (to computers + humans) = 
# obviously correct

# naming convention
# pick consistent style for long names
# don't override existing functions
# function names generally are verbs
# argument order 

# return statements


