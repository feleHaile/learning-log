# DT[i,j,by]
# 1 = double/numeric; 1L integer; NA = logical; NA_integer_ = integer
# columns can be called by .(); possible to use also functions, resulting in data.table. without .() results in vector if single column
# DT[,plot(A,C)]]
# DT[, {print (A) hist(C) NULL }]


# by=.(A) *group by
# by.(Grp = A%2) *group by whether it is odd/even
# if not bothered about column names, can be shortened to 
# DT[,sum(B), by=A%2] from DT[, .(MySum = sum(B)), by = .(Grp = A % 2)]

# count arg .N
# DT[, .(Count = .N), by = .(Area = 10 * round(Sepal.Length* Sepal.Width, 10))]
# only use .() when a function is used

# chaining

# subsetting
# DT[, lapply(.SD, median), by = Species]
# .SD calls the table by names, except the one specified in by
# .() is an alias to list() and lapply()


# DT[,lapply(.SD, sum), .SDcols = 2:4]
# .SDcols to select the columns

# DT[, .SD[-1], .SDcols = paste0("Q", 1:3), by = grp]
# removing each group's first row

# Sum of all columns and the number of rows
# Get the sum of all columns x, y and z and the number of rows in each group while grouping by x. Your answer should be identical to this:
#   x x  y  z N
# 1: 2 8 26 30 4
# 2: 1 3 23 26 3
# DT[, c(lapply(.SD, sum), .N), by = x, .SDcols = c("x", "y", "z")]

# Get the cumulative sum of column x and y while grouping by x and z > 8 such that the answer looks like this:
#   by1   by2 x  y
# 1:   2 FALSE 2  1
# 2:   2 FALSE 4  6
# 3:   1 FALSE 1  3
# 4:   1 FALSE 2 10
# 5:   2  TRUE 2  9
# 6:   2  TRUE 4 20
# 7:   1  TRUE 1 13
# Cumulative sum of column x and y while grouping by x and z > 8
# DT[, lapply(.SD, cumsum), by = .(by1 = x, by2 = z > 8), .SDcols = c("x", "y")]