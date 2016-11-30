# String manipulation -----------------------------------------------------

# Tutorial from: http://digitheadslabnotebook.blogspot.sg/2011/08/string-functions-in-r.html and
# http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf 

# Regular expressions: 
# http://stat545.com/block022_regular-expression.html

# grep --------------------------------------------------------------------


library(random)
string <- c(randomStrings(n=1000, len=5))

# Special characters
# requires backslash: ', ", n (new line), \r (return), \t (tab)

# print() v. cat()
print("hello\nworld!")
cat("hello\nworld!")

# Quantifiers
# match F and U with U trailing X number of times
# in order of F, U
# requires characters: * (zero), + (at least once), ? (at most once), {n} (n times), {n, } (at least n times), {n,m} (between n and m times)
grep("FU*", string, value=TRUE)
grep("FU+", string, value=TRUE)
grep("FU?", string, value=TRUE)
grep("FU{1,2}", string, value=TRUE)

# Positioning
# with "EJ"
grep("ej", string, value=TRUE)    
grep("^A", string, value=TRUE)   # start of string
grep("a$", string, value=TRUE)   # end of string
grep("\\bB", string, value=TRUE) # not in edge of word in string
grep("\\b", string, value=TRUE)  # at the edge of a word in string

# Operators

grep("\d{3}", string, value=TRUE)    # three numeric digits
grep("[a-z]{3}", string, value=TRUE) # three alphabets, lower case
grep("[A-Z]{4}", string, value=TRUE) # three alphabetes, upper case
grep("\S", string, value=TRUE)       # not a space
grep("[!]", string, value= TRUE)    # punctuation characters


# substring ---------------------------------------------------------------

substr("abcdef", 2, 4)
substring("abcdef", 1:6, 1:6)
## strsplit is more efficient ...

substr(rep("abcdef", 8), 
       1:4, 4:5)
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
substr(x, 2, 5)
substring(x, 2, 4:6)

substring(x, 2) <- c("..", "+++")
x

## Numeric manipulation

roundUp <- function(x) 10^ceiling(log10(x))