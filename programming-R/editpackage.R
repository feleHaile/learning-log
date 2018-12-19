people <- read.csv("C:\\Users\\JYESOH\\Desktop\\GIT\\useful.scripts\\R\\datasets\\people.txt")
library(editrules)

E <- editset( c("age >=0", "age <= 150") )
which(violatedEdits(E, people))

class(violatedEdits(E, people))
which(as.data.frame(violatedEdits(E, people))$num2 == T)

# identify rules
E1 <- editfile("C:\\Users\\JYESOH\\Desktop\\GIT\\useful.scripts\\R\\datasets\\edits.txt") # without comments
E2 <- editfile("C:\\Users\\JYESOH\\Desktop\\GIT\\useful.scripts\\R\\datasets\\edits2.txt") # with comments

ve <- violatedEdits(E2, people)
summary(ve)
class(ve)
as.data.frame(ve)

plot(ve)

plot(E2)
E2

# localize errors
id <- c(2,5)
people[id, ]
violatedEdits(E1, people[id,])

le <- localizeErrors(E2, people[id,], method = 'mip') # mixed integer programming
le$adapt


library(deducorrect)
