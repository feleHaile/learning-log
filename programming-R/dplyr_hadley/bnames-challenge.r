library(plyr)
setwd("C:\\Users\\JYESOH\\Desktop\\GIT\\useful.scripts\\R\\datasets")

# adding rank based on sex and year
bnames <- read.csv("bnames.csv", stringsAsFactors = FALSE)
bnames <- ddply(bnames, c("sex", "year"), transform, 
  rank = rank(-percent, ties.method = "first"))
# transform: taking a value, modifying it


# subsetting the dataset
top100 <- subset(bnames, rank <= 100)

# summed values 
top100s <- ddply(top100, c("sex", "year"), summarise,
  tot = sum(percent))
#summarize: taking a value, sumarizing it by a function

head(top100s)  
summary(top100s$year)

library(ggplot2)
ggplot(top100s, aes(x=year, y=tot, colour=sex)) +
  geom_line() + scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(limits=c(1880,2008))
       
ggsave("bname-top100.pdf", width = 8, height = 6)