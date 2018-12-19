data <- read.table(header=TRUE, text='
 subject sex condition before after change
                   1   F   placebo   10.1   6.9   -3.2
                   2   F   placebo    6.3   4.2   -2.1
                   3   M   aspirin   12.4   6.3   -6.1
                   4   F   placebo    8.1   6.1   -2.0
                   5   M   aspirin   15.2   9.9   -5.3
                   6   F   aspirin   10.9   7.0   -3.9
                   7   F   aspirin   11.6   8.5   -3.1
                   8   M   aspirin    9.5   3.0   -6.5
                   9   F   placebo   11.5   9.0   -2.5
                   10   M   placebo   11.9  11.0   -0.9
                   11   F   aspirin   11.4   8.0   -3.4
                   12   M   aspirin   10.0   4.4   -5.6
                   13   M   aspirin   12.5   5.4   -7.1
                   14   M   placebo   10.6  10.6    0.0
                   15   M   aspirin    9.1   4.3   -4.8
                   16   F   placebo   12.1  10.2   -1.9
                   17   F   placebo   11.0   8.8   -2.2
                   18   F   placebo   11.9  10.2   -1.7
                   19   M   aspirin    9.1   3.6   -5.5
                   20   M   placebo   13.5  12.4   -1.1
                   21   M   aspirin   12.0   7.5   -4.5
                   22   F   placebo    9.1   7.6   -1.5
                   23   M   placebo    9.9   8.0   -1.9
                   24   F   placebo    7.6   5.2   -2.4
                   25   F   placebo   11.8   9.7   -2.1
                   26   F   placebo   11.8  10.7   -1.1
                   27   F   aspirin   10.1   7.9   -2.2
                   28   M   aspirin   11.6   8.3   -3.3
                   29   F   aspirin   11.3   6.8   -4.5
                   30   F   placebo   10.3   8.3   -2.0
                   ')


library(plyr)
cdata <- ddply(data, c("sex", "condition"), summarise,
               N    = length(change),
               mean = mean(change),
               sd   = sd(change),
               se   = sd / sqrt(N)
)

dataNA <- data
dataNA$change[11:14] <- NA

cdata <- ddply(dataNA, c("sex", "condition"), summarise,
               N    = sum(!is.na(change)),
               mean = mean(change, na.rm=TRUE),
               sd   = sd(change, na.rm=TRUE),
               se   = sd / sqrt(N)
)

cdata



summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
