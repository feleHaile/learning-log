# https://www.r-bloggers.com/taskscheduler-r-package-to-schedule-r-scripts-with-the-windows-task-manager/

# install.packages("taskscheduleR")
# install.packages("taskscheduleR", repos = "http://www.datatailor.be/rcube", type = "source")


# http://stackoverflow.com/questions/1474081/how-do-i-install-an-r-package-from-source
# install.packages(path_to_file, repos = NULL, type="source")

library(taskscheduleR)

myscript <- system.file("extdata", "helloworld.R", package = "taskscheduleR")
## run script once within 62 seconds
taskscheduler_create(taskname = "myfancyscript", rscript = myscript, 
                     schedule = "ONCE", starttime = format(Sys.time() + 62, "%H:%M"))

## run script every day at 09:10
taskscheduler_create(taskname = "myfancyscriptdaily", rscript = myscript, 
                     schedule = "DAILY", starttime = "09:10")

## delete the tasks
taskscheduler_delete(taskname = "myfancyscript")
taskscheduler_delete(taskname = "myfancyscriptdaily")

## log file is at the place where the helloworld.R script was located
system.file("extdata", "helloworld.log", package = "taskscheduleR")