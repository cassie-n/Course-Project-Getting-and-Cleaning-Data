library(dplyr)
library(reshape2)

testx <- read.table("X_test.txt")
subjecttest <- read.table("subject_test.txt")
testy <- read.table("y_test.txt")
subjecttrain <- read.table("subject_train.txt")
trainx <- read.table("X_train.txt")
trainy <- read.table("Y_train.txt")
features <- read.table("features.txt")
## First we add together the training and test sets for x data (measurements) y data (activity) and subject data (each individual person)

## At this point I'm not ready to replace the column names with variable names since there are variables with duplicate names.  I'm going to add them as the first row until the duplicates are gone.
datax <- rbind(as.character(features$V2), trainx, testx)
## To avoid duplicate columns we add a first row containing the column name.  Then we change V1 so it doesn't get confused with the V1 in datax.
datay <- rbind(c("activity"), trainy, testy)
datay <- rename(datay, activity = V1)
## Similarly with the subject data
datasubject <- rbind(c("subject"), subjecttrain, subjecttest)
datasubject <- rename(datasubject, subject = V1)

##  Put all this data together, now this is a table with subject, activity, and motion measurements.
dataset <- cbind(datasubject, datay, datax)

## This filters features with "mean" or "std" written somewhere in variable name.  It creates a table with two columns:  The first is the index found in "features", the second is the variable name.
meanandstd <- filter(features, (V1 %in% grep("mean", V2))| (V1 %in% grep("std", V2)))
## Since subject and activity have been added to the motion measurements in the dataset, we need to shift the indexes over by 2 before matching up.
meanandstd <- mutate(meanandstd, V1 = V1 +2)
## keep is a vector of indices we will keep for our dataset.
keep <- union(c(1,2),meanandstd$V1)
## The dataset with mean, std variables only.
dataset <- select(dataset, keep)

## This changes the dataset so that the activities are descriptive instead of numbered (example, "walking" instead of the number 1.
dataset <-
dataset %>% mutate(activity = replace(activity, activity == 1, "Walking")) %>% mutate(activity = replace(activity, activity == 2, "Walking Upstairs")) %>% mutate(activity = replace(activity, activity == 3, "Walking Downstairs")) %>% mutate(activity = replace(activity, activity == 4, "Sitting")) %>% mutate(activity = replace(activity, activity == 5, "Standing")) %>% mutate(activity = replace(activity, activity == 6, "Laying"))
## Remember when we added that row of column names.  Now there are no duplicates.  We can replace the column names with the elements in that first row.
names(dataset) <- dataset[1,]
## Now we can remove the first row.
dataset <- dataset[2:10300,]
## We have arrived at the tidy data set required at the end of step 4.  This dataset has a column of subjects, a column of activities (all written out descriptively), and 79 columns of motion measurements which are all means or standard deviations.  Their column names are descriptive as they are taken from the "features.txt" table.

## Now it is time to create our second independent tidy data set.
## datamelt let's us identify the subject and activity as the values we're looking for, and the measurements as our variables.
datamelt <- melt(dataset, id = c("subject", "activity"), measure.vars = 3:81)
## This changes the measurements into numeric values
datamelt <- mutate(datamelt, value = as.numeric(value, options(digits = 8)))
## Next we create a new column by combining columns 1 and 2 (subject and activity)
sub_join_act <- transmute(datamelt, subject_activity = paste(subject,activity),variable = variable, value = value)
## Currently sub_join_act has three columns, subject_activity, a column of variables, and a column of values
## We need to take the mean of all values where subject_activity and variable are equal
## This will be our tidy data set
tidydata <- dcast(sub_join_act, subject_activity ~ variable, mean)
write.table(tidydata, "projecttidydata.txt", sep = " ", row.names = FALSE)


