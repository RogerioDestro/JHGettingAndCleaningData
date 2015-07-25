run_analysis <- function(directory = "C:\\Users\\Rogerio\\Documents\\CursosOnline\\Coursera\\GettingData\\project\\getdata-projectfiles-UCI HAR Dataset"){
  setwd(directory)
  
  ## Reading the Test and Training sets
  Testset <- read.table(".\\test\\X_test.txt")
  Trainset <- read.table(".\\train\\X_train.txt")
  
  ## Reading the activity labels for the training and test sets
  ActTrain <- read.table(".\\train\\y_train.txt")
  ActTest <- read.table(".\\test\\y_test.txt")
  
  ## Reading the subjects for training and test sets
  SubjTrain <- read.table(".\\train\\subject_train.txt")
  SubjTest <- read.table(".\\test\\subject_test.txt")
  
  ## Inserting the activity and subject information into the dataset
  Trainset <- cbind(ActTrain,SubjTrain,Trainset)
  Testset <- cbind(ActTest,SubjTest,Testset)
  
  ##Fusing the datasets into a super dataset
  superset <- rbind(Testset,Trainset)
  
  ##Reading the names of each variable into a vector 
  Names <- read.table(".\\features.txt", stringsAsFactors = F)
  Names <- Names[,2]
  
  ##Renaming the variables of the superset to a more undersandable name (Step 4 completed)
  names(superset) <- c("Activity","Subject",Names)
  
  ## Step 1 - Completed - Releasing memory
  rm(ActTest,ActTrain,SubjTest,SubjTrain,Testset,Trainset,Names)
 
  ## Getting the columns wuth means and standard deviations
  Mean <- superset[grep("mean",names(superset))]
  STD <- superset[grep("std",names(superset))]
  
  ## Redefining the superset to contain only the activities, subjects, means and standard deviations
  superset <- cbind(superset$Activity,superset$Subject,Mean,STD)
  names(superset)[1] <- "Activity"
  names(superset)[2] <- "Subject"
  
  ## Step 2 - Completed - Releasing memory
  rm(Mean, STD)
  
  ## Step 5 depends on ddply function, so loading the plyr package
  library(plyr)
  ## Calculating the column means, by subject and activity, and arranging the dataframe
  superset <- ddply(superset,.(Activity,Subject),colMeans)
  
  ## Labeling the activities in the dataset
  superset$Activity <- factor(superset$Activity, label=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
  
  write.table(superset,".\\tidyDataSet.txt",row.name = F)
  
  
  
  
}