
  # 1.Merges the training and the test sets to create one data set.
  data1<-read.table("UCI HAR Dataset/test/subject_test.txt")
  data2<-read.table("UCI HAR Dataset/test/X_test.txt")
  data3<-read.table("UCI HAR Dataset/test/y_test.txt")
  
  train_data1<-read.table("UCI HAR Dataset/train/subject_train.txt")
  train_data2<-read.table("UCI HAR Dataset/train/X_train.txt")
  train_data3<-read.table("UCI HAR Dataset/train/y_train.txt")
  
  train_data <-cbind(train_data1,train_data2,train_data3)
  test_data <-cbind(data1,data2,data3)
  test_data$type="test"
  train_data$type="train"
  
  name2<-read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
  # 4. Appropriately labels the data set with descriptive variable names
  names(test_data)<-c("subject",name2[,2],"activity","type")
  names(train_data)<-c("subject",name2[,2],"activity","type")
  merged<-rbind.data.frame(test_data,train_data)
  
  # 3. Uses descriptive activity names to name the activities in the data set
  merged$activity <-factor(merged$activity, levels = c(1, 2, 3, 4, 5,6), 
                           labels = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS",
                                      "SITTING","STANDING","LAYING"))
  
  sorted_data <- merged[order(merged$subject, merged$activity),] 
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement 
  # along with subject, activity and type.
  mean1<-grep("mean()",names(sorted_data))
  std1<-grep("std()",names(sorted_data))
  meanstd <- c(mean1,std1)
  extracted <-sorted_data[,c(1,563,564,sort(meanstd))]
  # Extracts only the measurements on the mean and standard deviation for each measurement 
  extracted1 <-sorted_data[,sort(meanstd)]
  
  # 5. From the data set in step 4, creates a second, independent tidy data set
  # with the average of each variable for each activity for each subject.
  
  summary_set<-aggregate(sorted_data[,2:562], by = list(subject = sorted_data$subject,
                                                        activity = sorted_data$activity), mean)
