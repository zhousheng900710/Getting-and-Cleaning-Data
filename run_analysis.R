trn_j <- read.table("UCI HAR Dataset/train/subject_train.txt")
tst_j <- read.table("UCI HAR Dataset/test/subject_test.txt")
trn <- read.table("UCI HAR Dataset/train/X_train.txt")
tst <- read.table("UCI HAR Dataset/test/X_test.txt")
trn_b <- read.table("UCI HAR Dataset/train/y_train.txt")
tst_b <- read.table("UCI HAR Dataset/test/y_test.txt")
act_b <- read.table("UCI HAR Dataset/activity_labels.txt")
feat <- read.table("UCI HAR Dataset/features.txt")
trn_names <- names(trn)
if(length(act_b[,2]) > length(unique(act_b[,2]))) 
{
        activities <- as.data.frame(paste(act_b[,1], act_b[,2], sep='_'))
} else { 
        activities <- as.data.frame(act_b[,2])
}
if(length(feat[,2]) > length(unique(feat[,2]))) 
{
        var_names <- as.data.frame(paste(feat[,2], feat[,1], sep='_'))
} else { 
        var_names <- as.data.frame(feat[,2])
}
names(var_names) <- c("Variable" )
outliers <- function(x) {
        has_outliers = 0
        qnt_x <- quantile(x)
        iqr_x <- IQR(x)
        lower <- qnt_x["25%"] - (iqr_x*1.5)
        upper <- qnt_x["75%"] + (iqr_x*1.5)
        if((qnt_x["0%"] < lower) || (qnt_x["100%"] > upper )) has_outliers = 1
        has_outliers
}

outlier_list <- apply(trn, 2, outliers)
trn_tst_j <- rbind(trn_j, tst_j)
names(trn_tst_j) <- c("Subject")
trn_tst_b <-rbind(trn_b, tst_b)
names(trn_tst_b) <- c("Activity")
trn_tst <- rbind(trn, tst)
names(trn_tst) <- var_names$Variable
library("dplyr")
trn_tst_mean <- select(trn_tst, contains("[Mm]ean|str"))
trn_tst_sdev <- select(trn_tst, contains("[Ss][Tt][Dd]|str"))
trn_tst_msr <- cbind(trn_tst_mean, trn_tst_sdev)
msr_names <- names(trn_tst_msr)
trn_tst_all <-cbind(trn_tst_j, trn_tst_b, trn_tst_msr)
getMeans <- function(Subject, Activity) {
        is_relevant <- trn_tst_all[,Subject==Subject & Activity==Activity]
        output <- as.data.frame(apply(trn_tst_all[is_relevant], 2, mean))
        output[1,] <- Subject
        if(Activity==1) output[2,] <- 'WALKING'
        if(Activity==2) output[2,] <- 'WALKING_UPSTAIRS'
        if(Activity==3) output[2,] <- 'WALKING_DOWNSTAIRS'
        if(Activity==4) output[2,] <- 'SITTING'
        if(Activity==5) output[2,] <- 'STANDING'
        if(Activity==6) output[2,] <- 'LAYING'  
        names(output) <- c(paste('Subject:', Subject, '_Activity:', output[2,], sep=''))
        output
}
rs_names <- c('Subject', 'Activity', msr_names)
resultSet <- data.frame(row.names=rs_names)
for(Subject in 1:30) {
        for(Activity in 1:6) {
                resultSet <- cbind(resultSet, getMeans(Subject, Activity))
        }
}
write.csv(resultSet, 'resultSet.csv')
Result <-read.csv("resultSet.csv")
write.table(Result,file="tidy data set.txt",row.names = FALSE)
