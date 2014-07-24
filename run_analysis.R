#!/usr/bin/env Rscript


SAMSUNG_DATASET_DIR <- "UCI HAR Dataset"

TIDY_DATASET_FILE_NAME <- "samsung_human_activities.txt"


load_mean_std_features <- function(features_file_name) {    
    features_names <- read.table(file = features_file_name, 
                                 header = FALSE,
                                 sep = " ",
                                 colClasses = c("integer", "character"),
                                 comment.char = "")
    
    mean_std_inds <- grep("(mean|std)", features_names[, 2], perl=TRUE)
    list(indexes = features_names[mean_std_inds, 1],
         names = sub("()", "", features_names[mean_std_inds, 2], fixed=TRUE))
}


load_activity_labels <- function(act_labels_file) {
    activities <- character()
    full_lines <- readLines(act_labels_file)
    for (x in strsplit(full_lines, " ")) {
        activities[as.integer(x[1])] <- x[2]
    }
    return(activities)
}


load_samsung_data <-
function(X_file_name, y_file_name, subject_file_name, mean_std_ftrs, activities) {
    raw_X_data <- read.table(file = X_file_name,
                         header = FALSE,
                         colClasses = "numeric",
                         comment.char = "")
    X_data <- raw_X_data[, mean_std_ftrs[["indexes"]]]
    names(X_data) <- mean_std_ftrs[["names"]]

    raw_y_data <- as.integer(readLines(y_file_name))
    y_data <- activities[raw_y_data]
    subject_data <- as.integer(readLines(subject_file_name))
    cbind(X_data, activity=y_data, subject=subject_data)
}


load_item_data <- function(dataset_dir, item, ...) {
    x_file = file.path(dataset_dir, item, paste("X_", item, ".txt", sep=""))
    y_file = file.path(dataset_dir, item, paste("y_", item, ".txt", sep=""))
    subj_file = file.path(dataset_dir, item, paste("subject_", item, ".txt", sep=""))
    load_samsung_data(x_file, y_file, subj_file, ...)
}


features_list_file <- file.path(SAMSUNG_DATASET_DIR, "features.txt")
mean_std_features <- load_mean_std_features(features_list_file)

activity_labels_file <- file.path(SAMSUNG_DATASET_DIR, "activity_labels.txt")
activities <- load_activity_labels(activity_labels_file)

train_data <- load_item_data(SAMSUNG_DATASET_DIR, "train", mean_std_features, activities)
test_data <- load_item_data(SAMSUNG_DATASET_DIR, "test", mean_std_features, activities)
samsung_data <- rbind(test_data, train_data)

result_data <- aggregate(formula= . ~ subject + activity, data=samsung_data, FUN=mean)
result_data <- result_data[order(result_data$subject, result_data$activity),]
rm(train_data, test_data, samsung_data)

write.table(result_data, TIDY_DATASET_FILE_NAME, sep="\t", quote = FALSE, row.names = FALSE)

