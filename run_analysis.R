#!/usr/bin/env Rscript

# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

SAMSUNG_DATASET_DIR <- "UCI_HAR_Dataset"

load_mean_std_features <- function(features_file_name) {
    # Load names of measurements (features) from file with
    # its indexes and names which passed via argument features_file_name.
    # Then search for "mean" and "std" measurements and return its
    # indexes and names as a list.
    
    features_names <- read.table(file = features_file_name, 
                                 header = FALSE,
                                 sep = " ",
                                 colClasses = c("integer", "character"),
                                 comment.char = "")
    
    mean_std_inds <- grep("(mean|std)", features_names[, 2], perl=TRUE)
    list(indexes = features_names[mean_std_inds, 1],
         names = features_names[mean_std_inds, 2])
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
    subject_data <- readLines(subject_file_name)
    cbind(X_data, activity=y_data, subject=subject_data)
}

features_list_file <- file.path(SAMSUNG_DATASET_DIR, "features.txt")
mean_std_features <- load_mean_std_features(features_list_file)

activity_labels_file <- file.path(SAMSUNG_DATASET_DIR, "activity_labels.txt")
activities <- load_activity_labels(activity_labels_file)

load_item_data <- function(dataset_dir, item, ...) {
    x_file = file.path(dataset_dir, item, paste("X_", item, ".txt", sep=""))
    y_file = file.path(dataset_dir, item, paste("y_", item, ".txt", sep=""))
    subj_file = file.path(dataset_dir, item, paste("subject_", item, ".txt", sep=""))
    load_samsung_data(x_file, y_file, subj_file, ...)
}

train_data <- load_item_data(SAMSUNG_DATASET_DIR, "train", mean_std_ftrs, activities)
test_data <- load_item_data(SAMSUNG_DATASET_DIR, "test", mean_std_ftrs, activities)
samsung_data <- rbind(test_data, train_data)

result_data <- aggregate(formula= . ~ activity + subject, data=samsung_data, FUN=mean)


