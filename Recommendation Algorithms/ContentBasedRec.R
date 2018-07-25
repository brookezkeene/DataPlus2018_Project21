# Model of a Content-Based Recommendation Algorithm
library(tidyverse)
library(tm)
library(googlesheets)

# Load Data from Excel (in csv format)
gs_tags <- gs_title("DukeGroups_Edited")
programs_df <- data.frame(gs_read_csv(gs_tags, col_names = TRUE))

program_names = programs_df[c(1)]              # Column of Program Names
program_names <- program_names[-1,]
programs_df <- programs_df[-1,-1]              # Remove column of program names
#### FOR TESTING REMOVE ROWS
programs_df <- programs_df[-c(144:158),]
program_names <- program_names[-c(144:158)]

# Determine DF and IDF vectors
DF = colSums(programs_df)                      # Document Frequency 
total_tags = rowSums(programs_df)              # Total number of tags for a program
N = NROW(program_names)                        # Total number of documents
IDF = log10(N/DF)                              # Inverse Document Frequency

# Create a data frame of a student and their participation
students_df = data.frame(Student1 = rbinom(N, 1, 0.01))
# To randomly generate student activities us either of the 2 below:
# Create empty matrix: matrix(0,N,1))
# Student1 = sample(c(0,1), size = N, replace = TRUE)) OR rbinom(N, 1, 0.5))
row.names(students_df) <- program_names

# Normalize Data
norm_prog = sweep(programs_df, 1, sqrt(total_tags), "/")

# Create matrix of student profiles
stud_profiles = matrix(NA, nrow = ncol(students_df), ncol = ncol(norm_prog))
for(i in 1:ncol(students_df)) {
  stud_profiles[i,] = apply(norm_prog, 2, function(x){t(x)%*%students_df[,i]})
}

# Create matrix of weighted scores of tags for each program
weighted_scores = t(apply(norm_prog, 1, function(x){x*IDF}))

# Create matrix of student predictions based on weighted scores
stud_predictions = matrix(NA, nrow = nrow(programs_df), ncol = ncol(students_df))
row.names(stud_predictions) <- program_names
for(c in 1:ncol(students_df)) {
  for(r in 1:nrow(programs_df)) {
    stud_predictions[r,c] = sum(stud_profiles[c,]*weighted_scores[r,])
  }
}
stud_predictions

# Factor in Programs that Students Have Already Participated in
for(i in 1:N) {
  if(students_df[i,] == 1) {
    stud_predictions[i] = -1 # change to -1
  }
}


# Analyze Data
#hist(stud_predictions)
#qqnorm(stud_predictions)
#qqline(stud_predictions)