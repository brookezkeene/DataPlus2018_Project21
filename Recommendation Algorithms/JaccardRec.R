# Model of Jaccard Similarity Index Calculations
library(tidyverse)
library(googlesheets)

# Jaccard Function
jaccard <- function(x, y) {
  inter_cardinality <- length(intersect(x, y))
  union_cardinality <- length(union(x, y))
  return(inter_cardinality/union_cardinality)
}

# Load Data from GoogleSheets (in csv format)
gs_progdf <- gs_title("DukeGroups_Edited")
progdf <- data.frame(gs_read_csv(gs_progdf, col_names = TRUE))

program_names = progdf[c(1)]              # Column of program names
program_names <- program_names[-1,]
progdf <- progdf[-1,-1]                   # Remove column of program names
prog_num <- nrow(progdf)                  # Number of total programs
tag_num <- ncol(progdf)                   # Number of total tags

# Change 1s to a New Number, Unique to Tag
for(r in 1:prog_num) {
  for(c in 1:tag_num) {
    if(progdf[r,c] == 1) {
      progdf[r,c] <- c                    # Sets column number as unique tag number
    }
  }
}

# Create New List of Vectors Containing Programs' Unique Tag Numbers
prog_vecs <- list()
for(r in 1:prog_num) {
  temp_vec <- vector(mode = 'numeric', length = 0)
  for(c in 1:tag_num) {
    if(progdf[r,c] != 0) {
      temp_vec <- append(temp_vec, progdf[r,c])
    }
  }
  prog_vecs[[r]] <- temp_vec              # Adds vector to list
}

# Create New Data Frame Containing Jaccard Similarity for Any 2 Programs
prog_sim <- as.data.frame(matrix(NA, nrow = nrow(progdf), ncol = nrow(progdf)))
rownames(prog_sim) <- program_names
colnames(prog_sim) <- program_names
for(r in 1:prog_num) {
  for(c in 1:prog_num) {
    prog_sim[r,c] <- jaccard(prog_vecs[[r]], prog_vecs[[c]])
  }
}

prog_index <- 3
prog_sim <- as.data.frame(prog_sim[,prog_index])

# Organize Final Similarities
final_sim <- as.data.frame(matrix(NA, nrow = nrow(prog_sim), ncol = 2, dimnames = list(program_names,c('Score','Description'))))

for(i in 1:nrow(prog_sim)) {
  final_sim[i,1] <- prog_sim[i,1]
}

final_sim <- final_sim[order(final_sim[,1], decreasing = TRUE),]
final_sim <- final_sim[-1,]
final_sim <- head(final_sim, n = 10) # display only top 10 programs

final_rows <- rownames(final_sim)

# Get Descriptions
gs_prog <- gs_title("Co-Curriculars")
prog_list <- gs_read_csv(gs_prog, col_names = TRUE)

for(i in 1:nrow(final_sim)) {
  prog_name <- rownames(final_sim)[i]
  index <- which(prog_list$CoCurriculars %in% prog_name)
  final_sim[i,2] <- prog_list[index,3]
}
