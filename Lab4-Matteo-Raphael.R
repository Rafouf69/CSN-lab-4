#=======================================================================
# CSN - Lab Assignment 4
# November 2022
#=======================================================================
# Raphaël Vignon
# Matteo Boursault
#=======================================================================
# Note: Please set the working directory to the current folder

write_summary <- function(language,file) {
  degree_sequence = read.table(file, header = FALSE)
  cat(
  	language,
  	length(degree_sequence$V1),
  	mean(degree_sequence$V1),
  	length(degree_sequence$V2),
  	mean(degree_sequence$V2),
  	"\n"
  )
}

# Read data from the file that list each language files
source = read.table("list_data.txt", 
                    header = TRUE,               # this is to indicate the first line of the file contains the names of the columns instead of the real data
                    as.is = c("language","file") # this is need to have the cells treated as real strings and not as categorial data.
)

# verification fucntion

k_verification <- function(n, k_2){
  if(k_2 >= d - 6/n & k_2 <= n-1){
    TRUE
  } else {
    FALSE
  }
}

for (x in 1:nrow(source)) {
  write_summary(source$language[x], source$file[x])
}