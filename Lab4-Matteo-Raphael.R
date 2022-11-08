#=======================================================================
# CSN - Lab Assignment 4
# November 2022
#=======================================================================
# Raphaël Vignon
# Matteo Boursault
#=======================================================================
# Note: Please set the working directory to the current folder

#=======================================================================
# Dependencies 
#=======================================================================

require(data.table)
require(xtable)

library(data.table)
library(xtable)

#=======================================================================
# Functions 
#=======================================================================

write_summary <- function(language,file) {
  data = read.table(file, header = FALSE)
  
  colnames(data) <- c("vertices","degree_2nd_moment", "mean_length")
  
  valid_rows <- apply(data, 1, verification)
  
  data = data[valid_rows,]

  cat(
  	language,
  	length(data$vertices),
  	mean(data$vertices),
  	length(data$degree_2nd_moment),
  	mean(data$degree_2nd_moment),
  	"\n"
  )
}

# Read data from the file that list each language files
source = read.table("list_data.txt", 
                    header = TRUE,               # this is to indicate the first line of the file contains the names of the columns instead of the real data
                    as.is = c("language","file") # this is need to have the cells treated as real strings and not as categorial data.
)

# verification function

verification <- function(row){
	n <- row["vertices"]
	k2 <- row["degree_2nd_moment"]
	d <- row["mean_length"]
	condition_k = (k2 >= 4-6/n && k2 <= n-1)
	condition_d = (d >= n/(8*(n-1)) * k2 + 0.5 && d <= n-1)
	return(condition_k && condition_d)
}

for (x in 1:nrow(source)) {
  write_summary(source$language[x], source$file[x])
	
	language = read.table(source$file[x], header = FALSE)
	colnames(language) = c("vertices","degree_2nd_moment", "mean_length")
	language = language[order(language$vertices), ]
	plot(language$vertices, language$mean_length,
			 xlab = "vertices", ylab = "mean dependency length")
}