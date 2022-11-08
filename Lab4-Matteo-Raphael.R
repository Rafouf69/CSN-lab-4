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

write_summary <- function(language, data) {
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
	language_data = read.table(source$file[x], header = FALSE)
	colnames(language_data) = c("vertices","degree_2nd_moment", "mean_length")
	language_data = language_data[order(language_data$vertices), ]
	
	valid_rows <- apply(language_data, 1, verification)
	
	language_data = language_data[valid_rows,]
	
	write_summary(source$language[x], language_data)
	
	plot(language_data$vertices, language_data$mean_length,
			 xlab = "vertices", ylab = "mean dependency length")
}