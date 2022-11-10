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
  	sd(data$vertices),
  	mean(data$degree_2nd_moment),
  	sd(data$degree_2nd_moment),
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

# plot 2 and 3 part

viz <- function(language_data) {
	mean_language_data = aggregate(
		language_data, 
		list(language_data$vertices), 
		mean
	)
	
	# VIZ - Part 3
	
	plot(language_data$vertices, 
			 language_data$mean_length,
			 xlab = "vertices", 
			 ylab = "mean dependency length", 
			 main=source$language[x])
	
	plot(log(language_data$vertices), 
			 log(language_data$mean_length),
			 xlab = "log(vertices)", 
			 ylab = "log(mean dependency length)", 
			 main=source$language[x])
	
	plot(mean_language_data$vertices, 
			 mean_language_data$mean_length,
			 xlab = "vertices", 
			 ylab = "mean mean dependency length",
			 main=source$language[x])
	
	plot(log(mean_language_data$vertices), 
			 log(mean_language_data$mean_length),
			 xlab = "log(vertices)", 
			 ylab = "log(mean mean dependency length)",
			 main=source$language[x])
	
	plot(log(language_data$vertices),
			 log(language_data$mean_length),
			 xlab = "vertices",
			 ylab = "mean dependency length",
			 main=source$language[x])
	
	lines(log(mean_language_data$vertices),log(mean_language_data$mean_length), col = "green")
	lines(log(mean_language_data$vertices),log((mean_language_data$vertices+1)/3), col = "red")
	
	plot(language_data$vertices,
			 language_data$degree_2nd_moment,
			 xlab = "vertices",
			 ylab = "degree 2nd moment",
			 main=source$language[x])
	
	lines(mean_language_data$vertices,mean_language_data$degree_2nd_moment, col = "green")
	lines(mean_language_data$vertices,
				(1 - 1/mean_language_data$vertices)*(5 - 6/mean_language_data$vertices), col = "red")
	lines(mean_language_data$vertices,4-6/mean_language_data$vertices, col = "blue")
	lines(mean_language_data$vertices,mean_language_data$vertices-1, col = "blue")
}

# model part 4-5

model <- function(data_language) {
	
	print("Model1")
	lm_1 = lm(log(data_language$degree_2nd_moment)~log(data_language$vertices), data_language)
	b_initial_1 = coef(lm_1)[2]
	nlm_1 = nls(data_language$degree_2nd_moment~(data_language$vertices*0.5)^b,
							data=data_language,
							start = list(b = b_initial_1), 
							trace = TRUE)
	
	print("Model2")
	lm_2 = lm(log(data_language$degree_2nd_moment)~log(data_language$vertices), data_language)
	a_initial_2 = exp(coef(lm_2)[1])
	b_initial_2 = coef(lm_2)[2]
	nlm_2 = nls(data_language$degree_2nd_moment~a*language_data$vertices^b,
							data=data_language,
							start = list(a = a_initial_2, b = b_initial_2), 
							trace = TRUE)
	
	print("Model3")
	lm_3 = lm(log(data_language$degree_2nd_moment)~data_language$vertices, data_language)
	a_initial_3 = exp(coef(lm_3)[1])
	c_initial_3 = coef(lm_3)[2]
	nlm_3 = nls(data_language$degree_2nd_moment~a*exp(c*language_data$vertices),
							data=data_language,
							start = list(a = a_initial_3, c = c_initial_3), 
							trace = TRUE)
	
	
	
}

for (x in 1:nrow(source)) {
	language_data = read.table(source$file[x], header = FALSE)
	colnames(language_data) = c("vertices","degree_2nd_moment", "mean_length")
	language_data = language_data[order(language_data$vertices), ]
	
	valid_rows <- apply(language_data, 1, verification)
	
	language_data = language_data[valid_rows,]
	
	write_summary(source$language[x], language_data)
	
	viz(language_data)
	
	# Non-linear regression - Part 4
	
	model(language_data)
}