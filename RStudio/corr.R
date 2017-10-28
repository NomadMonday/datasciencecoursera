corr <- function(directory, threshold = 0)
{
	getFileName <- function(n)
	{
		if(n < 10)
		{
			paste0("00", n, ".csv")
		}
		else if(n < 100)
		{
			paste0("0", n, ".csv")
		}
		else
		{
			paste0(n, ".csv")
		}
	}
	
	cplt <- complete(directory)
	index <- cplt[cplt$nobs > threshold, 1]
	result <- numeric()

	if(length(index) == 0)
	{
		result
	}
	else
	{
		for(i in index)
		{
			x <- read.csv(paste(directory, getFileName(i), sep = "\\"))
			result <- c(result, cor(x$nitrate, x$sulfate, use = "complete.obs"))
		}
		result
	}
}