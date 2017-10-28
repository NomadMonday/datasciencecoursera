complete <- function(directory, id = 1:332)
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
	
	nobs <- data.frame()
	for(i in id)
	{
		x <- read.csv(paste(directory, getFileName(i), sep = "\\"))
		t <- table(!is.na(x$sulfate) & !is.na(x$nitrate))
		if("TRUE" %in% names(t))
		{
			nobs <- rbind(nobs, c(i, t[["TRUE"]]))
		}
		else
		{
			nobs <- rbind(nobs, c(i, 0))
		}
	}
	colnames(nobs) <- c("id", "nobs")
	as.data.frame(nobs)
}