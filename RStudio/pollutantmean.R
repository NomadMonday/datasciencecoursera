pollutantmean <- function(directory, pollutant, id = 1:332)
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
	
	m <- NA
	for(i in id)
	{
		x <- read.csv(paste(directory, getFileName(i), sep = "\\"))
		m <- c(m, x[[pollutant]])
	}
	mean(m, na.rm = TRUE)
}