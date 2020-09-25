library(tidyverse)
library(stringr)
library(rstanarm)
library(lubridate)
library(gridExtra)

## Make URLs

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

## Specify the range of years and what months we want
years <- c(2000:2018)
months <- c(01:12)

## We have avoided the issue of two digit years by starting with 2000
urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("mr", years, sep = "")
month_files <- str_c("Month", months, sep = "")
plot_files <- str_c("Plot", months, sep = "")

## Read the data from the website
N <- length(urls)
M <- 12

## This for loop is executed 12 times, one for each month of every year.
for (j in 1:M){

  ## This for loop is for creating files called Month1, Month2, ..., Month12 and placing them in a data frame.
  for (i in 1:N){
    # Suppresses warnings
    suppressMessages(assign(filenames[i], read_table(urls[i], col_names = TRUE, na = "")))
    
    file <- get(filenames[i])
   
    
    # This is necessary because some years label their year as "YY" (i.e. 2007 is 07)
    colnames(file)[1] <-"YYYY"
    
    # For each year, we grab only the YYYY, MM, and ATMP columns and only keep the data for the month of August.
    # This also removes large NA values in ATMP due to lack of funding.
    file %<>%
      select(YYYY, MM, ATMP) %>% 
      transform(YYYY=as.numeric(YYYY), MM=as.numeric(MM), ATMP=as.numeric(ATMP)) %>% 
      filter(MM==j, ATMP<70)
      
    if(i == 1){
      MR <- file
    }
    else{
      MR <- rbind.data.frame(MR, file)
    }
  }

  month <- MR %>%
  group_by(YYYY) %>% 
  summarize(mean(ATMP))
  colnames(month)[2]<-"AvgTMP"
  assign(month_files[j], month)
}

## Plotting ...
for (k in 1:12){
  file <- get(month_files[k])
  fit <- stan_glm(AvgTMP ~ YYYY, data = file, refresh=0)
  print(month_files[k])
  print(max(file$AvgTMP)-min(file$AvgTMP))
  assign(plot_files[k], ggplot(file, aes(YYYY, AvgTMP)) + 
           geom_point() + 
           geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "blue") +
           labs(x = "Year", y = "Average Temp", title = month.abb[k]) + 
           xlim(2000, 2018) + 
           ylim(min(file$AvgTMP), min(file$AvgTMP) + 8))
}
grid.arrange(Plot1, Plot2, Plot3, Plot4, Plot5, Plot6, Plot7, Plot8, Plot9, Plot10, Plot11, Plot12, nrow=4, ncol=3, newpage = TRUE)


### More helpful information
# In April and August 2018, there was no recorded data due to lack of funding.



# Using Lubridate to rename year column

data_1997 <- read.table("https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h1997.txt.gz&dir=data/historical/stdmet/", header=T)
data_1997 <- as_tibble(data_1997)

# Use mutate to create a new column with a 4 digit year
data_1997 <- mutate(data_1997, YYYY = data_1997$YY + 1900)

# Use mutate and unite to create a new column with a 4 digit year
data_1997a <- mutate(data_1997, CC = 19)
data_1997a <- data_1997a %>% unite("YEAR", CC,YY, sep = "")
