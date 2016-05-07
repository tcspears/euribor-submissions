library(XLConnect)
library(dplyr)
library(stringr)
library(magrittr)
library(reshape2)
library(ggplot2)

setwd("~/Dropbox/Taylor/Repositories/euribor-submissions/")

# Read in CSV of URLs
urls <- read.csv("data_urls.csv", stringsAsFactors = FALSE)

# Set data directory
directory <- "~/Dropbox/Taylor/Repositories/euribor-submissions/data/"

# Download data files from URLs to directory
download_euribor_files(urls[,1], directory.name = directory)

# Read directory data into a list
data <- read_euribor_files(directory)

to.remove <- check_bad_sheets(data)

# We see some bad entries, and now need to manually delete these entries.

data[[7]] <- data[[7]][-c(2)]
data[[130]] <- data[[130]][-c(24,25,26)]
  
# Clean Euribor files now, and merge them together by XLS file.  
data.1 <- clean_euribor_files(data)

# Select only current Euribor fixings to make data rbindable.
data.2 <- lapply(data.1, FUN = function(x) select(x, one_of(c("Bank","X1W","X2W","X1M","X2M","X3M","X6M","X9M","X12M","Date"))))

# Rbind all of the modified data into a single data.frame
data.flat <- do.call("rbind", data.2)

# Group the data by date, and then summarise for each date and tenor the standard deviation
# of submissions (for 1M, 3M, 6M)
data.summarised <- data.flat %>% group_by(Date) %>% summarise(X12.avg = mean(as.numeric(X12M), na.rm = TRUE)/100,
                                                              X12.max = quantile(as.numeric(X12M), probs = 0.99, na.rm = TRUE)/100,
                                                              X12.min = quantile(as.numeric(X12M), probs = 0.01, na.rm = TRUE)/100,
                                                              X12.std = sd(as.numeric(X12M), na.rm = TRUE)/100)

# Make the dataset ggplot-able, and drop outliers
data.final <- data.summarised %>% melt(id.vars = "Date") %>% 
                                            mutate(variable.id = substr(variable, 5, 7)) %>% 
                                            mutate(tenor = factor(substr(variable, 2, 3), levels = c("12"), labels = c("12 Month Euribor Rate"))) %>%
                                            select(-variable) %>%
                                            dcast(Date + tenor ~ variable.id) %>%
                                            filter(Date %in% seq.Date(from=as.Date("2005-01-01"), to=as.Date("2011-12-31"), by = 1), min > 0.001, min < 1, max < 0.44, max > 0)

# Plot the submission history
pdf(file="euribor-submissions.pdf")                                            
ggplot(data = data.final, aes(x = Date, y = avg, ymin = min, ymax = max)) + geom_line() + geom_ribbon(alpha = 0.5) + scale_y_continuous(limits = c(0,0.055), labels = percent_format()) + facet_grid(tenor ~ .) + labs(title = "12M Euribor Submissions by Panel Banks", y = "")
dev.off()
