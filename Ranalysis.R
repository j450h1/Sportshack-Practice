getwd()
path <- "/Users/jas/Sportshack-Practice/"
setwd(path)
list.files()
#file is available on your local computer after you export from iPython
filename <- "businesslicenses.csv"
df <- read.csv(filename)
object.size(df)
str(df)
summary(df)
View(df)
colnames(df)
df$BusinessName <- as.character(df$BusinessName)
df$BusinessTradeName <- as.character(df$BusinessTradeName)

b <- subset(df, grepl("^b", BusinessName) | grepl("^b", BusinessTradeName))

#Finding other businesses in my office
consulting <- subset(df, grepl("consulting", BusinessName) | grepl("consulting", BusinessTradeName))
View(b)
pender <- subset(df, grepl("PENDER", Street))
cp <- pender[with(pender, House == '1140'),]
cp2015 <- cp[with(cp,grepl("^2015", IssuedDate)),]

bt <- cp[with(cp,grepl("^880", Unit)),]

#Convert dates to date type
colnames(df)
df$IssuedDateD <- strptime(df$IssuedDate, format = "y%-%m-%d")
df$ExpiredDateD <- strptime(df$ExpiredDate, format = "y%-%m-%d")
df$ExtractDateD <- strptime(df$ExtractDate, format = "y%-%m-%d")

# Check which columns have any NAs
unlist(lapply(df, function(x) any(is.na(x))))
# total number of rows
nrow(df)
# of NA values per column
colSums(is.na(df)) 

colnames(df)

library(dplyr)
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%

#fastest growing cities in terms of total employees
  
#what type of businesses stay in business the longest

  
  
  
#what type of businesses go out of business the most
  
# what days do most business licenses get issued




