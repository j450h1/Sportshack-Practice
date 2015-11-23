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
df$IssuedDateD <- as.Date(df$IssuedDate,format="%Y-%m-%d %H:%M:%S")
df$ExpiredDateD <- as.Date(df$ExpiredDate,format="%Y-%m-%d %H:%M:%S")
df$ExtractDateD <- as.Date(df$ExtractDate,format="%Y-%m-%d ")

# Check which columns have any NAs
unlist(lapply(df, function(x) any(is.na(x))))
# total number of rows
nrow(df)
# of NA values per column
colSums(is.na(df)) 

colnames(df)

#fastest growing cities in terms of total employees
  
#what type of businesses stay in business the longest
  
sort(table(df$City), desc = FALSE)

nrow(df)
795739/nrow(df)
library(dplyr)

#what type of businesses go out of business the most
table(df$Status)

OOB <- df %>%
  filter(Status == 'Gone Out of Business') %>%
  group_by(BusinessName, BusinessType, Status) %>%
  summarize(StartDate = min(IssuedDateD, na.rm=TRUE ), EndDate = max(ExpiredDateD, na.rm=TRUE )) %>%
  mutate(DaysInBusiness = EndDate - StartDate) %>%
  arrange(desc(DaysInBusiness))

figure1 <- OOB %>%
  group_by(BusinessType) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
#Office

#Bar chart

#devtools::install_github("timelyportfolio/rcdimple")
library(rcdimple)
demo(dimple)

my_data <- data.frame(
  variable = c("A", "B", "C", "D"),
  value = c(0.2, 0.2, 0.2, 0.4))

figure1 %>%
  filter(count > 2300) %>%
  dimple( x = "BusinessType", y = "count", type = "bar"  ) %>%
  xAxis( ) %>%
  default_colors( ) %>%
    add_title( html =
                 "<div style='text-align:center;width:100%'>
               <b style = 'font-size:130%;'>
               Out of Business Count <br> By BusinessType
               </b>
               </div>"
    )


head(Employees)  

#Change over time

# what days do most business licenses get issued




