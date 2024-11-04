# getwd()
# setwd("D:/R-projo")
df <- read.csv("Student Mental health.csv")
# remove Timestamp column
df$Timestamp <- NULL
View(df)
dim(df)
str(df)
summary(df)
attach(df)
hist(Age)
class(Your.current.year.of.Study)
unique(What.is.your.course.)
df <- df[, 2:length(df)]
View(sort(table(What.is.your.course.), decreasing = TRUE))
barplot(sort(table(What.is.your.course.), decreasing = TRUE))
View(df[is.na(Age),])
# Install necessary packages
# install.packages("tidyverse")
library("tidyverse")
# Handle NA values in Age column
# Get the median of Age
ageMid = median(Age, na.rm=TRUE)
ageMid
df <- df %>% mutate(Age = replace_na(Age, ageMid))
# Check for duplicates
df[duplicated(df),]
# rename columns
df <- df %>% 
  select(everything()) %>% 
  rename(gender = Choose.your.gender, course=What.is.your.course., age = Age,
         year.of.study = Your.current.year.of.Study, CGPA = What.is.your.CGPA.)
View(df)
df %>% 
  group_by(course) %>% 
  summarise(Youngest = min(age),
            Average = round(mean(age), 1),
            Oldest = max(age),
            Number = n()) %>% 
  arrange(Average) %>% 
  View()
names(df)
library('dplyr')
# clean year of study column
df$year.of.study<- sapply(df$year.of.study, function(row) {
  # Convert to lowercase first
  lowercs <- tolower(row)
  # Extract numerical value
  numerical_val <- strsplit(lowercs," ")[[1]][2]
  
  return(numerical_val)
})
View(df)
