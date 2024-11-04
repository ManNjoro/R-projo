# getwd()
# setwd("D:/R-projo")
df <- read.csv("Student Mental health.csv")
df
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
         "year of study" = Your.current.year.of.Study, CGPA = What.is.your.CGPA.)
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
