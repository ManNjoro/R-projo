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
View(df[is.na(df$age),])
# Install necessary packages
# install.packages("tidyverse")
library("tidyverse")
# Handle NA values in Age column
# Get the median of Age
ageMid = median(Age, na.rm=TRUE)
ageMid
df <- df %>% mutate(age = replace_na(age, ageMid))
# Check for duplicates
df[duplicated(df),]
# rename columns
df <- df %>% 
  select(everything()) %>% 
  rename(gender = Choose.your.gender, course=What.is.your.course., age = Age,
         year.of.study = Your.current.year.of.Study, CGPA = What.is.your.CGPA.)
View(df)
grouped_course <- df %>% 
  group_by(course) %>% 
  summarise(Youngest = min(age),
            Average = round(mean(age), 1),
            Oldest = max(age),
            Count = n()) %>% 
  arrange(-Count)
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
#cleaning course column
df$course <- sapply(df$course, function(row){
  lowercs <- tolower(row)
  trimmed_str <- trimws(lowercs)
  return(trimmed_str)
})
unique(df$course)
# Custom dictionary of mappings
course_mapping <- c(
  "engineering" = "Engineering",
  "engine" = "Engineering",
  "engin" = "Engineering",
  "mathemathics" = "Mathematics",
  "bit" = "Information Technology",
  "it" = "Information Technology",
  "bcs" = "Bachelor of Computer Science",
  "human resources" = "Human Resources",
  "irkhs" = "Islamic Knowledge and Human Sciences",
  "kenms" = "Kulliyyah of Economics and Management Sciences",
  "kirkhs" = "Kulliyyah of Islamic Revealed Knowledge and Human Sciences",
  "benl" = "Bachelor of English Language and Literature",
  "malcom" = "Malaysian Communication",
  "kop" = "Kulliyyah of Pharmacy",
  "usuluddin" = "Islamic Theology",
  "pendidikan islam" = "Islamic Education",
  "business administration" = "Business Administration",
  "law" = "Law",
  "laws" = "Law",
  "taasl" = "Teaching Arabic as a Second Language",
  "cts" = "Computer and Telecommunications Science",
  "biomedical science" = "Biomedical Science",
  "banking studies" = "Banking Studies",
  "econs" = "Economics",
  "human sciences" = "Human Sciences",
  "biotechnology" = "Biotechnology",
  "communication" = "Communication",
  "diploma nursing" = "Nursing",
  "fiqh" = "Islamic Jurisprudence",
  "fiqh fatwa" = "Islamic Jurisprudence and Fatwa",
  "enm" = "Environmental Management",       # or "Energy Management"
  "koe" = "Kulliyyah of Engineering",
  "ala" = "Arabic Language and Literature",
  "mhsc" = "Medical and Health Sciences",
  "diploma tesl" = "Diploma in Teaching English as a Second Language"
)

# Apply the mapping to the `course` column
df$course <- tolower(df$course)  # convert to lowercase
df$course <- recode(df$course, !!!course_mapping)  # apply mapping

# Confirm the unique values after cleaning
unique(df$course)
summary(df$age)
df$Age <- NULL
View(df)
median(c(3.00, 3.50))
unique(df$Do.you.have.Depression.)
df$CGPA
# Define a function to calculate the median of each range
calculate_median <- function(range) {
  # Split the range into two numbers
  nums <- as.numeric(unlist(strsplit(range, " - ")))
  # Calculate the median
  median_value <- round(median(nums), 2)
  return(median_value)
}

# Apply the function to each row in the CGPA column
df$CGPA <- sapply(df$CGPA, calculate_median)

# View the cleaned CGPA column
print(df$CGPA)

View(df)
write.csv(df, "Student Mental health cleaned.csv", row.names = FALSE)
df %>% 
  select(course, Do.you.have.Depression., Do.you.have.Anxiety., Do.you.have.Panic.attack., Did.you.seek.any.specialist.for.a.treatment.) %>% 
  View()
ggplot(data = df,
       mapping= aes(x = gender, color = Do.you.have.Depression., fill = Do.you.have.Depression.))+
  geom_bar()
df %>% 
  ggplot(aes(x = CGPA,
             color = gender,
             fill = gender))+
  geom_density(alpha=1)+
  theme_bw()

#Add a Risk Level column
df <- df %>% 
  mutate(Risk.Level = case_when(Do.you.have.Depression. == "Yes" & Do.you.have.Anxiety. == "Yes" & Do.you.have.Panic.attack. == "Yes" ~ "High",
                                (Do.you.have.Anxiety. == "Yes" & Do.you.have.Depression. == "Yes")| (Do.you.have.Anxiety. == "Yes" & Do.you.have.Panic.attack. == "Yes") | (Do.you.have.Depression. == "Yes"& Do.you.have.Panic.attack. == "Yes") ~ "Medium",
                                .default = "Low"))
ggplot(data = df,
       mapping = aes(x = Risk.Level, color=gender, fill = gender))+
  geom_bar()
write.csv(df, "Student Mental health cleaned.csv", row.names = FALSE)

df %>% 
  select(Risk.Level, Did.you.seek.any.specialist.for.a.treatment.) %>% 
  View()
ggplot(data = df,
       mapping = aes(x = Risk.Level, color=Did.you.seek.any.specialist.for.a.treatment., fill = Did.you.seek.any.specialist.for.a.treatment.))+
  geom_bar()

ggplot(data = df,
       mapping = aes(x=year.of.study, y=age, color=Risk.Level))+
  geom_point(size = 5, alpha = 0.5)+
  theme_minimal()+
  labs(title = "Year of study and Age by gender")
View(grouped_course)
grouped_course %>% 
  filter(Count > 10) %>% 
  View()

df %>% 
  filter(course %in% c("information technology", "engineering", "bachelor of computer science")) %>% 
  ggplot(mapping = aes(x=course, color=gender, fill = gender))+
  geom_bar()
