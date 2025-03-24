library(RSQLite)
library(dplyr)


db_connection <- dbConnect(SQLite(), dbname="C:/Users/Ashwin/Documents/R STUDIO/DB/la4.db")

#(i)
students_table <- tbl(db_connection, "la4")
print(students_table)

# (ii) Display the first few rows
first_rows <- students_table %>% head()
print(first_rows)

# (iii) Extract and display the Score column
score_column <- students_table %>% select(Score)
print(score_column)

# (iv) Filter and display rows where Score > 85
high_scorers_query <- students_table %>%
  filter(Score > 85) %>%
  select(id, Name, Score)
print(high_scorers_query)

# (v) Add a new column "Passed" (TRUE if Score >= 85)
dbExecute(db_connection, "UPDATE la4 SET Passed = (Score >= 85)")
students_table <- tbl(db_connection, "la4") # Reload table
print(students_table)

# (vi) Calculate summary statistics for Age and Score
summary_stats <- dbGetQuery(db_connection, 
                            "SELECT AVG(Age) AS Mean_Age, AVG(Score) AS Mean_Score FROM la4")
print(summary_stats)

# (vii) Update Name where ID = 2 to "Robert"
dbExecute(db_connection, "UPDATE la4 SET Name = 'Robert' WHERE id = 2")
students_table <- tbl(db_connection, "la4") # Reload table
print(students_table)

# (viii) Sort by Score in descending order
sorted_students <- students_table %>% arrange(desc(Score))
print(sorted_students)

# (ix) Replace missing Score values with mean Score
dbExecute(db_connection, 
          "UPDATE la4 SET Score = (SELECT AVG(Score) FROM la4) WHERE Score IS NULL")
students_table <- tbl(db_connection, "la4") # Reload table
print(students_table)

# (x) Extract and display Name and Score for Age < 30
young_students_query <- students_table %>%
  filter(Age < 30) %>%
  select(Name, Score)
print(young_students_query)

# Show generated SQL for any query
show_query(young_students_query)

# Execute the query and return actual data
young_students_data <- collect(young_students_query)
print(young_students_data)

# Disconnect from the database
dbDisconnect(db_connection)







library(dplyr)

# (i) Create a data frame with the provided table data
students <- data.frame(
  StudentID = c(101, 102, 103, 104, 105),
  Name = c("John", "Jane", "Mike", "Emily", "Anna"),
  Age = c(21, 22, 20, 21, 22),
  Gender = c("M", "F", "M", "F", "F"),
  GPA = c(3.5, 3.8, 2.9, 3.6, 3.9)
)

# (ii) Calculate the mean GPA of all students
mean_gpa <- mean(students$GPA)
print(paste("Mean GPA:", mean_gpa))

# (iii) Create separate data frames for male and female students
male_students <- students %>% filter(Gender == "M")
female_students <- students %>% filter(Gender == "F")

# (iv) Add a new column "Classification": High (GPA >= 3.5), Low otherwise
students <- students %>%
  mutate(Classification = ifelse(GPA >= 3.5, "High", "Low"))
print(students)

# (v) Identify the student with the highest age and display Name, Age, and GPA
oldest_student <- students %>% filter(Age == max(Age)) %>%
  select(Name, Age, GPA)
print(oldest_student)

# (vi) Remove duplicate entries based on Name, keeping only the first occurrence
students_unique <- students %>% distinct(Name, .keep_all = TRUE)
print(students_unique)

# (vii) Count and display the number of male and female students
gender_count <- students %>% group_by(Gender) %>% summarise(Count = n())
print(gender_count)

# (viii) Create a summary table of average GPA per gender
summary_gpa <- students %>% group_by(Gender) %>% summarise(Average_GPA = mean(GPA))
print(summary_gpa)

# Suppose some students have missing GPA values, replace them with the median GPA
students$GPA[is.na(students$GPA)] <- median(students$GPA, na.rm = TRUE)

# Display the updated data frame
print(students)








library(ggplot2)
library(dplyr)

# (i) Load the airquality dataset
data("airquality")

# (ii) Display the first few rows of the dataset
head(airquality)

# (iii) Provide summary statistics for the dataset
summary(airquality)

# (iv) Display the structure of the dataset
str(airquality)

# (v) Create a basic plot of Ozone levels over time
plot(airquality$Ozone, type = "l", col = "blue", 
     main = "Ozone Levels Over Time", 
     xlab = "Day", ylab = "Ozone Levels")

# (vi) Handling missing data (Remove rows with missing values)
clean_airquality <- na.omit(airquality)

# (vii) Display the first few rows of the cleaned dataset
head(clean_airquality)

# (viii & ix) Scatter Plot - Compare Ozone and Temperature
plot(clean_airquality$Temp, clean_airquality$Ozone,
     main = "Ozone vs Temperature",
     xlab = "Temperature (°F)", ylab = "Ozone Levels",
     col = "blue", pch = 19)

# (x & xi) Add a regression line to the scatter plot
model <- lm(Ozone ~ Temp, data = clean_airquality)
abline(model, col = "red", lwd = 2)

# (xii & xiii) Divide Temperature into bins
clean_airquality <- clean_airquality %>%
  mutate(TempCategory = cut(Temp, 
                            breaks = c(-Inf, 59, 69, 79, 89, Inf),
                            labels = c("Below 60°F", "60-69°F", "70-79°F", "80-89°F", "90°F and above")))

# (xiv) Create a boxplot of Ozone levels for temperature bins
ggplot(clean_airquality, aes(x = TempCategory, y = Ozone, fill = TempCategory)) +
  geom_boxplot() +
  labs(title = "Ozone Levels by Temperature Bins",
       x = "Temperature Range",
       y = "Ozone Levels") +
  theme_minimal()








library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)

# (i) Load the Dataset
diabetes_data <- read.csv("diabetes.csv")

# (ii.a) Display the first 10 rows of the dataset
head(diabetes_data, 10)

# (ii.b) Generate summary statistics for all variables
summary(diabetes_data)

# (ii.c) Check the structure of the dataset
str(diabetes_data)

# (iii.a) Identify missing values
colSums(is.na(diabetes_data))

# (iii.b) Replace missing values with the median of the respective columns
diabetes_data[is.na(diabetes_data)] <- sapply(diabetes_data, function(x) ifelse(is.numeric(x), median(x, na.rm = TRUE), x))

# (iv.a) Histogram of Glucose levels
ggplot(diabetes_data, aes(x = Glucose)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Glucose Levels", x = "Glucose", y = "Count")

# (iv.b) Boxplot of BMI distribution by Outcome
ggplot(diabetes_data, aes(x = as.factor(Outcome), y = BMI, fill = as.factor(Outcome))) +
  geom_boxplot() +
  labs(title = "BMI Distribution by Outcome", x = "Diabetes Outcome", y = "BMI")

# (iv.c) Scatter plot of Insulin vs Glucose, color-coded by Outcome
ggplot(diabetes_data, aes(x = Glucose, y = Insulin, color = as.factor(Outcome))) +
  geom_point(alpha = 0.7) +
  labs(title = "Insulin vs Glucose Levels", x = "Glucose", y = "Insulin")

# (iv.d) Histogram of Age distribution
ggplot(diabetes_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# (iv.e) Boxplot of Age distribution by Outcome
ggplot(diabetes_data, aes(x = as.factor(Outcome), y = Age, fill = as.factor(Outcome))) +
  geom_boxplot() +
  labs(title = "Age Distribution by Outcome", x = "Diabetes Outcome", y = "Age")

# (v.a) Compute the correlation matrix for numeric variables
cor_matrix <- cor(diabetes_data[, sapply(diabetes_data, is.numeric)], use = "complete.obs")

# (v.b) Identify the highest positive correlation and plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# Find the most positively correlated pair
max_cor <- sort(cor_matrix[upper.tri(cor_matrix)], decreasing = TRUE)[1]
max_cor



                                              
