library(httr)
library(jsonlite)
library(ggplot2)
library(lubridate)
library(dplyr)

api_key <- "YOUR_VALID_API_KEY"
city <- "London"

current_weather_url <- paste0("http://api.openweathermap.org/data/2.5/weather?q=", city, "&appid=", api_key, "&units=metric")

response <- GET(current_weather_url)

if (response$status_code == 200) {
  weather_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  city_name <- weather_data$name
  temperature <- weather_data$main$temp
  weather_desc <- weather_data$weather[1, "description"]
  humidity <- weather_data$main$humidity
  
  cat("City:", city_name, "\n")
  cat("Temperature:", temperature, "°C\n")
  cat("Weather:", weather_desc, "\n")
  cat("Humidity:", humidity, "%\n")
} else {
  cat("Error fetching data. Status Code:", response$status_code, "\n")
}
historical_data <- data.frame()
today <- Sys.Date()

for (i in 1:30) {
  date <- as.numeric(as.POSIXct(today - i, tz = "UTC"))
  
  historical_url <- paste0("https://api.openweathermap.org/data/2.5/onecall/timemachine?",
                           "lat=51.5074&lon=-0.1278",  # London Coordinates
                           "&dt=", date, "&appid=", api_key, "&units=metric")
  
  hist_response <- GET(historical_url)
  
  if (hist_response$status_code == 200) {
    hist_weather <- fromJSON(content(hist_response, "text", encoding = "UTF-8"))
    
    if ("current" %in% names(hist_weather)) {
      temp <- hist_weather$current$temp
      humidity <- hist_weather$current$humidity
      historical_data <- rbind(historical_data, data.frame(date = today - i, temp = temp, humidity = humidity))
    } else {
      cat("Missing 'current' data for", today - i, "\n")
    }
  } else {
    cat("Error fetching data for", today - i, "- Status Code:", hist_response$status_code, "\n")
  }
}

# Debugging: Check if data is populated
print(historical_data)

# ------------------------
# Plot the Historical Data
# ------------------------
if (nrow(historical_data) > 0) {
  ggplot(historical_data, aes(x = date)) +
    geom_line(aes(y = temp, color = "Temperature"), linewidth = 1) +  # Use 'linewidth' instead of 'size'
    geom_line(aes(y = humidity, color = "Humidity"), linewidth = 1) +
    scale_y_continuous(
      sec.axis = sec_axis(~ ., name = "Humidity (%)"),
      name = "Temperature (°C)"
    ) +
    labs(title = "Historical Weather Data - Last 30 Days", x = "Date") +
    scale_color_manual(values = c("Temperature" = "red", "Humidity" = "blue")) +
    theme_minimal()
} else {
  cat("No historical data available for plotting.\n")
}





library(RSQLite)
library(dplyr)

# Connect to the correct SQLite database
db_connection <- dbConnect(SQLite(), dbname="C:/Users/Ashwin/Documents/R STUDIO/DB/products.db")

# Load the products table
products_table <- tbl(db_connection, "products")

# Check if the table is loaded correctly
print(products_table)


# Query: Select product names and prices where units sold > 150
high_sales_query <- products_table %>%
  filter(Units_Sold > 150) %>%
  select(Name, Price)
print(high_sales_query)

# Display the SQL syntax stored in high_sales_query
show_query(high_sales_query)

# Execute the high_sales_query request, returning the actual data
high_sales_data <- collect(high_sales_query) # returns a tibble
print(high_sales_data)

# Disconnect from the database
dbDisconnect(db_connection)





library(RSQLite)
library(dplyr)

# Connect to the SQLite database
db_connection <- dbConnect(SQLite(), dbname="C:/Users/Ashwin/Documents/R STUDIO/DB/courses.db")

# Load the courses table
courses_table <- tbl(db_connection, "courses")

# 1. Identify all courses where more than 50 students are enrolled
high_enrollment_query <- courses_table %>%
  filter(Students_Enrolled > 50)
print(high_enrollment_query)
show_query(high_enrollment_query) 
high_enrollment_data <- collect(high_enrollment_query)
print(high_enrollment_data)

# 2. Select only Course Name and Course Fee columns
course_info_query <- courses_table %>%
  select(Course_Name, Course_Fee)
print(course_info_query)
show_query(course_info_query) 
course_info_data <- collect(course_info_query)
print(course_info_data)

# 3. Count how many courses have 50 or fewer students enrolled
low_enrollment_query <- courses_table %>%
  filter(Students_Enrolled <= 50) %>%
  summarise(Count = n())
print(low_enrollment_query)
show_query(low_enrollment_query) 
low_enrollment_data <- collect(low_enrollment_query)
print(low_enrollment_data)

# 4. Retrieve the Course ID and Course Name of the course with the highest fee
highest_fee_query <- courses_table %>%
  filter(Course_Fee == max(Course_Fee)) %>%
  select(Course_ID, Course_Name)
print(highest_fee_query)
show_query(highest_fee_query) 
highest_fee_data <- collect(highest_fee_query)
print(highest_fee_data)

# 5. Increase the fees for all courses by 5% and display updated fees
dbExecute(db_connection, "UPDATE courses SET Course_Fee = Course_Fee * 1.05;")
updated_fees_query <- dbGetQuery(db_connection, "SELECT Course_Name, Course_Fee FROM courses;")
print(updated_fees_query)

# Disconnect DB
dbDisconnect(db_connection)





library(ggplot2)
library(GGally)

# 1️⃣ Scatter Plot: Sepal.Length vs. Sepal.Width
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Scatter Plot of Sepal Length vs. Sepal Width",
       x = "Sepal Length (cm)", 
       y = "Sepal Width (cm)") +
  theme_minimal()

# 2️⃣ Pairwise Plot (Scatterplot Matrix)
ggpairs(iris, aes(color = Species)) +
  labs(title = "Pairwise Scatter Plot Matrix of the Iris Dataset")

# 3️⃣ Histogram of Petal.Length
ggplot(iris, aes(x = Petal.Length, fill = Species)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.7) +
  labs(title = "Histogram of Petal Length", 
       x = "Petal Length (cm)", 
       y = "Count") +
  theme_minimal()





library(ggplot2)
library(dplyr)

# Load the built-in airquality dataset
data("airquality")

# Convert Month and Day to factors for better visualization
airquality$Month <- factor(airquality$Month, labels = c("May", "June", "July", "August", "September"))

## 1. Line Plot: Temperature Trends Over Time
ggplot(airquality, aes(x = Day, y = Temp, group = Month, color = Month)) +
  geom_line() +
  labs(title = "Daily Temperature Trends", x = "Day", y = "Temperature (°F)") +
  theme_minimal()

## 2. Bar Plot: Average Ozone Levels Per Month
avg_ozone <- airquality %>%
  group_by(Month) %>%
  summarise(Average_Ozone = mean(Ozone, na.rm = TRUE))

ggplot(avg_ozone, aes(x = Month, y = Average_Ozone, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Ozone Levels per Month", x = "Month", y = "Ozone Level (ppb)") +
  theme_minimal()

## 3. Boxplot: Wind Speed Across Months
ggplot(airquality, aes(x = Month, y = Wind, fill = Month)) +
  geom_boxplot() +
  labs(title = "Wind Speed Distribution Across Months", x = "Month", y = "Wind Speed (mph)") +
  theme_minimal()





library(ggplot2)
library(dplyr)

# Load the built-in mtcars dataset
data("mtcars")

# Convert cyl and gear to factors for better visualization
mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)

## 1. Boxplot: MPG Distribution by Cylinder Count
ggplot(mtcars, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_boxplot() +
  labs(title = "MPG Distribution by Number of Cylinders", x = "Number of Cylinders", y = "Miles per Gallon (MPG)") +
  theme_minimal()

## 2. Scatter Plot: Relationship between Horsepower and MPG
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Horsepower vs MPG", x = "Horsepower (HP)", y = "Miles per Gallon (MPG)") +
  theme_minimal()

## 3. Facet Grid Plot: MPG vs Weight for Different Gear Types
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(color = gear)) +
  facet_grid(. ~ gear) +
  labs(title = "MPG vs Weight Across Different Gear Types", x = "Weight (1000 lbs)", y = "Miles per Gallon (MPG)") +
  theme_minimal()
