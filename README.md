# Analyze-the-trends-in-the-popularity-of-programming-languages
Project Description Discover insights into the most popular programming languages and technologies using data from Stack Overflow. Analyze the number of questions by manipulating the data with dplyr and examine how language popularity changes over time.

In this project, you will use data from the Stack Exchange Data Explorer to examine how the relative popularity of R, Python, Java, and JavaScript has changed over time.

You'll work with a dataset containing one observation per tag per year, including the number of questions for that tag and the total number of questions that year.

stack_overflow_data.csv

Column	         Description
year	           The year the question was asked (2008-2020)
tag             	A word or phrase that describes the topic of the question, such as the programming language
num_questions 	 The number of questions with a certain tag in that year
year_total	     The total number of questions asked in that year


# Load necessary packages
library(readr)
library(dplyr)
library(ggplot2)

# Load the dataset
data <- read_csv("stack_overflow_data.csv")

# Question 1: Has R been growing or shrinking over time? Analyze the number of questions tagged with R each year.

# Add a percentage column
data_percentage <- data %>%
  mutate(percentage = (num_questions / year_total) * 100)

# Filter for R tags
r_over_time <- data_percentage %>%
  filter(tag == "r")

print(r_over_time)


# Question 2: What percentage of the total number of questions asked in 2020 had the R tag?

# Filter for R tags in 2020
R_tag_2020 <- data_percentage %>% 
  filter(tag == "r", year == "2020")

# Select the fraction column
r_selected <- R_tag_2020 %>% select(percentage)

# Save as a numeric variable
r_percentage <- r_selected$percentage

# Question 3: What were the five most asked-about tags between 2015-2020?

# Find total number of questions for each tag in the period 2015-2020
sorted_tags <- data %>%
  filter(year >= 2015) %>% 
  group_by(tag) %>% 
  summarize(tag_total = sum(num_questions)) %>% 
  arrange(desc(tag_total))

# Get the five largest tags
highest_tags <- head(sorted_tags$tag, n = 5)

print(highest_tags)


# Question 4: Which tag experienced the largest year-over-year increase in its percentage of questions?

# Calculate the percentage of questions for each tag per year
data_perc_year <- data %>% 
  group_by(year) %>% 
  mutate(year_total = sum(num_questions)) %>% 
  ungroup() %>% 
  mutate(percentage = (num_questions / year_total) * 100)

# Calculate the ratio of the percentage of questions for each tag compared to the previous year
tag_ratios_filtered <- data_perc_year %>% 
  arrange(tag, year) %>% 
  group_by(tag) %>% 
  mutate(ratio = percentage / lag(percentage)) %>% 
  ungroup()

# Find the tag with the highest ratio increase
highest_ratios <- tag_ratios_filtered %>% 
  slice_max(ratio, n = 1)

highest_ratio_tag <- highest_ratios$tag

# Print the results
print(highest_ratio_tag)
