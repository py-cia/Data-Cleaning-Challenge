library(tidyverse)
library(stringr)
# load data
data_cleaning_challenge <- read.csv("C:/Users/valen/Downloads/data_cleaning_challenge.csv")

# checking dimensions
dim(data_cleaning_challenge)
# 76377 rows and 11 columns

sum(is.na(data_cleaning_challenge$X))
# Same amount of NAs as rows

sum(!is.na(data_cleaning_challenge$X.1))
# Same amount of NAs as rows
# both columns are safe to ignore

# selecting first 9 columns
df <- data_cleaning_challenge %>% select(1:9)
head(df)

# check if the value first name: has any different names
# Use '$' to turn the column into a vector
# "str_sub" only works on atomic vectors
df$Row.Type[str_detect(df$Row.Type, 'first name')] %>% 
  str_sub(13) %>%
  table() %>% 
  print()

# it is ok to remove rows that contain the value "first name" since there is no variability in name, but i 
# will leave them
df2 <- df %>% filter(Row.Type %in% c("first name: Person","Iter", "Average", "Maximum", "Std.Dev.", "Total"))
View(df2)

# to conform to the standard in the youtube video
df_yt <- df2 %>% filter(Row.Type != "first name: Person")
First_Name <- rep("Person", nrow(df_yt))
Last_Name <- rep("Human", nrow(df_yt))
Date <- rep("Date", nrow(df_yt))
df_yt <- cbind(First_Name, Last_Name, Date, df_yt)
View(df_yt)
# Same number of rows and one less column because I do not have the iterate column

# Solution per YT commentator tba ---------------------------------------------



