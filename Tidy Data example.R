#############################################################################
#                                                                          ## 
##          Project: Tidy data (example from Paper)                        ##
##-------------------------------------------------------------------------##
##          Request Date: 09-25-2018                                       ##
##          Initial Code: 09-25-2018                                       ##
##          Note:                                                          ##
##-------------------------------------------------------------------------##

## Step 1: Set work directory
rm(list = ls())

## Step 2: load required packages 
packages <- c("tidyverse", "lme4", "reshape2","stringr")
packages <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x)
        library(x, character.only = TRUE)
    }  
})

## Step 3: Set up key libraries and source code
proj.path = file.path("c:/temp/tidyverse");
data.path = file.path(proj.path,"data/");
out.path = file.path(proj.path,"out/");

setwd(proj.path)

# Install DSR, rcfss. package from github.
install.packages("devtools")
library(devtools)
devtools::install_github("garrettgman/DSR")
devtools::install_github("uc-cfss/rcfss")


# Examples
library(tidyr)
library(dplyr)
library(DSR)
library(rcfss)

# 4 dataset showing the same data with 4 different structures.
# one data set will be much easier to work with in R than the others.
# dataset 1 (tidy data)
table1

# dataset 2 (intermingles the values of population and 
#             cases in the same column, value)
table2

# dataset 3 (case and population are in the same column)
table3

# dataset 4
table4 # cases only

# dataset 5
table5 # population only 


## ------ spread() and gather()   -------- ##

# convert data2 to tidy data
# convert long data to wide data 
# spread(dat, key, value)
# dat - dataset
# key - variable from the data, all values in the 'key' variable will 
#       become new variable names in the new data.
# value - variable from the data, all the values in the 'value' variable
# will become new variable values corresponding to the new key variable. 
table2
spread(table2, key, value)
spread(table2,key = key, value = value)

# gather() convert wide data to wide data 
# gather collects a set of column names and places them into a single 
# "key" column.
# convert table 4 (check the 'Key:Value' combination )
table4
gather(table4,"year","case",2:3)               # 2:3 (column to single column)
gather(table5, "year", "population", 2:3)

tidy4 <- gather(table4, "year", "cases", 2:3)
tidy5 <- gather(table5, "year", "population", 2:3)
left_join(tidy4, tidy5)

# Separate() and unite()
# separate() pass separate the name of a data frame to reshape 
# and the name of a column to separate ("/" default separator)
table3
separate(table3, rate, into = c("cases", "population"))

# If you wish to use a specific character to separate a column, you can pass 
# the character to the sep argument of separate(). 
separate(table3, rate, into = c("cases", "population"), sep = "/")

# separate year by 2 integers
separate(table3, year, into = c("century", "year"), sep = 2)

# unite()
table6
unite(table6, "new", century, year, sep = "")

# -------------------- Practice Tidying data  --------------- #
# https://cfss.uchicago.edu/datawrangle_tidy_exercise.html
library(rcfss)

# Example 1: Race data and sorted by time 
race
race1 <- arrange(gather(race, "Time", "Score", 2:8), Name, Time)
race1

## ---- Answer  ---- ##
race %>%           
    gather(key = Time,     # new v (value) from variable name 
           value = Score,  # new v (value) from existing variable value 
           -Name,          # not gather var-name (keep the variable intact)  
           convert = TRUE) %>%  # convert string to numeric
    arrange(Name, Time)


# Example 2: Clinical Trial
results

## ---- Answer  ---- ##
results %>% 
    spread(key = Treatment, value = value) 


# --- Example 3: Grades  --- #
grades

grades %>% # fall:winter name to quarter, value to score 
    gather(key = Quarter, value = Score, Fall:Winter) %>% 
    spread(key = Test, value = Score) # new var name from test, value from score.

## ---- Answer  ---- ##
# We can use a function called str_c() from the 'stringr' library 
# to add text to the Test column
library(stringr)

# mutate() take out of data and create a new variable
grades %>% 
    gather(key = Quarter, value = Score, Fall:Winter) %>% # flat to tall 
    mutate(Test = str_c("Test", Test)) %>%                # Create a new variable with Test suffix
    spread(key = Test, value = Score) %>%                 # Flat the new variable
    arrange(ID, Year, Quarter)                            # Arrange by ID, Year, Quarter
    

# --- Example 4: Activities --- #
activities

## ---- Answer 1 ---- ##
activities %>%  # gather all variables except id and trt
    gather(key = time, value = value, -id, -trt) %>% 
    # default separator - any sequence of non-alphanumeric values (char)
    separate(col = time, into = c("location","time")) %>% 
    spread(key = location, value = value) %>% 
    arrange(id, trt, time) 
    
## ---- Answer 2 ---- ##
# You might think to use the unite() function
grades %>%
    gather(key = Quarter, value = Score, Fall:Winter) %>%
    mutate(test_label = "Test") %>%
    unite(col = Test, test_label, Test)



# -------------------- Practice Tidying data 2 --------------- #
# https://rpubs.com/tuyenhavan/346546
# This small exercise is designed to help you to practice to tidy untidy datasets. 
# Dataset used in this tutorial is called 'who'
# Working with an untidy dataset called who

# check the data structure and discover whether this data is tidy data.
names(who)
head(who)

# Taking wide dataset to long dataset
# key= type convert all the column names to variable-type value
# value = Case convert all the cell value to variable-Cases value
who %>% 
    gather(key = "Types", value = "Cases", 
           new_sp_m014:new_rel_f65, na.rm = TRUE) %>% 
    count(Cases)   # count Cases (create a new data by calculation)

# Counting the number of cases
mycount <- who %>% 
                gather(key = "Types", value = "Cases", 
                new_sp_m014:new_rel_f65, na.rm = TRUE) %>% 
                count(Cases)

# Replacing newrl by new_rl
df <- who %>%
    gather(key = "Types", value = "Cases", 
           new_sp_m014:new_rel_f65, na.rm = TRUE) %>% 
    mutate(Types = stringr::str_replace(Types, "newrel","new_rel"))

# Separate Types into three different columns
df1 <- df %>% 
    separate(Types, c("Col1","Col2","SexAge"), sep = "_")


# Separate sex and age into two columns
# sep=1 means it separates from 1st value from the left to right
# if sep=2, it separates from second value from the left to right
df2 <- df1 %>% 
    separate(SexAge, c("Sex","Age"), sep = 1)


# -------------------- Practice Tidying data 3 --------------- #
# Assignment : Cleaning Up and Manipulating Data With tidyr and dplyr
# https://rstudio-pubs-static.s3.amazonaws.com/115069_cc582a0f3464449fbe21e5999fb73126.html


# ---- tidyr example  ---- #
# gather(), spread(), separate(), and unite()

# ---- dplyr example ---- #
# using 'nycflights13' 
library(nycflights13)

# Create a data frame tbl - dplyr::tbl_df().
tblflights <- dplyr::tbl_df(flights)
head(tblflights,3) # Can also use print(tblflights,3) instead

# arrange() - reordering rows of data
# like sort() in base
arrange(tblflights, desc(dep_delay))

# mutate() - transforming variables to create new ones
# prints the new variable but does not store it:
tblflights %>%
    select(dep_delay, arr_delay) %>%
    mutate(delaysquare = dep_delay^2 + arr_delay^2) %>%
    head()

# store the new variable: 
tblflights <- tblflights %>% 
    mutate(delaysquare = dep_delay^2 + arr_delay^2) 


# filter() and select() - choosing subsets of the data
# filter() deal with observation.
# select() deal with columns

# filter() operation collects all rows in the dataset that match specified criteria.
filter(tblflights, carrier == "AA" & origin == "LGA")

#The "&" character is a Boolean "and" join, 
# which can also be indicated with a comma. 
# same or not the same?
filter(tblflights, carrier == "AA", origin == "LGA")


# same or not the same?
filter(tblflights, carrier == "AA" | carrier == "UA") 

# select()
# To select only certain variables, use the select() command. 
print(select(tblflights, dep_time, arr_time, flight), n = 6)

head(select(tblflights, flight:dest, contains("arr"), contains("dep")))

# summarize() (or summarise()) - collapsing data into summary statistics
# Create a table grouped by origin, and then summarise each group by taking the mean of dep_delay
tblflights %>%
    group_by(origin) %>%
    summarise(avg_delay = mean(dep_delay, na.rm = TRUE)) %>% 
    head()

# To do this for multiple variables, use the summarize_each() command:
# for each carrier, calculate the mean arrival and departure delays 
# at the different origin airports
tblflights %>%
    group_by(origin) %>%
    summarise_each(funs(mean(.,na.rm = TRUE)), arr_delay, dep_delay)

# Joining separate datasets with join()
flights %>% head(3) 
airlines %>% head(3)

# The different types of joins are as follows:
# inner_join(x,y): matching x + y
flights %>% 
    inner_join(airlines) %>% 
    head(3)

flights %>% 
    inner_join(airlines) %>% 
    select(distance:name) %>% 
    head(3)

# semi_join(x, y) : all x with match in y
airports %>% 
    left_join(faaflights) %>% 
    head(2)

airports %>% 
    semi_join(faaflights) %>% head(2)

# anti_join(x, y) : all x without match in y
faaflights %>% anti_join(airports)

# ------------- Keeping your code tidy with pipes: %>%  --------------- #

### Nested Option:
arrange(
    summarize(
        filter(data, variable == numeric_value),
        Total = sum(variable)
    ),
    desc(Total)
)

### Multiple Object Option:
a <- filter(data, variable == numeric_value)
b <- summarise(a, Total = sum(variable))
c <- arrange(b, desc(Total))

### %>% Option:

data %>%
    filter(variable == "value") %>%
    summarise(Total = sum(variable)) %>%
    arrange(desc(Total))



##  ----  EOF  ---- ##