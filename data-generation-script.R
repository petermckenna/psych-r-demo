
##########################
#### Student's t-test ----
##########################


#### 1. Housekeeoping ----

# install necessary packages

install.packages('readr')
install.packages('tidyr')
install.packages('dplyr')
install.packages('ggplot2')

# call in packages to R Studio

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

#### 2. Data ----

# rounding options

# options(digits = 2) -- this is not use as it prints 2 units rather than decimals
# function for converting data to 2d.p
# specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
# trimws removes any leading white space 

## solution shown in tidyverse code below



#### data generation

# create tibble (type of data table) with 2 vectors
# round vectors to 2 decimals places
# gather so data is in the long view
# animated and non-animated


# add data for participant response time

data <- tibble(
  animated = rnorm(30, mean = 4.7, sd = 1.2),
  non_animated = rnorm(30, mean = 3.1, sd = 1.1)) %>%
  gather(`animated`, `non_animated`, key = "animacy", value = "response_time") %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(id = seq(1,60)) %>%
  select(id, animacy, response_time)

str(data)
summary(data)


##### Generate ordinal likert data ----

# scale of "Very unfriendly" = 0, "Slightly unfriendly" = 2, "Neither friendly or unfriendly" = 3, "Slight friendly" = 4, "Very friendly" = 5

# non animated response data

# a <- c(rep("Very unfriendly", 0.3*10000), rep("Slightly unfriendly", 0.4*10000), rep("Neither friendly or unfriendly", 0.1*10000),rep("Slightly friendly", 0.05*10000), rep("Very friendly", 0.05 *1000))
# 
# a <- sample(a, 30)
# 
# prop.table(summary(as.factor(a)))
# 
# # animated response data
# 
# b <- c(rep("Very unfriendly", 0.03*10000), rep("Slightly unfriendly", 0.07*10000), rep("Neither friendly or unfriendly", 0.2*10000),rep("Slightly friendly", 0.3*10000), rep("Very friendly", 0.4 *1000))
# 
# b <- sample(b, 30)
# 
# prop.table(summary(as.factor(b)))
# 
# y <- tibble(
#   a, b) %>%
#   gather(`a`, `b`, key = "animacy", value = "likert_resp") %>%
#   mutate(id = seq(1,60)) %>%
#   select(id, likert_resp) 


## also add as numbers

# non animated response

c <- c(rep("5", 0.3*10000), rep("4", 0.4*10000), rep("3", 0.1*10000),rep("2", 0.05*10000), rep("1", 0.05 *1000))

c <- sample(c, 30)

prop.table(summary(as.factor(c)))

# animated response data

d <- c(rep("5", 0.03*10000), rep("4", 0.07*10000), rep("3", 0.2*10000),rep("2", 0.3*10000), rep("1", 0.4 *1000))

d <- sample(d, 30)

prop.table(summary(as.factor(d)))

z <- tibble(
  c, d) %>%
  gather(`c`, `d`, key = "animacy", value = "likert_num") %>%
  mutate(id = seq(1,60)) %>%
  select(id, likert_num) 

# create tibble with all ordinal values

ord <- y %>%
  right_join(z, key = id
)


# merge ordinal data with existing dataset
# change labels to animated and non_animated

data1 <- data %>%
  right_join(z, key = id) %>%
  select(id, animacy, likert_num, response_time) %>%
  mutate("animacy" = recode(animacy,
                          "neutral" = "non-animated",
                          "friendly" = "animated" ))
  

# write data as csv for students

# transfer this data to csv as students will have to deal with 
# reading in data types


write_csv(data1, "t-test-data.csv")




#### Data generation for anova ----

# in this experiment, children of different age groups play a computer game with a robot
# the DV is the amount of time children spend looking at the face of the robot


# response data - fixating on the face of the robot

# 5 - 7 year olds

repeat {
  x <- rnorm(20, mean = 3.2, sd = 2)
  if ((length(which(x<0)))==0){break}
}

x

# 8 - 10 years

repeat {
  y <- rnorm(20, mean = 4.8, sd = 1.6)
  if ((length(which(x<0)))==0){break}
}

y


# 11- 13 years

repeat {
  z <- rnorm(20, mean = 2.5, sd = 1.1)
  if ((length(which(x<0)))==0){break}
}

z

data_2 <- tibble(
  x,y,z)


# rename the vectors to each age group

data_2 <- data_2 %>%
  rename(`5-6` = x,
         `7-8` = y,
         `9-10` = z) 

# write csv

write.csv(data_2, "data_2.csv")


#### Create a vector for IQ scores ----

# 5 - 7 year olds

repeat {
  a <- rnorm(20, mean = 83, sd = 2)
  if ((length(which(x<0)))==0){break}
}

a

# 8 - 10 years

repeat {
  b <- rnorm(20, mean = 90, sd = 1.6)
  if ((length(which(x<0)))==0){break}
}

b


# 11- 13 years

repeat {
  c <- rnorm(20, mean = 102, sd = 1.1)
  if ((length(which(x<0)))==0){break}
}

c

iq <- tibble(a,b,c) %>%
  mutate_if(is.numeric, round, digits = 0) %>%
  gather(iq, score) %>%
  mutate(id = 1:n()) %>%
  select(id, score)

# write the csv

write.csv(iq, file = "iq_data.csv")




#### Tasks for next demo ----

# 1. read in the data_2 data using readr

# 2. convert data from wide view into long view
  # using gather
  # show how to convert back into wide view with spread

# 3. filter data according to value 
  # create an object for scores between 0-5
  # create an object for 5-6 and 7-8 year olds

# 4. Use mutate to...
  # create an id  column

# 5. Use select to subset the data
  # 

# 6. Use summarise to generate min, max, median, mean, sd for 
  # the whole dataset
  # for eachh group



# robot/human condition data

a <- rep("human", 30)
b <- rep("robot", 30)
c <- cbind(a,b)


data_2 <- tibble(
  x,y,z %>%
  gather(x,y,z, key = "age_group", value = "face_glance")) %>%
 
    
   mutate(robot = cbind(a,b)) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(id = seq(1,60)) #%>%
  #select(id, age_group, robot, response_time)

str(data)
summary(data)



 