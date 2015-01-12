library(ISLR)
library(plyr)
library(dplyr)
library(MASS)
library(car)      # companion to applied regresion -- diagnostic functions
library(robustHD) # used for winsorizing data
library(sqldf)
library(nycflights13)
library(ggplot2)

#
#
#   Exploratory Data Analysis
#
#




college <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/College.csv")

head(college)         # look at the first 10 obs
str(college)          # summary of the variable types and values


rownames(college) <- college[,1] # assign college names to the row names
college <- college[,-1]          # eliminate college name as a variable
head(college)

summary(college)      # statistical summary of all vars

# pairwise scatterplots for entire data set -- takes a bit to produce
pairs(college)        

# boxplot of Outstate using Private as binning var
plot(college$Private,college$Outstate) 

# divide universities into two groups based on whether or not the proportion of 
# students coming from the top 10% of their high school classes exceeds 50%
college$Elite <- rep("No",nrow(college))          # set default to No

# apply conditional logic to convert some to Yes
college$Elite[college$Top10perc > 50] <- "Yes"    
college$Elite <- as.factor(college$Elite)

# generate a boxplot of the new var and out-of-state attendance
plot(college$Elite,college$Outstate)  

# plot two histograms side-by-side
split.screen(c(1,2))
screen(1)
hist(college$Outstate)
screen(2)
hist(college$Apps)
close.screen(all=TRUE)

# cross-tabs
# BooksCuts will be rows, Private will be cols 
college$BooksCuts <- with(college, cut(Books,quantile(Books)))
mytable <- table(college$BooksCuts,college$Private) 
mytable

margin.table(mytable, 1) # A frequencies (summed over BooksCuts) 
margin.table(mytable, 2) # B frequencies (summed over Private)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages

# generate a heatmap
# sample some random rows first
sample <- college[sample(nrow(college),20), ]
sample <- sample[order(sample$Books), ]
heatmap(data.matrix(sample), Rowv = NA, Colv = NA,col = cm.colors(256), scale="column", margins=c(5,10))

# cross-tab more than 2 vars
mytable <- table(college$BooksCuts,college$Private,college$Elite) 
ftable(mytable)
# compare with the output from just calling mytable


# conditional summary (or any other function) using by()
x <- by(college$Books, college$BooksCuts, summary)

# sapply() can apply a function to all variables in a data set
# start by looking at the type for each column
sapply(college, class)

# select numeric vars
nums <- sapply(college, is.numeric)
means <- sapply(college[ , nums], mean)


# TO DO
# Merging data using native R syntax
# Aggregating data using SQL using sqldf
df0 <- sqldf("select BooksCuts, avg(Books) as books_avg 
           from college 
           group by BooksCuts")
df1 <- sqldf("select carrier, tailnum, flight, count(*) as count
           from flights 
           group by carrier, tailnum, flight")



# Reshaping/pivoting data using the reshape library
# Subsetting data
# First dot last dot processing -- see: 
# http://stackoverflow.com/questions/13765834/r-equivalent-of-first-or-last-sas-operator



# Using dplyr for data manipulation
# see: http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
dim(flights)
head(flights)
# subsetting on certain parameters
filter(flights, month == 1, day == 1)

# subsetting to rows by position
slice(flights, 1:10)

# sorting ascending
arrange(flights, year, month, day)

# sorting descending
arrange(flights, desc(arr_delay))

# select subsets of variables
select(flights, year, month, day)

# select subsets of variables by exclusion
select(flights, -(year:day))

# rename a variable
rename(flights, tail_num = tailnum)

# select distinct values
distinct(select(flights, tailnum))
# select distinct values with multiple criteria
distinct(select(flights, origin, dest))

# adding new columns
# note that mutate() allows new columns to be used an inputs to other calcc
# within the same statement
mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60,
       gain_per_hour = gain / (air_time / 60))

# add new columns and drop all other columns
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60))

# sampling can be done by count of proportion
sample_n(flights,10)
sample_frac(flights,.001)
# grouped operations
# applying group_by() to a data set allows commands to be applied across the by
# subsets, reducing a lot of repetitive code
# first apply group_by() -- here, by tailnum to do operations by plane
planes <- group_by(flights, tailnum)

# compute count, average distance traveled, and average arrival delay
# summarise() accepts any function that returns a single value
# common ones are min(), max(), mean(), sum(), sd(), median(), and IQR()
# unique ones to dplyr:
# n(): number of observations in the current group
# n_distinct(x): count the number of unique values in x.
# first(x), last(x) and nth(x, n) - these work similarly to x[1], x[length(x)], 
# and x[n] but give you more control of the result if the value isn't present
delay <- summarise(planes,
                    count = n(),
                    dist = mean(distance,na.rm=TRUE),
                    delay = mean(arr_delay, na.rm=TRUE))

# filter out some results and plot the data
delay <- filter(delay, dist<2000, count > 20)
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

# performing a progressive roll-up using group_by()
daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

# chaining of functions is done using the %>% operator, which creates a pipeline
# of operations
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)
