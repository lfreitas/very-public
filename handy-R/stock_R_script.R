# essential R packages
# good reference list here: https://support.rstudio.com/hc/en-us/articles/201057987-Quick-list-of-useful-R-packages

# data manipulation
require(dplyr)        # subsetting, summarizing, rearranging, and joining data
require(reshape2)     # reshaping wide to long, long to wide
require(stringr)      # working with regex and strings
require(lubridate)    # working with dates/times

# visualization (beside the standard libs)
require(ggplot2)      # fancy plotting

# modeling
require(randomForest)
require(glmnet)       # lasso and elastic-net regression with cross validation
require(survival)
require(caret)        # regression and classification models

# working with large data sets locally
require(data.table)   # alternative way to organize data sets for fast operations

# working with Hadoop environments
# These packages are available on github, outside of the CRAN repository
#
# Installing packages from github requires the following steps:
# install.packages('devtools')
# library(devtools)
# Download the required version of Rtools -- devtools will prompt, if needed
# find_rtools() #should return TRUE
# install_github('<user_name>/<package_name>/pkg)
# see ?install_github for more information

#require(plyrmr)       # use plyr commands in Hadoop
#require(rhdfs)        # interact with HDFS. Note that the environment variable HADOOP_CMD must be configured and pointing to the hadoop binary or the load will fail

#working with web data
require(xml)          # easy XML parsing
require(jsonlite)     # easy working with json

# large-ish data set with interesting time and cross-sectional elements
# dataframe will be called 'flights'
require(nycflights13)


