# Data are Cambridge Parking Tickets for the period January thru September, 2014
# Goal is to understand ticket issuing patterns, including locations and times

library(RCurl)
library(jsonlite)
library(httr)
library(dplyr)

# grab data from Cambridge data service

data <- list()
offset <- 0
get <- 1

if (length(get)==0){
  message("Nothing else to get.")
} else while (length(get)>0) {
  message(paste("Fetching at offset of",offset))
  url <- getURL(paste("https://data.cambridgema.gov/resource/vnxa-cuyr.json?$order=:id&$limit=25000&$offset=",as.integer(offset),sep=""),.opts = list(ssl.verifypeer = FALSE))
  get <- tryCatch({
    
    flatten(fromJSON(url))
    
    }, error=function(err) {
      
    return(NULL)
    
  })
  data[[length(data)+1]] <- get
  offset = offset + 25000
  
  #if (offset >75000) { break }
}
message("Done getting results.")


#for (i in 1:length(data)){print(head(data[[i]]$ticket_issue_date))}
#for (i in 1:length(data)){print(colnames(data[[i]]))}

df1 <- bind_rows(data)

# additional processing for addresses, since they are nested in their own JSON
addresses <- t(sapply(df1$location.human_address, fromJSON))
rownames(addresses) <- NULL
df2 <- cbind(df1[, !(colnames(df1) %in% "location.human_address") ], addresses)