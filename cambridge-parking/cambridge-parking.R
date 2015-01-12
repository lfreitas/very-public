# Data are Cambridge Parking Tickets for the period January thru September, 2014
# Goal is to understand ticket issuing patterns, including locations and times

library(RCurl)
library(jsonlite)

# grab data from Cambridge data service
data <- getURL("https://data.cambridgema.gov/resource/vnxa-cuyr.json",.opts = list(ssl.verifypeer = FALSE))
df0 <- fromJSON(data)
df1 <- flatten(df0)

# additional processing for addresses, since they are nested in their own JSON
addresses <- t(sapply(df1$location.human_address, fromJSON))
rownames(addresses) <- NULL
df2 <- cbind(df1[, !(colnames(df1) %in% "location.human_address") ], addresses)