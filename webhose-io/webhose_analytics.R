# Before getting started, head over to www.webhose.io and grab an API key. Webhose
# allows you to query the past 72 hours of activity across "hundreds of thousands 
# of forums, news and blogs"

# This script is designed to query instagram.com, but webhose's API offers tons of 
# flexibility. Have a look at the documentation to explore more specific or complex
# queries.

library(RCurl)
library(rjson)
library(tm)

apikey <- readline(prompt="Enter your webhose API key as plain text without quotes around it: ")
keyword <- readline(prompt="Enter the keyword around which the analysis should be defined: ")
URL <- paste("https://webhose.io/search?token=",apikey,"&format=json&q=",keyword,"&language=english&site=instagram.com",sep="")

# note the below GET asks that curl not try to validate the SSL certificate
data <- fromJSON(getURL(URL,.opts = list(ssl.verifypeer = FALSE)))
posts <- data[1]

# the following lines use some of the properties returned by Webhose to page through 
# the available results
if (data$moreResultsAvailable==0){
  print("Nothing else to get.")
} else while (data$moreResultsAvailable>=1){
  moredata <- fromJSON(getURL(paste("https://webhose.io",data$"next",sep=""),.opts = list(ssl.verifypeer = FALSE)))
  print(paste("Getting",moredata$"next"))
  data$moreResultsAvailable <- moredata$moreResultsAvailable
  posts <- c(posts,moredata[1])
}
print("Done getting results.")

# convert the list to a data frame
df <- NULL
for (i in 1:length(posts)){
  for (j in 1:length(posts[[i]])){
    df <- rbind(df,data.frame(posts[[i]][j]))
  }
}

# replace unreadable characters with a question mark to avoid a lot of encoding frustration
df$text<-iconv(df$text,from="UTF-8",to="ASCII",sub="?")

# lower-case, remove punctuation, numbers, and standard stopwords, and normalize whitespace
# the call using PlainTextDocument circumvents some variable typing issues with TDM and DTM
doc.vec <- VectorSource(as.vector(df$text))
doc.corpus <- Corpus(doc.vec)
doc.corpus <- tm_map(doc.corpus, tolower)
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
doc.corpus <- tm_map(doc.corpus, stripWhitespace)
doc.corpus <- tm_map(doc.corpus, PlainTextDocument)

# pull a random document to see how it looks
# run this repeatedly to see a random sample of documents
inspect(doc.corpus[sample(1:length(df$text),1)])

# build both the term-document document-term matrices
TDM <- TermDocumentMatrix(doc.corpus)
DTM <- DocumentTermMatrix(doc.corpus)

# list some of the most frequent terms, arbitrarily defined as appearing 10 or more times
findFreqTerms(TDM, lowfreq=10)

# show the degree of association between the keyword defining the analysis and other terms, limited to an arbitrary association of 0.29 or greater
findAssocs(TDM, keyword, 0.29)