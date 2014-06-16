# Complete rCharts installation instructions here: http://ramnathv.github.io/rCharts/

# The below worked for me using R 3.0.2 and RStudio 0.98.501 on a Mac, with devtools installed
#   require(devtools)
#   install_github('rCharts', 'ramnathv')

# generate a tutorial example
r1 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = "point", color = "gear")

# call on the save() method to publish it to a webpage
# open this in a browser to view a working data viz
setwd("/Users/Luiz/R/r/rCharts")
r1$save("filename.html")

