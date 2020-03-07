library(rvest)
library(tidyverse)
library(lubridate)
library(robotstxt)
library(XML)
library(RSelenium)

robotstxt::paths_allowed("https://www.cladea.org/en/members/list-of-members/country")
#binman::list_versions("chromedriver")

#if it tells you the versions don't match, run commented out line and see what version of chromedriver is available and put the 
#latest as chromever argument.
driver <- rsDriver(browser=c("chrome"), chromever = "77.0.3865.40")
remDr <- driver[["client"]]
remDr$navigate("https://www.cladea.org/en/members/list-of-members/country")

data <- data.frame()
data$Name <- character()
data$Country <- character()

#finds all elements which stand for country names
webElem <- remDr$findElements(using='css', ".nombrepais a")

#extracts text - country names
countries <- sapply(webElem, function(x){x$getElementText()})

#turns them into a dataframe
countries <- as.data.frame(unique(unlist(countries)))
names(countries) <- "Country"

#removes member counts
separate(countries, col=Country, into=c('Countries', 'b'), sep = "[(]" ) -> countries  
countries %>%  select(Countries) %>%  as.matrix() -> countries

#once again finds the elements that are the countries
webElem <- remDr$findElements(using='css', ".nombrepais a")

#loop that clicks on each country name and extracts school names, puts them in a dataframe, adds country name and 
#clicks back to the page where country names are to continue the loop. Sys.sleep(3) to all page loading in between.

for (i in 1:length(webElem)) {
  webElem[[i]]$highlightElement()
  webElem[[i]]$clickElement()
  Sys.sleep(1)
  webElem1 <- remDr$findElements(using='css', ".datosmiembro h3 span")
  schools <- sapply(webElem1, function(x){x$getElementText()})
  schools <- as.data.frame(unique(unlist(schools)))
  schools$Country <- countries[i]
  names(schools) <- c("Name", "Country")
  data <- rbind(data,schools)
  remDr$navigate("https://www.cladea.org/en/members/list-of-members/country")
  Sys.sleep(3)
  webElem <- remDr$findElements(using='css', ".nombrepais a")
  Sys.sleep(3)
}


write.csv(data, file = paste0("files/CLADEA_members",today(),".csv"))
remDr$close()
rm(list=ls())
gc()


