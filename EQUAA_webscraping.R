
library(rvest)
library(tidyverse)
library(lubridate)
library(robotstxt)
library(XML)
library(RSelenium)

#Members 

robotstxt::paths_allowed("http://en.equaa.org/Miembros")
#binman::list_versions("chromedriver")

#if it tells you the versions don't match, run commented out line and see what version of chromedriver is available and put the 
#latest as chromever argument.
driver <- rsDriver(browser=c("chrome"), chromever = "74.0.3729.6")
remDr <- driver[["client"]]
remDr$navigate("http://en.equaa.org/Miembros")
webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))
webElem <- remDr$findElement(using='id', value="miembros-container")
webElem$highlightElement()
try <- webElem$getElementText()[[1]]
text <- strsplit(try[[1]], split="-\n")
text <- strsplit(text[[1]], split="\n-")
names(text) <- purrr::map_chr(text, 2)
text_with_country <- purrr::map(text, function(x) x[-2])
text_df <- data.frame(country = rep(names(text_with_country), map(text_with_country, length)),
                                 member = unlist(text_with_country), row.names = NULL) 
text_df <- text_df[,c(2,1)]
write.csv(text_df, file = paste0("EQUAA_members",today(),".csv"))
remDr$close()
rm(list=ls())
gc()
