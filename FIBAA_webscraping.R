#seems to be working well

library(rvest)
library(tidyverse)
library(lubridate)
library(robotstxt)
library(XML)
library(RSelenium)
robotstxt::paths_allowed("http://www.fibaa.org/procedures-at-programme-level/prog-according-to-fibaa-quality-standards/accredited-programmes/?no_cache=1&L=1")
#binman::list_versions("chromedriver")

#if it tells you the versions don't match, run commented out line and see what version of chromedriver is available and put the 
#latest as chromever argument.
driver <- rsDriver(browser=c("chrome"), chromever = "74.0.3729.6")
remDr <- driver[["client"]]
#Germany

remDr$navigate("http://www.fibaa.org/procedures-at-programme-level/prog-according-to-fibaa-quality-standards/accredited-programmes/?no_cache=1&L=1")


#find the elements which stand for region names
webElem <- remDr$findElements(using='class', "accordBland")

#loop through each region and click on it - they all get expanded
for (i in 1:length(webElem)) {
  webElem[[i]]$highlightElement()
  webElem[[i]]$clickElement()
}
Sys.sleep(2)

#in the expanded a there are business school names, this line grabs those elements
webElem <- remDr$findElements(using='class', "collegeName")

#his line takes the text
schools <- sapply(webElem, function(x){x$getElementText()})

#put them in a datame
schools <- as.data.frame(unique(unlist(schools)))

#adds columns
names(schools) <- "Name"
schools$Country <- "Germany"


#the same for pages for other countries!
#Austria
remDr$navigate("http://www.fibaa.org/procedures-at-programme-level/prog-according-to-fibaa-quality-standards/accredited-programmes/?no_cache=1&L=1")
country <- remDr$findElement(using = 'xpath', "//*/option[@value = 'http://www.fibaa.org/nc/en/procedures-at-programme-level/prog-according-to-fibaa-quality-standards/accredited-programmes/?menu=oesterreich']")
country$clickElement()

webElem <- remDr$findElements(using='class', "accordBland")

for (i in 1:length(webElem)) {
  webElem[[i]]$highlightElement()
  webElem[[i]]$clickElement()
}
Sys.sleep(2)

webElem <- remDr$findElements(using='class', "active")
schools1 <- sapply(webElem, function(x){x$getElementText()})
schools1 <- as.data.frame(unique(unlist(schools1)))
names(schools1) <- "Name"
schools1$Country <- "Austria"
schools1 <- schools1[2:12,]

#Switzerland
remDr$navigate("http://www.fibaa.org/procedures-at-programme-level/prog-according-to-fibaa-quality-standards/accredited-programmes/?no_cache=1&L=1")
country <- remDr$findElement(using = 'xpath', "//*/option[@value = 'http://www.fibaa.org/nc/en/procedures-at-programme-level/prog-according-to-fibaa-quality-standards/accredited-programmes/?menu=schweiz']")
country$clickElement()

webElem <- remDr$findElements(using='class', "accordBland")

for (i in 1:length(webElem)) {
  webElem[[i]]$highlightElement()
  webElem[[i]]$clickElement()
}
Sys.sleep(2)

webElem <- remDr$findElements(using='class', "active")
schools2 <- sapply(webElem, function(x){x$getElementText()})
schools2 <- as.data.frame(unique(unlist(schools2)))
names(schools2) <- "Name"
schools2$Country <- "Switzerland"
schools2 <- schools2[2:8,]

#Other

remDr$navigate("http://www.fibaa.org/procedures-at-programme-level/prog-according-to-fibaa-quality-standards/accredited-programmes/?no_cache=1&L=1")
country <- remDr$findElement(using = 'xpath', "//*/option[@value = 'http://www.fibaa.org/nc/en/procedures-at-programme-level/prog-according-to-fibaa-quality-standards/accredited-programmes.html?menu=weitere']")
country$clickElement()

webElem <- remDr$findElements(using='css', "li.fibaa")

for (i in 1:length(webElem)) {
  webElem[[i]]$highlightElement()
  webElem[[i]]$clickElement()
}
Sys.sleep(2)

#get a list of countries
webElem <- remDr$findElements(using='class', "active")
other_countries <- sapply(webElem, function(x){x$getElementText()})
other_countries <- as.data.frame(unique(unlist(other_countries)))
other_countries <- as.matrix(other_countries[2:25,])


webElem <- remDr$findElements(using='xpath', "//*[(@id = 'accordion')]//a")
schools3 <- sapply(webElem, function(x){x$getElementText()})
schools3 <- as.data.frame(unique(unlist(schools3)))
names(schools3) <- "Name"
schools3$Country <- NA

for (i in 1:(length(schools3$Name))) {
  if (schools3$Name[i] %in% other_countries) {
    schools3$Country[i] <- paste0(schools3$Name[i])
  }
}
     
schools3 %>% 
  mutate(id = row_number(),
         Country = ifelse(!is.na(Country), 
                           paste(Country), paste(Country[lag(id)]))) ->schools3

if (sum(schools3$Country=="NA")!=0) {
  schools3 %>% 
    mutate(id = row_number(),
           Country = ifelse(Country!="NA", 
                            paste(Country), paste(Country[lag(id)]))) ->schools3
  
} 
  
if (sum(schools3$Country=="NA")!=0) {
  schools3 %>% 
    mutate(id = row_number(),
           Country = ifelse(Country!="NA", 
                            paste(Country), paste(Country[lag(id)]))) ->schools3
}  
  
if (sum(schools3$Country=="NA")!=0) {
  schools3 %>% 
    mutate(id = row_number(),
           Country = ifelse(Country!="NA", 
                            paste(Country), paste(Country[lag(id)]))) ->schools3
}

if (sum(schools3$Country=="NA")!=0) {
  schools3 %>% 
    mutate(id = row_number(),
           Country = ifelse(Country!="NA", 
                            paste(Country), paste(Country[lag(id)]))) ->schools3
}
if (sum(schools3$Country=="NA")!=0) {
  schools3 %>% 
    mutate(id = row_number(),
           Country = ifelse(Country!="NA", 
                            paste(Country), paste(Country[lag(id)]))) ->schools3
}
schools3 <- na.omit(schools3)
schools3 <- schools3[,1:2]
for (i in 1:length(schools3$Name)) {
  if (schools3$Name[i]== schools3$Country[i]) {
    schools3[-i,] -> schools3
  }
}
schools3$Name <- as.character(schools3$Name)

for (i in 1:length(schools3$Name)) {
  if(nchar(schools3$Name[i])<1) {
    schools3[-i,] -> schools3
  }
}

schools <- rbind(schools, schools1, schools2, schools3)
write.csv(schools, file = paste0("files/FIBAA_accredited",today(),".csv"))
remDr$close()
rm(list=ls())
gc()

