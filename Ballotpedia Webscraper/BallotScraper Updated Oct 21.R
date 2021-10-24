#Libraries
library(rvest)
library(stringr)
library(tidyverse)
library(RCurl)
library(plyr)
library(rlang)
library(writexl)
library(magrittr)

#Functions
`%notin%` <- Negate(`%in%`)

#Number of House Congressional Districts in 2022 (by State)
dists <- c(7,1,9,4,52,8,5,1,28,14,2,2,17,9,4,4,6,6,2,8,9,13,8,4,8,2,3,4,2,12,3,
           26,14,1,15,5,6,17,2,7,1,9,38,4,1,11,10,2,8,1)

#Creates a loop that writes the link for all Ballotpedia Races in House
RaceLinks <- vector()
DistList <- vector()
for(i in 1:50){
  state <- rep(state.name[i], dists[i])
  state <- ifelse(stri_sub(state, -1)=="s", paste(state, "%27", sep=""), paste(state, "%27s", sep=""))
  dist <- as.character(1:dists[i])
  distnames <- ifelse(dist %in% c("1", "21", "31", "41", "51"), paste(dist, "st", sep=""), 
                      ifelse(dist %in% c("3", "23", "33", "43", "53"), paste(dist, "rd", sep=""), 
                             ifelse(dist %in% c("2", "22", "32", "42", "52"), paste(dist, "nd", sep=""), paste(dist, "th", sep=""))))
  link <- if(length(dist)==1){paste("https://ballotpedia.org/United_States_House_of_Representatives_election_in_", state.name[i],",_2022", sep="")
  } else{paste(state, distnames) %>% str_replace_all(" ", "_") %>% paste("https://ballotpedia.org/", ., "_Congressional_District_election,_2022", sep="")}
  RaceLinks <- c(RaceLinks, link)
  DistList <- c(DistList, paste(state.name[i], distnames))
}

rm(dist, dists, i, link, state, distnames)


namesFin <- data.frame()
for(i in 1:435){
  url <- RaceLinks[i] #Gets the Ballotpedia URL for a district
  
  webpage <- read_html(url) #Reads the URL
  
  #Retrieves the election type, year, and candidate names for all elections on the page
  districts <- html_nodes(webpage,  '.votebox-header-election-type, .votebox-results-cell--text , .results_text, #District_history') 
  
  names <- html_text(districts) #Changes the list to a vector

  #Reads how many candidates there are for 2022
  rows <- if(is_empty(names)==TRUE){0
  }else{if(TRUE %in% grepl("2020|2018|2016|District history", names)){ifelse(min(grep("2020|2018|2016|District history", names))==1, 0, min(grep("2020|2018|2016|District history", names))-1)
  }else{length(names)}} 
  
  #Extracts names based on rows (Also accounts for new districts in 2022)
  names <- if(rows %in% c(0,1)){0}else if(TRUE %in% grepl("Montana|Texas District 37|Texas District 38|North Carolina District 14|Florida District 28|Colorado District 8|Oregon District 6|", names)){names[1:rows]}else{names[1:rows-1]}
  
  #Creates a data frame with each candidates name, district, and a link for their individual candidate web page
  names <- data.frame(Names=if(rows %in% c(0,1)){"NA"}else{names[grep("\t\t\t", names)] %>% str_remove_all("\\\t")}) %>% 
    mutate(Names = ifelse(Names=="NA", NA, str_remove_all(Names, "\\(.*")),
           Dist = DistList[i])
  linkpage <- read_html(url) %>% html_nodes(".votebox-results-cell--text") %$% data.frame(hrefs=as(., "character"))
  names$CandPage <- if(TRUE %in% is.na(names$Names)){NA}else{linkpage[1:nrow(names), 1] %>% str_extract("https.*(\")") %>% str_sub( 1, -2)}
  
  namesFin <- rbind.fill(namesFin, names) #Adds district to total frame
}

rm(linkpage, RaceLinks, districts, names, webpage, url, rows)


Handles <- data.frame()
for(i in 1:nrow(namesFin)){
  #Goes to candidate links (if working)
  if(is.na(namesFin[i,1])==FALSE & url.exists(namesFin[i, 3])==TRUE){
    links <- read_html(namesFin[i, 3]) %>% html_nodes("a") %$% data.frame(hrefs=as(., "character")) #Reads webpage
    
    #Identifies Campaign Website
    CampWeb <- links[grep("Campaign website", links$hrefs), 1] %>% str_extract("http(.+)(com|org)")
    CampWeb <- ifelse(is_empty(CampWeb)==TRUE, NA, CampWeb)
    
    #Identifies Campaign Twitter Handle
    CampHand <- links[grep("www.twitter.*Camp", links$hrefs), 1] %>% str_remove(".*com\\/") %>% str_remove("(\").*")
    CampHand <- ifelse(is_empty(CampHand)==TRUE, NA, CampHand)
    
    #Identifies Official Twitter Handle
    OffHand <- links[grep("www.twitter.*Off", links$hrefs), 1] %>% str_remove(".*com\\/") %>% str_remove("(\").*")
    OffHand <- ifelse(is_empty(OffHand)==TRUE, NA, OffHand)
    
    #Identifies Personal Twitter Handle
    PerHand <- links[grep("www.twitter.*Pers", links$hrefs), 1] %>% str_remove(".*com\\/") %>% str_remove("(\").*")
    PerHand <- ifelse(is_empty(PerHand)==TRUE, NA, PerHand)
    
    #Identifies Party
    Party <- ifelse(is_empty(grep("Category.*Party|Independent|Unaffiliated", links$hrefs))==FALSE,
                    links[max(grep("Category.*Party|Independent|Unaffiliated", links$hrefs)), 1] %>% str_remove(".*Category:") %>% str_remove("(\").*"),
                    links[max(grep("(Party|Independent|Unaffiliated).*(Party|Independent|Unaffiliated)", links$hrefs)), 1] %>% str_remove(".*(\">)") %>% str_remove("<.*"))
    Party <- ifelse(is_empty(Party)==TRUE, NA, Party)
    
    #Creates the data frame
    df <- data.frame(Names=namesFin[i,1], CampWeb, CampHand, OffHand, PerHand, Party)
  }else {df <- data.frame(Names= namesFin[i,1], CampWeb = NA, CampHand = NA, OffHand = NA, PerHand = NA, Party=NA)} #Accounts for non-working links
  Handles <- rbind(Handles, df)
}

namesFin <- cbind(namesFin, Handles %>% select(-Names))

rm(df, links, CampHand, CampWeb, DistList, i, OffHand, Party, PerHand)

namesFin <- namesFin %>% mutate(Party = Party %>% str_remove(" (page does not exist)"))
