library(rvest)
library(gtools)
library(writexl)
library(tidyverse)

url <- paste0("https://www.afl.com.au/draft/prospects")
webpage <- read_html(url)
setwd("/Volumes/AARON USB/Projects/AFL Draft Simulator/draftSimulator")


# Scrape player names
PlayerNames <- c( webpage %>%
                    html_nodes('.promo-list-item__title') %>% 
                    html_text() )

# Only first 25 elements are the players names
PlayerNames <- PlayerNames[1:25]

# Transform names into URL format
PlayerURLnames <- tolower( gsub(PlayerNames , pattern = " " , replacement = "-") )
PlayerURLnames <- tolower( gsub(PlayerURLnames , pattern = "'" , replacement = "") ) # This fixes the url for 'Nathan O'Driscoll'


# Generate URL's for ea. player
PlayerURLs <- c("https://www.afl.com.au/draft/prospects/") %>%
  rep(each = length(PlayerURLnames)) %>%
  paste0(c(PlayerURLnames))


# Scrape player bio stats
PlayerMaster <- data.frame()
for (i in 1:length(PlayerURLs)) {
  
  try({
    
    url <- paste0(PlayerURLs[i])
    webpage <- read_html(url)
    
    BioTable <- webpage %>%
      html_nodes("table") %>%
      html_table(fill=TRUE) %>%
      as.data.frame() %>%
      mutate(Player = PlayerNames[i]) %>%
      relocate(Player)
    
    # Take from long to wide
    PlayerRow <- reshape(BioTable, idvar = "Player", timevar = "X1", direction = "wide") 
    
    # Fix colnames
    colnames(PlayerRow) <- gsub(colnames(PlayerRow) , pattern = "X2." , replacement = "")
    
    # Remove double player name col.
    PlayerRow <- PlayerRow[,-c(2)]
    
    # Bind to master DF
    PlayerMaster <- smartbind(PlayerMaster , PlayerRow)
    
    print(paste0("Player done: ", PlayerNames[i]))
    
  } , silent = T )
}

# Check there were no ID names that didnt scrape
setdiff(PlayerNames , PlayerMaster$Player)

# Change bad date of birth colname
PlayerMaster <- PlayerMaster %>%
  rename("DOB" = 4)

# write_xlsx(PlayerMaster, "DrafteeData.xlsx")

