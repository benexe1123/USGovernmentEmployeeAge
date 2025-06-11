# Install Libraries if not already installed
# install.packages("httr")
# install.packages("rvest")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("tidyr")
# install.packages("ggplot2")
# install.packages("ggthemes")

# Libraries
library(httr)
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# Initialize empty data frames
senators <- NULL
representatives <- NULL
presidents <- NULL

# Senators
suppressWarnings({
  url <- "https://en.wikipedia.org/wiki/List_of_current_United_States_senators"
  response <- GET(url)
  
  if (response$status_code == 200) {
    webpage <- read_html(url)

    tbls <- html_nodes(webpage, "table")
    tbls <- html_table(tbls, fill = TRUE)

    senators <- tbls[[6]][, c(3, 6)]
    names(senators) <- c("Name", "Born")
    senators$Born <- str_extract(senators[[2]], "\\d{4}-\\d{2}-\\d{2}")
    senators$BirthYear <- str_extract(senators$Born, "\\d{4}")

    # print("Senators:")
    # print(senators)

  } else {
    warning("Failed to retrieve the webpage. Status code: ", response$status_code)
  }
})

# Representatives
suppressWarnings({
  url <- "https://en.wikipedia.org/wiki/List_of_current_United_States_representatives"
  response <- GET(url)
  
  if (response$status_code == 200) {
    webpage <- read_html(url)

    tbls <- html_nodes(webpage, "table")
    tbls <- html_table(tbls, fill = TRUE)

    representatives <- tbls[[7]][, c(2, 5)]
    names(representatives) <- c("Name", "Born")
    representatives$Born <- str_extract(representatives[[2]], "\\d{4}-\\d{2}-\\d{2}")
    representatives$BirthYear <- str_extract(representatives$Born, "\\d{4}")

    # print("Representatives:")
    # print(representatives)

  } else {
    warning("Failed to retrieve the webpage. Status code: ", response$status_code)
  }
})

# Living Presidents
suppressWarnings({
  url <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States"
  response <- GET(url)
  
  if (response$status_code == 200) {
    webpage <- read_html(url)

    tbls <- html_nodes(webpage, "table")
    tbls <- html_table(tbls, fill = TRUE)

    presidents <- tbls[[1]][, c(3)]
    names(presidents) <- c("Combined")

    presidents$Name <- str_extract(presidents$Combined, "^[^(]+")
    presidents$BirthYear <- str_extract(presidents$Combined, "\\(b\\.\\s*\\d{4}\\)") %>%
        sub("\\(b\\.\\s*", "", .) %>%  # remove "(b. " from start
        sub("\\)", "", .)               # remove ")" at end
    presidents <- presidents[!is.na(presidents$BirthYear), ] # Keep only living presidents
    presidents$Combined <- NULL

    # print("Living Presidents:")
    # print(presidents)

  } else {
    warning("Failed to retrieve the webpage. Status code: ", response$status_code)
  }
})

# Output the senators, representatives, and living US Presidents that fit the criteria (born before 1945)
year <- 1945 # ENIAC, https://en.wikipedia.org/wiki/ENIAC
suppressWarnings({
  print(paste0("Government employees born before ", year, ":"))  
  all_employees <- bind_rows(
    mutate(senators, Type = "Senator"),
    mutate(representatives, Type = "Representative"),
    mutate(presidents, Type = "President")
  )
  
  all_employees <- all_employees %>%
    filter(!is.na(BirthYear) & as.numeric(BirthYear) <= year) %>%
    select(Name = Name, BirthYear, Type)
  
  print(all_employees, n=Inf)

  # Count total by Type
  counts <- all_employees %>%
    group_by(Type) %>%
    summarise(Total = n())
  
  print(counts)
})