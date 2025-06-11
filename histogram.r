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

# Create histograms of the birth years of senators, representatives, and presidents
create_histogram <- function(data, title) {
  data %>%
    ggplot(aes(x = as.numeric(BirthYear))) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = title, x = "Birth Year", y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}
# Plot histograms
if (!is.null(senators)) {
  p <- create_histogram(senators, "Birth Year of U.S. Senators")
  ggsave("senators_histogram.png", plot = p, width = 7, height = 7)
}

if (!is.null(representatives)) {
  p <- create_histogram(representatives, "Birth Year of U.S. Representatives")
  ggsave("representatives_histogram.png", plot = p, width = 7, height = 7)
}

if (!is.null(presidents)) {
  p <- create_histogram(presidents, "Birth Year of Living U.S. Presidents")
  ggsave("presidents_histogram.png", plot = p, width = 7, height = 7)
}