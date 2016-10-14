library(shiny)
library(shinythemes)
library(whisker)
library(dplyr)
library(rvest)
library(ggplot2)

rstudio <- read.csv("rstudio.csv", stringsAsFactors = FALSE)

convertToNum <- function(num) {
  if (grepl("k", num)) num <- paste0(num, "00")
  as.numeric(gsub("[^[:digit:]]", "", num))
}

getStats <- function(username) {
  url <- read_html(paste0("https://github.com/", username))
  counters <- url %>% html_nodes(".counter") %>% html_text()
  ncounters <- counters %>% as.numeric()
  if (any(is.na(ncounters))) {
    ncounters[is.na(ncounters)] <- convertToNum(counters[is.na(ncounters)])
  }
  contribs <- url %>% html_nodes(".js-contribution-graph h2") %>%
    html_text() %>% convertToNum()
  c(ncounters, contribs)
}

usernames <- as.list(rstudio[!(rstudio$GitHubUsername == ""), "GitHubUsername"])
gh_data <- lapply(usernames, getStats)
gh_df <- data.frame(matrix(unlist(gh_data), nrow=length(gh_data), byrow=T),
                    row.names=usernames)
names(gh_df) <- c("Repos", "Stars", "Followers", "Following", "Contributions")
gh_df$Usernames <- rownames(gh_df)
colnames(gh_df)[6] <- "GitHubUsername"

data <- left_join(rstudio, gh_df)

data$GitHubUsername <- factor(data$GitHubUsername, levels <- data$GitHubUsername)
data$Color <-  rep("#9d2", nrow(data))
data$yaxis <-  rep(0, nrow(data))
