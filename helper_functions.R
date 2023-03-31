# Helper functions for browsing the Major League Baseball games data from Retrosheet. 


# Helper functions for determining the start and end date of a team. 

team_start <- function(team_id) {
  date <- gameLog[grep(team_id, gameLog$h_name), ] %>% 
    pull(date) %>%
    min()
  return(date)
}

team_end <- function(team_id) {
  date <- gameLog[grep(team_id, gameLog$h_name), ] %>% 
    pull(date) %>%
    max()
  return(date)
}