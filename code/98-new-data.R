# determining which new data sources to use 

library(tidyverse)
marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')

head(marbles)

marbles <- mutate(marbles, date = parse_date(date, format = "%d-%b-%y"))
marbles$date

head(marbles)

library(skimr)

skimr::skim(marbles)

library(GGally)

ggpairs(marbles)

marbles %>% #count(race)
  #count(source)
  count(marble_name)
marbles %>% select(-race, -source, -marble_name, -team_name, -pole, -notes) %>% ggpairs

###########################################################################################
# football data 


attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')

skim(attendance)
skim(standings)
skim(games)
