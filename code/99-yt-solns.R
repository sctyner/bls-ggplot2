# Your turn solutions 

# Histogram
#' # Your Turn
#'
#' 
#' Create the Christmas song histogram. Data are in the `dat/` folder. 
#' There are many custom elements here; aim to get 1 or 2 of them in 
#' addition to the `ggplot2` defaults.  
#' 
library(readr)

xmas <- read_csv("dat/xmassongs.csv")

ggplot(data = xmas, aes(x = copy_year)) + 
  geom_histogram(binwidth = 10, center = 1935, 
                 fill = "darkred", color = "black") + 
  scale_x_continuous(name = "Copyright year", 
                     breaks = seq(1930, 2010, by = 10)) + 
  scale_y_continuous(name = "Count", breaks = 1:12) + 
  theme_bw() + 
  ggtitle("ASCAP Top 25 Holiday Songs of 2019", 
          subtitle = "Nielsen data from August 2, 2019 to November 21, 2019")

# Bar Chart 
#' # Your Turn
#' 
#' Recall the `mpg` data from earlier. Recreate the bar chart below. Try to match it exactly.
#' 
data(mpg)

ggplot(data = mpg, aes(x = class)) + 
  geom_bar() + 
  ylab("Number of Cars")

# Density plot
#' # Your Turn
#' 
#' Recreate the density plot below, again using data included `ggplot2`: 
#' the `x` column in `diamonds`. Again, there are many style elements here. 
#' Aim for one or two. 
#' 
data("diamonds")

# this is a reference to "The Little Prince": https://en.wikipedia.org/wiki/The_Little_Prince 
ggplot(diamonds, aes(x)) + 
  geom_density(fill = "forestgreen", alpha = .7, adjust = 4) + 
  theme_bw() + 
  labs(x = "Snake (x)", y = "Elephant (density)", 
       title = "Why should anyone be frightened by a hat?")

# Line/time series 
#' ---
#' # Your Turn
#' 
#' Recreate the time series plot below from the Chicago area CPI data. 
#' 
chi_cpi %>% 
  ggplot(aes(x = date, y = cpi)) + 
  geom_line()

# Box plot 
#' # Your Turn 
#'  
#' Recreate this (admittedly unexciting) boxplot of the Chicago area data. 
#' 
chi_cpi %>%
  ggplot() + 
  geom_boxplot(aes(x = reorder(periodName, period), y = cpi)) + 
  labs(x = "Month", y = "CPI", title = "Chicago-Naperville-Elgin Metro Area")

# Create two 
#' ---
#' # Your Turn 
#' 
#' Using one or more of the data sets we've seen today, create **two** new two-variable data visualizations. Your visualizations must: 
#' 
#' - Use two variables 
#' - Be from 2 of the three categories (2 numeric; 2 categorical; 1 numeric, 1 categorical)
#' - Have titles (See `?labs` or `?ggtitle`)
#' 
#' There is no solution here, just have fun. 
#' 
# Facets & Maps 
#' ---
#' # Your Turn (skip?)
#' 
#' Using a data set of your own or from the web (e.g. Census), create 2 data visualizations: 
#' 
#' - one using facets, visualize 4 or more variables simultaneously 
#' - one using a map, use color to represent a variable of interest  
#' 
#' 

# Add layer same data 
#' 
#' ---
#' # Your Turn
#' 
#' Recreate this data visualization, which adds a layer to a previous viz. 
#' 
ggplot(data = cpi_all, aes(x = date, y = value)) + 
  geom_line(aes(group = area_code), alpha = .4) + 
  geom_smooth(aes(color = region), se = FALSE)

# Add layer different data 
#' 
#' ---
#' # Your Turn
#' 
#' Using the datasets `cpi_all` and `chi_dmv_cpi`, recreate the viz on the slide.
#'  
## ----fig.height=4,fig.width=9-------------------------------------------------------------------------------------------------------------
ggplot() + 
  geom_line(data = cpi_all, aes(x = date, y = value, group = area_code), alpha=.4) + 
  geom_line(data = chi_dmv_cpi, aes(x = date , y = cpi, group =area_code), color = "green", size = 1.5) + 
  labs(x = "Date", y = "CPI", title = "CPI by metro area. Chicago and Washington, DC in green")

# Recreate the christmas histogram with maximum accuracy. 

xmas2 <- readr::read_rds("slides/dat/xmasdata.rds")
bbs <- gens %>% slice(2) # baby boomer dates & info

# load the xkcd font 
source("code/add-fonts.R")

# plot 
ggplot(data = xmas2, aes(x = decade, y = height)) + 
  #annotate("rect", xmin = 3, xmax = 5, ymin = 1, ymax = 11.1, fill = "grey70") + 
  #annotate("text", x = 4, y = 10.5, label = "Baby Boom", family = "xkcd", size = 5, hjust = .3) + 
  geom_tile(aes(fill = as.factor(color_group)), color = "black", size = 1) + 
  scale_x_discrete(breaks = levels(xmas2$decade), labels = levels(xmas2$decade), drop = F) + 
  scale_fill_manual(values = c("#dc5555", "#74b37d")) + 
  geom_text(aes(label = title2, size =text_size2),family = "xkcd", lineheight = .7, 
            check_overlap = F) + 
  scale_size_continuous(range = c(2, 6)) + 
  theme(legend.position = "none", 
        text = element_text(family = "xkcd"), 
        panel.background = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(), axis.text.x = element_text(color = "black", size = 10),
        plot.tag.position = "bottom", plot.caption.position = "plot") + 
  labs(caption = "Source: ASCAP", y = "", x="",  title = "The 20 Most-Played Christmas Songs", 
       subtitle = "2019 Radio Airplay, by decade of copyright", 
       tag = "Every year, American culture embarks on a massive project to\n
       carefully recreate the Christmases of Baby Boomers' childhoods.")

# gganimate your turn
#' ---
#' # Your Turn
#' 
#' Make an animation using any data we or you have already used in this tutorial. Use at least three `gganimate` functions.
#' 
#' No solution, just have fun! 
#' 

library(gganimate)
ggplot(data = cpi_all, aes(x = date, y = value, group = area_code)) + 
  geom_line(alpha = .4) + 
  ggtitle("CPI increases over time, with the exception of the great recession") + 
  transition_reveal(along = date) + 
  view_static() + 
  ease_aes('bounce-in')


# extensions your turn
#' ---
#' # Your Turn
#' 
#' Explore the ggplot extensions, find one you like, and apply it to a dataset we or you have seen already. 
#' 
#' No solution, just have fun! 
#' 
install.packages("ggthemes")
library(ggthemes)
ggplot(data = cpi_all, aes(x = date, y = value, group = area_code, color = region)) + 
  geom_line() + 
  ggtitle("CPI increases over time, with the exception of the great recession") + 
  theme_excel() + 
  scale_color_excel()


# plotly your turn
#' ---
#' # Your Turn
#' 
#' Create your own interactive visualization with `ggplotly` using any one of the data sets and/or visualizations we've seen already. 
#' 
#' No solution, just have fun. 
library(plotly)
p <- ggplot(data = mpg) + 
  geom_jitter(aes(x = cty, y = hwy, color = class, tooltip = paste(manufacturer, model)))
ggplotly(p, tooltip = c("x", "y", "tooltip", 'color'))

