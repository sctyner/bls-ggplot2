#' ---
#' title: "Creating Beautiful Data Visualizations in R:"
#' subtitle: "a `ggplot2` Crash Course"  
#' author: "Samantha Tyner, Ph.D."
#' date: 'April 21, 2020, 1-3pm'

library(dplyr)

#' ---
#' # Learning Goals 
#' 
#' Upon completion of this tutorial, you will be able to: 
#' 
#' 1. identify the appropriate plot types and corresponding `ggplot2` `geom`s to consider when visualizing your data;
#' 2. implement the `ggplot2` grammar of graphics by using `ggplot()` and building up plots with the `+` operator;
#' 3. iterate through multiple visualizations of their data by changing the aesthetic mappings, geometries, and other graph properties;  
#' 4. incorporate custom elements (colors, fonts, etc.) into their visualizations by adjusting `ggplot2` theme elements; and 
#' 5. investigate the world of `ggplot2` independently to expand upon the skills learned in the course.
#' 
#' # Part 1: `ggplot2` basics
#' 
#' ---
#' # Data <--> Noun     
#' ---
#' # Example
#' # In `ggplot2` code
#' 
#' The data are stored in a `data.frame` named `cpi19`.
#' 
cpi19 <- readr::read_rds("slides/dat/cpi19.rds")
library(ggplot2)
ggplot(data = cpi19)

#' you should just see a gray blank rectangle
#' 
#' ---
#' # Geom <--> Verb 
#' ### The **geom** is what relates the data to a visual element.
#' ---
#' # Example
#' ### Geom (Verb)
#' 
#' - bar chart a.k.a. column chart
#' - [`geom_col()`](https://ggplot2.tidyverse.org/reference/geom_bar.html)
#' ---
#' # In `ggplot2` code
#' 
#' We build up a data visualization in `ggplot2` with the `+` operator. 
ggplot(data = cpi19) +
   geom_col()

#' above code will result in an error: "Error: geom_col requires the following missing aesthetics: x and y"
#'
#' ---
#' # aes mapping <--> Pronouns
#' ### **Aesthetic** (aes) **mappings** substitute visual properties (aesthetics) for the data
#' ---
#' # Example
#' ### aes mapping (pronouns)
#' 
#' - x-axis: Category
#' - y-axis: % change
#' - Fill color: Category
#' 
#' ---
#' # In `ggplot2` code
#' 
#' Use the `aes()` function inside `ggplot()`. (Can also use in `geom_*()`.)
#' 
ggplot(data = cpi19,
       aes(x = Category, y = `% change`, fill = Category)) + 
  geom_col(width = .6)

#' 
#' ---
#' # In `ggplot2` code (part 2)
#' 
#' - **Note**: aesthetics / aes values do not have to be connected to data. 
#' - To change an aes value for the entire plot, use the aes value *outside* of the `aes()` function.
#' 
ggplot(data = cpi19, aes(x = Category, y = `% change`)) + 
  geom_col(width = .6, fill = "#b41f24")

#' 
#' ---
#' # Stat <--> Adverb
#' ### The **stat** describes how the data are modified in order to be expressed through the geom. 

#' ---
#' # Example
#' ### stat (adverb)
#' 
#' - **Identity**: The data are not altered in any way
#' 
#' ---
#' # In `ggplot2` code
#' 
#' `stat_identity` is default for `geom_col`, so it looks the same. 
#' 
ggplot(data = cpi19,
       aes(x = Category, y = `% change`, fill = Category)) + 
  stat_identity(geom="col", width = .6) #<<

#' ---
#' # Theme <--> Adjective 
#' ### The **theme** describes the appearance of the plot, such as the background color, font size, positions of labels, etc.
#' ---
#' # Example
#' ### Theme (adjectives)
#' 
#' - white background
#' - dotted gray major gridlines for y-axis
#' - text font, size & face
#' 
#' ---
#' # In `ggplot2` code
#' 
#' The `theme()` function can modify any non-data element of the plot. 
#' 
p <- ggplot(data = cpi19,
       aes(x = Category, y = `% change`, fill = Category)) + 
  geom_col(width = .6) 

#' This is a common trick. Create an object `p` that is the `ggplot` to add to later on
#' The `theme()` function can modify any non-data element of the plot. 
#' 
p2 <- p + theme(
 panel.background = element_rect(fill = "white", 
                                 color = "black"),
 panel.grid.major.y = element_line(color = "gray80", 
                                   linetype = "dashed"), 
 axis.text = element_text(family = "Calibri", 
                          color = "black", 
                          size = 12), 
 title = element_text(family = "Arial", 
                           face = "bold",
                           size = 14)
)
p2

#' ---
#' # Guides <--> Prepositions
#' ### The **guide** or **legend** connects non-axis aesthetics in the data visualization like color and size to the data
#' ---
#' # Example
#' ### guide (preposition)
#' 
#' - There is no guide/legend.
#' - Not needed here, because color and x-axis are the same.
#'
#' ---
#' # In `ggplot2` code
#' 
#' The `guides()` function controls all legends by connecting to the aes. 
#' 
p3 <- p2 + guides(fill = "none")
p3

#' ---
#' # Annotations <--> Interjections
#' ### An **annotation** is a separate layer that doesn't connect to other 
#' elements in the plot and is used to add fixed elements to a data visualization
#' 
#' ---
#' # Example
#' ### Annotation (interjection)
#' 
#' - Solid gray line at y = 0
#' 
#' 
#' ---
#' # In `ggplot2` code
#' 
#' The `annotate()` function creates an annotation layer
#' 
p4 <- p3 + annotate("segment", x = -Inf, xend = Inf, 
                    y = 0, yend = 0, color = "gray80") 
p4

#' ---
#' # Facets <--> Conjunctions
#' ### **Facetting** generates "small multiples", which show the same visualization for different subsets of the data
#' 
#' The current example is not facetted, so I present another toy example. More on facetting later. 
#' 
#' ---
#' # Example & `ggplot2` code
#' 
data(mpg)
ggplot(mpg, aes(cty, hwy)) + geom_point() + 
  facet_wrap(vars(drv), labeller = "label_both")
 
#' ---
#' # Other grammar elements
#' ### **Scales**: control how data are translated to visual properties (sentence structure) 
#' ### **Coordinate system**: how data are positioned in a 2D data visualization (verb tense)
#' ### **Position**: How to deal with overlap, if any (word order)
#' 
#' - "native speakers" don't have to think about these too much
#' - `ggplot2` has smart defaults here, less work for you
#' - Largely up to individual taste/style
#' 
#' ---
#' # Example
#' #### Scales, coordinate systems, positions (sentence structure, verb tense, word order)
#' 
#' - y-axis labeled every 0.5%
#' - color of bars 
#' - cartesian coordinates
#' - no position shift (identity)
#' - axes and plot titles 
#' 
#' ---
#' # In `ggplot2` code 
#' 
#' There are no overlapping elements, so no position needed.
#' 
p4 +
  scale_y_continuous(name = "Percent",
                     breaks = seq(-1, 2.5, by = .5), expand = c(0,0)) +
  scale_fill_manual(values = c("#910000", "#2f7ed8",
                               "#0d233a", "#8bbc21")) +
  coord_cartesian(ylim = c(-1.05, 2.5)) +
  labs(x = "", title = "12-month percentage change, Consumer Price Index,\nselected categories, November 2019, not seasonally adjusted", 
       subtitle = "Major categories") + 
  # needed for adjusting the subtitle
  theme(plot.subtitle = element_text(hjust = .5))

#' 
#' ---
#' # Complete `ggplot2` code
#' 
#' Note that only the first 3 lines are needed to create a meaningful
#' data visualization. the rest is just style. (like reading a book for a 3rd 
#' grader vs. a book for adults )
ggplot(data = cpi19,
       aes(x = Category, y = `% change`, fill = Category)) +
  geom_col(width = .6) +
  scale_y_continuous(name = "Percent", expand = c(0,0),
                     breaks = seq(-1, 2.5, by = .5)) +
  scale_fill_manual(values = c("#910000", "#2f7ed8", "#0d233a",
                               "#8bbc21"), guide = "none") +
  coord_cartesian(ylim = c(-1.05, 2.5)) +
  theme(panel.background = element_rect(fill = "white",
                                        color = "black"),
        panel.grid.major.y = element_line(color = "gray80",
                                          linetype = "dashed"),
        axis.text = element_text(family = "Calibri",
                                 color = "black", size = 12),
        title = element_text(family = "Arial",
                                  face = "bold", size = 14),
        plot.subtitle = element_text(hjust = .5)) +
  annotate("segment", x = -Inf, xend = Inf,
                    y = 0, yend = 0, color = "gray80") +
  labs(x = "", title = "12-month percentage change, Consumer Price Index,\nselected categories, November 2019, not seasonally adjusted",
       subtitle = "Major categories")


# bonus (not in slides): adjust the theme elements so that the y axis title will appear on top of the y axis 

ggplot(data = cpi19,
       aes(x = Category, y = `% change`, fill = Category)) +
  geom_col(width = .6) +
  scale_y_continuous(name = "Percent", expand = c(0,0),
                     breaks = seq(-1, 2.5, by = .5)) +
  scale_fill_manual(values = c("#910000", "#2f7ed8", "#0d233a",
                               "#8bbc21"), guide = "none") +
  coord_cartesian(ylim = c(-1.05, 2.5)) +
  theme(panel.background = element_rect(fill = "white",
                                        color = "black"),
        panel.grid.major.y = element_line(color = "gray80",
                                          linetype = "dashed"),
        axis.text = element_text(family = "Calibri",
                                 color = "black", size = 12),
        plot.title = element_text(family = "Arial",
                             face = "bold", size = 14),
        plot.subtitle = element_text(hjust = .5, family = "Arial",
                                                  face = "bold", size = 14),
        # 
        axis.title.y  = element_text(family = "Arial",
                     face = "bold", size = 12, angle = 0, vjust = 1.05)
        ) +
  annotate("segment", x = -Inf, xend = Inf,
           y = 0, yend = 0, color = "gray80") +
  labs(x = "", title = "12-month percentage change, Consumer Price Index,\nselected categories, November 2019, not seasonally adjusted",
       subtitle = "Major categories")



#' ---
#' # What's next? 
#' 
#' More and more detail on the `ggplot2` universe: 
#' 
#' - `geom`s for one- and two-variable visualization
#' - Including three or more variables in the visualization 
#'     * Colors, sizes, shapes, linetypes
#'     * Grouping & facetting 
#'     * Maps 
#' --------------------------------------------------------------------------------------------------------------------------------
#' ---
#' # One-variable visualization 
#' ---
#' # Histogram 
#' 
#' > A **histogram** approximates the distribution of a single numeric variable. It shows frequency of values in specified ranges. 
#' 
#' ### **`geom_histogram`**
#' 
#' - requires the `x` aesthetic inside `aes()`
#' - Specify width of bars with the `bins` or `binwidth` argument
#' - Can change appearance of the bars with `color`, `fill`, `alpha` arguments 
#' 
#' ---
#' # Your Turn
#'
#' 
#' Create the Christmas song histogram. Data are in the `dat/` folder. 
#' There are many custom elements here; aim to get 1 or 2 of them in 
#' addition to the `ggplot2` defaults.  
#' 
## ----echo=T-------------------------------------------------------------------------------------------------------------------------------
library(readr)
xmas <- read_csv("slides/dat/xmassongs.csv")
# your solution goes below. here's something to get you started: 
xmas
ggplot(data = xmas, aes(x = copy_year)) + 
  geom_histogram(binwidth = 10, center = 1935, fill = "darkred", 
                 color = "black") + 
  labs(title = "ASCAP Top 25 Holiday Songs of 2019" )

#' 
#' 
#' 
#' ---
#' # Bar chart 
#' 
#' > A **bar chart** displays counts of a categorical variable, and is the categorical equivalent of the histogram. 
#' 
#' ### **`geom_bar`**
#' 
#' - requires the `x` aesthetic inside `aes()`
#' - Can change appearance of the bars with `color`, `fill`, `alpha` arguments 
#' 
#' ---
#' # Your Turn
#' 
#' Recall the `mpg` data from earlier. Recreate the bar chart below. Try to match it exactly.
#' 
data(mpg)
# your solution goes below. here's something to get you started: 
mpg
ggplot(data = mpg, aes(x = class)) + 
  geom_bar() + 
  ylab("Number of Cars")

#' 
#' ---
#' # Density plot 
#' 
#' > A **density** estimate is a smoothed version of the histogram which is especially useful if the data come from a continuous distribution. 
#' 
#' ### **`geom_density`**
#' 
#' - requires the `x` aesthetic inside `aes()`
#' - change the underlying kernel smoother with the `kernel` parameter
#' - change bandwith with the `adjust` parameter
#' - Can change appearance of the bars with `color`, `fill`, `alpha` arguments 
#' 
#' ---
#' # Your Turn
#' 
#' Recreate the density plot below, again using data included `ggplot2`: 
#' the `x` column in `diamonds`. Again, there are many style elements here. 
#' Aim for one or two. Play around with the kernel and bandwidth parameters.
#' # this viz is a reference to "The Little Prince": https://en.wikipedia.org/wiki/The_Little_Prince
data("diamonds")
# your solution goes below.
diamonds
ggplot(data = diamonds, aes(x = x)) + 
  geom_density(adjust = 4, fill = "forest green", alpha = .7)

#' 
#' ---
#' # Other one-variable viz
#' 
#' - `geom_freqpoly`: behaves the same as `geom_histogram` but with connected lines instead of bars 
#' - `geom_dotplot`: show values in bins as individual dots 
#' - `geom_rug` (not shown): place lines along an axis for each observation  

#' `geom_freqpoly` code :
ggplot(data = xmas, aes(x = copy_year)) + 
  geom_freqpoly(binwidth = 10, center = 1935, color = "darkred") + 
  scale_x_continuous(name = "Copyright year", breaks = seq(1930, 2010, by = 10)) + 
  scale_y_continuous(name = "Count", breaks = 0:12) + 
  theme_bw() + 
  ggtitle("ASCAP Top 25 Holiday Songs of 2019", subtitle = "Nielsen data from August 2, 2019 to November 21, 2019")

#' `geom_dotplot`code :
ggplot(data = xmas, aes(x = copy_year)) + 
  geom_dotplot(binwidth = 10, fill = "darkred", method = "histodot", dotsize = .5, origin = 1930) + 
  scale_x_continuous(name = "Copyright year", breaks = seq(1930, 2010, by = 10)) + 
  scale_y_continuous(name = "") + 
  theme_bw() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  ggtitle("ASCAP Top 25 Holiday Songs of 2019", subtitle = "Nielsen data from August 2, 2019 to November 21, 2019")

#' -----------------------------------------------------------------------------------------------------------------------
#' # Two-variable visualization
#' ---
#' # Example data 
#' 
#' Two CPI time series downloaded from BLS: one for the Chicago metro area, and one for the DC metro area.
#' 
chi_dmv_cpi <- read_csv("slides/dat/chi_dmv_cpi-99-18.csv")
glimpse(chi_dmv_cpi)

#' 
#' ---
#' # Two numeric variables 
#' 
#' ### `date`, `cpi` 
#' 
#' Focus on the Chicago area for now.  (Code `S23A`)
library(dplyr)
chi_cpi <- chi_dmv_cpi %>% filter(area_code == "S23A")

#' 
#' ### **`geom_line`**
#' 
#' - requires the `x, y` aesthetics inside `aes()`
#' - Can change appearance of the lines with `color`, `linetype`, `alpha` arguments 
#' - The `group` `aes` draws lines according to a grouping variable (later)
#' 
#' ---
#' # Your Turn
#' 
#' Recreate the time series plot below from the Chicago area CPI data. 
#' your solution goes below: 
ggplot(data = chi_cpi, aes(x = date, y = cpi)) + 
  geom_line()

#' 
#' ---
#' # One numeric, one categorical
#' 
#' ### `period`, `cpi`
#' 
#' Continue to focus on the Chicago area for now.  (Code `S23A`)
#' 
#' ### **`geom_boxplot`**
#' 
#' - requires the `x,y` aesthetics inside `aes()`
#' - Can change appearance of the boxes with `color`, `fill`, `alpha` arguments 
#' 
#' ---
#' # Your Turn 
#'  
#' Recreate this (admittedly unexciting) boxplot of the Chicago area data. 
#' your code goes below: 
ggplot(data = chi_cpi, aes(x = reorder(periodName,period), y = cpi)) + 
  geom_boxplot() + 
  xlab("Month")

#' 
#' ---
#' # Two categorical variables
#' 
#' 
#' Data from BLS: ownership (foreign or domestic) and industry (retail, wholesale, etc.)
emp_own <-  read_csv("slides/dat/bls-emp-ownership.csv")
glimpse(emp_own)

# https://www.bls.gov/spotlight/2019/a-look-at-employment-and-wages-in-u-s-establishments-with-foreign-ownership/home.htm

#' 
ggplot(emp_own, aes(x = ownership, y = industry, size = perc_by_own)) + 
  geom_count() + 
  labs(x = "Ownership", y = "Industry", size = "Percent")

#' 
#' ---
#' # Your Turn 
#' 
#' Using one or more of the data sets we've seen today, create **two** new two-variable data visualizations. Your visualizations must: 
#' 
#' - Use two variables 
#' - Be from 2 of the three categories (2 numeric; 2 categorical; 1 numeric, 1 categorical)
#' - Have titles (See `?labs` or `?ggtitle`)
#' 
#' Data sets: 
cpi19
diamonds
mpg
xmas
chi_dmv_cpi
chi_cpi
emp_own

# your solutions go below: 
# From Florent Bled 
ggplot(data = diamonds, aes(x=cut,y=price,group=cut)) + 
  geom_boxplot() +
  labs(y="Price ($)",x="Cut",
       title="Might as well go with Zirconium I guess!")

ggplot(data = diamonds, aes(x=cut,y=price,group=cut)) + 
  geom_violin(fill = "skyblue")


#' ------------------------------------------------------------------------------------------------------------------------
#' # Three or more variable visualization
#' ---
#' # Additional `aes()` mappings
#' 
#' Add a third variable to a plot with:  
#' 
#' **color** : `color, fill`  
#' 
#' **size**: `size, stroke`   
#' 
#' **shape**: `shape, linetype`  
#' 
#' **contour lines**: `geom_contour(), geom_density_2d(), z`  
#' 
#' **bins**: `geom_bin2d(), geom_hex(), geom_tile()`   
#' 
#' 
#' ---
#' # Add a discrete/categorical variable
#' 
#' Recall the CPI data from before: it has data from the Chicago metro area and the Washington DC metro area. 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
chi_dmv_cpi %>% arrange(year, period)

#' 
#' ---
#' # Two time series, one plot
#' 
#' Use color to indicate region (S23A is Chicago)
#' 
## ----echo = TRUE, fig.width=9, fig.height=4-----------------------------------------------------------------------------------------------
ggplot(data = chi_dmv_cpi, aes(x = date, y = cpi, color = area_code)) + #<<
  geom_line() 

#' 
#' ---
#' # Two time series, one plot
#' 
#' Use linetype to indicate region (S23A is Chicago)
#' 
## ----echo = TRUE, fig.width=9, fig.height=4-----------------------------------------------------------------------------------------------
ggplot(data = chi_dmv_cpi, aes(x = date, y = cpi, linetype = area_code)) + #<<
  geom_line() 

#' 
#' 
#' ---
#' # Two time series, one plot
#' 
#' Use size to indicate region (S23A is Chicago). 
#' Note: I do not recommended this! Why? 
#' 
## ----echo = TRUE, fig.width=9, fig.height=4-----------------------------------------------------------------------------------------------
ggplot(data = chi_dmv_cpi, aes(x = date, y = cpi, size = area_code)) + #<<
  geom_line() 

#' 
#' ---
#' # Adding a continuous/numeric variable
#' 
#' Add data about the population of the region 
#' 
## ----echo = T-----------------------------------------------------------------------------------------------------------------------------
chi_dmv_pop <- read_csv("slides/dat/chi_dmv_pop.csv")
chi_dmv_cpi_pop <- left_join(chi_dmv_cpi, chi_dmv_pop)
glimpse(chi_dmv_cpi_pop)

#' 
#' ---
#' # Size 
#' 
ggplot(data = chi_dmv_cpi_pop, aes(x = date, y = cpi, size = pop_est)) + #<<
  geom_point(alpha = .5) 

#' 
#' ---
#' # Control for Missings
#' 
ggplot(data = chi_dmv_cpi_pop, aes(x = date, y = cpi, size = pop_est)) +
  geom_point(alpha = .5) + 
  scale_size_area(na.value = 1) #<<

#' 
#' ---
#' # Color
#' 
ggplot(data = chi_dmv_cpi_pop, aes(x = date, y = cpi, color = pop_est)) + #<<
  geom_point() 

#' ---
#' # Grouping
#' 
#' > The `group` aesthetic partitions the data for plotting into groups 
#' 
ggplot(data = chi_dmv_cpi_pop, aes(x = date, y = cpi)) + geom_line()
ggplot(data = chi_dmv_cpi_pop, aes(x = date, y = cpi, group = area_code)) + geom_line()

#' ---
#' # Many groups 
#' 
#' Larger set of CPI data from more major metropolitan areas in the contiguous U.S. 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
cpi_all <- read_csv("slides/dat/cpi_all_msa.csv")
glimpse(cpi_all)

#' ---
#' # Visualizing many groups
#' 
ggplot(data = cpi_all, aes(x = date, y = value, group = area_code)) + 
  geom_line(alpha = .4)

#' ---
#' # Add another variable 
#' 
ggplot(data = cpi_all, aes(x = date, y = value, group = area_code, color = region)) + 
  geom_line()

#' 
#' # Facets 
#' 
#' > Facetting generates small multiples, each displaying a different subset of the data. 
#' Facets are an alternative to aesthetics for displaying additional discrete variables.
#' 
#' - plot subgroups separately
#' - can be arranged by rows, columns, or both 
#' - can be "wrapped" for many subgroups 
#' - great for exploratory analyses 
#' 
#' 
#' ---
#' # Facetting functions 
#' 
#' #### `facet_grid()`
#' 
#' - create a grid of graphs, by rows and columns 
#' - use `vars()` to call on the variables 
#' - adjust scales with `scales = "free"`
#' 
#' #### `facet_wrap()`
#' 
#' - create small multiples by "wrapping" a series of plots
#' - use `vars()` to call on the variables 
#' - `nrow` and `ncol` arguments for dictating shape of grid 
#' 
p <- ggplot(data = cpi_all, aes(x = date, y = value, group = area_code)) +
  geom_line()
p
#' 
#' ---
#' # Facetting example 
#' 
p + facet_grid(rows = vars(region))

p + facet_wrap(vars(region), ncol = 2)

#' 
#' ---
#' # Maps 
#' 
#'  See material by [Angela Li](bit.ly/dc-spatial-2019)
#' 
#' If you don't have shapefiles, you need to geocode! See Angela Li's slides linked above for more info. 
#' 
#' ---
#' # The `sf` package 
#' 
#' The [`sf`](https://r-spatial.github.io/sf/) package
#' - `tidyverse`-compatible spatial data frames
#' - "geometry" column stores the spatial information

library(sf)
states <- USAboundaries::us_boundaries()
states %>% select(name, geometry) %>% slice (1:2)

#' ---
#' # `geom_sf()`
#' 
#' `geom_sf()` requires the `geometry` aes value. Automatically detects column named "geometry" 
#' 
states48 <- states %>% filter(stringr::str_detect(name, "Hawaii|Alaska|Puerto", negate = T))
ggplot() + geom_sf(data = states48) #<<

#' ---
#' # Adding color  
#' 
#' Can use the `fill` aes to color geometries by another variable in the data. 
#' In this example, we color by the water area in the state.
#' 
ggplot() + geom_sf(data = states48, aes(fill = awater)) 

#' ---
#' # Your Turn (skip?)
#' 
#' Using a data set of your own or from the web (e.g. Census), create 2 data visualizations: 
#' 
#' - one using facets, visualize 4 or more variables simultaneously 
#' - one using a map, use color to represent a variable of interest  
#' 
#' Some R packages containing `sf` data: 
??USAboundaries
??USAboundariesData
??albersusa

#' 
#' Read in a shapefile with `sf::st_read()`
## mydata <- st_read("filepath")
#' The `readr`, `readxl`, and `haven` packages can be used to read in additional data. 
#' 
#' -----------------------------------------------------------------------------------------------------------------------
#' --------End of day one ------------------------------------------------------------------------------
#' -----------------------------------------------------------------------------------------------------------------------

