#' ---
#' title: "Creating Beautiful Data Visualizations in R:"
#' subtitle: "a `ggplot2` Crash Course"  
#' author: "Samantha Tyner, Ph.D."
#' date: 'April 23, 2020, 1-3pm'

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
#' 
#' # Part 2: Advanced customization  
#' # Combining layers
#' ---
#' # Using the same data 
#' 
#' Add another layer by adding a different geom 
#' 
ggplot(data = cpi_all, aes(x = date, y = value)) + 
  geom_line(aes(group = area_code), alpha = .4) + 
  geom_smooth(se = FALSE)

#' 
#' ---
#' # Your Turn
#' 
#' Recreate the data visualization on the slide which adds a layer to a previous viz. 
#' your code goes below

ggplot(data = cpi_all, aes(x = date, y = value)) + 
  geom_line(aes(group = area_code), alpha = .4) + 
  geom_smooth(aes(color = region), se = FALSE)





#' ---
#' # Using different data 
#'  
gens <- read_csv("slides/dat/generations.csv")
ggplot() + 
  geom_rect(data = gens, inherit.aes = F, #<<
            aes(xmin = birth_start, xmax = birth_end, ymin = 0, ymax = Inf, fill = generation), color = "black", alpha = .3) +  #<<
  geom_histogram(data = xmas, aes(x = copy_year), binwidth = 10, center = 1935, fill = "darkred", color = "black") + 
  scale_x_continuous(name = "Copyright year", breaks = seq(1930, 2010, by = 10)) + scale_y_continuous(name = "Count", breaks = 1:12) +
  scale_fill_brewer(palette = "Reds") + theme_bw() + 
  ggtitle("ASCAP Top 25 Holiday Songs of 2019", subtitle = "Nielsen data from August 2, 2019 to November 21, 2019")

#' 
#' ---
#' # Your Turn
#' 
#' Using the datasets `cpi_all` and `chi_dmv_cpi`, recreate the viz on the slide.
#' your code goes below

ggplot() + 
  geom_line(data = cpi_all, aes(x = date, y = value, group = area_code), alpha = .4) + 
  geom_line(data = chi_dmv_cpi, aes(x = date, y = cpi, group = area_code), color = "green", size = 1.5) + 
  labs(x = "Date", y="CPI", title = "CPI by metro area. Chicago and Washington, DC areas in green")

ggplot(mapping = aes(x = date, group = area_code)) + 
  geom_line(data = cpi_all, aes(y = value), alpha = .4) + 
  geom_line(data = chi_dmv_cpi, aes(y = cpi), color = "green", size = 1.5)


#' ---
#' class: inverse, center
#' # Graph appearance
#' ---
#' # Scales
#' 
#' > "Scales control the details of how data values are translated to visual properties." 
#' 
#' - Every aes value has a corresponding family of scales functions 
#' - Of the form `scale_{aes}_*()`, e.g. `scale_x_continuous()`
#' - Values of the * depend on the aes 
#' - Possible scale function arguments: 
#'     * `name`: label of the axis/legend
#'     * `breaks`: numeric positions of breaks on axes/legends
#'     * `labels`: labels of the breaks on axes/legends
#'     * `limits`: continuous axis limits
#'     * `expand`: padding around data 
#'     * `na.value`: what to do with missings
#'     * `trans`: continuous transformations of data 
#'     * `guide`: function to create guide/legend
#'     * `date_breaks`: breaks for date variables
#' 
#' 
#' ---
#' # Scales for axes
#' 
#' `scale_x_*()`, `scale_y_*()`
#' - continuous
#' - discrete
#' - binned
#' - log10
#' - sqrt 
#' - date
#' - datetime
#' - reverse

ggplot(chi_cpi, aes(x = date, y = cpi)) + 
  geom_line() + 
  scale_x_date(breaks = "2 years", date_labels = "%Y")

#' ---
#' # Scales for color
#' 
#' `scale_color_*()`, `scale_fill_*()`
#' - manual 
#' - continuous 
#' - brewer/distiller/fermenter
#' - gradient/gradient2/gradientn
#' - steps
#' - viridis 

ggplot(chi_dmv_cpi, aes(x = date, y = cpi, color = area_code)) + 
  geom_line() + 
  scale_color_manual(values = c("blue", "orange"))

#' 
#' ---
#' # Scales for size
#' `scale_*()`
#' - `size`
#' - `radius` 
#' - `size_binned`
#' - `size_area`
#' - `size_binned_area`
#' 
ggplot(data = chi_dmv_cpi_pop, aes(x = date, y = cpi, size = pop_est)) +
  geom_point(alpha = .3) + 
  scale_size_binned("Population", n.breaks = 5)

#' ---
#' # Other scale functions 
#' - `scale_alpha_*()`: for mapping a variable to transparency 
#' - `scale_linetype_*()`: for mapping a variable to linetype (`geom_line`)
#' - `scale_shape_*()`: for mapping a variable to shape (`geom_point`) 
#'   
ggplot(data = mpg, aes(x = cty, y = hwy, shape = class, alpha = cyl)) + 
  geom_jitter(color = "red") + theme(legend.position = "top") +  
  scale_shape_manual(values = c("◐", '◑', '◒' ,'◓', '◔','◕','◖'))

#' ---
#' # Labels 
#' 
#' Labels are also scale elements 
#' 
#' - `ggtitle(main, subtitle)`: plot title & subtitle
#' - `xlab()`, `ylab()`: axes titles 
#' - `labs()`: all of the above plus captions, tags, and other aes values 
#'     * e.g. `color = "My variable"` names the legend "My variable" 
#' 
#' ---
#' # Coordinates
#' 
#' The `coord_*()` family of functions dictate position aesthetics (e.g. `x`, `y`):
#' 
#' - Controls the "canvas" the data are "drawn" on
#' - Especially useful for maps 
#' 
#' Function examples: 
#' 
#' - `coord_cartesian()`: the default. x, y axes 
#' - `coord_polar()`: x becomes radius, y becomes angle
#' 
#' You can apply limits and transformations to axes in scales or coordinates 
#' (e.g. `xlim()`, `ylim()`) but using coordinates is probably what you want. 
#' 
#' ---
#' # Example 
#' 
#' All three visualizations below begin with the same plot code: 
p <- ggplot(chi_cpi) + geom_line(aes(x = date, y = cpi))

p + scale_x_date(breaks = "2 years", date_labels = "%Y") + 
  ggtitle('scale_x_date(breaks = "2 years", date_labels = "%Y")')
p + scale_x_date(breaks = "2 years", date_labels = "%Y", limits = as.Date(c("2004-01-01", "2014-01-01"))) + 
  ggtitle('scale_x_date(breaks = "2 years", date_labels = "%Y",\nlimits = as.Date(c("2004-01-01", "2014-01-01")))')
p + scale_x_date(breaks = "2 years", date_labels = "%Y") +
  coord_cartesian(xlim = as.Date(c("2004-01-01", "2014-01-01"))) + 
  ggtitle('scale_x_date(breaks = "2 years", date_labels = "%Y")+\ncoord_cartesian(xlim = as.Date(c("2004-01-01", "2014-01-01")))')

#' ---
#' # Themes
#' 
#' Specific themes: 
#' 
#' - `theme_grey()`: default
#' - `theme_bw()`: white background, gray gridlines
#' - `theme_classic()`: looks more like base R plots
#' - `theme_void()`: removes all background elements, all axes elements, keeps legends 
#' 
#' General `theme` function for advanced customization: 
#' 
#' - `theme()`
#'     * adjust the appearance every "non-data element" of the viz
#'     * fonts, background, text positioning, legend appearance, facet appearance, etc. 
#' - <i class="fas fa-bullhorn" style="color: #0F81BF;"></i> Rule of thumb: when changing an element that shows data, use aes() and scales. Otherwise, use themes. 
#' 
#' ---
#' # Theme elements
#' 
#' Every theme element is either a line, a rect, or text. See [documentation](https://ggplot2.tidyverse.org/reference/theme.html) for more.
#' 
#' To modify a theme element, use: 
#' 
#' - `element_line()`: change lines' appearance (color, linetype, size, etc.)
#' - `element_rect()`: change rectangles' appearance (fill, border lines)
#' - `element_text()`: change text elements' appearance (family, face, color, etc.)
#' - `element_blank()`: draw nothing 
#' 
#' Note: there are 92 possible arguments used to modify a ggplot theme. Usually, we will only need to call on a handful. 
#' 
#' ---
#' # Example 
#' 
mytheme <- theme(legend.position =  "top", 
                 axis.text = element_text(face = "italic", color = "navy"), 
                 plot.background = element_rect(fill = "#a0d1f2"), 
                 panel.background = element_blank(), 
                 panel.grid = element_line(linetype = "dotdash"))
ggplot(data = mpg) + 
  geom_jitter(aes(x = cty, y = hwy, color = class)) + 
  mytheme

#' ---
#' # Legends 
#' 
#' The `guides()` family of functions control legends' appearance  
#' - `guide_colorbar()`: continuous colors
#' - `guide_legend()`: discrete values (shapes, colors)
#' - `guide_axis()`: control axis text/spacing, add a secondary axis 
#' - `guide_bins()`: creates "bins" of values in the legend
#' - `guide_colorsteps()`: makes colorbar discrete 
#' 
#' ---
#' # Example 
#' 
ggplot(data = mpg) + 
  geom_jitter(aes(x = cty, y = hwy, color = class),key_glyph = draw_key_pointrange) + #<<
  mytheme + 
  guides(color = guide_legend(nrow = 1)) #<<

#' ---
#' # Fonts
#' 
#' To change fonts in a `ggplot2` viz: 
#' 
#' - Use the `element_text()` function inside of `theme()`
#'     * `family`: font family 
#'     * `face` : bold, italic, bold.italic, plain 
#'     * `color`, `size`, `angle`, etc. 
#' - Include additional fonts with the [`extrafont`](https://github.com/wch/extrafont) package: 
library(extrafont)
font_import()
fonts()
fonttable()

#' 
#' ---
#' # Example 
#' 
## ---- echo=TRUE, fig.width=9, fig.height=4------------------------------------------------------------------------------------------------
ggplot(data = mpg) + 
  geom_jitter(aes(x = cty, y = hwy, color = class)) + 
  theme(text = element_text(family = "Comfortaa"))

#' 
#' ---
#' # Your turn 
#' 
#' Try to recreate the XKCD comic at left as closely as possible. (Sam's version is at right.) Use this data: 
#' 
xmas2 <- readr::read_rds("slides/dat/xmasdata.rds")
bbs <- gens %>% slice(2) # baby boomer dates & info

# original:
# knitr::include_graphics("img/tradition.png")
# sam's: 
# knitr::include_graphics("img/tradition-redo.png")

# your code goes below:










#'
#' 
#' ---
#' # Your turn hints
#' 
#' - Check out the xkcd package documentation. (`vignette("xkcd-intro")`) The xkcd font files are stored in the fonts folder.
#' 
#' --
#' 
#' - geoms used: `tile`, `text`
#' 
#' --
#' 
#' - annotations used: `rect`, `text` (see `?annotate`)
#' 
#' --
#' 
#' - all named arguments of `labs` are used 
#' 
#' --
#' 
#' - scales used: `scale_x_*()`, `scale_size_*()`, `scale_fill_*()`. Fill in the *.
#' 
#' --
#' 
#' - scales used: `scale_x_discrete()`, `scale_size_continuous()`, `scale_fill_manual()`
#' 
#' --
#' 
#' - 9 `theme()` arguments used. 4 are `element_blank()`, 3 are position arguments. 
#' 
#' 
#' ---------------------------------------------------------------------------------------------------------------------------------
#' # `ggplot2` extensions
#' ---
#' # Where to find them
#' 
#' Maintainers of packages can put their `ggplot2` extension on [exts.ggplot2.tidyverse.org/gallery](http://exts.ggplot2.tidyverse.org/gallery/)
#' 
#' ---
#' # Animation
#' 
#' [`gganimate`](https://gganimate.com/): a grammar of animated graphics 
#' From the documentation, here are the `gganimate` function families: 
#' 
#' - `transition_*()`: defines how the data should be spread out and how it relates to itself across time.
#' - `view_*()`: defines how the positional scales should change along the animation.
#' - `shadow_*()`: defines how data from other points in time should be presented in the given point in time.
#' - `enter_*()/exit_*()`: defines how new data should appear and how old data should disappear during the course of the animation.
#' - `ease_aes()`: defines how different aesthetics should be eased during transitions.
#' 
#' ---
#' # Example
#' 
library(gganimate)
ggplot(data = chi_cpi, aes(x = period, y = cpi)) + 
  geom_line() + 
  labs(title = 'Year: {closest_state}') +
  transition_states(year, transition_length = 3, state_length = 2) +
  shadow_mark(alpha = .3, size = .5) + 
  ease_aes('linear')

#' 
#' ---
#' # Your Turn
#' 
#' Make an animation using any data we or you have already used in this tutorial. Use at least three `gganimate` functions.
#' 
#' Some possible datasets are: 
chi_dmv_cpi
chi_cpi
cpi_all
cpi19
diamonds
emp_own
mpg
states48
xmas 
xmas2
#' 
#' 

library(gganimate)

ggplot(data = cpi_all, aes(x = date, y = value, group = area_code)) + 
  geom_line(alpha = .4) +
  transition_reveal(along = date) + 
  view_static() + 
  ease_aes("bounce-in")


#' 
#' 
#' 
#' ---
#' # Domain specific
#' 
#' Networks: 
#' 
#' - [geomnet](https://github.com/sctyner/geomnet)
#' - [ggraph](https://github.com/thomasp85/ggraph)
#' 
#' Time Series: 
#' 
#' - [ggaluvial](http://corybrunson.github.io/ggalluvial/)
#' - [sugrrants](https://pkg.earo.me/sugrrants/)
#' 
#' Sciences: 
#' 
#' - [gggenes](https://wilkox.org/gggenes/)
#' - [ggtree](https://guangchuangyu.github.io/software/ggtree/)
#' - [ggseqlogo](https://github.com/omarwagih/ggseqlogo)
#' - [ggspectra](https://docs.r4photobiology.info/ggspectra/)

#' ---
#' # Appearance customization
#' - Arrange ggplots: 
#'     * [cowplot](https://wilkelab.org/cowplot/)
#'     * [patchwork](https://patchwork.data-imaginist.com/) 
#' - Custome themes and/or scales: 
#'     * [ggthemes](https://jrnold.github.io/ggthemes/)
#'     * [ggsci](https://nanx.me/ggsci/)
#'     * [ggtech](https://github.com/ricardo-bion/ggtech)
#'     * [ggthemr](https://github.com/cttobin/ggthemr)
#'     * [xkcd](https://cran.r-project.org/web/packages/xkcd/index.html)
#'     * [ggpubr](https://github.com/kassambara/ggpubr/issues)
#' ---
#' # Your Turn
#' 
#' Explore the ggplot extensions, find one you like, and apply it to a dataset we or you have seen already. 
#' 
#' Reminder of some data sets we've seen already: 
chi_dmv_cpi
chi_cpi
cpi_all
cpi19
diamonds
emp_own
mpg
states48
xmas 
xmas2

library(ggthemes)


ggplot(data = cpi_all, aes(x = date, y = value, group = area_code, color = region)) + 
  geom_line() + 
  theme_excel() + 
  scale_color_excel()

# ----------------------------------------------------------------------------------------------
#' # Interactivity with `plotly`
#' ---
#' # What is plotly? 
#' 
#' Plotly ([plotly.com](https://plotly.com/)) is a data visualization and dashboard company 
#' 
#' Provides open-source data visualization libraries for 
#' 
#' - [R](https://plotly.com/r/) 
#' - [Python](https://plotly.com/python/) 
#' - [JavaScript](https://plotly.com/javascript/)
#' 
#' # The `plotly` package
#' 
# install.packages("plotly")
library(plotly)
s <- seq(1, 4, by = 0.25)
plot_ly(x = ~s, y = ~s)

#' ---
#' # `ggplotly()`
#' 
#' Turn a `ggplot2` object into an interactive visualization with one function!
#' 
p <- ggplot(data = cpi_all, aes(x = date, y = value, group = area_code, tooltip = area_name)) +
  geom_line(alpha = .4)
ggplotly(p)

#' ---
#' # Tooltips 
#' 
#' What data should appear in the tooltip (on hover)? 
#' 
p <- ggplot(data = cpi_all, aes(x = date, y = value, group = area_code,
                                tooltip = area_name)) + #<<
  geom_line(alpha = .4)
ggplotly(p, tooltip = c("x", "y", "tooltip"))

#' ---
#' # Crosstalk 
#' 
d <- highlight_key(cpi_all)
p1 <- ggplot(data = d, aes(x = date, y = value, group = area_code,
                           tooltip = area_name, color = region )) +
  geom_line() + theme(legend.position = "none")
p2 <- ggplot(data = d) +
  geom_jitter(aes(x = as.factor(period),  y = value, color = region), alpha = .3) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

p1 %>% subplot(p2) %>%
  layout(title = "Select points at right to highlight lines at left") %>%
  highlight("plotly_selected")

#' ---
#' # Your Turn
#' 
#' Create your own interactive visualization with `ggplotly` using any one of the data sets and/or visualizations we've seen already. 
#' 
#' 
#' ---
#' # Additional resources 
#' 
#' - [ggplot2 book](https://ggplot2-book.org/)
#' - [plotly book](https://plotly-r.com/)
#' - [R for Data Science book](https://r4ds.had.co.nz/)
#' - [Tidy Tuesday](https://github.com/rfordatascience/tidytuesday)
#' - [My advice for getting help in R](https://sctyner.github.io/rhelp.html)
#' - Thomas Lin Pedersen's ggplot2 webinar: [part 1](https://youtu.be/h29g21z0a68) and [part 2](https://youtu.be/0m4yywqNPVY)

#' 