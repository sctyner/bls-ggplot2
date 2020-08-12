# 4-10-2020: xkcd font is not working right now. 

# install.packages("extrafont")
# install.packages("xkcd")
library(extrafont)
library(xkcd)

download.file("http://simonsoftware.se/other/xkcd.ttf", dest="fonts/xkcd.ttf", mode="wb")
download.file("https://github.com/ipython/xkcd-font/raw/master/xkcd-script/font/xkcd-script.ttf", dest = "fonts/xkcd-script.ttf", mode = "wb")
font_import() 
# type y in console when prompted
font_import(pattern = "[X/x]kcd", prompt=FALSE, paths = "fonts")
font_import(pattern = "Caveat", prompt=FALSE, paths = "fonts")
loadfonts()
# see the fonts available
fonts()
fonttable()

# visualize font 
library(ggplot2)

fontTable = fonttable()

fontTable$Face = with(fontTable, ifelse(Bold & Italic, "bold.italic", 
                                        ifelse(Bold, "bold",
                                               ifelse(Italic, "italic", "plain"))))
fontTable$Face = factor(fontTable$Face, levels = c("plain","bold","italic","bold.italic"), ordered = TRUE)
fontTable$FamilyName = factor(fontTable$FamilyName, levels = rev(sort(unique(fontTable$FamilyName))), ordered = TRUE)

p = ggplot(fontTable) +
  geom_text(aes(x=Face, y=FamilyName, label=FullName, family=FamilyName, fontface=Face)) +
  labs(title="Windows Fonts in R", x=NULL, y=NULL) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text=element_text(size=12, colour="gray40", family='Arial'),
        axis.text.x=element_text(face=c("plain","bold","italic","bold.italic")),
        plot.title=element_text(size=16, family='Arial'))
p


ggplot() + geom_point(aes(x=mpg, y=wt), data=mtcars) +
  ggtitle("HELLO DARKNESS MY OLD FRIEND") + theme(text = element_text(family = "Comfortaa"))

# ------------------------------------------------

# trying showtext package 

install.packages("showtext")

library(showtext)

font_add(family = "xkcd", regular = "fonts/xkcd.ttf")
showtext_auto()
p
