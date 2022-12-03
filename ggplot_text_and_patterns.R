#plotting new stuff

name <- c("Crowned", "Ring", "Gray")
n <- c(2094, 7490,12275)
lemurs <- data.frame(name, n)
lemurs

library(showtext)
library(ggplot2)
font_add_google(name = "Bungee Shade",
                family = "bungee")
showtext_auto()

#add a tittle
ggplot(lemurs,
       aes( x = name, 
            y = n))+
  geom_col()+
  labs(title = "Lemurs")

#add a title with different font
ggplot(lemurs,
       aes( x = name, 
            y = n))+
  geom_col()+
  labs(title = "Lemurs")+
  theme(plot.title = element_text(
    family = "bungee"
  ))


#practice colour
ggplot(lemurs,
       aes( x = name, 
            y = n,
            fill = name))+
  geom_col()+
  labs(title = "Lemurs")

#add different colour
ggplot(lemurs,
       aes( x = name, 
            y = n,
            fill = name))+
  geom_col()+
  labs(title = "Lemurs")+
  scale_fill_brewer(
    palette = "Dark2")
  )

#even another palete
library(rcartocolor)

ggplot(lemurs,
       aes( x = name, 
            y = n,
            fill = name))+
  geom_col()+
  labs(title = "Lemurs")+
  scale_fill_carto_d("Prism")
)

#define your own colours

ggplot(lemurs,
       aes( x = name, 
            y = n,
            fill = name))+
  geom_col()+
  labs(title = "Lemurs")+
  scale_fill_manual(
    values = c("#157145",
                "#4C1E4F",
                "#DE6E4B"))

#check with colorblindr

g <- ggplot(lemurs,
       aes( x = name, 
            y = n,
            fill = name))+
  geom_col()+
  labs(title = "Lemurs")+
  scale_fill_manual(
    values = c("#157145",
               "#4C1E4F",
               "#DE6E4B"))


remotes::install_github("wilkelab/cowplot")
install.packages("colorspace", repos = "http://R-Forge.R-project.org")
remotes::install_github("clauswilke/colorblindr")

library(colorblindr)

cvd_grid(g)


#backgrounds
theme(plot.background = element_rect(fill = "transparent",
                                     colour = "transparent"),
      panel.background = element_rect(fill = "transparent",
                                      colour = "transparent"))

ggplot(mtcars,
       aes(x = mpg,
           y = disp,
           colour = factor(cyl),
           shape = factor(cyl)))+
         geom_point(size = 4)

#dont use just colours

remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
ggplot(lemurs,
            aes( x = name, 
                 y = n,
                 fill = name,
                 pattern = name))+
  geom_col_pattern()

#alt text
g <- ggplot(lemurs, aes(x = name, y = n, fill = name)) +
  geom_col() +
  labs(x = "",
       y = "Number of lemurs",
       title = "Lemurs at Duke Lemur Center", 
       alt = "A bar chart titled Lemurs at Duke Lemur Center. On the x-axis three species of lemurs are shown including the Crowned lemur, Gray mouse lemur, and Ring-tailed lemur. On the y-axis the count of the number of each species is shown. The number of lemurs ranges from just under 2500 for Crowned lemurs, to almost 12500 for Gray mouse lemurs. The number of Crowned lemurs is significantly lower than the other two species shown.") 
g

get_alt_text(g)

#automatically generating alt text
install.packages("BrailleR")
library(BrailleR)
VI(g)
