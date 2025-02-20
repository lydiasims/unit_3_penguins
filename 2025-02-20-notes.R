# LRDS 2025-02-20

library(tidyverse)
library(palmerpenguins)
library(ggplot2)


find("filter")  # if there is a conflict the one listed first will be called unless you specify

#how we spefiy what package we want
gentoo=penguins %>%
  dplyr :: filter(species=='Gentoo') %>%
  dplyr :: select(-body_mass_g)%>%
  print()

head(penguins)

ggplot(data=penguins)+
  geom_point(aes(x=flipper_length_mm, y=body_mass_g),color='red') #maps the aesthetics of the plot to a data set

penguins_no_nas=penguins %>%
  filter(!is.na(sex)) %>%
  filter(!is.na(body_mass_g))

ggplot(data=penguins_no_nas)+
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=bill_length_mm, shape=sex))+ #maps the aesthetics of the plot to a data set
  geom_smooth(aes(x=flipper_length_mm, y=body_mass_g),method='lm')+
  xlab('Flipper length (mm)')+
  ylab('Body mass (g)')+
  ggtitle('Penguins are cute :)')+
  theme_bw()


penguin_ts = penguins %>%
  group_by(species, year) %>%
  summarize(n=n())

ggplot(data=penguin_ts) +
  geom_line(aes(x=year,y=n))+
  theme_classic()
  
  
  geom_line(aes(x=year, y=n, color=species))  #the fix to make a nice plot
  
  
  #histograms
  x_plot=ggplot(data=penguins)+
    geom_histogram(aes(x=flipper_length_mm, fill=species), #color does the line/outline of the polygon
                   binwidth = 2,
                   color='black', 
                   position='identity',
                   alpha=0.7) +
    scale_fill_manual(values=c('darkorange','darkorchid','cyan4'))
  
  
  # box plots :)
  
  ggplot(data=penguins)+
    geom_boxplot(aes(y=flipper_length_mm,x=species))+
    geom_jitter(aes(y=flipper_length_mm,x=species,color=species),
                width=0.2) # 0.4 is the default
  
    
    
  # geom_point(aes(y=flipper_length_mm,x=species),color='powderblue',alpha=0.8)
  

    
    ## bar charts
  ggplot(data=penguins)+
    geom_bar(aes(x=sex,fill=species))+
    facet_wrap(~species, nrow=3)
    #coord_flip() # or set x to y
 
  ggsave('figures/penguin_sex_species.png',device='png',units='in',width=5,heigh=7,dpi=300)   
  
  ggsave('figures/penguin_sex_species.png',plot=x_plot, device='png',units='in',width=5,heigh=7,dpi=300)   
  
  
  #excersise 2.2
  
  ggplot(data=penguins)+
    geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=sex))+
    facet_wrap(~species)+
    theme_bw()
    
  
  
  