#LRDS 2025-02-18 class notes

library(tidyverse)
tidyverse_packages() # shows me everything in tidyverse


my_data = as.data.frame(my_data)


library(palmerpenguins)
head(penguins)
summary(penguins)
dim(penguins)

glimpse(penguins)

gentoo = filter(penguins, species=="Gentoo")


# gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")
gentoo_ladies = filter(gentoo, sex=="female")
summary(gentoo_ladies)

# %>% # is used to clarify function messes

gentoo=penguins %>%
  filter(species=='Gentoo') %>%
  filter(sex=='female')

summary(gentoo)


female_mass= penguins %>%
  filter(sex=='female') %>%
  summarize(mean_mass_g=mean(body_mass_g), mean_bill_depth_mm=mean(bill_depth_mm))
female_mass


species_mean_mass_g= penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarise(mean_mass_g=mean(body_mass_g, na.rm=TRUE), count=n()) %>%
  print()
write.csv(species_mean_mass_g, file="data/processed/mass_table_with_a_dot.csv")
write_csv(species_mean_mass_g, file="data/processed/mass_table_with_a_underscore.csv")
#tidy verse created this write_csv to auto rid itself of the assignmed row numbers


temp=read_csv("data/processed/mass_table_with_a_underscore.csv")

species_mean_mass_g


penguins_for_america=penguins %>%
  mutate(body_mass_lb=body_mass_g*0.0022) %>% #conversion: 0.0022 lb/g
  print()

penguins %>%
  distinct(island)

penguins_brief=penguins %>%
  select(-body_mass_g) %>%
  print()


penguins_sorted= penguins %>%
  arrange(desc(body_mass_g), desc(bill_depth_mm)) %>%
  print()


Adelie_mean_bill_length_in= penguins %>%
  filter(species=="Adelie") %>%
  filter(island != "Torgersen") %>%
  mutate(bill_length_in=bill_length_mm*0.0394) %>%
  summarise(mean_bill_length_in=mean(bill_length_in, na.rm=TRUE), 
            sd_bill_length_in=sd(bill_length_in)) %>%
  print()
