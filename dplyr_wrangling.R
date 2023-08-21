#------------- Section 1: Filter ------------#


library(tidyverse)
library(palmerpenguins)
library(lterdatasampler)

#look for an exact match  : ==

penguins_biscoe <- penguins %>% filter(island == "Biscoe")

penguins_2007 <- penguins |> filter(year == 2007)

adelie_torgersen <- penguins %>% filter(species == "Adelie" & island == "Torgersen")

#alt: penguins %>% filter(species == "Adelie", island == "Tordgersen)
#make a subset that only contains gentoo penguins observed in 2008

gentoo_2008 <- penguins %>% filter(species =="Gentoo", year == 2008)


#what about or statements? the way we speak about things is not the way we should be writing the code. 
# Make a subset that contains Gentoos and Adelies "|" is the OR statement. 
gentoo_adelie <- penguins %>% filter(species == "Gentoo" | species == "Adelie")

#Make a subset that contains observations where the island is Dream OR the year is 2009

penguins_dream_09 <- penguins %>% filter(island == "Dream" | year == 2009)


#relationship between water temperature and size
ggplot(data= pie_crab, aes(x = water_temp, y = size)) +
 geom_point(aes(color = size))
#colder temperatures we see higher crab sizes. 

#Keep NIB, ZI, DB, JC
# we can use the %in% operator to ask: does the va;ue in our column match any of the values in this vector?

pie_sites <- pie_crab %>% filter(site %in% c("NIB","ZI","DB","JC"))

#line of code in the Console to confirm that only the sites above remain in the new subset you made
#unique(pie_sites$site)

#make a subset that includes sites PIE, ZI, NIB, BB, and CC
pie_sites3 <- pie_crab %>% filter(site %in% c("PIE","ZI","NIB","BB","CC"))

#what about excluding? Excluding filter statements

#!= is this NOT equal to that value?

exclude_zi <- pie_crab %>% filter(site != "ZI")

#What if I want to exclude sites "BB", "CC", and "PIE"
exclude_bb_cc_pie <- pie_crab %>% filter(!site %in% c("BB","CC","PIE")) # is something besides BB, CC, PIE included in site?

#Make a subset from pie_crab that only contains observations from NIB, CC, and ZI for crabs with carapace size exceeding 13. 

big_sized_pie_crab_nib_cc_zi <- pie_crab %>% filter(site %in% c("NIB","CC","ZI"),
                                                    size > 13)

#-------------------------------Selecting columns-----------------------------#
# we can select ranges of columns at a time, to select individual columns by name, seperaate them by a comma.

crabs_subset <- pie_crab %>% select(latitude, size, water_temp)

#Select a range of columns using :
crabs_subset2 <- pie_crab %>% select(site:air_temp)


# select a range and an individual column
crab_subset3 <- pie_crab %>% select(date:water_temp, name)

#----------------------------Mutate!---------------------------------#

#Use dplyr::mutate() to add or update a column, while keeping all existing columns

crabs_cm <- pie_crab %>% 
  mutate(size_cm = size / 10)

#What happens if I use muttae to add a new column containing the mean of the size column

mean_size <- pie_crab %>% 
  mutate(mean_size= mean(size, na.rm=TRUE))

crabs_awesome <- pie_crab %>% 
  mutate(name = "Teddy is awesome")

#when in doubt, make a new column, never overwrite columns

# Reminder: groub_by + summarize
mean_size_by_site <- pie_crab %>% 
  group_by(site) %>% 
  summarize(mean_size = mean(size, na.rm = TRUE),
            sd_size = sd(size, na.rm = TRUE))

#What about a group by then mutate? will be group by site 
group_mutate <- pie_crab %>% 
  group_by(site) %>% 
  mutate(mean_size = mean(size, na.rm = TRUE))



penguins %>% 
  group_by(species, island, year) %>% 
  summarize(mean_body_mass = mean(body_mass_g , na.rm = TRUE))

#What if i want to make a new column in pie_Crab that contains "giant" if the size is greater than 20, not giant if the size is less than or equal to 20

#use dplyr::case_when() to write if-else stataments more easily
crabs_bin <- pie_crab %>% 
  mutate(size_binned = case_when(
    size > 20 ~ "giant ",
    size <= 20 ~ "not giant"
  ))


sites_bin <- pie_crab %>% 
  mutate(region = case_when(
    site %in% c("ZI","CC","PIE") ~ "LOW",
    site %in% c("BB","NIB") ~ "MIDDLE",
    .default = "HIGH"
  ))