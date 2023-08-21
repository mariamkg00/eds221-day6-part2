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

#-------------------------------Selecting columns---------------------#

