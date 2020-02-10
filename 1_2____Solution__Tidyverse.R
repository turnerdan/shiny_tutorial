###########################################
## CODE IT ## TIDYVERSE ## * ## SOLUTION ##
###########################################

#  Author:  Dan Turner (dturner@u.northwestern.edu)
#  Updated: 2-10-19

## What is the price of the most popular dog smaller than a lapcat?
pets %>% 
  select( BreedName, animal, price, MaleWtKg, LapCat, PopularityUS2017 ) %>%
  filter( MaleWtKg < min(na.omit(pets$MaleWtKg[pets$LapCat == "Lap"])) ) %>%
  filter( animal == "dog") %>%
  top_n(1, PopularityUS2017)


## Make a table showing the average price of cat by fur type (bald, shorthair, medium, longhair).
pets %>% 
  filter( animal == "cat") %>%
  group_by(Fur) %>%
  summarize(price = mean(price))

