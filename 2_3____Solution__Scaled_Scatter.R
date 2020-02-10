################################################
## CODE IT ## SCALED SCATTER ## * ## SOLUTION ##
################################################

#  Author:  Dan Turner (dturner@u.northwestern.edu)
#  Updated: 2-10-19

# Create new column in pets for scaled_pop
pets <- pets %>%
  group_by(animal) %>%
  mutate(scaled_pop = scale(PopularityUS2017))

# Create a scatter plot comparing weight to scaled_pop
ggplot(pets, aes(y = MaleWtKg,          # weight on the y axis
                 x = scaled_pop,  # popularity on the x axis
                 color = animal)) +     # color by value of $animal
  ggtitle("Pet Popularity vs Weight") + # a title for this plot
  geom_point() # show a scatter plot