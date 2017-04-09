
# Obtain the species of interest.
species.vec <- c('POPR')



# Obtain the MWD averages (to be used in calculating the correlations).
a <- group_by(soil, Block, Treatment) %>%
  summarise(mean(MWD))
b <- a$`mean(MWD)`
mean.MWD <- c(b[1], b[2], b[3], b[5], b[6], b[7], b[8], b[4])

# Run
cor.vec <- rep(NA, times=length(species.vec))
for (ii in 1:length(species.vec))
{
  # Obtain the subset of the data corresponding to this species.
  this.species <- species.vec[ii]
  pc.this.species <- subset(pc, Species==this.species)
  pc.this.species <- pc.this.species[ ,c(3,4,9)]
  
  
  
  # Boxplots of cover value for the two treatments, across the four
  # blocks.
  ggplot(aes(y=Cover.value, x=Treatment, fill=Treatment, alpha=0.4),
         data=pc.this.species) +
    geom_boxplot() +
    geom_point() +
    facet_wrap(~Block)
  
  
  
  # Find the correlation between MWD and each species' cover value.
  this.mean.cv <- as.numeric(as.character(this.dat.avg$y.avg))
  cor.vec[ii] <- cor(mean.MWD, this.mean.cv)
}


# Print



# Print the correlations between MWD and each species' cover value.
cbind(species.vec, cor.vec)



