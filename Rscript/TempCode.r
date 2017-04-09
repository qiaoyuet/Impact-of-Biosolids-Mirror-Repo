
# Obtain the species of interest.
all.species <- c('POPR')
species.vec <- c('POPR')
for (ii in 1:length(all.species))
{
  # Obtain the subset of the data corresponding to this species.
  species.try <- all.species[ii]
  pc.sub.try <- subset(pc, Species==species.try)
  pc.sub.try <- pc.sub.try[ ,c(3,4,9)]
  
  # Table of counts for each block, by treatment group, for this species.
  print(table(pc.this.species$Block, pc.this.species$Treatment))
  
  
  
}






# Obtain the MWD averages (to be used in calculating the correlations).
# WHY REORDERING!?!?!?
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
  
  # Table of counts for each class of cover value, by treatment group, for
  # this species.
  print(tapply(pc.this.species$Cover.value, pc.this.species$Treatment, table))
  
  # Table of counts for each block, by treatment group, for this species.
  entries <- table(pc.this.species$Block, pc.this.species$Treatment)
  which.entries.miss <- which(entries==0, arr.ind=TRUE)
  
  #########################
  # Modelling cover value:
  #########################
  
  # Histogram of cover value for this species.
  hist(pc.this.species$Cover.value,
       main=paste('Histogram of cover value for ',this.species,sep=''),
       xlab=paste(this.species,' Cover Value (%)',sep=''), col='mistyrose')
  
  # Normal Q-Q plot of cover value for this species.
  # Note: Strange behaviour due to discreteness.
  qqnorm(pc.this.species$Cover.value)
  
  # Boxplots of cover value for this species, for the two treatments.
  ggplot(aes(y=Cover.value, x=Treatment, fill=Treatment, alpha=0.4),
         data=pc.this.species) +
    geom_boxplot() +
    geom_point()
  
  # Boxplot of cover value for this species, for the two treatments,
  # across the four blocks.
  ggplot(aes(y=Cover.value, x=Treatment, fill=Treatment, alpha=0.4),
         data=pc.this.species) +
    geom_boxplot() +
    geom_point() +
    facet_wrap(~Block)
  
  # Interaction plot for block and treatment, with cover value for this
  # species as the response.
  ggplot(aes(x=Block, y=Cover.value, group=Treatment, colour=Treatment),
         data=pc.this.species) +
    stat_summary(fun.y='mean', geom='line') +
    labs(x='Block', y=paste(this.species,' Cover Value',sep=''),
         title=paste('Change in ',this.species,'
                     Cover Value over Different Blocks'),sep='')
  
  # Obtain averages over the transects and plots.
  by_blockTrt.this.species <- group_by(pc.this.species, Block, Treatment)
  dat.avg.this.species <- summarise(by_blockTrt.this.species,
                                    y.avg=sum(Cover.value)/50)
  add2control <- c('2', 'Control', 0)
  dat.avg[8, ] <- add2control
  dat.avg$y.avg <- as.numeric(dat.avg$y.avg)
  
  
  
  
  
  
  
  # Interaction plot for block and treatment, with POPR cover value as the
  # response (Figure 2.3).
  # Note: pc.subset has 120 observations of 3 variables: Block, Treatment
  #       and Cover.value.
  #     : dat.avg gives the average cover value for each block-treatment
  #       combination, since 50 is the number of observations you would expect
  #       to see (5 transects × 10 samples per transect), assuming all missing
  #       rows are due to a cover value of 0.
  ggplot(aes(x=Block, y=y.avg, group=Treatment, colour=Treatment), data=dat.avg) +
    geom_point() +
    geom_line() +
    labs(x='Block', y='POPR Cover Value (%)',
         title='Comparison of POPR Cover Values between Biosolids and Control')
  
  # Linear regression of average POPOR cover value, with treatment effect.
  # Note: MS(Treatment)/MS(Treatment:Block) = F statistic from lm output of
  #       adding-zero approach.
  dat.avg$Treatment <- relevel(dat.avg$Treatment, ref='Control')
  model_avg <- lm(as.numeric(y.avg)~Treatment, data=dat.avg)
  summary(model_avg) # p-value of 0.00984 suggests treatment is significant
  
  # Boxplot of POPR cover value over the two treatments (Figure 2.4).
  ggplot(aes(y=y.avg, x=Treatment, fill=Treatment, alpha=0.4), data=dat.avg) +
    geom_boxplot() +
    geom_point() +
    labs(y='POPR Cover Value (%)', title='Boxplot of POPR Cover Value')
  
  
  
  
  
  
  
  
  
  #########################
  # Correlations:
  #########################
  
  # Find the correlation between MWD and each species' cover value.
  this.mean.cv <- as.numeric(as.character(this.dat.avg$y.avg))
  cor.vec[ii] <- cor(mean.MWD, this.mean.cv)
}


# Print



# Print the correlations between MWD and each species' cover value.
cbind(species.vec, cor.vec)


