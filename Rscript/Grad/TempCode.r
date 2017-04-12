rm(list=ls())
setwd('~/19. UBC 2016 Winter Term 2/STAT 550/Case/Code')

library('ggplot2') # for plotting
library('lme4') # for mixed-effects models
library('reshape2') # for ...
library('dplyr') # for %>%
library('MASS') # for ...
library('base') # for ...

# Import the MWD data.
soil <- read.table('MWD.csv', sep=',', header=T)
soil.unreleveled <- soil
soil$Treatment <- relevel(soil$Treatment, ref='con')
head(soil)
str(soil)
with(soil, table(Treatment,Date))

# Import the cover value data.
pc <- read.csv('plant_cover.csv', header=T)
pc$Block <- as.factor(pc$Block)
head(pc)
str(pc)
with(pc, table(Treatment,Block))
tapply(pc$Cover.value, pc$Treatment, table)


##############################
# Grad Analysis:
##############################

# Obtain the relevant species.
# Note: The client said they are interested in POPR, PSSP, POJU, HECO,
#       ALCE, ANDI, TAOF, SOIL and BRYO, as well as anything with a cover
#       greater than 5% across all the sites.
#     : None of the species occurred in all of the 400 sites (4 blocks x
#       2 treatments × 5 transects × 10 samples per transect), although HECO
#       and LITT were close. The rest occurred in below half of the sites.
#       So, we change our criterion to choose species whose averages over
#       the (occurring) transects and plots, for every block-treatment
#       combination, are all greater than 5%.
summary(pc$Species)
all.species <- levels(pc$Species)
interesting <- c('POPR', 'PSSP', 'POJU', 'HECO', 'ALCE', 'ANDI', 'TAOF',
                 'SOIL', 'BRYO')
with(pc, table(Species, Block))
with(pc, table(Species, Treatment))
species.vec <- NULL
for (ii in 1:length(all.species))
{
  # Obtain the subset of the data corresponding to this species.
  species.try <- all.species[ii]
  pc.sub.try <- subset(pc, Species==species.try)
  pc.sub.try <- pc.sub.try[ ,c(3,4,5,6,9)]
  
  # Check if this species occurrs in every block-treatment combination.
  # Note: entries.try is the table of counts for each block, by treatment
  #       group, for this species.
  entries.try <- table(pc.sub.try$Block, pc.sub.try$Treatment)
  which.entries.miss.try <- which(entries.try==0, arr.ind=TRUE)
  is.in.all.combs <- ifelse(test=(nrow(which.entries.miss.try)==0), yes=1,
                            no=0)
  
  # Obtain averages over the (occurring) transects and plots, for every
  # (occurring) block-treatment combination.
  by_blockTrt.try <- group_by(pc.sub.try, Block, Treatment)
  #dat.avg.try <- summarise(by_blockTrt.try, y.avg=sum(Cover.value)/50)
  dat.avg.try2 <- summarise(by_blockTrt.try, y.avg=mean(Cover.value))
  print(species.try)
  print(dat.avg.try2)
  all.occurring.greater.than5 <- ifelse(test=(sum(dat.avg.try2$y.avg<5)==0),
                                        yes=1, no=0)
  all.greater.than5 <- is.in.all.combs * all.occurring.greater.than5
  
  # Determine whether the species should be included.
  if ((species.try%in%interesting) || (all.greater.than5))
  {
    species.vec <- c(species.vec, species.try)
  }
}
print(species.vec)

# Obtain the MWD averages (to be used in calculating the correlations).
a <- group_by(soil.unreleveled, Block, Treatment) %>%
     summarise(mean(MWD))
b <- a$`mean(MWD)`
mean.MWD <- b

# Loop over all the relevant species.
model.avg.list <-list()
p.vals.vec <- rep(NA, times=length(species.vec))
coefs.vec <- rep(NA, times=length(species.vec))
nonparam.model.list <-list()
nonparam.p.vals.vec <- rep(NA, times=length(species.vec))
nonparam.coefs.vec <- rep(NA, times=length(species.vec))
cor.vec <- rep(NA, times=length(species.vec))
for (ii in 1:length(species.vec))
{
  # Obtain the subset of the data corresponding to this species.
  this.species <- species.vec[ii]
  pc.this.species <- subset(pc, Species==this.species)
  pc.this.species <- pc.this.species[ ,c(3,4,9)]
  print(this.species)
  
  # Table of counts for each class of cover value, by treatment group, for
  # this species.
  #print(tapply(pc.this.species$Cover.value, pc.this.species$Treatment, table))
  
  # Table of counts for each block, by treatment group, for this species.
  entries <- table(pc.this.species$Block, pc.this.species$Treatment)
  which.entries.miss <- which(t(entries)==0)
  which.entries.miss.ind <- which(entries==0, arr.ind=TRUE)
  
  # Obtain averages over the transects and plots.
  by_blockTrt.this.species <- group_by(pc.this.species, Block, Treatment)
  dat.avg.this.species <- summarise(by_blockTrt.this.species,
                                    y.avg=sum(Cover.value)/50)
  if (length(which.entries.miss) > 0)
  {
    for (jj in 1:length(which.entries.miss))
    {
      which.row <- which.entries.miss[jj]
      which.block <- toString(which.entries.miss.ind[jj,1])
      which.trt <- ifelse(test=(which.entries.miss.ind[jj,2]==1), yes='Biosolids',
                          no='Control')
      new.row <- c(which.block, which.trt, 0)
      dat.avg.this.species <- rbind(dat.avg.this.species[1:(which.row-1),],
                                    new.row,
                                    dat.avg.this.species[-(1:(which.row-1)),])
    }
  }
  dat.avg.this.species$y.avg <- as.numeric(dat.avg.this.species$y.avg)
  print(dat.avg.this.species)
  
  #########################
  # Plots:
  #########################
  
  # Histogram of cover value for this species.
  hist(pc.this.species$Cover.value,
       main=paste('Histogram of cover value for ',this.species,sep=''),
       xlab=paste(this.species,' Cover Value (%)',sep=''), col='mistyrose')
  
  # Normal Q-Q plot of cover value for this species.
  # Note: Strange behaviour due to discreteness.
  #qqnorm(pc.this.species$Cover.value)
  
  # Boxplots of cover value for this species, for the two treatments.
  #print(ggplot(aes(y=Cover.value, x=Treatment, fill=Treatment, alpha=0.4),
  #             data=pc.this.species) +
  #      geom_boxplot() +
  #      geom_point())
  
  # Boxplot of cover value for this species, for the two treatments,
  # across the four blocks.
  #print(ggplot(aes(y=Cover.value, x=Treatment, fill=Treatment, alpha=0.4),
  #             data=pc.this.species) +
  #      geom_boxplot() +
  #      geom_point() +
  #      facet_wrap(~Block))
  
  # Boxplot of average cover value for this species, for the two treatments.
  print(ggplot(aes(y=y.avg, x=Treatment, fill=Treatment, alpha=0.4),
                   data=dat.avg.this.species) +
        geom_boxplot() +
        geom_point() +
        labs(y=paste(this.species,' Cover Value (%)',sep=''),
             title=paste('Boxplot of ',this.species,' Cover Value',sep='')))
  
  # Interaction plot for block and treatment, with cover value for this
  # species as the response.
  #print(ggplot(aes(x=Block, y=Cover.value, group=Treatment,
  #                 colour=Treatment),
  #             data=pc.this.species) +
  #      stat_summary(fun.y='mean', geom='line') +
  #      labs(x='Block', y=paste(this.species,' Cover Value',sep=''),
  #           title=paste('Change in ',this.species,'
  #                        Cover Value over Different Blocks'),sep=''))
  
  # Interaction plot for block and treatment, with average cover value for 
  # this species as the response.
  #print(ggplot(aes(x=Block, y=y.avg, group=Treatment, colour=Treatment),
  #             data=dat.avg.this.species) +
  #      geom_point() +
  #      geom_line() +
  #      labs(x='Block', y=paste(this.species,' Cover Value (%)',sep=''),
  #           title=paste('Comparison of ',this.species,
  #                       ' Cover Values between Biosolids and Control',
  #                       sep='')))
  
  #########################
  # Modelling cover value:
  #########################
  
  # Linear regression of average cover value for this species, with treatment
  # effect.
  dat.avg.this.species$Treatment <- relevel(dat.avg.this.species$Treatment,
                                            ref='Control')
  model_avg.this.species <- lm(as.numeric(y.avg)~Treatment,
                               data=dat.avg.this.species)
  print(summary(model_avg.this.species))
  model.avg.list[[ii]] <- model_avg.this.species
  p.vals.vec[ii] <- summary(model_avg.this.species)$coefficients[2,4]
  coefs.vec[ii] <- summary(model_avg.this.species)$coefficients[2,1]
  
  #########################
  # Non-parametric test:
  #########################
  
  # Parametric tests:
  #t.test(x=dat.avg.this.species$y.avg[dat.avg.this.species$Treatment=='Biosolids'],
  #       y=dat.avg.this.species$y.avg[dat.avg.this.species$Treatment=='Control'],
  #       alternative='two.sided', mu=0, paired=FALSE, var.equal=TRUE,
  #       conf.level=0.95)
  #t.test(x=dat.avg.this.species$y.avg[dat.avg.this.species$Treatment=='Biosolids'],
  #       y=dat.avg.this.species$y.avg[dat.avg.this.species$Treatment=='Control'],
  #       alternative='two.sided', mu=0, paired=TRUE, var.equal=TRUE,
  #       conf.level=0.95)
  
  # Non-parametric test:
  wilcox.this.species <- wilcox.test(x=dat.avg.this.species$y.avg[dat.avg.this.species$Treatment=='Biosolids'],
                                     y=dat.avg.this.species$y.avg[dat.avg.this.species$Treatment=='Control'],
                                     alternative='two.sided', mu=0, paired=TRUE,
                                     conf.int=TRUE, conf.level=0.95)
  nonparam.model.list[[ii]] <- wilcox.this.species
  nonparam.p.vals.vec[ii] <- wilcox.this.species$p.value
  nonparam.coefs.vec[ii] <- wilcox.this.species$estimate
  
  #########################
  # Correlations:
  #########################
  
  # Find the correlation between MWD and the average cover value for this
  # species.
  mean.cv.this.species <- as.numeric(as.character(dat.avg.this.species$y.avg))
  cor.vec[ii] <- cor(mean.MWD, mean.cv.this.species)
}

# Print the linear regression p-values and coefficients for each species.
cbind(species.vec, p.vals.vec, coefs.vec)

# Print the non-parametric p-values and estimates for each species.
cbind(species.vec, nonparam.p.vals.vec, nonparam.coefs.vec)

# Print the correlations between MWD and each species' cover value.
cbind(species.vec, cor.vec)

