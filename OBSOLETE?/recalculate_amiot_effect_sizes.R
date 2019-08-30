
original.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review"
setwd(original.data.dir)
setwd("Amiot 2016")

library(foreign)

dat = read.spss("data.sav", to.data.frame=TRUE)

# confirm which variables are total meat consumption at each time point
attr(dat, "variable.labels")[names(dat)=="size"]
attr(dat, "variable.labels")[names(dat)=="xsize"]
attr(dat, "variable.labels")[names(dat)=="ysize"]
# also corroborated by their code (codefile2, line 224)

# sanity check vs. Table 2 stats
# agrees! :)
library(dplyr)
dat %>% group_by(CE) %>%
  summarise(size.mean = mean(size),
            size.sd = sd(size))
dat %>% group_by(CE) %>%
  summarise(size.mean = mean(size),
            size.sd = sd(size))



##### Fit Own Model #####
# whether they were above or below (baseline) median 
#  for total meat consumption at time 3, controlling for 
# (continuous) meat consumption at baseline
# dichtomize at baseline med
bl.med = median(dat$size)
dat$hi.t3 = dat$ysize > bl.med

mod = glm( hi.t3 ~ size + CE, 
     data = dat,
     family = "poisson" )
exp( mod$coefficients["CEExperimental"] )

table(dat$hi.t3, dat$CE)


##### Sensitivity Analysis: Convert SMD Directly #####
dat$change = dat$ysize - dat$size
summary(dat$change)

library(metafor)
es1 = escalc( m2i = mean( dat$change[dat$CE == "Control"] ),
        m1i = mean( dat$change[dat$CE == "Experimental"] ),
        sd2i = sd( dat$change[dat$CE == "Control"] ),
        sd1i = sd( dat$change[dat$CE == "Experimental"] ),
        n2i = length( dat$change[dat$CE == "Control"] ),
        n1i = length( dat$change[dat$CE == "Experimental"] ),
        measure = "SMD"
        )
es2 = d_to_logRR(smd = es1$yi,
           smd.se = sqrt(es1$vi))

exp(es2$logRR)
es2$varlogRR
# fairly different...


