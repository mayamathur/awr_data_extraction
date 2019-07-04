
# they fit a cumulative logit mixed model
# for direct comparability with other meta-analyzed studies, 
#  we are instead dichotomizing at low vs. high consumption using Anderson's own criteria
#  (i.e., low = eating pork <=3X/month; high = more than that; see pg 22)

original.data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Literature search/Full texts for review"
setwd(original.data.dir)
setwd("Anderson 2017 (iAnimal)")

library(data.table)
dtfu = data.table( read.csv("dtfu_prepped_data.csv") ) # I made this by running the initial data-prep section of their code (cast as data table after reading in to avoid errors)

# verbatim from their code (cumulative llogit mixed-model)
#model2dcontrol <- clmm(factor(diet.f2) ~ cond.f + (1|campus), data=dtfu[cond.f != 2])
#model360control <- clmm(factor(diet.f2) ~ cond.f + (1|campus), data=dtfu[cond.f != 1])

# instead use high vs. low
# Poisson link wouldn't fit, so used logistic and then converted to RR
library(lme4)
mod.2d = glmer( lowmeat2 ~ cond.f + (1|campus),
                data=dtfu[cond.f != 2],
                family = "binomial" )
summary(mod.2d)

mod.3d = glmer( lowmeat2 ~ cond.f + (1|campus),
                data=dtfu[cond.f != 1],
                family = "binomial" )
summary(mod.3d)

# convert directly to RR
library(sjstats)
# get SE from CI limit
library(MetaUtility)
scrape_meta( type = "RR",
             est = odds_to_rr(mod.2d)["cond.f", "RR"],
             hi = odds_to_rr(mod.2d)["cond.f", "upper.ci"] )

scrape_meta( type = "RR",
             est = odds_to_rr(mod.3d)["cond.f", "RR"],
             hi = odds_to_rr(mod.3d)["cond.f", "upper.ci"] )




