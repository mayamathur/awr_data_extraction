

################################# PREP ################################# 

data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Data extraction"
code.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Data extraction/awr_data_extraction_git"
setwd(code.dir); source("helper.R")

# prepped dataset
setwd(data.dir)
d = read.csv("prepped_data.csv")

# number of point estimates
nrow(d)

# number of studies
length( unique(d$authoryear) )




################################# OVERALL META-ANALYSIS #################################

##### First Try Regular RE Meta-Analysis ######
# ignores potential correlation of point estimates within papers
# and assumes normality
library(metafor)

( meta = rma.uni( yi = d$logRR,
                  vi = d$varlogRR, 
                  method = "REML",
                  knha = TRUE ) )


##### Check Normality ######
std = (d$logRR - c(meta$b)) / sqrt(c(meta$tau2) + d$varlogRR)
hist(std, breaks=20)
shapiro.test(std)
# not great
# seems heavy-tailed and maybe right-skewed


##### Robust Meta-Analysis ######
# allows for correlated point estimates within studies
# and has no distributional assumptions
library(robumeta)
( meta.rob = robu( logRR ~ 1, 
                   data = d, 
                   studynum = as.factor(authoryear),
                   var.eff.size = varlogRR,
                   modelweights = "HIER",
                   small = TRUE) )


mu = meta.rob$b.r
t2 = meta.rob$mod_info$tau.sq
mu.lo = meta.rob$reg_table$CI.L
mu.hi = meta.rob$reg_table$CI.U
mu.se = meta.rob$reg_table$SE
mu.pval = meta.rob$reg_table$prob



################################# FOREST PLOT #################################

# relative weight of each study in meta-analysis
d$rel.wt = 100 * (1/d$varlogRR) / sum(1/d$varlogRR)


# lean plotting df
keepers = c("logRR",
            "varlogRR",
            "RR.lo",
            "RR.hi",
            "authoryear",
            "unique",
            "rel.wt")
dp = d[ , keepers ]


dp = dp[ order(dp$logRR, decreasing = FALSE), ]


# add pooled point estimates as first rows
# arbitrary relative weight
library(dplyr)
dp = add_row( dp,
              .before = 1,
              logRR = meta.rob$b.r,
              varlogRR = meta.rob$reg_table$SE^2,
              RR.lo = exp( meta.rob$b.r - qnorm(.75) * meta.rob$reg_table$SE ),
              RR.hi = exp( meta.rob$b.r + qnorm(.75) * meta.rob$reg_table$SE ),
              #X.cat = "pooled",
              #Y.cat = "pooled",
              unique = "POOLED",
              rel.wt = 5 )


# make sure POOLED is the first level of the factor variable so it's displayed at bottom
correct.order = dp$unique
dp$unique = factor(dp$unique, levels = correct.order)
levels(dp$unique)


dp$is.pooled = as.factor( c( "Pooled estimate", rep("Individual study", nrow(dp) - 1) ) )


#colors = c("red", "black", "blue")
shapes = c(17, 19, 15)
breaks = seq(0.4, 6.0, .2)

#breaks = exp( seq( log(0.4), log(6), .2 )

library(ggplot2)
ggplot( data = dp, aes( x = exp(logRR), 
                        y = unique,
                        size = rel.wt,
                        shape = is.pooled ) ) +
                        #color = X.intensiveness ) ) +
  geom_errorbarh( aes(xmin = RR.lo,
                      xmax = RR.hi ),
                  lwd = .5,
                  height = .001,
                  color = "black") +
  
  geom_point() +
  
  xlab( "Estimated relative risk of reduced meat" ) +
  ylab("") +
  
  geom_vline(xintercept = 1, lty = 2) +
  
  guides(size = guide_legend("% weight in analysis") ) +
  
  # scale_color_manual(values = colors,
  #                    name = "") +
  
  scale_shape_manual(values = shapes,
                     name = "") +
  #guide=FALSE) +
  
  scale_x_continuous( breaks = breaks,
                      lim = c(breaks[1], breaks[length(breaks)] ),
                      trans = "log10") +
  
  theme_bw()
  
  #facet_wrap( ~Y.cat)  # to facet by outcome measure type


################################# CALIBRATED ENSEMBLE ESTIMATES #################################

# which individual interventions appeared most effective?
d$ens = my_ens( yi = d$logRR, sei = sqrt(d$varlogRR) )

plot( density(exp(d$ens)), main = "Ensemble estimates' KDE" )

sort(d$ens)


################################# PHAT ################################# 

##### Proportion above RR = 1.1 ######
# parametric: 94%
prop_stronger( q = log(1.1), 
               M = mu,
               t2 = t2,
               se.M = NA,
               se.t2 = NA,
               tail = "above",
               dat = d,
               R = 500)  

# nonparametrically using quantiles of ensemble estimates: 59%
sum( d$ens > log(1.1) ) / length( d$ens )

##### Proportion above RR=1.5 ######
# parametric: 12%
library(MetaUtility)
prop_stronger( q = log(1.5), 
               M = mu,
               t2 = t2,
               se.M = NA,
               se.t2 = NA,
               tail = "above",
               dat = d,
               R = 500)  

# nonparametrically: 51%
sum( d$ens > log(1.5) ) / length( d$ens )



# ################################# FOR PASTING INTO WANG'S SPREADSHEET ################################# 
# 
# temp = data.frame( unique = d$study, 
#                    logRR = d$logRR, 
#                    se = d$logRR.se )
# 
# write.csv(temp, "bianchi_data_for_wang_spreadsheet.csv", row.names = FALSE)


################################# PUBLICATION BIAS ################################# 

##### Significance Funnel ######
library(PublicationBias)
significance_funnel(yi = d$logRR,
                    vi = d$varlogRR)

# affirmative vs. non-affirmative
d$pval = 2 * ( 1 - pnorm( abs(d$logRR) / sqrt(d$varlogRR) ) )
d$affirm = d$pval < 0.05 & d$logRR > 0
table(d$affirm)

# meta-analyze only the nonaffirmatives
# 2-sided pval
( meta.worst = robu( yi ~ 1, 
                     data = d[ d$affirm == FALSE, ], 
                     studynum = authoryear,
                     var.eff.size = varlogRR,
                     modelweights = "HIER",
                     small = TRUE) )
# mu = meta.rob$b.r
# t2 = meta.rob$mod_info$tau.sq
# mu.lo = meta.rob$reg_table$CI.L
# mu.hi = meta.rob$reg_table$CI.U
# mu.se = meta.rob$reg_table$SE

# cannot really use selection models given substantial clustering
# and non-normality
res = svalue( yi = d$logRR,
              vi = d$varlogRR,
              q = log(1), 
              clustervar = d$authoryear,
              model = "robust" )

res$svals
# quite robust to publication bias



# ~~~~ below not modified from Bianchi work

################################# MODERATION BY TYPE OF APPEAL #################################

##### Regress on X type ######

# reference level: animals
levels(d$X.cat)

# sample size too small to say much 
# all CIs quite wide
library(robumeta)
( meta.rob.Xcat = robu( logRR ~ X.cat, 
                        data = d, 
                        studynum = as.factor(group.with),
                        var.eff.size = logRR.se^2,
                        modelweights = "HIER",
                        small = TRUE) )

##### Just Animals ######
library(robumeta)
( meta.rob.animals = robu( logRR ~ 1, 
                           data = d[d$X.cat == "animals",], 
                           studynum = as.factor(group.with),
                           var.eff.size = logRR.se^2,
                           modelweights = "HIER",
                           small = TRUE) )
# again, very imprecise estimates 

################################# MODERATION BY OUTCOME TYPE #################################

table(d$Y.cat)
( meta.rob.Ycat = robu( logRR ~ Y.cat, 
                        data = d, 
                        studynum = as.factor(group.with),
                        var.eff.size = logRR.se^2,
                        modelweights = "HIER",
                        small = TRUE) )
# again, little precision here


################################# MODERATION BY INTENSIVENESS #################################

table(d$X.intensiveness)

( meta.rob.Xintens = robu( logRR ~ X.intensiveness, 
                           data = d, 
                           studynum = as.factor(group.with),
                           var.eff.size = logRR.se^2,
                           modelweights = "HIER",
                           small = TRUE) )
# seems like intense ones may be more effective, but low precision

################################# MODERATION BY TIME LAG #################################

# plot time lag vs. ensemble estimate
ggplot( data = d, 
        aes( x = Y.lag.days,
             y = ens ) ) +
  geom_point() +
  theme_bw()

