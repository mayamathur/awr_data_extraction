
# MM audited XXXX

############################### PRELIMINARIES ###############################

data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction"

library(dplyr)
library(tidyverse)
library(naniar)
library(tidyr)
library(readxl)
library(irr)
library(testthat)

setwd(data.dir)
setwd("Dual review of intervention components")

# read in each coder's dataset
dd = read.csv("component_coding_dr.csv")  # DR
dm = read_xlsx("component_coding_mm.xlsx")  # MBM

dats = list(dd, dm)


############################### PREP INDIVIDUAL CODERS' SUBJECTIVE RISKS OF BIAS ###############################


# remove blank spacer rows
dats = lapply( dats, FUN = function(.d) .d = .d[ !is.na(.d$authoryear), ] )
expect_equal( unlist( lapply(dats, nrow) ), c(100, 100) )

# recode missing data in all columns
dats = lapply( dats, FUN = function(.d) .d %>% replace_with_na_all( condition = ~.x == "Unclear" ) )

# sanity check: each var should be only in {0,1}
rated.vars = c("mind.attribution", "social.norms", "id.victim", "impl.suggest", "pets")
lapply( dats, FUN = function(.d) apply(.d[,rated.vars], 2, table) )

# cast the 0/1s as numeric
dats = lapply( dats, FUN = function(.d) .d %>% mutate_at( rated.vars, as.numeric ) )
lapply( dats, FUN = function(.d) apply(.d[,rated.vars], 2, table) )


# initialize empty dataframe of interrater discrepancies with same headers
diffs = dats[[1]]
diffs[ , c(rated.vars, "notes") ] = NA
diffs = diffs %>% mutate_at( rated.vars, as.numeric )

diffs = matrix( NA, nrow = nrow(dats[[1]]), ncol = length(rated.vars) )

for( i in 1:nrow(diffs) ){
  diffs[i,] = my_diff_NA( dats[[1]][i, rated.vars], dats[[2]][i, rated.vars ] )
}

# put back the other columns
diffs2 = cbind( data.frame( authoryear = dats[[1]]$authoryear,
                            unique = dats[[1]]$unique ), 
                as.data.frame( diffs ),
                data.frame( notesDR = dats[[1]]$notes, 
                            notesMBM = dats[[2]]$notes ) )

names(diffs2)[3:7] = rated.vars

View(diffs2)


############################### SAVE DATASETS ###############################

setwd(data.dir)
setwd("Dual review of intervention components")

write.csv(diffs2, "interrater_discrepancies.csv")
write.csv(dats[[1]], "component_coding_dr_prepped.csv")
write.csv(dats[[2]], "component_coding_mm_prepped.csv")



