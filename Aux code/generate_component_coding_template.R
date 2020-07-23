
# Generates empty spreadsheet for coders to enter intervention component data

data.dir = "~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction"

library(testthat)

##### Read In Main Dataset #####

# prepped dataset
setwd(data.dir)
d = read.csv("prepped_data.csv")
d = d %>% filter( !is.na(authoryear) )  # because that table has blank rows for prettiness

# main-analysis dataset without SSWS
expect_equal( sum( is.na(d$exclude.main) ), 0 )  # indicator for being a high-bias challenge should never be NA
d = d[ d$use.rr.analysis == 1 & d$exclude.main == 0, ]  
d = droplevels(d)
expect_equal( nrow(d), 100 )

# for component-coding, exclude ones that had no text
d = d %>% filter( !is.na(x.has.text) & x.has.text == 1 )
expect_equal( nrow(d), 83 )


# simplify dataset for coders
d = d %>% select( authoryear, unique, ref ) %>%
  add_column( mind.attribution = "", social.norms = "", id.victim = "", impl.suggest = "", pets = "", notes = "" )

setwd("~/Dropbox/Personal computer/Independent studies/2019/AWR (animal welfare review meat consumption)/Linked to OSF (AWR)/Data extraction/Dual review of intervention components")


# # COMMENTED OUT TO AVOID OVERWRITING PEOPLE'S WORK
# write.csv(d, "component_coding_jp.csv", row.names = FALSE)
# write.csv(d, "component_coding_dr.csv", row.names = FALSE)
# write.csv(d, "component_coding_jn.csv", row.names = FALSE)
# write.csv(d, "component_coding_mm.csv", row.names = FALSE)
