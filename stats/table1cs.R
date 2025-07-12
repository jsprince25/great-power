# table 1
cs <- readRDS("stouffer_cs.rds")
summary(cs$v35)
table(cs$v35)
100 * prop.table(table(cs$v35))
