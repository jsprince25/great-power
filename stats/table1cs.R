sink("table1.txt")

# table 1
cs <- readRDS("stouffer_cs.rds")
summary(cs$v35)
table(cs$v35)
ncs <- 100 * prop.table(table(cs$v35))


ls <- readRDS("stouffer_l.rds")
summary(ls$v35)
table(ls$v35)
scl <- 100 * prop.table(table(ls$v35))


joint <- as.table(matrix(c(14,2,84,31,11,59), ncol=3, byrow=TRUE))
dimnames(joint) <- list(
  Cases = c("Selected Community Leaders", "National Cross Section"), 
  Percentage_Answering = c("No", "No Answer", "Yes"))
joint

sink()