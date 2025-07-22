c(4, 5, 6)
c(1,2,3)
rbind(c(1,2,3),c(2,4,9))
as.table(rbind(c(1,2,3),c(2,4,9)))
tab <- as.table(rbind(c(1,2,3),c(2,4,9)))
dimnames(tab)=list(gender = c("M","F",), 
                      party = c("Democrat", "Independent", "Republican"))
tab

tabs <- as.table(matrix(c(762, 327, 468, 484, 239, 477), ncol=3, dimnames=list(gender = c("M","F"), 
                                                                                   party = c("Democrat", "Independent", "Republican"))))
table