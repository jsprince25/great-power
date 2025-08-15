sink("report-var21.txt")

setwd("/Users/josephprince/Desktop/Github/great-power/stats")
cs <- readRDS("stouffer_cs.rds")
ls <- readRDS("stouffer_l.rds")


cs2114<- xtabs(~ v21 + v14, data= cs)
fcs2114<- cs2114[-c(5,6),-5]
# dimnames(fcs2114) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v14 Communists in U.S."= c("Mentioned as Most Important","Mentioned as Next Most Important","Talked About, Not Most or Next Most Important","Not Talked About, Not Most or Next Most Important"))
dimnames(fcs2114) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v21 and v14 cs\n")
fcs2114
chisq.test(fcs2114)

ls2114 <- xtabs(~v21 + v14, data= ls)
fls2114 <- ls2114[-c(5,6),-5]
dimnames(fls2114) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v21 and v14 ls\n")
fls2114
chisq.test(fls2114)

cs2115<- xtabs(~ v21 + v15, data= cs)
fcs2115<- cs2115[-c(5,6),-5]
dimnames(fcs2115) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v15 Comm Threat Freedom U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v21 and v15 cs\n")
fcs2115
chisq.test(fcs2114)

ls2115 <- xtabs(~v21 + v15, data= ls)
fls2115 <- ls2115[-c(5,6),-5]
dimnames(fls2115) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v15 Comm Threat Freedom U.S"= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v21 and v15 ls\n")
fls2115
chisq.test(fls2115)

cs2135<- xtabs(~ v21 + v35, data= cs)
fcs2135<- cs2135[-c(5,6),-3]
dimnames(fcs2135) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v21 and v35 cs\n")
fcs2135
chisq.test(fcs2135)

ls2135 <- xtabs(~v21 + v35, data= ls)
fls2135 <- ls2135[-c(5,6),-3]
dimnames(fls2135) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v21 and v35 ls\n")
fls2135
chisq.test(fls2135)

cs2136<- xtabs(~ v21 + v36, data= cs)
fcs2136<- cs2136[-c(5,6),-3]
dimnames(fcs2136) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v21 and v36 cs\n")
fcs2136
chisq.test(fcs2136)

ls2136 <- xtabs(~v21 + v36, data= ls)
fls2136 <- ls2136[-c(5,6),-3]
dimnames(fls2136) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v21 and v36 ls\n")
fls2136
chisq.test(fls2136)

cs2137<- xtabs(~ v21 + v37, data= cs)
fcs2137<- cs2137[-c(5,6),-3]
dimnames(fcs2137) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v21 and v37 cs\n")
fcs2137
chisq.test(fcs2137)

ls2137 <- xtabs(~v21 + v37, data= ls)
fls2137 <- ls2137[-c(5,6),-3]
dimnames(fls2137) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v21 and v37 ls\n")
fls2137
chisq.test(fls2137)

cs2142<- xtabs(~ v21 + v42, data= cs)
fcs2142<- cs2142[-c(5,6),-6]
dimnames(fcs2142) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v21 and v42 cs\n")
fcs2142
chisq.test(fcs2142)

ls2142 <- xtabs(~v21 + v42, data= ls)
fls2142 <- ls2142[-c(5,6),-6]
dimnames(fls2142) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v21 and v42 ls\n")
fls2142
chisq.test(fls2142)

# messy due to v43a v43b v43c thing

cs_long <- data.frame(
  v21 = rep(cs$v21, 3),
  v43 = c(cs$v43a, cs$v43b, cs$v43c)
)

cs2143<- xtabs(~ v21 + v43, data = cs_long)

# cs2143<- xtabs(~ v21 + v43a, data= cs)
fcs2143<- cs2143[-c(5,6),-c(1,9,10)]
dimnames(fcs2143) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v21 and v43 cs\n")
fcs2143
chisq.test(fcs2143)

ls_long <- data.frame(
  v21 = rep(ls$v21, 3),
  v43 = c(ls$v43a, ls$v43b, ls$v43c)
)

ls2143<- xtabs(~ v21 + v43, data = ls_long)
fls2143 <- ls2143[-c(5,6),-c(1,9,10)]
dimnames(fls2143) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v21 and v43 ls\n")
fls2143
chisq.test(fls2143)

cs2144<- xtabs(~ v21 + v44, data= cs)
fcs2144<- cs2144[-c(5,6),-c(4)]
dimnames(fcs2144) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v21 and v44 cs\n")
fcs2144
chisq.test(fcs2144)

ls2144 <- xtabs(~v21 + v44, data= ls)
fls2144 <- ls2144[-c(5,6),-c(4)]
dimnames(fls2144) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v21 and v44 ls\n")
fls2144
chisq.test(fls2144)

cat("skipping v45 out of confusion with the table, multiple rounds of questions, etc\n" )
cat("\n")
#messy again

csv46 <- data.frame(
  v21 = rep(cs$v21, 3),
  v46 = c(cs$v46a, cs$v46b, cs$v46c)
)

cs2146<- xtabs(~ v21 + v46, data= csv46)
fcs2146<- cs2146[-c(5,6),-c(1,10)]
dimnames(fcs2146) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v21 and v46 cs\n")
fcs2146
chisq.test(fcs2146)

lsv46 <- data.frame(
  v21 = rep(ls$v21, 3),
  v46 = c(ls$v46a, ls$v46b, ls$v46c)
)

ls2146 <- xtabs(~v21 + v46, data= lsv46)
fls2146 <- ls2146[-c(5,6),-c(1,10)]
dimnames(fls2146) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v21 and v46 ls\n")
fls2146
chisq.test(fls2146)

cs2147<- xtabs(~ v21 + v47, data= cs)
fcs2147<- cs2147[-c(5,6),-3]
dimnames(fcs2147) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v21 and v47 cs\n")
fcs2147
chisq.test(fcs2147)

ls2147 <- xtabs(~v21 + v47, data= ls)
fls2147 <- ls2147[-c(5,6),-3]
dimnames(fls2147) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v21 and v47 ls\n")
fls2147
chisq.test(fls2147)

#messy again

csv48 <- data.frame(
  v21 = rep(cs$v21, 3),
  v48 = c(cs$v48a, cs$v48b, cs$v48c)
)

cs2148<- xtabs(~ v21 + v48, data= csv48)
fcs2148<- cs2148[-c(5,6),-c(1,6)]
dimnames(fcs2148) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v21 and v48 cs\n")
fcs2148
chisq.test(fcs2148)

lsv48 <- data.frame(
  v21 = rep(ls$v21, 3),
  v48 = c(ls$v48a, ls$v48b, ls$v48c)
)

ls2148 <- xtabs(~v21 + v48, data= lsv48)
fls2148 <- ls2148[-c(5,6),-c(1,6)]
dimnames(fls2148) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v21 and v48 ls\n")
fls2148
chisq.test(fls2148)

cs2149<- xtabs(~ v21 + v49, data= cs)
fcs2149<- cs2149[-c(5,6),-c(3,4)]
dimnames(fcs2149) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v21 and v49 cs\n")
fcs2149
chisq.test(fcs2149)

ls2149 <- xtabs(~v21 + v49, data= ls)
fls2149 <- ls2147[-c(5,6),-c(3,4)]
dimnames(fls2149) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v21 and v49 ls\n")
fls2149
chisq.test(fls2149)

cs2150<- xtabs(~ v21 + v50, data= cs)
fcs2150<- cs2150[-c(5,6),-c(3,4,5)]
dimnames(fcs2150) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v21 and v50 cs\n")
fcs2150
chisq.test(fcs2150)

ls2150 <- xtabs(~v21 + v50, data= ls)
fls2150 <- ls2150[-c(5,6),-c(3,4,5)]
dimnames(fls2150) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v21 and v50 ls\n")
fls2150
chisq.test(fls2150)

cs2151<- xtabs(~ v21 + v51, data= cs)
fcs2151<- cs2151[-c(5,6),-c(3,4,5)]
dimnames(fcs2151) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v21 and v51 cs\n")
fcs2151
chisq.test(fcs2151)

ls2151 <- xtabs(~v21 + v51, data= ls)
fls2151 <- ls2151[-c(5,6),-c(3,4,5)]
dimnames(fls2151) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v21 and v51 ls\n")
fls2151
chisq.test(fls2151)

cs2152<- xtabs(~ v21 + v52, data= cs)
fcs2152<- cs2152[-c(5,6),-c(3,4)]
dimnames(fcs2152) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v21 and v52 cs\n")
fcs2152
chisq.test(fcs2152)

ls2152 <- xtabs(~v21 + v52, data= ls)
fls2152 <- ls2152[-c(5,6),-c(3,4)]
dimnames(fls2152) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v21 and v52 ls\n")
fls2152
chisq.test(fls2152)


#messy again

csv53 <- data.frame(
  v21 = rep(cs$v21, 3),
  v53 = c(cs$v53a, cs$v53b, cs$v53c)
)

cs2153<- xtabs(~ v21 + v53, data= csv53)
fcs2153<- cs2153[-c(5,6),-c(9,10,11)]
dimnames(fcs2153) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v21 and v53 cs\n")
fcs2153
chisq.test(fcs2153)

lsv53 <- data.frame(
  v21 = rep(ls$v21, 3),
  v53 = c(ls$v53a, ls$v53b, ls$v53c)
)

ls2153 <- xtabs(~v21 + v53, data= lsv53)
fls2153 <- ls2153[-c(5,6),-c(9,10,11)]
dimnames(fls2153) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v21 and v53 ls\n")
fls2153
chisq.test(fls2153)

#messy again
csv55 <- data.frame(
  v21 = rep(cs$v21, 3),
  v55 = c(cs$v55a, cs$v55b, cs$v55c)
)

cs2155<- xtabs(~ v21 + v55, data= csv55)
fcs2155<- cs2155[-c(5,6),-c(1,9)]
dimnames(fcs2155) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v21 and v55 cs\n")
fcs2155
chisq.test(fcs2155)

lsv55 <- data.frame(
  v21 = rep(ls$v21, 3),
  v55 = c(ls$v55a, ls$v55b, ls$v55c)
)

ls2155 <- xtabs(~v21 + v55, data= lsv55)
fls2155 <- ls2155[-c(5,6),-c(1,9)]
dimnames(fls2155) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v21 and v55 ls\n")
fls2155
chisq.test(fls2155)

cs2157<- xtabs(~ v21 + v57, data= cs)
fcs2157<- cs2157[-c(5,6),-c(3,4)]
dimnames(fcs2157) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v21 and v57 cs\n")
fcs2157
chisq.test(fcs2157)

ls2157 <- xtabs(~v21 + v57, data= ls)
fls2157 <- ls2157[-c(5,6),-c(3,4)]
dimnames(fls2157) <- list("v21 For U.S. Best Way Deal Russia"= c("Talk over Problems","Have Nothing to do with Russia","Fight Russia","Qualified"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v21 and v57 ls\n")
fls2157
chisq.test(fls2157)

sink()
