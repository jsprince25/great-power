sink("report-var20.txt")

setwd("/Users/josephprince/Desktop/Github/great-power/stats")
cs <- readRDS("stouffer_cs.rds")
ls <- readRDS("stouffer_l.rds")


cs2014<- xtabs(~ v20 + v14, data= cs)
fcs2014<- cs2014[-c(5,6),-5]
# dimnames(fcs2014) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia iff Allies help","Other"), "v14 Communists in U.S."= c("Mentioned as Most Important","Mentioned as Next Most Important","Talked About, Not Most or Next Most Important","Not Talked About, Not Most or Next Most Important"))
dimnames(fcs2014) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v20 and v14 cs\n")
fcs2014
chisq.test(fcs2014)

ls2014 <- xtabs(~v20 + v14, data= ls)
fls2014 <- ls2014[-c(5,6),-5]
dimnames(fls2014) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v20 and v14 ls\n")
fls2014
chisq.test(fls2014)

cs2015<- xtabs(~ v20 + v15, data= cs)
fcs2015<- cs2015[-c(5,6),-5]
dimnames(fcs2015) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v15 Comm Threat Freedom U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v20 and v15 cs\n")
fcs2015
chisq.test(fcs2014)

ls2015 <- xtabs(~v20 + v15, data= ls)
fls2015 <- ls2015[-c(5,6),-5]
dimnames(fls2015) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v15 Comm Threat Freedom U.S"= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v20 and v15 ls\n")
fls2015
chisq.test(fls2015)

cs2035<- xtabs(~ v20 + v35, data= cs)
fcs2035<- cs2035[-c(5,6),-3]
dimnames(fcs2035) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v20 and v35 cs\n")
fcs2035
chisq.test(fcs2035)

ls2035 <- xtabs(~v20 + v35, data= ls)
fls2035 <- ls2035[-c(5,6),-3]
dimnames(fls2035) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v20 and v35 ls\n")
fls2035
chisq.test(fls2035)

cs2036<- xtabs(~ v20 + v36, data= cs)
fcs2036<- cs2036[-c(5,6),-3]
dimnames(fcs2036) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v20 and v36 cs\n")
fcs2036
chisq.test(fcs2036)

ls2036 <- xtabs(~v20 + v36, data= ls)
fls2036 <- ls2036[-c(5,6),-3]
dimnames(fls2036) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v20 and v36 ls\n")
fls2036
chisq.test(fls2036)

cs2037<- xtabs(~ v20 + v37, data= cs)
fcs2037<- cs2037[-c(5,6),-3]
dimnames(fcs2037) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v20 and v37 cs\n")
fcs2037
chisq.test(fcs2037)

ls2037 <- xtabs(~v20 + v37, data= ls)
fls2037 <- ls2037[-c(5,6),-3]
dimnames(fls2037) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v20 and v37 ls\n")
fls2037
chisq.test(fls2037)

cs2042<- xtabs(~ v20 + v42, data= cs)
fcs2042<- cs2042[-c(5,6),-6]
dimnames(fcs2042) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v20 and v42 cs\n")
fcs2042
chisq.test(fcs2042)

ls2042 <- xtabs(~v20 + v42, data= ls)
fls2042 <- ls2042[-c(5,6),-6]
dimnames(fls2042) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v20 and v42 ls\n")
fls2042
chisq.test(fls2042)

# messy due to v43a v43b v43c thing

cs_long <- data.frame(
  v20 = rep(cs$v20, 3),
  v43 = c(cs$v43a, cs$v43b, cs$v43c)
)

cs2043<- xtabs(~ v20 + v43, data = cs_long)

# cs2043<- xtabs(~ v20 + v43a, data= cs)
fcs2043<- cs2043[-c(5,6),-c(1,9,10)]
dimnames(fcs2043) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v20 and v43 cs\n")
fcs2043
chisq.test(fcs2043)

ls_long <- data.frame(
  v20 = rep(ls$v20, 3),
  v43 = c(ls$v43a, ls$v43b, ls$v43c)
)

ls2043<- xtabs(~ v20 + v43, data = ls_long)
fls2043 <- ls2043[-c(5,6),-c(1,9,10)]
dimnames(fls2043) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v20 and v43 ls\n")
fls2043
chisq.test(fls2043)

cs2044<- xtabs(~ v20 + v44, data= cs)
fcs2044<- cs2044[-c(5,6),-c(4)]
dimnames(fcs2044) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v20 and v44 cs\n")
fcs2044
chisq.test(fcs2044)

ls2044 <- xtabs(~v20 + v44, data= ls)
fls2044 <- ls2044[-c(5,6),-c(4)]
dimnames(fls2044) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v20 and v44 ls\n")
fls2044
chisq.test(fls2044)

cat("skipping v45 out of confusion with the table, multiple rounds of questions, etc\n" )
cat("\n")
#messy again

csv46 <- data.frame(
  v20 = rep(cs$v20, 3),
  v46 = c(cs$v46a, cs$v46b, cs$v46c)
)

cs2046<- xtabs(~ v20 + v46, data= csv46)
fcs2046<- cs2046[-c(5,6),-c(1,10)]
dimnames(fcs2046) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v20 and v46 cs\n")
fcs2046
chisq.test(fcs2046)

lsv46 <- data.frame(
  v20 = rep(ls$v20, 3),
  v46 = c(ls$v46a, ls$v46b, ls$v46c)
)

ls2046 <- xtabs(~v20 + v46, data= lsv46)
fls2046 <- ls2046[-c(5,6),-c(1,10)]
dimnames(fls2046) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v20 and v46 ls\n")
fls2046
chisq.test(fls2046)

cs2047<- xtabs(~ v20 + v47, data= cs)
fcs2047<- cs2047[-c(5,6),-3]
dimnames(fcs2047) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v20 and v47 cs\n")
fcs2047
chisq.test(fcs2047)

ls2047 <- xtabs(~v20 + v47, data= ls)
fls2047 <- ls2047[-c(5,6),-3]
dimnames(fls2047) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v20 and v47 ls\n")
fls2047
chisq.test(fls2047)

#messy again

csv48 <- data.frame(
  v20 = rep(cs$v20, 3),
  v48 = c(cs$v48a, cs$v48b, cs$v48c)
)

cs2048<- xtabs(~ v20 + v48, data= csv48)
fcs2048<- cs2048[-c(5,6),-c(1,6)]
dimnames(fcs2048) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v20 and v48 cs\n")
fcs2048
chisq.test(fcs2048)

lsv48 <- data.frame(
  v20 = rep(ls$v20, 3),
  v48 = c(ls$v48a, ls$v48b, ls$v48c)
)

ls2048 <- xtabs(~v20 + v48, data= lsv48)
fls2048 <- ls2048[-c(5,6),-c(1,6)]
dimnames(fls2048) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v20 and v48 ls\n")
fls2048
chisq.test(fls2048)

cs2049<- xtabs(~ v20 + v49, data= cs)
fcs2049<- cs2049[-c(5,6),-c(3,4)]
dimnames(fcs2049) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v20 and v49 cs\n")
fcs2049
chisq.test(fcs2049)

ls2049 <- xtabs(~v20 + v49, data= ls)
fls2049 <- ls2047[-c(5,6),-c(3,4)]
dimnames(fls2049) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v20 and v49 ls\n")
fls2049
chisq.test(fls2049)

cs2050<- xtabs(~ v20 + v50, data= cs)
fcs2050<- cs2050[-c(5,6),-c(3,4,5)]
dimnames(fcs2050) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v20 and v50 cs\n")
fcs2050
chisq.test(fcs2050)

ls2050 <- xtabs(~v20 + v50, data= ls)
fls2050 <- ls2050[-c(5,6),-c(3,4,5)]
dimnames(fls2050) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v20 and v50 ls\n")
fls2050
chisq.test(fls2050)

cs2051<- xtabs(~ v20 + v51, data= cs)
fcs2051<- cs2051[-c(5,6),-c(3,4,5)]
dimnames(fcs2051) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v20 and v51 cs\n")
fcs2051
chisq.test(fcs2051)

ls2051 <- xtabs(~v20 + v51, data= ls)
fls2051 <- ls2051[-c(5,6),-c(3,4,5)]
dimnames(fls2051) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v20 and v51 ls\n")
fls2051
chisq.test(fls2051)

cs2052<- xtabs(~ v20 + v52, data= cs)
fcs2052<- cs2052[-c(5,6),-c(3,4)]
dimnames(fcs2052) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v20 and v52 cs\n")
fcs2052
chisq.test(fcs2052)

ls2052 <- xtabs(~v20 + v52, data= ls)
fls2052 <- ls2052[-c(5,6),-c(3,4)]
dimnames(fls2052) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v20 and v52 ls\n")
fls2052
chisq.test(fls2052)


#messy again

csv53 <- data.frame(
  v20 = rep(cs$v20, 3),
  v53 = c(cs$v53a, cs$v53b, cs$v53c)
)

cs2053<- xtabs(~ v20 + v53, data= csv53)
fcs2053<- cs2053[-c(5,6),-c(9,10,11)]
dimnames(fcs2053) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v20 and v53 cs\n")
fcs2053
chisq.test(fcs2053)

lsv53 <- data.frame(
  v20 = rep(ls$v20, 3),
  v53 = c(ls$v53a, ls$v53b, ls$v53c)
)

ls2053 <- xtabs(~v20 + v53, data= lsv53)
fls2053 <- ls2053[-c(5,6),-c(9,10,11)]
dimnames(fls2053) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v20 and v53 ls\n")
fls2053
chisq.test(fls2053)

#messy again
csv55 <- data.frame(
  v20 = rep(cs$v20, 3),
  v55 = c(cs$v55a, cs$v55b, cs$v55c)
)

cs2055<- xtabs(~ v20 + v55, data= csv55)
fcs2055<- cs2055[-c(5,6),-c(1,9)]
dimnames(fcs2055) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v20 and v55 cs\n")
fcs2055
chisq.test(fcs2055)

lsv55 <- data.frame(
  v20 = rep(ls$v20, 3),
  v55 = c(ls$v55a, ls$v55b, ls$v55c)
)

ls2055 <- xtabs(~v20 + v55, data= lsv55)
fls2055 <- ls2055[-c(5,6),-c(1,9)]
dimnames(fls2055) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v20 and v55 ls\n")
fls2055
chisq.test(fls2055)

cs2057<- xtabs(~ v20 + v57, data= cs)
fcs2057<- cs2057[-c(5,6),-c(3,4)]
dimnames(fcs2057) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v20 and v57 cs\n")
fcs2057
chisq.test(fcs2057)

ls2057 <- xtabs(~v20 + v57, data= ls)
fls2057 <- ls2057[-c(5,6),-c(3,4)]
dimnames(fls2057) <- list("v20 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia i.f.f Allies help","Other"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v20 and v57 ls\n")
fls2057
chisq.test(fls2057)

sink()
