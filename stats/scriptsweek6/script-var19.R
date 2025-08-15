sink("report-var19.txt")

setwd("/Users/josephprince/Desktop/Github/great-power/stats")
cs <- readRDS("stouffer_cs.rds")
ls <- readRDS("stouffer_l.rds")


cs1914<- xtabs(~ v19 + v14, data= cs)
fcs1914<- cs1914[-c(5,6),-5]
# dimnames(fcs1914) <- list("v19 To Save Asia from Comm take over, Fight Russia"= c("Let Comm Take Over","Fight Russia","Fight Russia iff Allies help","Other"), "v14 Communists in U.S."= c("Mentioned as Most Important","Mentioned as Next Most Important","Talked About, Not Most or Next Most Important","Not Talked About, Not Most or Next Most Important"))
dimnames(fcs1914) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v19 and v14 cs\n")
fcs1914
chisq.test(fcs1914)

ls1914 <- xtabs(~v19 + v14, data= ls)
fls1914 <- ls1914[-c(5,6),-5]
dimnames(fls1914) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v14 Communists in U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v19 and v14 ls\n")
fls1914
chisq.test(fls1914)

cs1915<- xtabs(~ v19 + v15, data= cs)
fcs1915<- cs1915[-c(5,6),-5]
dimnames(fcs1915) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v15 Comm Threat Freedom U.S."= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v19 and v15 cs\n")
fcs1915
chisq.test(fcs1914)

ls1915 <- xtabs(~v19 + v15, data= ls)
fls1915 <- ls1915[-c(5,6),-5]
dimnames(fls1915) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v15 Comm Threat Freedom U.S"= c("Most Important","Next Most Important","Talked About","Not Talked About"))
cat("v19 and v15 ls\n")
fls1915
chisq.test(fls1915)

cs1935<- xtabs(~ v19 + v35, data= cs)
fcs1935<- cs1935[-c(5,6),-3]
dimnames(fcs1935) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v19 and v35 cs\n")
fcs1935
chisq.test(fcs1935)

ls1935 <- xtabs(~v19 + v35, data= ls)
fls1935 <- ls1935[-c(5,6),-3]
dimnames(fls1935) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v35 Don't Censor Socialist Speaker"= c("Yes","No"))
cat("v19 and v35 ls\n")
fls1935
chisq.test(fls1935)

cs1936<- xtabs(~ v19 + v36, data= cs)
fcs1936<- cs1936[-c(5,6),-3]
dimnames(fcs1936) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v19 and v36 cs\n")
fcs1936
chisq.test(fcs1936)

ls1936 <- xtabs(~v19 + v36, data= ls)
fls1936 <- ls1936[-c(5,6),-3]
dimnames(fls1936) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v36 Don't Censor Socialist Teacher"= c("Yes","No"))
cat("v19 and v36 ls\n")
fls1936
chisq.test(fls1936)

cs1937<- xtabs(~ v19 + v37, data= cs)
fcs1937<- cs1937[-c(5,6),-3]
dimnames(fcs1937) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v19 and v37 cs\n")
fcs1937
chisq.test(fcs1937)

ls1937 <- xtabs(~v19 + v37, data= ls)
fls1937 <- ls1937[-c(5,6),-3]
dimnames(fls1937) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v37 Censor Socialist Book"= c("Yes","No"))
cat("v19 and v37 ls\n")
fls1937
chisq.test(fls1937)

cs1942<- xtabs(~ v19 + v42, data= cs)
fcs1942<- cs1942[-c(5,6),-6]
dimnames(fcs1942) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v19 and v42 cs\n")
fcs1942
chisq.test(fcs1942)

ls1942 <- xtabs(~v19 + v42, data= ls)
fls1942 <- ls1942[-c(5,6),-6]
dimnames(fls1942) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v42 American Comm Danger U.S. Rn"= c("Very Great","Great","Some","Hardly Any","No"))
cat("v19 and v42 ls\n")
fls1942
chisq.test(fls1942)

# messy due to v43a v43b v43c thing

cs_long <- data.frame(
  v19 = rep(cs$v19, 3),
  v43 = c(cs$v43a, cs$v43b, cs$v43c)
)

cs1943<- xtabs(~ v19 + v43, data = cs_long)

# cs1943<- xtabs(~ v19 + v43a, data= cs)
fcs1943<- cs1943[-c(5,6),-c(1,9,10)]
dimnames(fcs1943) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v19 and v43 cs\n")
fcs1943
chisq.test(fcs1943)

ls_long <- data.frame(
  v19 = rep(ls$v19, 3),
  v43 = c(ls$v43a, ls$v43b, ls$v43c)
)

ls1943<- xtabs(~ v19 + v43, data = ls_long)
fls1943 <- ls1943[-c(5,6),-c(1,9,10)]
dimnames(fls1943) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v43 Why Comm Dangerous?"= c("Espionage","Sabotage","Subversion and Conversion","Dangerous, Other","Not bc FBI","Not bc McCathy","Not, Other"))
cat("v19 and v43 ls\n")
fls1943
chisq.test(fls1943)

cs1944<- xtabs(~ v19 + v44, data= cs)
fcs1944<- cs1944[-c(5,6),-c(4)]
dimnames(fcs1944) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v19 and v44 cs\n")
fcs1944
chisq.test(fcs1944)

ls1944 <- xtabs(~v19 + v44, data= ls)
fls1944 <- ls1944[-c(5,6),-c(4)]
dimnames(fls1944) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v44 Can Cmnst be loyal Amrcn"= c("Yes","Qualified","No"))
cat("v19 and v44 ls\n")
fls1944
chisq.test(fls1944)

cat("skipping v45 out of confusion with the table, multiple rounds of questions, etc\n" )
cat("\n")
#messy again

csv46 <- data.frame(
  v19 = rep(cs$v19, 3),
  v46 = c(cs$v46a, cs$v46b, cs$v46c)
)

cs1946<- xtabs(~ v19 + v46, data= csv46)
fcs1946<- cs1946[-c(5,6),-c(1,10)]
dimnames(fcs1946) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v19 and v46 cs\n")
fcs1946
chisq.test(fcs1946)

lsv46 <- data.frame(
  v19 = rep(ls$v19, 3),
  v46 = c(ls$v46a, ls$v46b, ls$v46c)
)

ls1946 <- xtabs(~v19 + v46, data= lsv46)
fls1946 <- ls1946[-c(5,6),-c(1,10)]
dimnames(fls1946) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v46 What do Comm Believe"= c("Russia/World Dom","Loss Individual Rights","Gov Ownership Property","Dictatorship","Egalitarian","Anti-Religon","Immoral/Amoral","Slogan"))
cat("v19 and v46 ls\n")
fls1946
chisq.test(fls1946)

cs1947<- xtabs(~ v19 + v47, data= cs)
fcs1947<- cs1947[-c(5,6),-3]
dimnames(fcs1947) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v19 and v47 cs\n")
fcs1947
chisq.test(fcs1947)

ls1947 <- xtabs(~v19 + v47, data= ls)
fls1947 <- ls1947[-c(5,6),-3]
dimnames(fls1947) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v47 Have you known a Comm"= c("Yes","No"))
cat("v19 and v47 ls\n")
fls1947
chisq.test(fls1947)

#messy again

csv48 <- data.frame(
  v19 = rep(cs$v19, 3),
  v48 = c(cs$v48a, cs$v48b, cs$v48c)
)

cs1948<- xtabs(~ v19 + v48, data= csv48)
fcs1948<- cs1948[-c(5,6),-c(1,6)]
dimnames(fcs1948) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v19 and v48 cs\n")
fcs1948
chisq.test(fcs1948)

lsv48 <- data.frame(
  v19 = rep(ls$v19, 3),
  v48 = c(ls$v48a, ls$v48b, ls$v48c)
)

ls1948 <- xtabs(~v19 + v48, data= lsv48)
fls1948 <- ls1948[-c(5,6),-c(1,6)]
dimnames(fls1948) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v48 How could you tell/know?"= c("It was admitted","What was Said/Read","Groups/Organizations","Deviant/Strange Behavior"))
cat("v19 and v48 ls\n")
fls1948
chisq.test(fls1948)

cs1949<- xtabs(~ v19 + v49, data= cs)
fcs1949<- cs1949[-c(5,6),-c(3,4)]
dimnames(fcs1949) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v19 and v49 cs\n")
fcs1949
chisq.test(fcs1949)

ls1949 <- xtabs(~v19 + v49, data= ls)
fls1949 <- ls1947[-c(5,6),-c(3,4)]
dimnames(fls1949) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v49 Your city asked you to report Comm to FBI"= c("Yes","No"))
cat("v19 and v49 ls\n")
fls1949
chisq.test(fls1949)

cs1950<- xtabs(~ v19 + v50, data= cs)
fcs1950<- cs1950[-c(5,6),-c(3,4,5)]
dimnames(fcs1950) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v19 and v50 cs\n")
fcs1950
chisq.test(fcs1950)

ls1950 <- xtabs(~v19 + v50, data= ls)
fls1950 <- ls1950[-c(5,6),-c(3,4,5)]
dimnames(fls1950) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v50 Heard of other cities citizens asked to report Comm to FBI"= c("Yes","No"))
cat("v19 and v50 ls\n")
fls1950
chisq.test(fls1950)

cs1951<- xtabs(~ v19 + v51, data= cs)
fcs1951<- cs1951[-c(5,6),-c(3,4,5)]
dimnames(fcs1951) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v19 and v51 cs\n")
fcs1951
chisq.test(fcs1951)

ls1951 <- xtabs(~v19 + v51, data= ls)
fls1951 <- ls1951[-c(5,6),-c(3,4,5)]
dimnames(fls1951) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v51 Discussed v50 idea with anyone"= c("Yes","No"))
cat("v19 and v51 ls\n")
fls1951
chisq.test(fls1951)

cs1952<- xtabs(~ v19 + v52, data= cs)
fcs1952<- cs1952[-c(5,6),-c(3,4)]
dimnames(fcs1952) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v19 and v52 cs\n")
fcs1952
chisq.test(fcs1952)

ls1952 <- xtabs(~v19 + v52, data= ls)
fls1952 <- ls1952[-c(5,6),-c(3,4)]
dimnames(fls1952) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v52 Do you think reporting neighbors a good idea"= c("Yes","No"))
cat("v19 and v52 ls\n")
fls1952
chisq.test(fls1952)


#messy again

csv53 <- data.frame(
  v19 = rep(cs$v19, 3),
  v53 = c(cs$v53a, cs$v53b, cs$v53c)
)

cs1953<- xtabs(~ v19 + v53, data= csv53)
fcs1953<- cs1953[-c(5,6),-c(9,10,11)]
dimnames(fcs1953) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v19 and v53 cs\n")
fcs1953
chisq.test(fcs1953)

lsv53 <- data.frame(
  v19 = rep(ls$v19, 3),
  v53 = c(ls$v53a, ls$v53b, ls$v53c)
)

ls1953 <- xtabs(~v19 + v53, data= lsv53)
fls1953 <- ls1953[-c(5,6),-c(9,10,11)]
dimnames(fls1953) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v53 Is v52 dangerous, how?"= c("No Dangers","Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v19 and v53 ls\n")
fls1953
chisq.test(fls1953)

#messy again
csv55 <- data.frame(
  v19 = rep(cs$v19, 3),
  v55 = c(cs$v55a, cs$v55b, cs$v55c)
)

cs1955<- xtabs(~ v19 + v55, data= csv55)
fcs1955<- cs1955[-c(5,6),-c(1,9)]
dimnames(fcs1955) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v19 and v55 cs\n")
fcs1955
chisq.test(fcs1955)

lsv55 <- data.frame(
  v19 = rep(ls$v19, 3),
  v55 = c(ls$v55a, ls$v55b, ls$v55c)
)

ls1955 <- xtabs(~v19 + v55, data= lsv55)
fls1955 <- ls1955[-c(5,6),-c(1,9)]
dimnames(fls1955) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v55 How is reporting aquaintances dangerous?"= c("Fear/Reprisal","Hurt Innocent/Wrongly Accused","Malicious Reporting","Atmosphere Suspicion/Mistrust","Inefficent/Wasteful","Un-American/Undemocratic","Other Danger"))
cat("v19 and v55 ls\n")
fls1955
chisq.test(fls1955)

cs1957<- xtabs(~ v19 + v57, data= cs)
fcs1957<- cs1957[-c(5,6),-c(3,4)]
dimnames(fcs1957) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v19 and v57 cs\n")
fcs1957
chisq.test(fcs1957)

ls1957 <- xtabs(~v19 + v57, data= ls)
fls1957 <- ls1957[-c(5,6),-c(3,4)]
dimnames(fls1957) <- list("v19 Comm Stopped in Asia w/o War"= c("Very Likely","Rather Likely","Rather Unlikely","Not Likely At All"), "v57 To catch Comm, Can gov listen to phone calls?"= c("Yes","No"))
cat("v19 and v57 ls\n")
fls1957
chisq.test(fls1957)

sink()
