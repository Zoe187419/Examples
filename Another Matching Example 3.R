#https://r.iq.harvard.edu/docs/matchit/2.4-20/matchit.pdf

setwd("G:/marketing/Marketing Analytics/Bohning_Jessica/Projects/2018/181031 SAS Optimal Matching Investigation/R Stuff")

library(dplyr)
library(tableone)
library(Matching)
library(MatchIt)
library(ggplot2)
library(optmatch)

rawdata<-read.csv(file="prematchdata.csv")

prematchdata<-rawdata %>%
  filter(new_1617_growth<2 &
           new_1718_growth<2 &
           prior_1617_growth<2 &
           prior_1718_growth<2 &
           
           new_1617_growth>-0.7 &
           new_1718_growth>-0.7 &
           prior_1617_growth>-0.7 &
           prior_1718_growth>-0.7
  )

#Variables to match on
xvars<-c("total_17","new_1617_growth","prior_1617_growth", "avg_nac_17",
         "avg_refund_17","avg_cltotagi_17","prct_eitc_17","prct_homeowner_17",
         "prct_mfj_17","prct_single_17","prct_has_dependents_17","prct_F1040_17",
         "prct_F1040A_17","prct_F1040EZ_17","prct_jan_17","prct_feb_17",
         "prct_mar_17","prct_apr_17")



test<-prematchdata %>% select(c("test_indicator","TAXLOCTN",xvars))
rownames(test)<-test$TAXLOCTN
options("optmatch_max_problem_size" = Inf)
if(1==2){
set.seed(1234)
match.it<-matchit(test_indicator~
                    total_17+new_1617_growth+prior_1617_growth + avg_nac_17+
                    avg_refund_17+avg_cltotagi_17+prct_eitc_17+prct_homeowner_17+
                    prct_mfj_17+prct_single_17+prct_has_dependents_17+prct_F1040_17+
                    prct_F1040A_17+prct_F1040EZ_17+prct_jan_17+prct_feb_17+
                    prct_mar_17+prct_apr_17,
                  data=test,
                  method="nearest",
                  ratio=1)
}
set.seed(1234)
match.it<-matchit(test_indicator~
                    total_17+new_1617_growth+prior_1617_growth,
                  data=test,
                  method="optimal",
                  ratio=1)
summary(match.it)

df.match <- match.data(match.it)
print(CreateTableOne(vars=xvars, strata ="test_indicator", 
                     data=df.match, test = FALSE),smd=TRUE)

zz.out<-data.frame(test=rownames(match.it$match.matrix),match.it$match.matrix)
zz.out$test<-as.numeric(as.character(zz.out$test))
zz.out$control<-as.numeric(as.character(zz.out$X1))
zz.out<-subset(zz.out,select=-c(X1))

#playing around with results
final_df_test<-df.match[match(zz.out$test,df.match$TAXLOCTN),]
final_df_control<-df.match[match(zz.out$control,df.match$TAXLOCTN),]
matched_taxloctn<-zz.out
        
ydiff=final_df_test$new_1617_growth-final_df_control$new_1617_growth

plot(ydiff)
ggplot(data.frame(ydiff),aes(ydiff))+geom_histogram()