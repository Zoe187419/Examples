#Proc mixed equivalent
#http://www.math.ttu.edu/~atrindad/software/MixedModels-RandSAScomparison.pdf

library(nlme)
library(dplyr)

setwd("G:/marketing/Marketing Analytics/Bohning_Jessica/Projects/2018/181031 SAS Optimal Matching Investigation/R Stuff")
rawdata<-read.csv(file="prematchdata_formatted.csv")

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
#subset to only the test and controls with matchs (2843 test offices)
prematchdata<-prematchdata[prematchdata$Random_Match_ID<=2843,]

#Convert test and ID variables to factors
prematchdata$test_indicator<-as.factor(prematchdata$test_indicator)
prematchdata$Random_Match_ID<-as.factor(prematchdata$Random_Match_ID)

fit1<-lme(new_1718_growth ~ total_17+new_1617_growth+prior_1617_growth+
          avg_nac_17+avg_cltotagi_17+prct_eitc_17,
          data=prematchdata, random= ~1 | test_indicator/Random_Match_ID)



library(lme4)
library(lmerTest)

#NOTES:
  #For random factors, you have three basic variants:
  #Intercepts only by random factor: (1 | random.factor)
  #Slopes only by random factor: (0 + fixed.factor | random.factor)
  #Intercepts and slopes by random factor: (1 + fixed.factor | random.factor)
#The formula follows the form dependent ~ independent | grouping. 
#The groupingis generally a random factor, you can include fixed factors 
#without any grouping and you can have additional random factors without 
#any fixed factor (an intercept-only model)

#http://rpsychologist.com/r-guide-longitudinal-lme-lmer
m1 <- lmer(Informed.liking ~ Gender*Information +(1|Consumer), data=ham)
ls_means(m1, test.effs="Gender:Information")
m <- lmer(Coloursaturation ~ TVset*Picture + (1|Assessor), data=TVbo)
plot(ls_means(m))
ls_means(m, test.effs="TVset")


m2<-lmer(new_1718_growth ~ test_indicator+total_17+new_1617_growth+
           prior_1617_growth+
           avg_nac_17+avg_cltotagi_17+prct_eitc_17 +(1|test_indicator),
         data=prematchdata)

m4<-lmer(new_1718_growth ~ test_indicator+total_17+new_1617_growth+
           prior_1617_growth+
           avg_nac_17+avg_cltotagi_17+prct_eitc_17 +
           (1|Random_Match_ID),
         data=prematchdata)

if(1==2){#m3 is definitely not the answer
  m3<-lmer(new_1718_growth ~ test_indicator+total_17+new_1617_growth+
             prior_1617_growth+
             avg_nac_17+avg_cltotagi_17+prct_eitc_17+Random_Match_ID+
             (1|Random_Match_ID),
           data=prematchdata)
}#m3 is definitely not the answer
if(1==2){ # I can't get the following to work out
test_ids<-prematchdata %>% filter(test_indicator==1)
control_ids<-prematchdata %>% filter(test_indicator==0)
test_ids<-test_ids$Random_Match_ID
control_ids<-control_ids$Random_Match_ID
temp_treat<-prematchdata %>% 
  filter(test_indicator==1 &
           Random_Match_ID %in% control_ids)%>%
  arrange(Random_Match_ID)
temp_control<-prematchdata %>% 
  filter(test_indicator==0 &
           Random_Match_ID %in% test_ids)%>%
  arrange(Random_Match_ID)
temp_df<-rbind(temp_treat,temp_control)
temp_df<-temp_df$TAXLOCTN
temp_prematchdata<-prematchdata %>% filter(TAXLOCTN %in% temp_df)
temp_prematchdata$Random_Match_ID<-as.numeric(as.character(temp_prematchdata$Random_Match_ID))
temp_prematchdata$Random_Match_ID<-as.factor(temp_prematchdata$Random_Match_ID)

m6<-lmer(new_1718_growth ~ test_indicator+total_17+new_1617_growth+
           prior_1617_growth+
           avg_nac_17+avg_cltotagi_17+prct_eitc_17 +
           (Random_Match_ID|test_indicator),
         data=temp_prematchdata)

m7<-lmer(new_1718_growth ~ test_indicator+total_17+new_1617_growth+
           prior_1617_growth+
           avg_nac_17+avg_cltotagi_17+prct_eitc_17 +
           (1|Random_Match_ID:test_indicator),
         data=prematchdata)

test_ids<-prematchdata %>% filter(test_indicator==1)
control_ids<-prematchdata %>% filter(test_indicator==0)
test_ids<-test_ids$Random_Match_ID
control_ids<-control_ids$Random_Match_ID

temp_treat<-prematchdata %>% 
              filter(test_indicator==1 &
                       Random_Match_ID %in% control_ids)%>%
              arrange(Random_Match_ID)
temp_control<-prematchdata %>% 
              filter(test_indicator==0 &
                       Random_Match_ID %in% test_ids)%>%
              arrange(Random_Match_ID)
df<-data.frame(test_taxloctn=temp_treat$TAXLOCTN,
               test_new_1718_growth=temp_treat$new_1718_growth,
               Random_Match_ID=temp_treat$Random_Match_ID,
               control_taxloctn=temp_control$TAXLOCTN,
               control_new_1718_growth=temp_control$new_1718_growth)
m5<-lmer(test_new_1718_growth~offset(control_new_1718_growth),
         data=df)
}

#I think m4 is what I want
summary(m2)
temp<-ls_means(m2,test.effs="test_indicator")
temp
temp1<-data.frame(lower=temp$lower,upper=temp$upper,names=c("0","1"))
temp1<-t(data.frame(lower_0=temp$lower[1],lower_1=temp$lower[2],
                  upper_0=temp$upper[1],upper_1=temp$upper[2]))
temp1[order(temp1[,1]),]


#Throw every variable in there to see what is important
fit2<-lmer(new_1718_growth ~ total_17+
          new_17+
          #prior_17+
          prct_new_17+
          #prct_prior_17+
          avg_nac_17+
          avg_new_nac_17+
          #avg_prior_nac_17+
          avg_cltotagi_17+
          avg_new_cltotagi_17+
          #avg_prior_cltotagi_17+
          avg_refund_17+
          avg_new_refund_17+
          #avg_prior_refund_17+
          avg_refundminusbaldue_17+
          avg_new_refundminusbaldue_17+
          #avg_prior_refundminusbaldue_17+
          F1040EZ_17+
          F1040A_17+
          F1040_17+
          prct_F1040EZ_17+
          prct_F1040A_17+
          prct_F1040_17+
          jan_17+
          feb_17+
          mar_17+
          #apr_17+
          prct_jan_17+
          prct_feb_17+
          prct_mar_17+
          #prct_apr_17+
          eitc_17+
          prct_eitc_17+
          single_17+
          prct_single_17+
          mfj_17+
          prct_mfj_17+
          hoh_17+
          prct_hoh_17+
          homeowner_17+
          prct_homeowner_17+
          has_dependents_17+
          prct_has_dependents_17+
          total_1617_growth+
          new_1617_growth+
          #prior_1617_growth+
          avg_nac_1617_growth+
          avg_new_nac_1617_growth+
          avg_prior_nac_1617_growth+
          avg_cltotagi_1617_growth+
          avg_new_cltotagi_1617_growth+
          #avg_prior_cltotagi_1617_growth+
          avg_refund_1617_growth+
          avg_new_refund_1617_growth+
          #avg_prior_refund_1617_growth+
          avg_refundminusbaldue_1617_growth+
          avg_new_refundminusbaldue_1617_growth+
          #avg_prior_refundminusbaldue_1617_growth+
          F1040EZ_1617_growth+
          F1040A_1617_growth+
          F1040_1617_growth+
          jan_1617_growth+
          feb_1617_growth+
          mar_1617_growth+
          apr_1617_growth+
          eitc_1617_growth+
          single_1617_growth+
          mfj_1617_growth+
          hoh_1617_growth+
          homeowner_1617_growth+
          has_dependents_1617_growth+
          (1|test_indicator),
          data=prematchdata)
summary(fit2)
ls_means(fit2,test.effs="test_indicator")



############################################################################

fit3<-lm(test_indicator~total_17+
           new_17+
           #prior_17+
           prct_new_17+
           prct_prior_17+
           avg_nac_17+
           avg_new_nac_17+
           #avg_prior_nac_17+
           avg_cltotagi_17+
           avg_new_cltotagi_17+
           #avg_prior_cltotagi_17+
           avg_refund_17+
           avg_new_refund_17+
           #avg_prior_refund_17+
           avg_refundminusbaldue_17+
           avg_new_refundminusbaldue_17+
           avg_prior_refundminusbaldue_17+
           F1040EZ_17+
           F1040A_17+
           F1040_17+
           prct_F1040EZ_17+
           prct_F1040A_17+
           prct_F1040_17+
           jan_17+
           feb_17+
           mar_17+
           apr_17+
           prct_jan_17+
           prct_feb_17+
           prct_mar_17+
           prct_apr_17+
           eitc_17+
           prct_eitc_17+
           single_17+
           prct_single_17+
           mfj_17+
           prct_mfj_17+
           hoh_17+
           prct_hoh_17+
           homeowner_17+
           prct_homeowner_17+
           has_dependents_17+
           prct_has_dependents_17+
           total_1617_growth+
           new_1617_growth+
           #prior_1617_growth+
           avg_nac_1617_growth+
           avg_new_nac_1617_growth+
           avg_prior_nac_1617_growth+
           avg_cltotagi_1617_growth+
           avg_new_cltotagi_1617_growth+
           #avg_prior_cltotagi_1617_growth+
           avg_refund_1617_growth+
           avg_new_refund_1617_growth+
           #avg_prior_refund_1617_growth+
           avg_refundminusbaldue_1617_growth+
           avg_new_refundminusbaldue_1617_growth+
           #avg_prior_refundminusbaldue_1617_growth+
           F1040EZ_1617_growth+
           F1040A_1617_growth+
           F1040_1617_growth+
           jan_1617_growth+
           feb_1617_growth+
           mar_1617_growth+
           apr_1617_growth+
           eitc_1617_growth+
           single_1617_growth+
           mfj_1617_growth+
           hoh_1617_growth+
           homeowner_1617_growth+
           has_dependents_1617_growth,data=prematchdata)
summary(fit3)