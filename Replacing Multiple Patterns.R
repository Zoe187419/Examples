#Replace in List

library(DataCombine)
temp<-data.frame(comments=c("Jessica is...", 
                 "I think Ben could improve on...",
                 "Everyone who knows Wyatt...",
                 "I think Mary's best strength is..."))


#nameslist<-data.frame(from=c("Jessica","Frank","Dog","Ben","Wyatt","car","Mary"), to="EMPLOYEE")

library(babynames)
nameslist<-data.frame(from=babynames$name,to="EMPLOYEE")
nameslist<-subset(nameslist,!duplicated(nameslist$from))

NEWDF<-FindReplace(data=temp,Var="comments",replaceData=nameslist,
                   from="from",to="to",exact=FALSE)


#Notice that the word "Everyone" has two names in it: "EVE" and "RYON" when
#you use an exact match. When you don't use an exact match, you will miss
#the name "Mary's,"  which means that I need to double the names list with 
#a "'s" version of each name getting replaced with "EMPLOYEE's". 
#NOPE. You'll have to go word by frickin word.


#How long would a loop take?
library(babynames)
library(tictoc)
library(stringr)
nameslist<-babynames$name
nameslist<-subset(nameslist,!duplicated(nameslist))
nameslist2<-paste0(nameslist,"'s")
nameslist3<-rbind(data.frame(names=nameslist),data.frame(names=nameslist2))
#add spaces so that gsub doesn't take partial words
nameslist4<-paste0('\\<',nameslist3$names,'\\>')

tic()
NEWDF<-c("Jessica is...", 
         "I think Ben could improve on...",
         "Everyone who knows Wyatt...",
         "I think Mary's best strength is...")
for (i in 1:length(nameslist4)){
    NEWDF<-gsub(pattern=nameslist4[i], 
                replacement="EMPLOYEE",
                x=NEWDF,ignore.case=TRUE)
}
toc()


#Took 3.16 Seconds





