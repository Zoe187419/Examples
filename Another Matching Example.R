library(Matching)
data(lalonde)
X  <- cbind(lalonde$age,lalonde$married)
colnames(X)<-c("age","married")
Tr  <- lalonde$treat
#Define caliper for age within 5 years (see package documentation for caliper)
5/sd(lalonde$age) #=0.7041973
# 5-to-one matching with replacement (the "M=5" option), with ties set to false to limit multiple matches
# Exact matching on "married" (the "exact=c(0,1)" option), corresponding to 0 (false) for age, 1 (true) for "married"
rr  <- Match(Tr=Tr, X=X, M=1,ties=F,exact=c(0,1),caliper=0.7041973) #caliper set to match age within 5 years
summary(rr)
#Put results (rownames of treated and controls) into a dataframe
rr<-data.frame(rr$index.treated,rr$index.control)
colnames(rr)<-c("cases","controls")
lalonde$rowID<-as.numeric(rownames(lalonde))

#Option 1 to join case control table to original characteristics
rr$CaseMarried <- lalonde$married[match(rr$cases, lalonde$rowID)]
rr$ControlMarried<-lalonde$married[match(rr$controls, lalonde$rowID)]
rr$CaseAge <- lalonde$age[match(rr$cases, lalonde$rowID)]
rr$ControlAge<-lalonde$age[match(rr$controls, lalonde$rowID)]
head(rr)

