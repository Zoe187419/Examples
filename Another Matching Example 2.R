#https://www.r-bloggers.com/how-to-use-r-for-matching-samples-propensity-score/

library(wakefield)
library(knitr)
set.seed(1234)
df.patients <- r_data_frame(n = 250, 
                            age(x = 30:78, 
                                name = 'Age'), 
                            sex(x = c("Male", "Female"), 
                                prob = c(0.70, 0.30), 
                                name = "Sex"))
df.patients$Sample <- as.factor('Patients')

set.seed(1234)
df.population <- r_data_frame(n = 1000, 
                              age(x = 18:80, 
                                  name = 'Age'), 
                              sex(x = c("Male", "Female"), 
                                  prob = c(0.50, 0.50), 
                                  name = "Sex"))
df.population$Sample <- as.factor('Population')

mydata <- rbind(df.patients, df.population)
mydata$Group <- as.logical(mydata$Sample == 'Patients')
mydata$Distress <- ifelse(mydata$Sex == 'Male', age(nrow(mydata), x = 0:42, name = 'Distress'),
                          age(nrow(mydata), x = 15:42, name = 'Distress'))

table1 <- CreateTableOne(vars = c('Age', 'Sex', 'Distress'), 
                         data = mydata, 
                         factorVars = 'Sex', 
                         strata = 'Sample')

table1 <- print(table1, 
                printToggle = FALSE, 
                noSpaces = TRUE)
kable(table1[,1:3],  
      align = 'c', 
      caption = 'Table 1: Comparison of unmatched samples')


set.seed(1234)
match.it <- matchit(Group ~ Age + Sex, data = mydata, method="nearest", ratio=1)
a <- summary(match.it)

kable(a$nn, digits = 2, align = 'c', 
      caption = 'Table 2: Sample sizes')

kable(a$sum.matched[c(1,2,4)], digits = 2, align = 'c', 
      caption = 'Table 3: Summary of balance for matched data')

plot(match.it, type = 'jitter', interactive = FALSE)

df.match <- match.data(match.it)[1:ncol(mydata)]
rm(df.patients, df.population)

table4 <- CreateTableOne(vars = c('Age', 'Sex', 'Distress'), 
                         data = df.match, 
                         factorVars = 'Sex', 
                         strata = 'Sample')
table4 <- print(table4, 
                printToggle = FALSE, 
                noSpaces = TRUE)
kable(table4[,1:3],  
      align = 'c', 
      caption = 'Table 4: Comparison of matched samples')





