library(tidyverse)
library(sparklyr)
library(dplyr)
library(DBI)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(readr)

Sys.setenv("SPARK_HOME"="/opt/cloudera/parcels/SPARK2/lib/spark2")
Sys.setenv("SPARK_HOME_VERSION"="2.0.0")

sc <-
  spark_connect(master = "yarn-client", config = list(
    spark.submit.deployMode = "client",
    spark.yarn.queue="ellington",
    spark.executor.memory = "8G",
    spark.executor.cores = 4,
    "sparklyr.shell.driver-memory"="8G"
  ))

#get DB
dbGetQuery(sc, "USE cadm")
# create a hook for the table in hive

RtnPull<-function(Yr){
  Yr<-as.integer(Yr)
  Yr_After<-Yr+1
  
  Yr_Z2<-if(nchar(Yr)<2){paste0("0",Yr)} else Yr_Z2=as.character(Yr)
  YrAfter_Z2<-if(nchar(Yr_After)<2){paste0("0",Yr_After)} else YrAfter_Z2=as.character(Yr_After)
  
  OLG<-if(Yr>6){paste0("or y",YrAfter_Z2,"_online_GUID <> 'null' ")}
  SWG<-if(Yr>11){paste0("or y",YrAfter_Z2,"_software_GUID <> 'null' ")}

  temp<-gsub("/n"," ",paste0(
    "select 'TS",Yr_Z2,"' as TaxYr, ",
    "case when clage < 15 then '01. Age < 15' ",
    "when clage between 15 and 24 then '02. Age 15-24' ",
    "when clage between 25 and 34 then '03. Age 25-34' ",
    "when clage between 35 and 44 then '04. Age 35-44' ",
    "when clage between 45 and 54 then '05. Age 45-54' ",
    "when clage between 55 and 64 then '06. Age 55-64' ",
    "when clage between 65 and 74 then '07. Age 65-74' ",
    "when clage between 75 and 84 then '08. Age 75-84' ",
    "else '09. Age 85+' end as Cl_Age, ",
    "case when cltotagi < 15000 then '01. AGI < $15k' ",
    "when cltotagi between 15000 and 24999.99 then '02. AGI $15-25k' ",
    "when cltotagi between 25000 and 34999.99 then '03. AGI $25-35k' ",
    "when cltotagi between 35000 and 49999.99 then '04. AGI $35-50k' ",
    "when cltotagi between 50000 and 74999.99 then '05. AGI $50-75k' ",
    "when cltotagi between 75000 and 99999.99 then '06. AGI $75-100k' ",
    "when cltotagi between 100000 and 124999.99 then '07. AGI $100-125k' ",
    "when cltotagi between 125000 and 149999.99 then '08. AGI $125-150k' ",
    "when cltotagi between 150000 and 199999.99 then '09. AGI $150-200k' ",
    "when cltotagi between 200000 and 249999.99 then '10. AGI $200-250k' ",
    "when cltotagi between 250000 and 499999.99 then '11. AGI $250-500k' ",
    "else '12. AGI $500k+' end as Cl_AGI, ",
    "case when tenure_enterprise_behav = 'FT' then '01. FT' ",
    "when tenure_enterprise_behav = 'RC2' then '02. 2nd Yr' ",
    "when tenure_enterprise_behav in ('RC3','RC4','RC5') then '03. 3-5 Yr' ",
    "when tenure_enterprise_behav like 'RC%' and ",
    "tenure_enterprise_behav not in ('RC2','RC3','RC4','RC5') ",
    "then '04. 5+ Yr' ",
    "else '05. Lapsed' end as Tenure, ",
    "count(distinct GUID) as ClientCnt, ",
    "sum(case when y",YrAfter_Z2,"_GUID <> 'null' ", OLG, SWG,
    "then 1 else 0 end)/ ",
    "count(distinct GUID) as ClRttn ",
    "from return_subset",Yr_Z2,if(Yr>8){"_v "},
    " group by 1,2,3,4 "
  )
  )
  return(temp)
}

query <- paste0("SELECT * FROM (", RtnPull(6), ')')
for (i in c(7:17)){
  query <- paste0(query, ' union (', RtnPull(i), ')')
}
query <- paste0(query," order by 1,2,3,4,5 ")
print(query)
all_years <- tbl(sc, sql(query))
sdf_register(all_years,"Summary0X_query")