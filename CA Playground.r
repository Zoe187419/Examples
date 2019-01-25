 
Sys.setenv("SPARK_HOME"="/opt/cloudera/parcels/SPARK2/lib/spark2")
Sys.setenv("SPARK_HOME_VERSION"="2.3.0")

sc <-
  spark_connect(master = "yarn-client", config = list(
    spark.submit.deployMode = "client",
    spark.yarn.queue="ellington",
    spark.executor.memory = "18G",
    spark.executor.cores = 4,
    "sparklyr.shell.driver-memory"="8G",
    spark.scheduler.mode="FAIR"
  ))

#get DB
dbGetQuery(sc, "USE caplayground") 
 
