library(sparklyr)
#install.packages("sparklyr")

#  install latest sparklyr
devtools::install_github("rstudio/sparklyr")


#install.packages("DBI")
library(DBI)
#spark_install(version = "1.6.2")
#spark_install(version = "2.0.0")
sc <- spark_connect(master = "local")

#uninstall spark 1.6.2
spark_uninstall(version = "1.6.2", hadoop_version = "2.6")

#uninstall spark 1.6.2
spark_uninstall(version = "2.0.0", hadoop_version = "2.7")

sc <- spark_connect(master = "local", spark_home=spark_home_dir(version = "2.0.2"))
#devtools::install_github("rstudio/sparklyr")
#install.packages("readr")
install.packages("digest")

iris_tbl <- copy_to(sc, iris)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
batting_tbl <- copy_to(sc, Lahman::Batting, "batting")

## Spark home directory

spark_home_dir()
spark_installed_versions()


#### connect to Spark
sc <- spark_connect(master = "local", spark_home=spark_home_dir(version = "2.0.2"))

config <- spark_config()
config[["sparklyr.shell.conf"]] <- "spark.driver.extraJavaOptions=-Dhttp.proxyHost=<proxy_IP> -Dhttp.proxyPort=<port> -Dhttps.proxyHost=<IP> -Dhttps.proxyPort=<port> -Dhttp.proxyUser=<user> -Dhttp.proxyPassword=<*****>"
sc <- spark_connect(master = "yarn-client", version = "1.6.2", config = config)