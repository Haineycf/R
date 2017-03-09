

Sys.setenv(SPARK_HOME = "C:/home/spark-2.1.0-bin-hadoop2.7")

#set the library path
.libPaths(c(file.path(Sys.getenv("SPARK_HOME"),"R", "lib"),.libPaths()))

library(SparkR)

sc <- sparkR.session(master = "local")
#sqlContext <- sparkRSQL.init(sc) ### deprecated

## read a file needs to be in the spark data folder documents
df <- read.df("Crime_Philadelphia/crime.csv",  header = "true", inferSchema = "true", na.strings = "NA")


df <-  read.df(sqlContext=sqlContext, source="text", path="Crime_Philadelphia/crime.csv")
# Create a simple local data.frame
localDF <- data.frame(name=c("John", "Smith", "Sarah"), age=c(19, 23, 18))

# Convert local data frame to a SparkR DataFrame
df <- createDataFrame(sqlContext, localDF)
df2<- createDataFrame(localDF)
# Print its schema
printSchema(df2)
# root
#  |-- name: string (nullable = true)
#  |-- age: double (nullable = true)

# Create a DataFrame from a JSON file
path <- file.path(Sys.getenv("SPARK_HOME"), "examples/src/main/resources/people.json")
peopleDF <- jsonFile(sqlContext, path)
printSchema(peopleDF)

# Register this DataFrame as a table.
registerTempTable(peopleDF, "people")

# SQL statements can be run by using the sql methods provided by sqlContext
teenagers <- sql(sqlContext, "SELECT name FROM people WHERE age >= 13 AND age <= 19")

# Call collect to get a local data.frame
teenagersLocalDF <- collect(teenagers)

# Print the teenagers in our dataset 
print(teenagersLocalDF)


### End Session
sparkR.stop()



####################################################################################################

