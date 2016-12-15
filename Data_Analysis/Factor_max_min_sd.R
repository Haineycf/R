##### ways to find values based on another columns

df <- read.table(header = TRUE, text = 'Gene   Value
A      12
A      10
B      3
B      5
B      6
C      1
D      3
D      4')

# aggregate
aggregate(df$Value, by = list(df$Gene), max)
aggregate(Value ~ Gene, data = df, max)

# tapply
tapply(df$Value, df$Gene, max)

# split + lapply
lapply(split(df, df$Gene), function(y) max(y$Value))

# plyr
require(plyr)
ddply(df, .(Gene), summarise, Value = max(Value))

# dplyr
require(dplyr)
df %>% group_by(Gene) %>% summarise(Value = max(Value))

# data.table
require(data.table)
dt <- data.table(df)
dt[ , max(Value), by = Gene]

# doBy
require(doBy)
summaryBy(Value~Gene, data = df, FUN = max)

# sqldf
require(sqldf)
sqldf("select Gene, max(Value) as Value from df group by Gene", drv = 'SQLite')

# ave
df[as.logical(ave(df$Value, df$Gene, FUN = function(x) x == max(x))),]