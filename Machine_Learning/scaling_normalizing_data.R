############################
############### Scaling

dat <- as.data.frame(state.x77)
summary(dat)
names <-colnames(state.x77)
y <-dat$Murder
y_name <-names[5]
x1 <-dat$Population
x1_nam <-names[1]
x2 <-dat$Income
x2_name <-names[2]
x3 <-dat$Illiteracy
x3_name <-names[3]
x4 <-dat$`Life Exp`
x4_name <-names[4]
x5 <-dat$`HS Grad`
x5_name <-names[6]
x6 <-dat$Frost
x6_name <-names[7]
x7 <-dat$Area
x7_name <-names[8]

#scale with base
df <- cbind.data.frame(y,x1,x2,x3,x4,x5,x6,x7)
fn <- function(x) x * 100/max(x, na.rm = TRUE)
df_scale <- data.frame(lapply(df, fn))
#head(df_scale)

### using base, needs to be matrix
df_matrix <- as.matrix(df)
df_matrix_scale <-scale(df_matrix, center = TRUE, scale = TRUE)
head(df_matrix_scale)
summary(df_matrix_scale)

####pcaPP, ScaleAdv(x, center = mean, scale = sd), creates a list
library(pcaPP)
df_ScaleAdv<-ScaleAdv(df)
summary(df_ScaleAdv)
head(df_ScaleAdv)

##########pls stdize(x, center = TRUE, scale = TRUE)
library(pls)
df_stdize <-stdize(df)
class(df_stdize)
################caret
library(caret)
df_preProcess<-preProcess(df)


#################sparseLDA
library(sparseLDA)
df_normalize<-normalize(df)
class(df_normalize)
