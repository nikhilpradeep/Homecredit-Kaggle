library(scorecard)
library(tidyverse)
library(caTools)
data <- read.csv("newtrain.csv")
data$X = NULL
set.seed(88)
split <- sample.split(data$TARGET, SplitRatio = 0.75)

#split

#get training and test data
train <- subset(data, split == TRUE)
test  <- subset(data, split == FALSE)

iv = iv(train, y = 'TARGET') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

iv_1 <- filter(iv,info_value>0.1)

iv_1 %>%
  knitr::kable()
dim(iv_1)


View(train[,iv_1$variable])
imp_var = c("SK_ID_CURR",iv_1$variable,"TARGET")


bins = woebin(train[,c(iv_1$variable,"TARGET")], y= 'TARGET')

bins$EXT_SOURCE_3 %>%
  knitr::kable()

train_woe = woebin_ply(train[,c(iv_1$variable,"TARGET")],bins) %>%
  as_tibble()


train_woe
vars = names(train_woe)
vars = vars[ vars != 'TARGET']

formula = as.formula( paste( 'TARGET ~', paste( vars , collapse = '+') ) )
lasso = glm(formula,data = train_woe, family=binomial(link="logit"))

test_woe_t = woebin_ply(test[,c(iv_1$variable)],bins) %>%
  as_tibble()

#pred <- predict(lasso,newtype = "response")
pred <- predict(lasso,newdata = test_woe_t,type = "response")
library(pROC)
roc_o <- roc(test$TARGET,pred)
auc(roc_o)

# Test

test_data <- read.csv("new_test24.csv")

test_data$X = NULL

test_woe = woebin_ply(test_data[,c(iv_1$variable)],bins) %>%
  as_tibble()

pred_test <- predict(lasso,newdata = test_woe,type = "response")

View(pred_test)
View(pred)
result <- cbind(test_data$SK_ID_CURR,pred_test)
result <- as.data.frame(result)
names(result) <- c("SK_ID_CURR","TARGET")
View(result)
write.csv(result,"result1.csv",row.names=FALSE)

# Metrics----------------------
library(ROCR)
ROCpred <- prediction(pred,test$TARGET)
ROCperf <- performance(ROCpred,"tpr","fpr")
plot(ROCperf)
plot(ROCperf, colorize=T, 
     print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# Conc-----------------------------
library(InformationValue)
Conc = Concordance(test$TARGET, pred)
Conc
C = Conc$Concordance
D = Conc$Discordance
T = Conc$Tied

#Goodman - Kruskal Gamma
#------------------------------------------------------------------------
gamma = (C-D)/(C+D+T)
gamma



#Somer D
#------------------------------------------------------------------------
D = (C-D)/(C+D)
D

#3B -> Creating a Gain Table
#--------------------------------------------------------------------------------------
pred1 = predict(lasso, newdata=test_woe_t, type="response")
actual = test$TARGET
newdata = data.frame(actual,pred1)
View(newdata)

newdata = newdata[order(-newdata$pred1), ]

groups = rep(1:10,each=floor(nrow(newdata)/10))
extra = rep(10, nrow(newdata)-length(groups))   #Filling up the extras
groups = c(groups,extra)
groups

#Attach the groups to the data
newdata$groups = groups
View(newdata)

#We will use SELECT query from the sqldf library
library(sqldf)


#Calculate the number of Bads (or 1's) in each of the groups (keeping track of the total counts in each groups)
gainTable = sqldf("select groups, count(actual) as N, 
                  sum(actual) as N1 from newdata 
                  group by groups ")
class(gainTable)
View(gainTable)

#Calculate the cumulative sum of bads (or 1's)
gainTable$cumN1 = cumsum(gainTable$N1)


#Calculate the cumulative percentage of bads (or 1's)
gainTable$Gain = round(gainTable$cumN1/sum(gainTable$N1)*100,3)


#Calculate Cumulative Lift
gainTable$Lift = round(gainTable$Gain/((1:10)*10),3)


#Print the Gain Table
gainTable



#3C -> Plot the Cumulative Gain and Cumulative Lift Chart
#-------------------------------------------------------------------------------------

#Gain Chart
plot(gainTable$groups, gainTable$Gain, type="b", 
     main = "Gain Plot",
     xlab = "Groups", ylab = "Gain")



#Lift Chart
plot(gainTable$groups, gainTable$Lift, type="b", 
     main = "Lift Plot",
     xlab = "Groups", ylab = "Lift")







# KS plot------------------------------------
ks_plot(newdata$actual, newdata$pred1)
ks_stat(newdata$actual, newdata$pred1)
