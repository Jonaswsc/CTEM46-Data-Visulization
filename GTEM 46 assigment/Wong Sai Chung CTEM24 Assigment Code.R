# Load Packages
library(tidyverse) # for data massage 
library(corrplot) # for drawing correlation heatmap
library(gridExtra)
library(caret)
library(e1071)
library(cowplot)
library(kernlab)
library(rpart) # for decision tree function
library(rpart.plot) # for drawing decision tree
library(ranger)
library(mlbench)
library('MLmetrics') # for correlation metrics 
install.packages('PRROC') # for plotting ROC, AUC and PR AUC
library('PRROC')
# Part I. Data Preprocessing
# 1. Data Cleaning and Filtering
# Load datasets
MANA <- read_csv("~/Desktop/Mac Desktop/R study/KaggleV2-May-2016.csv", col_names = TRUE)
summary(MANA)

# Data Transformation
MANA <- MANA %>%
  rename(
    Handicap = Handcap,
    Hypertension = Hipertension,
    No_Show = 'No-show',  # camel case, same as other variables
    SMS_Received = SMS_received # camel case, same as other variables
  ) %>%
  mutate(
    Gender = as.factor(Gender),
    ScheduledDay = as.Date(ScheduledDay),
    AppointmentDay = as.Date(AppointmentDay),
    Age = as.numeric(Age),
    Neighbourhood = as.factor(Neighbourhood),
    Scholarship = as.logical(Scholarship),
    Hypertension = as.logical(Hypertension),
    Diabetes = as.logical(Diabetes),
    Alcoholism = as.logical(Alcoholism),
    Handicap = as.logical(Handicap),
    SMS_Received = as.logical(SMS_Received),
    No_Show = as.factor(No_Show)
  )%>%
  # Next, we add a few columns I think will be helpful in further analysis
  mutate(
    # Number of days between when appointment was made and the appointment itself
    DaysWaiting = as.numeric(AppointmentDay - ScheduledDay)
  ) %>%
  filter(
    # Accept only positive age, keep age 0 since they may be newborns
    Age >= 0,
    # Remove records we find had appointment date before creation date
    DaysWaiting >= 0,
  ) %>% 
  select(-c(PatientId, AppointmentID, AppointmentDay, ScheduledDay))

# Find Missing Records
show_missing <-function(df) {n <- sum(is.na(df))
cat("Missing values:", n, "\n", sep = "")
invisible(df)}
show_missing(MANA)
# Find Duplicate observations
nrow(MANA) - nrow(distinct(MANA))
# observed the Mean, Mode, Median and Total number of the variables
str(MANA);summary(MANA)

# Part II. Pattern Mining
# 2. Visualising Variables Pattern
# Table 1. Gender Distribution and Medical Application No Show Implication
# By frequency
G1 <- ggplot(MANA)+
  geom_bar(aes(x = Gender, fill = No_Show))+
  ggtitle("Gender vs Patient No Show Stacked Bar Diagram (Frequency)")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Count")+
  xlab("Gender")
# by proportion data
G2 <- ggplot(MANA)+
  geom_bar(aes(x = Gender, fill = No_Show), position = position_fill())+
  ggtitle("Gender vs Patient No Show Bar Diagram (Proportion)")+
  ylab('Proportion')+
  xlab("Gender")+
  theme(plot.title = element_text(hjust = 0.5))
grid.arrange(G1, G2,ncol=2, top='Gender distribution and No_Show implication')
# No difference between Male and female
# Table 2. Age distribution, Outliers and No_Show Implication
G3 <- ggplot(MANA, aes(x=Age, fill=No_Show)) + geom_histogram(bins=40)
G4 <- ggplot(MANA, aes(x=No_Show, y=Age, col=Gender)) + geom_boxplot()
grid.arrange(G3, G4,ncol=2, top='Age distribution, outliers and No_Show implication')

# Table 3. Variables and No show Implication
# by Frequency
f1 <- ggplot(MANA)+geom_bar(aes(Scholarship, fill = No_Show))
f2 <- ggplot(MANA)+geom_bar(aes(Hypertension, fill = No_Show))
f3 <- ggplot(MANA)+geom_bar(aes(Diabetes, fill = No_Show))
f4 <- ggplot(MANA)+geom_bar(aes (Alcoholism, fill = No_Show))
f5 <- ggplot(MANA)+geom_bar(aes(Handicap, fill = No_Show))
f6 <- ggplot(MANA)+geom_bar(aes(SMS_Received, fill = No_Show))
grid.arrange(f1,f2,f3,f4,f5,f6, nrow = 2, top = 'Other Variables and No show Implication (By Frequency)')

# by Proportion
p1 <- ggplot(MANA)+geom_bar(aes(Scholarship, fill = No_Show), position = position_fill())+
  ylab('Proportion')
p2 <- ggplot(MANA)+geom_bar(aes(Hypertension, fill = No_Show), position = position_fill())+
  ylab('Proportion')
p3 <- ggplot(MANA)+geom_bar(aes(Diabetes, fill = No_Show), position = position_fill())+
  ylab('Proportion')
p4 <- ggplot(MANA)+geom_bar(aes (Alcoholism, fill =  No_Show), position = position_fill())+
  ylab('Proportion')
p5 <- ggplot(MANA)+geom_bar(aes(Handicap, fill = No_Show), position = position_fill())+
  ylab('Proportion')
p6 <- ggplot(MANA)+geom_bar(aes(SMS_Received, fill = No_Show), position = position_fill())+
  ylab('Proportion')
grid.arrange(p1,p2,p3,p4,p5,p6, nrow = 2, top = 'Other Variables and No show Implication (By Proprotion)')
# Show no difference by proprotionally 

# Table 4. Neighbourhood Vs No-Show
MANA_NH_F <- data.frame(table(MANA$Neighbourhood, MANA$No_Show))
names(MANA_NH_F) <- c("Neighbourhood", "No_Show", 'Count')
head(MANA_NH_F)
MANA_NH_P <- MANA %>% select(Neighbourhood, No_Show)
# By Frequency
fp <- ggplot(MANA_NH_F)+
  geom_bar(aes(x = reorder(Neighbourhood, -Count), y = Count, fill = No_Show), stat = 'identity')+
  theme(axis.text.y = element_text(size= 7, angle = 0, hjust = 1))+
  ggtitle("Neighbourhood vs Patient No Show (By Frequency)")+
  ylab('Count')+
  xlab('Neighbourhood')+
  theme(plot.title = element_text(hjust = 0.5, size = 15))+
  theme(axis.title.y = element_text(size =18))+ 
  theme(axis.title.x = element_text(size =18)) +
  coord_flip()
# By Proportion
pp <- ggplot(MANA_NH_P)+
  geom_bar(aes(x = Neighbourhood, fill = No_Show), position = "fill")+
  theme(axis.text.y = element_text(size= 7, angle = 0, hjust = 1))+
  ggtitle("Neighbourhood vs Patient No Show (By Proportion)")+
  ylab('Proportion')+
  xlab('Neighbourhood')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5, size = 15))+
  theme(axis.title.y = element_text(size =18))+ 
  theme(axis.title.x = element_text(size =18)) +
  coord_flip()
grid.arrange(fp,pp, ncol = 2, top = 'Neighbourhood and No Show implication')
# No difference by propotionally

# Day Waiting distribution
# By Frequency
g_DayWaiting_1 <- ggplot(MANA, aes(x=No_Show, y=DaysWaiting, col=No_Show)) + geom_boxplot()
g_DayWaiting_2 <- ggplot(MANA, aes(x=DaysWaiting, fill=No_Show)) + 
  geom_density(alpha=0.30) + 
  coord_cartesian(xlim=c(0, 100))
grid.arrange(g_DayWaiting_1, g_DayWaiting_2, ncol=2, top='Day Waiting distribution')

# using correlation to find the highest correlated variables
correlationMatrix <- cor(sapply(MANA[,-which(names(MANA) %in% c("No_Show"))],as.numeric))
corrplot(correlationMatrix)
# summarize the correlation matrix
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)
highlyCorrelated

# Change Column No Show to Show
New_MANA <- MANA %>% mutate(No_Show = ifelse(No_Show == "No", 1,0))
New_MANA$No_Show <- factor(New_MANA$No_Show, levels=c(0,1), labels=c("No", "Yes"))
New_MANA <- New_MANA %>% rename(Show = No_Show)

# Logistic regression
MANA_lr <- select(New_MANA, Age, Scholarship, Hypertension, Diabetes, Alcoholism, Handicap,SMS_Received, DaysWaiting, Show)
MANA_lr_model <- glm(Show ~ . ,family = binomial(link = 'logit'),  data = MANA_lr)
summary(MANA_lr_model)
# Split data for model training
MANA_lr$Show <- as.factor(MANA_lr$Show)
library(caTools)
split = sample.split(MANA_lr$Show, SplitRatio = 0.70)
train_data_lr = subset(MANA_lr, split == TRUE)
test_data_lr = subset(MANA_lr, split == TRUE)

# Logistic Regression Model training
MANA_lr_t <- predict(MANA_lr_model,newdata=train_data_lr,type='response')
summary(MANA_lr_t)
pred_train <-  ifelse(MANA_lr_t>0.5,1,0)
str(pred_train)
train_data_lr <- train_data_lr %>%
  mutate(Show = ifelse(Show == "No",0,1))
confusionMatrix(data = as.factor(pred_train), reference = as.factor(train_data_lr$Show), mode = 'prec_recall')

# Logistic Regression Model Prediction
MANA_lr_p <- predict(MANA_lr_model,newdata=test_data_lr,type='response')
summary(MANA_lr_p)
pred_test <-  ifelse(MANA_lr_t>0.5,1,0)
str(pred_test)
test_data_lr <- test_data_lr %>%
  mutate(Show = ifelse(Show == "Yes",0,1))
confusionMatrix(data = as.factor(pred_test), reference = as.factor(test_data_lr$Show), mode = 'prec_recall')

# Prediction Performance
require(PRROC)
pr <- pr.curve(pred_test, test_data_lr$Show, curve = T)
plot(pr)


# Split sampling for randomforest
# Down sampling of the imbalanced data
Sorted_New_MANA <- New_MANA %>% arrange(Show)
keep_idx<- sample(22315:nrow(Sorted_New_MANA), 20000)
keep_no <- Sorted_New_MANA[keep_idx, ]
keep_no
final_data <- rbind(keep_no, Sorted_New_MANA[1:22314,])
final_data
# Predictive Model
# split the data to train and test sets
set.seed(1234)
MANA_tModel1 <- createDataPartition(final_data$Show, p = 0.7, list = FALSE)
train_data <- final_data[MANA_tModel1,]
test_data <- final_data[-MANA_tModel1,]


# Decision Tree
model1 <- rpart(formula = Show ~ Age + DaysWaiting + Alcoholism + Hypertension + 
        Scholarship + SMS_Received, data = train_data, method = "class", 
      maxdepth = 5, minsplit = 2, minbucket = 1, cp = -1,)
print(model1, digits = 3)

# visualize the tree
par(xpd = NA) 
plot(model1)
text(model1, digits = 3)

# visualize using rpart.plot
rpart.plot(model1)
prp(model1, box.palette="auto", tweak = 1.2) # only shows the results


# RF predict
pDT <- predict(model1, test_data)
pDT



# Random Forest
MANA_rf <- ranger(final_data$Show ~ .,
                   data= final_data,
                   num.trees=100,
                   mtry=ncol(final_data)/3,
                   importance="impurity",
                   write.forest=TRUE,
                   min.node.size=2,
                   max.depth=6
)
MANA_rf
MANA_rf$variable.importance

vi <- tibble(x = MANA_rf$variable.importance, attri=attr(MANA_rf$variable.importance, 'name'))
vi <- vi %>% dplyr::arrange(desc(x)) 
vi %>%
  ggplot(aes(reorder(attri, x), x)) +
  geom_col() +
  coord_flip() + 
  ggtitle("Variable Importance")

# Prediction 
Rf_pred <- predict(MANA_rf, data=test_data)
confusionMatrix(Rf_pred$predictions, test_data$Show, mode = 'prec_recall') 
pr <- pr.curve(Rf_pred$predictions, test_data$Show, curve = T)
plot(pr)
