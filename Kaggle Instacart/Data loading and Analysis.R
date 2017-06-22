library(plyr)
library(ggplot2)
library(data.table)

#Data Loading
aisles <- read.csv("aisles.csv", header = T, stringsAsFactors = F, na.strings = c("", "None"))
dep <- read.csv("departments.csv", header = T, stringsAsFactors = F, na.strings = c("", "None"))
prod <- read.csv("products.csv", header = T, stringsAsFactors = F, na.strings = c("", "None"))
orders <- read.csv("orders.csv", header = T, stringsAsFactors = F, na.strings = c("", "None"))
prior <- read.csv("order_products__prior.csv", header = T, stringsAsFactors = F, na.strings = c("", "None"))
train <- read.csv("order_products__train.csv", header = T, stringsAsFactors = F, na.strings = c("", "None"))

ord.id <- orders$order_id[orders$user_id ==5]
sample <- prior[prior$order_id %in% ord.id,]

prior.comb <- join(prior,prod, type="inner")
rm(prior)
rm(aisles, dep,prod)

prior.comb1 <- join(prior.comb, dep, type="inner")
rm(prior.comb)

prior.comb2 <- join(prior.comb1, aisles, type="inner")
rm(prior.comb1)

write.csv(prior.comb2, "combined.csv", row.names = F)

master.comb <- join(prior.comb2, orders, type="inner", "order_id")
rm(prior.comb2)


order.prdcnt1 <- data.frame(table(master.comb$order_id))
colnames(order.prdcnt1) <- c("order_id", "Count")


#agg_byorderid <- aggregate(master.comb$product_id, by=list(master.comb$order_id), FUN=count)
#colnames(agg_byDep) <- c("order_id", "Count")

orders.withcnt <- join(orders,order.prdcnt1, type="inner")

avg_byorderid <- aggregate(orders.withcnt$Count, by=list(orders.withcnt$order_number), FUN=mean)

testset <- orders[orders$eval_set=="test",]
############ Test repeat with sub smaple################

set.seed(100)
users <- sample(unique(orders$user_id), size=1000, replace=F)

#users <- unique(testset$user_id)

orders20 <- data.table(orders[orders$user_id %in% users,])

master20 <- data.table(master.comb[master.comb$user_id %in% users,])

train20 <- join(train,orders20[orders20$eval_set=="train",], type="inner", by="order_id")

# Find unique combinations of user is and prd id
#usr_prd20 <- unique(master20[,c('user_id', 'product_id')])

#usr_prd20$cnt  <- 0

#usr_prd20 <- ddply(master20,~user_id+product_id,nrow)
#colnames(usr_prd20) <- c("user_id", "product_id", "cnt")
#usr_prd20$ordpct  <- 0

orders20 <- orders20[order(user_id,order_number,decreasing=TRUE),]

usr <- 0
orders20$Age <- 0
prevAge <- 0

for (i in 1:nrow(orders20))
{
  if (usr != orders20$user_id[i])
  {
    Age <- 0
    prevAge <- orders20$days_since_prior_order[i]
    usr <- orders20$user_id[i]
  }
  else
  {
    Age <- prevAge + Age
    prevAge <- orders20$days_since_prior_order[i]
  }
  
  orders20$Age[i] <- Age
  
}

xx30 <- data.table(merge(x=master20, y=orders20,by=c("user_id", "order_id") ,all.x = T))

setkeyv(xx30, c('user_id', 'product_id'))
usr_prd.latest <- xx30[ , .SD[which.max(order_number.x)], by = list(user_id, product_id)]

usr_prd.latest$recency <- 0

usr_prd.latest$recency <- ifelse(usr_prd.latest$Age<31,1,usr_prd.latest$recency)
usr_prd.latest$recency <- ifelse(usr_prd.latest$Age>30 & usr_prd.latest$Age<61 ,
                                 0.8,usr_prd.latest$recency)

usr_prd.latest$recency <- ifelse(usr_prd.latest$Age>60 & usr_prd.latest$Age<91 ,
                                 0.6,usr_prd.latest$recency)

usr_prd.latest$recency <- ifelse(usr_prd.latest$Age>90 & usr_prd.latest$Age<181 ,
                                 0.4,usr_prd.latest$recency)

usr_prd.latest$recency <- ifelse(usr_prd.latest$Age>180 & usr_prd.latest$Age<366 ,
                                 0.2,usr_prd.latest$recency)

setkeyv(master20, c('user_id', 'product_id'))
usr_prd20 <- master20[ , cnt := .N, by = list(user_id, product_id)]

usr_prd20 <- unique(usr_prd20[,c(10,2,16)])
usr_prd20$ordpct  <- 0

# Calculate the repeat %
#for (i in 1:nrow(usr_prd20))
#{
#    usr_prd20$cnt[i] <- nrow(master20[master20$user_id == usr_prd20$user_id[i] &
#                                     master20$product_id == usr_prd20$product_id[i],])
#
#}

#usrprdcnt <- ddply(master.comb,~user_id+product_id,summarise,orderpct=count(order_number))

user20_ordcnt <- data.frame(table(orders20$user_id[orders20$eval_set=="prior"]))

colnames(user20_ordcnt) <- c("user_id", "OrdCount")

all.data <- plyr::join(x=usr_prd20,y=user20_ordcnt, type="right", match="all", by="user_id")
all.data$ordpct <- all.data$cnt/all.data$OrdCount

all.data.merged <- merge(x=all.data, y=usr_prd.latest,by=c("user_id", "product_id") ,all.x = T, all.y = T)

all.data.merged$RFscore <- all.data.merged$ordpct * all.data.merged$recency

#pred.data <- all.data[all.data.merged$RFscore > 0.0001,]

pred.data <- all.data[all.data$ordpct > 0.25,]

comp.data <- train20[,c(5,2)]
comp.data$Act <- 1

xx <- merge(x=comp.data, y=pred.data,by=c("user_id", "product_id") ,all.x = T, all.y = T)

xx$Pred <- 1
xx$Pred[is.na(xx$cnt)] <- 0
xx$Act[is.na(xx$Act)]  <- 0

calc.F(xx)



### For Prediction ###
pred.dt <- data.table(pred.data[,c(1,2)])
new.dt <- pred.dt[,lapply(.SD, paste, collapse = " "),'user_id']

p1 <- merge(x=testset, y=new.dt, by="user_id", all.x=T)

null.usr <- p1$user_id[is.na(p1$product_id)]

remain.data <- data.table(all.data[all.data$user_id %in% null.usr,])

remain.data.max <- remain.data[ , .SD[which.max(ordpct)], by=user_id]

p1$product_id[is.na(p1$product_id)] <- remain.data.max$product_id

colnames(p1) <- c("user_id", "order_id", "eval_set", "order_number", "order_dow", "order_hour_of_day",
                  "days_since_prior_order", "products")

write.csv(p1[,c(2,8)], "Sub-15June-01.csv", row.names = F)

rm(train20, orders20)
rm(all.data, users)
rm(usr)

