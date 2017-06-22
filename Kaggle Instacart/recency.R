# Recency

# order data

#users1 <- sample(unique(orders$user_id), size=200, replace=F)

users1 <- users
  
orders30 <- data.table(orders[orders$user_id %in% users1,])
master30 <- data.table(master.comb[master.comb$user_id %in% users1,])

orders30 <- orders30[order(user_id,order_number,decreasing=TRUE),]

usr <- 0
orders30$Age <- 0
prevAge <- 0

for (i in 1:nrow(orders30))
{
  if (usr != orders30$user_id[i])
  {
    Age <- 0
    prevAge <- orders30$days_since_prior_order[i]
    usr <- orders30$user_id[i]
  }
  else
  {
    Age <- prevAge + Age
    prevAge <- orders30$days_since_prior_order[i]
  }
  
  orders30$Age[i] <- Age
  
}

xx30 <- data.table(merge(x=master30, y=orders30,by=c("user_id", "order_id") ,all.x = T))

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

