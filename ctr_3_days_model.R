library(plyr)
library(dplyr)
library(data.table)
library(stringdist)
library(gbm)
library(caret)


load(file="/mnt/last_3.rda")


# Reading and preparing impressions data: names, filtering by query visits

all_data <- tbl_df(last_3)
rm(last_3)

valid_column_names <- make.names(names=names(all_data), unique=TRUE, allow_ = TRUE)
names(all_data) <- valid_column_names

all_data$msrp <-as.numeric(all_data$msrp)
all_data$auction <-NULL
all_data$norm_income[ is.na(all_data$norm_income) ] <- 0
all_data$clicks[ is.na(all_data$clicks) ] <- 0

all_data$local_time<-NULL
all_data$resolution<-NULL
all_data$device<-NULL
all_data$datetime<-NULL
all_data$income_attached<-NULL
all_data$browser<-NULL
all_data$platform<-NULL
all_data$origin<-NULL
all_data$returning_customer<-NULL
all_data$longitude<-NULL
all_data$latitude<-NULL
all_data$country<-NULL
all_data$city<-NULL
all_data$matchtype<-NULL
all_data$local_time<-NULL
all_data$user_ip<-NULL
all_data$page_load_id<-NULL
all_data$free_shipping<-NULL
all_data$keyword<-NULL
all_data$clustered<-NULL
all_data$timezone_offset<-NULL
all_data$cluster<-NULL
all_data$date<-NULL
all_data$fake_user<-NULL
all_data$ab_test<-NULL
all_data$ab_test_parameters<-NULL
all_data$first_click<-NULL
all_data$last_click<-NULL



# Creating pre-features for the text features
all_data<-all_data %>% mutate(brand_name_2=gsub('[[:punct:]&[:space:]]','',tolower(brand_name)),
                              brand_strsplit=strsplit(gsub('[[:punct:]]',' ',tolower(brand_name)), "\\s+"),
                              condition_2=as.character(gsub('[[:punct:]&[:space:]]','',tolower(condition))),
                              shop_2=gsub('[[:punct:]&[:space:]]','',tolower(shop)),
                              title_strsplit=strsplit(gsub('[[:punct:]]',' ',tolower(title)), "\\s+"),
                              query_2=gsub('[[:punct:]&[:space:]]','',tolower(query)),
                              query_strsplit=strsplit(gsub('[[:punct:]]',' ',tolower(query)), "\\s+"), 
                              title_2=gsub('[[:punct:]&[:space:]]','',tolower(title)))

all_data$shop<-NULL
all_data$condition<-NULL

gc()

all_data<-all_data %>%
  mutate(discount=(msrp-displayed_price)/msrp,
         log_discount=(log(msrp)-log(displayed_price))/log(msrp))

all_data<-all_data %>%
  mutate(log_price=log(displayed_price))

all_data$price2 <- findInterval(all_data$displayed_price, c(0.00001, 10, 20, 50, 100, 200, 300, 500, 750, 1000, 1500, 2000, 3000, 5000))
all_data$price2 <- as.character(all_data$price2)

all_data$discount2 <- findInterval(all_data$discount, c(0.00001, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
all_data$discount2 <- as.character(all_data$discount2)

all_data <- all_data %>%
  mutate (clicks2=ifelse(clicks==0,0,1))

all_data<-all_data %>%
  rowwise()%>%
  mutate(match_sum_unique_relative1=sum(ain(unique(query_strsplit),unique(title_strsplit), method = "dl",maxDist=1))/length(unique(query_strsplit)))
################################################################




trainIndex <- createDataPartition(all_data$clicks2, p = 0.2, list = FALSE)
train.data <-all_data[-trainIndex,] 
learn.coef.data <-all_data[trainIndex,]  
rm(trainIndex)

rm(all_data)

gc()
# Clicks and incomes SUM by absolute position, position CTR and Income (by absolute positions)

pos_abs_click_data<- learn.coef.data %>%
  select (pos_abs, norm_income, clicks2) %>%
  group_by(pos_abs) %>%
  summarise ( position_ctr=sum(clicks2)/n() )
learn.coef.data <- left_join(learn.coef.data, pos_abs_click_data)
train.data <- left_join(train.data, pos_abs_click_data)
rm(pos_abs_click_data)

##################### CONSTRUCTING FEATURES: QUERY RELATED   ###########################

source_pos_api_data<-learn.coef.data%>%group_by(source,position_in_api_request)%>%
  summarise (source_pos_api_relative_ctr=sum(clicks2)/sum(position_ctr))
learn.coef.data <- left_join(learn.coef.data, source_pos_api_data)    ### REDIS
train.data <- left_join(train.data, source_pos_api_data)   ### REDIS
rm(source_pos_api_data)


query_data<-learn.coef.data %>%
  group_by(query_2) %>%
  summarise (query_relative_ctr=sum(clicks2)/sum(position_ctr))
learn.coef.data <- left_join(learn.coef.data, query_data)
train.data <- left_join(train.data, query_data)
rm(query_data)

product_data<-learn.coef.data %>%
  group_by(vendor_product_id) %>%
  summarise (product_relative_ctr=sum(clicks2)/sum(position_ctr))
learn.coef.data <- left_join(learn.coef.data, product_data)
train.data <- left_join(train.data, product_data)
rm(product_data)

brand_data<-learn.coef.data %>%
  group_by(brand_name_2) %>%
  summarise (brand_relative_ctr=sum(clicks2)/sum(position_ctr))
learn.coef.data <- left_join(learn.coef.data, brand_data)
train.data <- left_join(train.data, brand_data)
rm(brand_data)

shop_data<-learn.coef.data %>%
  group_by(shop_2) %>%
  summarise (shop_relative_ctr=sum(clicks2)/sum(position_ctr))
learn.coef.data <- left_join(learn.coef.data, shop_data)
train.data <- left_join(train.data, shop_data)
rm(shop_data)

query_price_data<-learn.coef.data %>%
  group_by(query_2,price2) %>%
  summarise (query_price_relative_ctr=sum(clicks2)/sum(position_ctr))
learn.coef.data <- left_join(learn.coef.data, query_price_data)
train.data <- left_join(train.data, query_price_data)
rm(query_price_data)

shop_pos_api_data<-learn.coef.data %>%
  group_by(shop_2,position_in_api_request) %>%
  summarise (shop_pos_api_relative_ctr=sum(clicks2)/sum(position_ctr))
learn.coef.data <- left_join(learn.coef.data, shop_pos_api_data)
train.data <- left_join(train.data, shop_pos_api_data)
rm(shop_pos_api_data)

query_condition_data<-learn.coef.data %>%
  group_by(query_2,condition_2) %>%
  summarise (query_condition_relative_ctr=sum(clicks2)/sum(position_ctr) )
learn.coef.data <- left_join(learn.coef.data, query_condition_data)
train.data <- left_join(train.data, query_condition_data)
rm(query_condition_data)

query_discount_data<-learn.coef.data %>%
  group_by(query_2,discount2) %>%
  summarise (query_discount_relative_ctr=sum(clicks2)/sum(position_ctr))
learn.coef.data <- left_join(learn.coef.data, query_discount_data)
train.data <- left_join(train.data, query_discount_data)
rm(query_discount_data)      

query_brand_data<-learn.coef.data %>%
  group_by(query_2,brand_name_2) %>%
  summarise (query_brand_relative_ctr=sum(clicks2)/sum(position_ctr))
learn.coef.data <- left_join(learn.coef.data, query_brand_data)
train.data <- left_join(train.data, query_brand_data)
rm(query_brand_data)

query_shop_data<-learn.coef.data %>%
  group_by(query_2,shop_2) %>%
  summarise (query_shop_relative_ctr=sum(clicks2)/sum(position_ctr) )
learn.coef.data <- left_join(learn.coef.data, query_shop_data)
train.data <- left_join(train.data, query_shop_data)
rm(query_shop_data)                     

query_product_data<-learn.coef.data %>%
  group_by(query_2,vendor_product_id) %>%
  summarise (query_product_relative_ctr=sum(clicks2)/sum(position_ctr) )
learn.coef.data <- left_join(learn.coef.data, query_product_data)
train.data <- left_join(train.data, query_product_data)
rm(query_product_data)

mean_query_data<-learn.coef.data %>%
  group_by(query_2) %>%
  summarise (mean_query_log_price=mean(log_price,na.rm=T),
             sd_query_log_price=sd(log_price, na.rm=T))
learn.coef.data <- left_join(learn.coef.data, mean_query_data)
train.data <- left_join(train.data, mean_query_data)
rm(mean_query_data)



train.data<-train.data %>%
  mutate (log_price_diff=log_price-mean_query_log_price/sd_query_log_price)
meantt<-mean(learn.coef.data$query_product_relative_ctr, na.rm = T)

######################################################################################
train.data <-train.data[!is.na(train.data$query_product_relative_ctr),]


train.data$title_strsplit<-NULL
train.data$query_strsplit<-NULL
train.data$brand_strsplit<-NULL
train.data$calc<-NULL
train.data$brand<-NULL
# 
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
is.inf.data.frame <- function(x)
  do.call(cbind, lapply(x, is.infinite))

train.data[is.nan(train.data)] <- NA
train.data[is.inf.data.frame(train.data)] <- NA

#############################################################################################
#############################################################################################
##### FITTING THE DATA #########
#######################################################################
#  train.data$id<-train.data$vendor_product_id

train <-train.data[!is.na(train.data$query_product_relative_ctr),]

trainIndex2 <- createDataPartition(train$query_product_relative_ctr, p = 0.5,
                                   list = FALSE)
train.TEMP <-train[-trainIndex2,]
test.TEMP <-train[trainIndex2,]

temp <-train.TEMP %>%
  select (pos_abs, norm_income, clicks2) %>%
  group_by(pos_abs) %>%
  summarise ( position_ctr_real=sum(clicks2)/n())
train.TEMP <- full_join(train.TEMP, temp)

temp <-test.TEMP %>%
  select (pos_abs, norm_income, clicks2) %>%
  group_by(pos_abs) %>%
  summarise ( position_ctr_real=sum(clicks2)/n())
test.TEMP <- full_join(test.TEMP, temp)

temp <-train.TEMP %>%
  group_by(query_2,vendor_product_id) %>%
  summarise(query_product_relative_ctr_real=sum(clicks2)/sum(position_ctr_real))
train.TEMP <- full_join(train.TEMP, temp)

temp <-test.TEMP %>%
  group_by(query_2,vendor_product_id) %>%
  summarise(query_product_relative_ctr_real=sum(clicks2)/sum(position_ctr_real))
test.TEMP <- full_join(test.TEMP, temp)
rm(temp)


train.TEMP <-train.TEMP[!is.na(train.TEMP$query_product_relative_ctr_real),]
test.TEMP <-test.TEMP[!is.na(test.TEMP$query_product_relative_ctr_real),]


#####################################################################################


lm_inc <- gbm(query_product_relative_ctr_real ~
                log_price+
                match_sum_unique_relative1+
                log_price_diff+
                log_discount+
                source_pos_api_relative_ctr+
                query_relative_ctr+
                product_relative_ctr+
                brand_relative_ctr+
                shop_relative_ctr+
                query_price_relative_ctr+
                shop_pos_api_relative_ctr+
                query_condition_relative_ctr+
                query_discount_relative_ctr+
                query_brand_relative_ctr+
                query_shop_relative_ctr+
                query_product_relative_ctr,
              train.TEMP, distribution = "gaussian", n.trees = 750, keep.data = F,
              bag.fraction = 0.75, interaction.depth = 8, shrinkage = 0.001,n.minobsinnode = 20)

save(lm_inc, file="lm_ctr_mod3_8m_subset_no_w10.rda")


comp<-cbind(test.TEMP$query_product_relative_ctr, test.TEMP$query_product_relative_ctr_real)
#Benchmark RMSE
RMSE (comp[,1], comp[,2], na.rm=T)

ppp<-rep(mean(test.TEMP$query_product_relative_ctr_real), length(comp[,1]))
ccc<-cbind(test.TEMP$query_product_relative_ctr_real, ppp)
RMSE(ccc[,1],ccc[,2])

pred_train<-predict(lm_inc, train.TEMP, n.trees = 750)
ccc<-cbind(train.TEMP$query_product_relative_ctr_real, pred_train)
RMSE(ccc[,1],ccc[,2])

pred_test<-predict(lm_inc, test.TEMP, n.trees = 750)
ccc<-cbind(test.TEMP$query_product_relative_ctr_real, pred_test)
RMSE(ccc[,1],ccc[,2])



library(ggplot2)
cut.point<-ceiling(length(pred_test)/100)
cut.vec<-rep(0,length(pred_test))
for (i in 1:100)
  cut.vec[order(pred_test)[((i-1)*cut.point+1):(min(cut.point*i,length(pred_test)))]]<-i
e<-aggregate(.~bucket,data.frame("x.l"=pred_test, "y.l"=test.TEMP$query_product_relative_ctr_real,"bucket"=cut.vec),mean)
g11<-ggplot(e, aes(x = x.l, y=y.l)) + geom_point(size=2, color='#CC0000') +
  xlab("Prediction_test") + ylab("Query_product_relative_ctr_real") + ggtitle("CTR_w_subset_pred_test") +
  geom_smooth() + 
  geom_abline()
g11
ggsave("CTR_w_subset_pred_test.png",g11)


cut.point<-ceiling(length(pred_train)/100)
cut.vec<-rep(0,length(pred_train))
for (i in 1:100)
  cut.vec[order(pred_train)[((i-1)*cut.point+1):(min(cut.point*i,length(pred_train)))]]<-i
e<-aggregate(.~bucket,data.frame("x.l"=pred_train, "y.l"=train.TEMP$query_product_relative_ctr_real,"bucket"=cut.vec),mean)
g12<-ggplot(e, aes(x = x.l, y=y.l)) + geom_point(size=2, color='#CC0000') +
  xlab("Prediction_train") + ylab("Query_product_relative_ctr_real") + ggtitle("CTR_w_subset_pred_train") +
  geom_smooth() + 
  geom_abline()
g12
ggsave("CTR_w_subset_pred_train.png",g12)



cut.point<-ceiling(length(test.TEMP$query_product_relative_ctr)/100)
cut.vec<-rep(0,length(test.TEMP$query_product_relative_ctr))
for (i in 1:100)
  cut.vec[order(test.TEMP$query_product_relative_ctr)[((i-1)*cut.point+1):(min(cut.point*i,length(test.TEMP$query_product_relative_ctr)))]]<-i
e<-aggregate(.~bucket,data.frame("x.l"=test.TEMP$query_product_relative_ctr, "y.l"=test.TEMP$query_product_relative_ctr_real,"bucket"=cut.vec),mean)
g2<-ggplot(e, aes(x = x.l, y=y.l)) + geom_point(size=2, color='#CC0000') +
  xlab("Query_product_relative_ctr") + ylab("Query_product_relative_ctr_real") + ggtitle("CTR_w_subset_test") +
  geom_smooth() + 
  geom_abline()
g2
ggsave("CTR_w_subset_test.png",g2)

cut.point<-ceiling(length(train.TEMP$query_product_relative_ctr)/100)
cut.vec<-rep(0,length(train.TEMP$query_product_relative_ctr))
for (i in 1:100)
  cut.vec[order(train.TEMP$query_product_relative_ctr)[((i-1)*cut.point+1):(min(cut.point*i,length(train.TEMP$query_product_relative_ctr)))]]<-i
e<-aggregate(.~bucket,data.frame("x.l"=train.TEMP$query_product_relative_ctr, "y.l"=train.TEMP$query_product_relative_ctr_real,"bucket"=cut.vec),mean)
g3<-ggplot(e, aes(x = x.l, y=y.l)) + geom_point(size=2, color='#CC0000') +
  xlab("Query_product_relative_ctr") + ylab("Query_product_relative_ctr_real") + ggtitle("CTR_w_subset_train") +
  geom_smooth() + 
  geom_abline()
g3
ggsave("CTR_w_subset_train.png",g3)




