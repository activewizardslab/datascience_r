library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)
options(scipen=999)

load(file="/mnt/week1.rda")
data<-week1
rm(week1)



data$displayed_price <-as.numeric(data$displayed_price)
data$price2 <- findInterval(data$displayed_price, c(0.00001, 10, 20, 50, 100, 200, 300, 500, 750, 1000, 1500, 2000, 3000, 5000))
# #data$price2 <- as.character(data$price2)
data$log_price <- log(data$displayed_price)
data$price3 <- findInterval(data$displayed_price, seq(0,5000,25))
data$norm_income[ is.na(data$norm_income) ] <- 0
data$clicks[ is.na(data$clicks) ] <- 0

dat <- tbl_df(dat)
nrow(dat)
dat <- unique(dat)
nrow(dat)


gc()

valid_column_names <- make.names(names=names(dat), unique=TRUE, allow_ = TRUE)
names(dat) <- valid_column_names

dat$msrp <-as.numeric(dat$msrp)
dat$auction <-NULL
dat$norm_income[ is.na(dat$norm_income) ] <- 0
dat$clicks[ is.na(dat$clicks) ] <- 0


q_pos<-data%>%filter(pos_abs<40)%>%group_by(page_load_id)%>%summarise(m_papi=max(position_in_api_request, na.rm=T))
mean(q_pos$m_papi, na.rm=T)



data$displayed_price <-as.numeric(data$displayed_price)
data$price2 <- findInterval(data$displayed_price, c(0.00001, 10, 20, 50, 100, 200, 300, 500, 750, 1000, 1500, 2000, 3000, 5000))
# #data$price2 <- as.character(data$price2)
data$log_price <- log(data$displayed_price)
data$price3 <- findInterval(data$displayed_price, seq(0,5000,25))


pos_1_2_3<-data%>%filter(pos_abs==1 | pos_abs==2 | pos_abs==3 | pos_abs==4 | pos_abs==5 | pos_abs==6)

dat_123<-pos_1_2_3%>%
  select(page_load_id,pos_abs,source,norm_income)%>%
  group_by(page_load_id)%>% arrange(pos_abs)%>%
  summarise(sources=paste(source,sep="|",collapse="|"), inc3= paste(norm_income ,sep="|",collapse="|"))

test<-dat_123%>%separate(sources, c('s1','s2','s3','s4','s5','s6'),sep="\\|")%>%separate(inc3, c('i1','i2','i3','i4','i5','i6'),sep="\\|")
pos3<-test%>%group_by(s1,s2,s3,s4,s5,s6)%>%
  summarise(nn=n(),
            mean_income=mean((as.numeric(i6)+as.numeric(i5)+as.numeric(i4))+(as.numeric(i3)+as.numeric(i2)+as.numeric(i1)), na.rm=T))

write.csv(pos3,"1-6.csv")

### M ###

pos_1_2_3<-data%>%filter(pos_abs==1 | pos_abs==2 | pos_abs==3 | pos_abs==4 | pos_abs==5 | pos_abs==6)%>%filter(device=="m")#%>%na.omit()

dat_123<-pos_1_2_3%>%
  select(page_load_id,pos_abs,source,norm_income)%>%
  group_by(page_load_id)%>% arrange(pos_abs)%>%
  summarise(sources=paste(source,sep="|",collapse="|"), inc3= paste(norm_income ,sep="|",collapse="|"))

test<-dat_123%>%separate(sources, c('s1','s2','s3','s4','s5','s6'),sep="\\|")%>%separate(inc3, c('i1','i2','i3','i4','i5','i6'),sep="\\|")
pos3<-test%>%group_by(s1,s2,s3,s4,s5,s6)%>%
  summarise(nn=n(),
            mean_income=mean((as.numeric(i6)+as.numeric(i5)+as.numeric(i4))+(as.numeric(i3)+as.numeric(i2)+as.numeric(i1)), na.rm=T))

write.csv(pos3,"1-6m.csv")

### C ###

pos_1_2_3<-data%>%filter(pos_abs==1 | pos_abs==2 | pos_abs==3 | pos_abs==4 | pos_abs==5 | pos_abs==6)%>%filter(device=="c")#%>%na.omit()

dat_123<-pos_1_2_3%>%
  select(page_load_id,pos_abs,source,norm_income)%>%
  group_by(page_load_id)%>% arrange(pos_abs)%>%
  summarise(sources=paste(source,sep="|",collapse="|"), inc3= paste(norm_income ,sep="|",collapse="|"))

test<-dat_123%>%separate(sources, c('s1','s2','s3','s4','s5','s6'),sep="\\|")%>%separate(inc3, c('i1','i2','i3','i4','i5','i6'),sep="\\|")
pos3<-test%>%group_by(s1,s2,s3,s4,s5,s6)%>%
  summarise(nn=n(),
            mean_income=mean((as.numeric(i6)+as.numeric(i5)+as.numeric(i4))+(as.numeric(i3)+as.numeric(i2)+as.numeric(i1)), na.rm=T))

write.csv(pos3,"1-6c.csv")

### T ###

pos_1_2_3<-data%>%filter(pos_abs==1 | pos_abs==2 | pos_abs==3 | pos_abs==4 | pos_abs==5 | pos_abs==6)%>%filter(device=="t")#%>%na.omit()

dat_123<-pos_1_2_3%>%
  select(page_load_id,pos_abs,source,norm_income)%>%
  group_by(page_load_id)%>% arrange(pos_abs)%>%
  summarise(sources=paste(source,sep="|",collapse="|"), inc3= paste(norm_income ,sep="|",collapse="|"))

test<-dat_123%>%separate(sources, c('s1','s2','s3','s4','s5','s6'),sep="\\|")%>%separate(inc3, c('i1','i2','i3','i4','i5','i6'),sep="\\|")
pos3<-test%>%group_by(s1,s2,s3,s4,s5,s6)%>%
  summarise(nn=n(),
            mean_income=mean((as.numeric(i6)+as.numeric(i5)+as.numeric(i4))+(as.numeric(i3)+as.numeric(i2)+as.numeric(i1)), na.rm=T))

write.csv(pos3,"1-6t.csv")

######################################################


dat<-data
pageloads_incomes<-dat%>%group_by(page_load_id)%>%summarise(page_inc=sum(norm_income,na.rm=T))

dat<-left_join(dat,pageloads_incomes)

pos_1_2_3<-dat%>%filter(pos_abs==1 | pos_abs==2 | pos_abs==3 | pos_abs==4 | pos_abs==5 | pos_abs==6)

dat_123<-pos_1_2_3%>%
  select(page_load_id,pos_abs,source,norm_income,page_inc)%>%
  group_by(page_load_id)%>% arrange(pos_abs)%>%
  summarise(sources=paste(source,sep="|",collapse="|"), inc3= paste(norm_income ,sep="|",collapse="|"),
            m_page_inc=mean(page_inc))

test<-dat_123%>%separate(sources, c('s1','s2','s3','s4','s5','s6'),sep="\\|")%>%
  separate(inc3, c('i1','i2','i3','i4','i5','i6'),sep="\\|")

pos3<-test%>%group_by(s1,s2,s3,s4,s5,s6)%>%
  summarise(nn=n(),
            mean_income=mean((as.numeric(i6)+as.numeric(i5)+as.numeric(i4)+as.numeric(i3)+as.numeric(i2)+as.numeric(i1)), na.rm=T),
            mean_page_imcome=mean(m_page_inc))

write.csv(pos3,"1-6_allinc.csv")

### M ###

dat<-data%>%filter(device=="m")
pageloads_incomes<-dat%>%group_by(page_load_id)%>%summarise(page_inc=sum(norm_income,na.rm=T))

dat<-left_join(dat,pageloads_incomes)

pos_1_2_3<-dat%>%filter(pos_abs==1 | pos_abs==2 | pos_abs==3 | pos_abs==4 | pos_abs==5 | pos_abs==6)

dat_123<-pos_1_2_3%>%
  select(page_load_id,pos_abs,source,norm_income,page_inc)%>%
  group_by(page_load_id)%>% arrange(pos_abs)%>%
  summarise(sources=paste(source,sep="|",collapse="|"), inc3= paste(norm_income ,sep="|",collapse="|"),
            m_page_inc=mean(page_inc))

test<-dat_123%>%separate(sources, c('s1','s2','s3','s4','s5','s6'),sep="\\|")%>%
  separate(inc3, c('i1','i2','i3','i4','i5','i6'),sep="\\|")

pos3<-test%>%group_by(s1,s2,s3,s4,s5,s6)%>%
  summarise(nn=n(),
            mean_income=mean((as.numeric(i6)+as.numeric(i5)+as.numeric(i4)+as.numeric(i3)+as.numeric(i2)+as.numeric(i1)), na.rm=T),
            mean_page_imcome=mean(m_page_inc))

write.csv(pos3,"1-6_allinc_M.csv")

### C ###

dat<-data%>%filter(device=="c")
pageloads_incomes<-dat%>%group_by(page_load_id)%>%summarise(page_inc=sum(norm_income,na.rm=T))

dat<-left_join(dat,pageloads_incomes)

pos_1_2_3<-dat%>%filter(pos_abs==1 | pos_abs==2 | pos_abs==3 | pos_abs==4 | pos_abs==5 | pos_abs==6)

dat_123<-pos_1_2_3%>%
  select(page_load_id,pos_abs,source,norm_income,page_inc)%>%
  group_by(page_load_id)%>% arrange(pos_abs)%>%
  summarise(sources=paste(source,sep="|",collapse="|"), inc3= paste(norm_income ,sep="|",collapse="|"),
            m_page_inc=mean(page_inc))

test<-dat_123%>%separate(sources, c('s1','s2','s3','s4','s5','s6'),sep="\\|")%>%
  separate(inc3, c('i1','i2','i3','i4','i5','i6'),sep="\\|")

pos3<-test%>%group_by(s1,s2,s3,s4,s5,s6)%>%
  summarise(nn=n(),
            mean_income=mean((as.numeric(i6)+as.numeric(i5)+as.numeric(i4)+as.numeric(i3)+as.numeric(i2)+as.numeric(i1)), na.rm=T),
            mean_page_imcome=mean(m_page_inc))

write.csv(pos3,"1-6_allinc_C.csv")

### T ###

dat<-data%>%filter(device=="t")
pageloads_incomes<-dat%>%group_by(page_load_id)%>%summarise(page_inc=sum(norm_income,na.rm=T))

dat<-left_join(dat,pageloads_incomes)

pos_1_2_3<-dat%>%filter(pos_abs==1 | pos_abs==2 | pos_abs==3 | pos_abs==4 | pos_abs==5 | pos_abs==6)

dat_123<-pos_1_2_3%>%
  select(page_load_id,pos_abs,source,norm_income,page_inc)%>%
  group_by(page_load_id)%>% arrange(pos_abs)%>%
  summarise(sources=paste(source,sep="|",collapse="|"), inc3= paste(norm_income ,sep="|",collapse="|"),
            m_page_inc=mean(page_inc))

test<-dat_123%>%separate(sources, c('s1','s2','s3','s4','s5','s6'),sep="\\|")%>%
  separate(inc3, c('i1','i2','i3','i4','i5','i6'),sep="\\|")

pos3<-test%>%group_by(s1,s2,s3,s4,s5,s6)%>%
  summarise(nn=n(),
            mean_income=mean((as.numeric(i6)+as.numeric(i5)+as.numeric(i4)+as.numeric(i3)+as.numeric(i2)+as.numeric(i1)), na.rm=T),
            mean_page_imcome=mean(m_page_inc))

write.csv(pos3,"1-6_allinc_T.csv")



unqiue_price=unique(data$price3)
for(i in unqiue_price){
  
  data_f<-filter(data,price3==i)
  
  
  
  income_from_price_groups_pos0 <- data_f%>%
    group_by(position_in_api_request,source)%>%
    summarise(nn=n(),
              sum_clicks=sum(clicks),
              inc_sum=sum(norm_income),
              inc_count=sum(norm_income!=0),
              inc_count_vs_clicks = inc_count/sum_clicks,
              inc_vs_clicks=inc_sum/sum_clicks,
              inc_mean = mean(norm_income),
              log_price_mean = mean(log_price)
    )
  write.csv(income_from_price_groups_pos0, file = paste("table/income_from_price_groups_price",i*25,".csv",sep=""))
  
  plot_pos0_log_pr<-ggplot(income_from_price_groups_pos0,
                           aes(x=position_in_api_request,
                               y=log_price_mean,
                               color=source))+
    geom_point()+geom_path()
  plot_pos0_log_pr
  ggsave(paste("graph/plot_price",i*25,"_log_pr.png",sep=""),plot_pos0_log_pr)
  
  plot_pos0_inc_count_vs_clicks<-ggplot(income_from_price_groups_pos0,
                                        aes(x=position_in_api_request,
                                            y=inc_count_vs_clicks,
                                            color=source))+
    geom_point()+geom_path()
  plot_pos0_inc_count_vs_clicks
  ggsave(paste("graph/plot_price",i*25,"_inc_count_vs_clicks.png",sep=""),plot_pos0_inc_count_vs_clicks)
  
  plot_pos0_inc_mean<-ggplot(income_from_price_groups_pos0,
                             aes(x=position_in_api_request,
                                 y=inc_mean,
                                 color=source))+
    geom_point()+geom_path()
  plot_pos0_inc_mean
  ggsave(paste("graph/plot_price",i*25,"_inc_mean.png",sep=""),plot_pos0_inc_mean)
  
  plot_pos0_inc_sum<-ggplot(income_from_price_groups_pos0,
                            aes(x=position_in_api_request,
                                y=inc_sum,
                                color=source))+geom_point()+geom_path()
  plot_pos0_inc_sum
  ggsave(paste("graph/plot_price",i*25,"_inc_sum.png",sep=""),plot_pos0_inc_sum)
  
  plot_pos0_inc_vs_clicks<-ggplot(income_from_price_groups_pos0,
                                  aes(x=position_in_api_request,
                                      y=inc_vs_clicks,
                                      color=source))+
    geom_point()+geom_path()
  plot_pos0_inc_vs_clicks
  ggsave(paste("graph/plot_price",i*25,"_inc_vs_clicks.png",sep=""),plot_pos0_inc_vs_clicks)
  
  
}
