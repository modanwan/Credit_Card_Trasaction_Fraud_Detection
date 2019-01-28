library(dplyr)
library(stringr)
library(lubridate)
setwd("./data")
#data= readRDS("df_afterKS.rds")
df = read.csv("train_all.csv")
saveRDS(df, file = 'df_master.RDS')

df$click_time = as.character(df$click_time)

byclick = df %>%
  group_by(click_time) %>%
  summarise(count = n()) %>%
  arrange(click_time)

str(df)
names(df)
# remove the first 60s before ks
data.new <- df %>%
  filter(!click_time %in% c(byclick$click_time[1:60])) %>%
  select(-c(ip,app,device,os,channel,click_time,hour,record))

## ks selection
t=data.new%>%filter(is_attributed==1)
t1=data.new%>%filter(is_attributed==0)


names=names(data.new%>%select(-is_attributed))
ks=data.frame()

for (j in names){
  kj=ks.test(t[,j],t1[,j],alternative="two.sided")
  ks[j,'KS']=kj[["statistic"]][["D"]]
}

ks$name=rownames(ks)
k=(ks%>%arrange(-KS))[1:30,]

# remove the first 60s records
dfks=df%>%
  filter(!click_time %in% c(byclick$click_time[1:60])) %>%
  select(k$name,is_attributed)

saveRDS(dfks,"df_afterKS.rds")
