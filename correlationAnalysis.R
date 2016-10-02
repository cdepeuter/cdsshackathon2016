train=read.csv("train.csv")
product=read.csv("product_attributes.csv")
store=read.csv("store_features.csv")
train=tbl_df(train)
all_sales=train %>% group_by(product_key) %>% summarise(all_sales=sum(total_sales))
promo_sales= train %>% group_by(product_key) %>% summarise(promo_sales=sum(promotional_sales))
promo_percentage=data.frame(product_key=promo_sales$product_key,promo_percent=(promo_sales$promo_sales/all_sales$all_sales))
promo_percentage=tbl_df(promo_percentage)
promo_percentage=arrange(promo_percentage,desc(promo_percent))
badcount=matrix(0,nrow=60,ncol=1)
badcount=tbl_df(badcount)
badcountrank=cbind(badcount,p_key=rownames(badcount))
badcountrank=arrange(badcount,desc(V1))
sales_rank=arrange(all_sales,desc(all_sales))
traintest=train
traintest=traintest[!is.na(traintest$on_hand_qty),]
total_units_sold= traintest %>% group_by(product_key) %>% summarise(tot_units=(sum(total_units)))
total_onhand=traintest %>% group_by(product_key) %>% summarise(tot_on_hand=(sum(on_hand_qty)))
ratio=matrix(nrow=length(traintest$product_key),ncol=1)
for(i in 1:length(traintest$product_key)){
  if(traintest$on_hand_qty[i]<=0){ratio[i]=0}
  else{ratio[i]=traintest$total_units[i]/traintest$on_hand_qty[i]}
}
traintest=tbl_df(cbind(traintest,ratio))
ratios=traintest %>% group_by(product_key) %>% summarise(mean_ratio=(mean(ratio)))
a1=cbind(product_key=ratios$product_key,all_sales=all_sales$all_sales,badcount=badcount$V1,ratio=ratios$mean_ratio)
a1=data.frame(a1)
ggplot(a1,aes(x=product_key,fill=ratio))+geom_bar()+facet_wrap(~all_sales)
print("DONE")
