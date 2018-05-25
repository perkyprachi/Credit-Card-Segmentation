

# Credit Card Segmentation

Cred_seg<- read.csv(file.choose(),header = TRUE)
View(Cred_seg)

colSums(is.na(Cred_seg)|Cred_seg == 'NULL')

Cred_seg$Monthly_Avg_Purchase<-  Cred_seg$PURCHASES/12
Cred_seg$Cash_advance_Avg_Amt<-  Cred_seg$CASH_ADVANCE/12 
Cred_seg$purchase_type[Cred_seg$ONEOFF_PURCHASES>Cred_seg$INSTALLMENTS_PURCHASES]<-1
Cred_seg$purchase_type[Cred_seg$ONEOFF_PURCHASES==Cred_seg$INSTALLMENTS_PURCHASES]<-1
Cred_seg$purchase_type[Cred_seg$ONEOFF_PURCHASES<Cred_seg$INSTALLMENTS_PURCHASES]<-0
Cred_seg$PURCHASES_TRX[Cred_seg$PURCHASES_TRX == "0"]<- 1
Cred_seg$Avg_Amt_Per_Purchase<- Cred_seg$PURCHASES/Cred_seg$PURCHASES_TRX

Cred_seg$Limit_Usage<- Cred_seg$BALANCE/Cred_seg$CREDIT_LIMIT 

Cred_seg$Payments_ratio <- Cred_seg$PAYMENTS/Cred_seg$MINIMUM_PAYMENTS
Cred_seg[Cred_seg=="Inf"] <- NA

write.csv(Cred_seg,file = "D:/AnalitixLabs/New folder/CreditCard2/Cred_seg.csv")

#2.________________________________________________________


mystats=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.95,na.rm=T)
    LC2=quantile(x,0.05,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1_=ot1,ot_m2_=ot2,ot_m3_=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    return( c(Var_Type=Var_Type, n=n,nmiss=nmiss))
  }
}
  
vars<-c("BALANCE","BALANCE_FREQUENCY",
          "PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES",
          "CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY",
          "PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX",
          "PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS",
          "MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE","Monthly_Avg_Purchase","Cash_advance_Avg_Amt",
        "purchase_type","Avg_Amt_Per_Purchase","Limit_Usage","Payments_ratio"
)

diag_stats<-t(data.frame(apply(Cred_seg[vars], 2, FUN = mystats)))        
View(diag_stats)
write.csv(diag_stats,file = "D:/AnalitixLabs/New folder/CreditCard2/Diag_stat.csv")

###Missing Value Treatment

install.packages("Hmisc")
require(Hmisc)

Cred_seg[vars]<- data.frame(apply(Cred_seg[vars],2,function(x) impute(x,mean)))
colSums(is.na(Cred_seg)|Cred_seg== 'NA')

write.csv(Cred_seg,file = "D:/AnalitixLabs/New folder/CreditCard2/MissingValuesTreated.csv")

####Outlier Treatment
M1_fun <- function(x){
  quantiles <- quantile( x, c(.01, .99 ),na.rm=TRUE )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

vars1<- c( "BALANCE","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","CASH_ADVANCE",
          "CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX","PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS",
          "MINIMUM_PAYMENTS","TENURE","Monthly_Avg_Purchase","purchase_type","Avg_Amt_Per_Purchase",
          "Limit_Usage","Payments_ratio")

Cred_seg[,vars1] <-lapply(Cred_seg[,vars1],M1_fun)

summary(Cred_seg)

write.csv(Cred_seg,file = "D:/AnalitixLabs/New folder/CreditCard2/outlierTreated.csv")

xyz<- sapply(Cred_seg,is.numeric)
inputdata_final<- Cred_seg[,xyz]

#Factor Analysis

corrm1<-cor(inputdata_final)

write.csv(corrm1,file = "D:/AnalitixLabs/New folder/CreditCard2/CorrelationMatrix1.csv")
names(inputdata_final)
Final_data1<- subset(inputdata_final, select = -c( PURCHASES,ONEOFF_PURCHASES,INSTALLMENTS_PURCHASES,
                                                   ONEOFF_PURCHASES_FREQUENCY,PURCHASES_INSTALLMENTS_FREQUENCY,
                                                   CASH_ADVANCE,CASH_ADVANCE_TRX,PURCHASES_TRX))

View(Final_data1)
CorrM<- cor(Final_data1)
View(CorrM)
write.csv(CorrM,file = "D:/AnalitixLabs/New folder/CreditCard2/CorrelationMatrix2.csv")

install.packages("psych")
install.packages("GPArotation")
require(psych)
require(GPArotation)

#using Scree plot to using the variation in Eigen Values

scree(CorrM, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE)

#Eigen Values

eigen(CorrM)$values

#Calculating Variance
install.packages("dplyr")
require(dplyr)
eigen_values <- mutate(data.frame(eigen(CorrM)$values)
                       ,cum_sum_eigen=cumsum(eigen.CorrM..values)
                       , pct_var=eigen.CorrM..values/sum(eigen.CorrM..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.CorrM..values))
View(eigen_values)

write.csv(eigen_values,file = "D:/AnalitixLabs/New folder/CreditCard2/Eigen_Values.csv")

#Factor Analysis (ml=Maximum Liklihood)

FA<-fa(r=CorrM,5, rotate="varimax", fm="ml")

print(FA)

#Sorting

FA_SORT<-fa.sort(FA)
### LISTING OUT THE OBJECTS

ls(FA_SORT)                                                 
FA_SORT$loadings
 

#CApturing Loadings

Loadings <- data.frame(FA_SORT$loadings[1:ncol(Final_data1),])
View(Loadings)
write.csv(Loadings,file = "D:/AnalitixLabs/New folder/CreditCard2/Loadings.csv")
names(Final_data1)
#building Clusters using K-means
Final_Data<- (Final_data1[,c("BALANCE","BALANCE_FREQUENCY","PURCHASES_FREQUENCY","CASH_ADVANCE_FREQUENCY","Cash_advance_Avg_Amt",
                             "CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS", "Monthly_Avg_Purchase",    
                              "purchase_type","Limit_Usage","Payments_ratio")])        
                                
View(Final_Data)

Final_Data2<-scale(Final_Data)


View(Final_Data2)
cluster_three <- kmeans(Final_Data2,3)
cluster_four <- kmeans(Final_Data2,4)
cluster_five <- kmeans(Final_Data2,5)
cluster_six <- kmeans(Final_Data2,6)
cluster_three$cluster


clust_data1<-cbind(Final_Data,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster,km_clust_6=cluster_six$cluster )
View(clust_data1)
#Cluster Graph
install.packages("cluster")
require(cluster)
clusplot(Final_Data2, #dataframe
         cluster_five$cluster, #clusterdata
         color = TRUE, #color
         #shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)


#Data Profiling



clust_data1$km_clust_3<-factor(clust_data1$km_clust_3)
clust_data1$km_clust_4=factor(clust_data1$km_clust_4)
clust_data1$km_clust_5=factor(clust_data1$km_clust_5)
clust_data1$km_clust_6=factor(clust_data1$km_clust_6)
View(clust_data2)
clust_data2 <- data.frame(clust_data1)

write.csv(clust_data2,file = "D:/AnalitixLabs/New folder/CreditCard2/clustdata.csv")

install.packages("tables")
require(tables)

profiling <- tabular(1+BALANCE+BALANCE_FREQUENCY+PURCHASES_FREQUENCY+CASH_ADVANCE_FREQUENCY+Cash_advance_Avg_Amt+
                       CREDIT_LIMIT+PAYMENTS+MINIMUM_PAYMENTS+Monthly_Avg_Purchase+    
                       purchase_type+Limit_Usage+Payments_ratio
                     ~mean+(mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5)+(mean*km_clust_6), data=clust_data2)
profile1<-as.matrix(profiling)
profile1<-data.frame(profile1)

View(profile1)

profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                 data=clust_data2)
profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
View(profile2)

write.csv(profile1,file = "D:/AnalitixLabs/New folder/CreditCard2/profile1.csv",row.names = F)
write.csv(profile2,file = "D:/AnalitixLabs/New folder/CreditCard2/profile2.csv",row.names = F)
