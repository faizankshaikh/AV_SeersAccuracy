total<-read.csv("train.csv",header=T)
library(lubridate)

total$Transaction_Date<-as.character(total$Transaction_Date)
total$date<-lubridate::dmy(total$Transaction_Date)
total$yr<-lubridate::year(total$date)

table(total$Gender)
levels(total$Gender)
levels(total$Gender)[levels(total$Gender) %in% c(" ","C")]<-"M"

table(total$Referred_Friend)
levels(total$Referred_Friend)[levels(total$Referred_Friend)==""]<-"NO"

levels(total$Purchased_in_Sale)
levels(total$Purchased_in_Sale)[levels(total$Purchased_in_Sale)=="N"]<-"0"
levels(total$Purchased_in_Sale)[levels(total$Purchased_in_Sale)=="Y"]<-"1"

table(total$Payment_Mode)
levels(total$Payment_Mode)[levels(total$Payment_Mode)==""]<-"Other"

total$birth<-lubridate::dmy(total$DOB)
total$dob<-substr(total$birth,3,4)
total$dob = paste(rep(19,1), total$dob, sep = "")
total$dob<-as.numeric(total$dob)


total$age=2007-total$dob
total$amount=total$Transaction_Amount*(10^(-2*total$Number_of_EMI))


total$Sales_Executive_ID=NULL
total$date=NULL
total$Transaction_Date=NULL
total$Transaction_ID=NULL
total$DOB=NULL
total$birth=NULL
total$dob=NULL
total$Number_of_EMI=NULL
total$Transaction_Amount=NULL

require(reshape2)
mcast=dcast(total,Client_ID~yr)
mcast1<-dcast(total,Client_ID~Purchased_in_Sale+Gender+Referred_Friend)
a<-aggregate(age~Client_ID, data = total, FUN=mean)
b<-aggregate(amount~Client_ID, data = total, sum)
mcast2<-dcast(total,Client_ID~Lead_Source_Category)
mcast3<-dcast(total,Client_ID~Sales_Executive_Category)
mcast4<-dcast(total,Client_ID~Product_Category)
mcast5<-dcast(total,Client_ID~Payment_Mode)
mcast6<-aggregate(var~Client_ID, data=total,sum)
mcast7<-dcast(total,Client_ID~Var1)
mcast8<-dcast(total,Client_ID~Var2)
mcast9<-dcast(total,Client_ID~Var3)

X<-cbind(mcast,mcast1,mcast2,mcast3,mcast4,mcast5,mcast6,mcast7,mcast8,mcast9,a,b)
X<-X[,-c(6,15,20,26,35,40,42,44)]

X$payment=NA
for(i in 1:417107){
  if (X[i,32]==max(X[i,31],X[i,32],X[i,33],X[i,34])){
    X[i,38]="Cash"
  }else if (X[i,33]==max(X[i,31],X[i,32],X[i,33],X[i,34])){
    X[i,38]="Cheque"
  } else if (X[i,34]==max(X[i,31],X[i,32],X[i,33],X[i,34])){
     X[i,38]="Card"
  }else if (X[i,31]==max(X[i,31],X[i,32],X[i,33],X[i,34])){
    X[i,38]="Others"
  }
}

X$lead=NA
for(i in 1:417107){
  if (X[i,14]==max(X[i,14],X[i,15],X[i,16],X[i,17])){
    X[i,39]="Advertisement"
  }else if (X[i,16]==max(X[i,14],X[i,15],X[i,16],X[i,17])){
    X[i,39]="References"
  }else if (X[i,17]==max(X[i,14],X[i,15],X[i,16],X[i,17])){
    X[i,39]="Walkin"
  }else if (X[i,15]==max(X[i,14],X[i,15],X[i,16],X[i,17])){
    X[i,39]="others"
  }
}

X$Product=NA
for(i in 1:417107){
  if (X[i,23]==max(X[i,23],X[i,24],X[i,25],X[i,26],X[i,27],X[i,28],X[i,29],X[i,30])){
    X[i,40]="A"
  }else if (X[i,24]==max(X[i,23],X[i,24],X[i,25],X[i,26],X[i,27],X[i,28],X[i,29],X[i,30])){
    X[i,40]="B"
  }else if (X[i,25]==max(X[i,23],X[i,24],X[i,25],X[i,26],X[i,27],X[i,28],X[i,29],X[i,30])){
    X[i,40]="C"
  }else if (X[i,26]==max(X[i,23],X[i,24],X[i,25],X[i,26],X[i,27],X[i,28],X[i,29],X[i,30])){
    X[i,40]="D"
  }else if (X[i,27]==max(X[i,23],X[i,24],X[i,25],X[i,26],X[i,27],X[i,28],X[i,29],X[i,30])){
    X[i,40]="E"
  }else if (X[i,28]==max(X[i,23],X[i,24],X[i,25],X[i,26],X[i,27],X[i,28],X[i,29],X[i,30])){
    X[i,40]="F"
  }else if (X[i,29]==max(X[i,23],X[i,24],X[i,25],X[i,26],X[i,27],X[i,28],X[i,29],X[i,30])){
    X[i,40]="G"
  }else if (X[i,30]==max(X[i,23],X[i,24],X[i,25],X[i,26],X[i,27],X[i,28],X[i,29],X[i,30])){
    X[i,40]="H"
  }
}

X$employee=NA
for(i in 1:417107){
  if (X[i,18]==max(X[i,18],X[i,19],X[i,20],X[i,21],X[i,22])){
    X[i,41]="A"
  }else if (X[i,19]==max(X[i,18],X[i,19],X[i,20],X[i,21],X[i,22])){
    X[i,41]="B"
  }else if (X[i,20]==max(X[i,18],X[i,19],X[i,20],X[i,21],X[i,22])){
    X[i,41]="C"
  }else if (X[i,21]==max(X[i,18],X[i,19],X[i,20],X[i,21],X[i,22])){
    X[i,41]="D"
  }else if (X[i,22]==max(X[i,18],X[i,19],X[i,20],X[i,21],X[i,22])){
    X[i,41]="E"
  }
}

X$target=NA
for( i in 1:417107){
  if((X[i,2]>=1 & X[i,3]>=1)|(X[i,3]>=1& X[i,4]>=1)|(X[i,4]>=1& X[i,5]>=1)){
    X[i,42]=1
  }
  else{
    X[i,42]=0
  }
}

X$target<-as.factor(X$target)
table(X$target)
summary(X$age)
summary(X$amount)
boxplot(X$amount)

Y=X[,-c(14:34)]

write.csv(Y,"data_cleaned.csv")