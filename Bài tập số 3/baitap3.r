library(readxl)
library(table1)
library(ggplot2)
library(anytime)
library(dplyr)


###############################################
############## Doc du lieu ####################
###############################################

Dulieu <- read.csv("C:/Users/nqhun/OneDrive/Desktop/KTQD/DataAnalytics/Visastatus.csv")
View(Dulieu)
names(Dulieu)
dim(Dulieu)
sum(is.na(Dulieu))
str(Dulieu)
summary(Dulieu)

#as.numeric(Dulieu$PREVAILING_WAGE_PER_YEAR)

###############################################
#############    Cau 1    #####################
###############################################

table(Dulieu$WORK_STATE)
table1(~WORK_STATE|JOB_TITLE_SUBGROUP,data=Dulieu,overall="Total")

###############################################
#############    Cau 2    #####################
###############################################
unique(Dulieu$VISA_CLASS)
table(Dulieu$VISA_CLASS)

###############################################
#############    Cau 3    #####################
###############################################

table(Dulieu$JOB_TITLE_SUBGROUP)
table1(~JOB_TITLE_SUBGROUP,data=Dulieu)

###############################################
#############    Cau 4    #####################
###############################################

#bao nhieu % theo nganh nghe
sum(is.na(Dulieu$JOB_TITLE_SUBGROUP))
unique(Dulieu$JOB_TITLE_SUBGROUP)
software_engineer=length(Dulieu$JOB_TITLE_SUBGROUP[Dulieu$JOB_TITLE_SUBGROUP=="software engineer"])
assist_professor=length(Dulieu$JOB_TITLE_SUBGROUP[Dulieu$JOB_TITLE_SUBGROUP=="assistant professor"])
teacher=length(Dulieu$JOB_TITLE_SUBGROUP[Dulieu$JOB_TITLE_SUBGROUP=="teacher"])
business_analyst=length(Dulieu$JOB_TITLE_SUBGROUP[Dulieu$JOB_TITLE_SUBGROUP=="business analyst"])
management_consultant=length(Dulieu$JOB_TITLE_SUBGROUP[Dulieu$JOB_TITLE_SUBGROUP=="management consultant"])
data_analyst=length(Dulieu$JOB_TITLE_SUBGROUP[Dulieu$JOB_TITLE_SUBGROUP=="data analyst"])
attorney=length(Dulieu$JOB_TITLE_SUBGROUP[Dulieu$JOB_TITLE_SUBGROUP=="attorney"])
data_scientist=length(Dulieu$JOB_TITLE_SUBGROUP[Dulieu$JOB_TITLE_SUBGROUP=="data scientist"])

#tinh phan tram
num_each_job=c(software_engineer,assist_professor,business_analyst,data_scientist,data_analyst,teacher,attorney,management_consultant)
phantram=round(num_each_job/sum(num_each_job)*100,1)
phantram=paste(phantram,"%")

##############################
#####  cach 1 (ham pie)  #####
##############################
jobtitle=unique(Dulieu$JOB_TITLE_SUBGROUP)
pie(num_each_job,labels = phantram, col=c("red","green","yellow","blue","pink","purple","light blue","black"), main="Phan bo du lieu theo nganh nghe")
legend("topleft",legend=jobtitle,fill=c("red","green","yellow","blue","pink","purple","light blue","black"))

##############################
#####  cach 2 (ggplot)  ######
##############################
a=data.frame(num_each_job,phantram,jobtitle)
ggplot(a, aes(x = "", y = num_each_job, fill = jobtitle)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label=paste0(phantram)),position=position_stack(vjust=0.5)) +
  coord_polar("y") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Phan bo du lieu theo nganh nghe", fill = "Nganh nghe")

#Su dung ham for de khai bao
job_title <- unique(Dulieu$JOB_TITLE_SUBGROUP)
list_job <- numeric(length(job_titles))
for (i in seq_along(job_titles)){
  list_job[i] <- length(Dulieu$JOB_TITLE_SUBGROUP[Dulieu$JOB_TITLE_SUBGROUP == job_titles[i]])
}
phantram2=round(list_job/sum(list_job)*100,1)
b=data.frame(list_job,phantram2,job_title)
b

#dung table de tinh phan tram
phantram3=round(table(Dulieu$JOB_TITLE_SUBGROUP)/length(Dulieu$JOB_TITLE_SUBGROUP)*100,1)
phantram3
phantram3=paste(phantram2,"%",sep="")
pie(table(Dulieu$JOB_TITLE_SUBGROUP),labels=phantram2,col=c("red","green","yellow","blue","pink","purple","light blue","black"), main="Phan bo du lieu theo nganh nghe")
legend("bottomleft",legend=names(table(Dulieu$JOB_TITLE_SUBGROUP)),fill=c("red","green","yellow","blue","pink","purple","light blue","black"))

###############################################
#############    Cau 5    #####################
###############################################

#cach 1
barplot(table(Dulieu$JOB_TITLE_SUBGROUP),col = 'green')
# cach 2
ggplot(Dulieu,aes(x=JOB_TITLE_SUBGROUP)) +
  geom_bar(fill="green") +
  theme_minimal()

###############################################
#############    Cau 6    #####################
###############################################
#trong cac cong cu thi bang mang nhieu thong tin nhat, tuy nhien de dua ra cai nhin tot nhat thi nen dung barplot

###############################################
#############    Cau 7    #####################
###############################################
hist(Dulieu$PAID_WAGE_PER_YEAR,breaks=50)
abline(v=mean(Dulieu$PAID_WAGE_PER_YEAR),col='red',lwd=3)
abline(v=median(Dulieu$PAID_WAGE_PER_YEAR),col="blue",lwd=3)
hist(Dulieu$PREVAILING_WAGE_PER_YEAR,main = 'Tan so phan phoi ki vong',xlab = 'Muc luong ky vong')
abline(v=mean(Dulieu$PREVAILING_WAGE_PER_YEAR,na.rm=TRUE),col='red',lwd=3)
abline(v=median(Dulieu$PREVAILING_WAGE_PER_YEAR,na.rm=TRUE),col="blue",lwd=3)

###############################################
#############    Cau 8    #####################
###############################################
boxplot(cbind(Dulieu$PAID_WAGE_PER_YEAR,Dulieu$PREVAILING_WAGE_PER_YEAR)
        , beside=TRUE
        , names=c("Thuc te","Ky vong")
        , xlab = "Muc luong"
        , ylab = " Gia tri"
        , main = "Bieu do boxplot ve muc luong thuc te va muc luong ky vong")
###############################################
#############    Cau 9    #####################
###############################################
#cach 1
plot(density(Dulieu$PAID_WAGE_PER_YEAR),main  = 'Bieu do tan suat kernel')
#cach 2
ggplot(Dulieu,aes(x=PAID_WAGE_PER_YEAR)) +
  geom_density(col="red",fill="blue")

length(Dulieu$PAID_WAGE_PER_YEAR[Dulieu$PAID_WAGE_PER_YEAR>mean(Dulieu$PAID_WAGE_PER_YEAR)])/length(Dulieu$PAID_WAGE_PER_YEAR)


#Cau9
Dulieu$PAID_WAGE_PER_YEAR
Dulieu$thucte=ifelse(Dulieu$PAID_WAGE_PER_YEAR<200000,Dulieu$PAID_WAGE_PER_YEAR,NA)
Dulieu$kyvong=ifelse(Dulieu$PREVAILING_WAGE_PER_YEAR<150000,Dulieu$PREVAILING_WAGE_PER_YEAR,NA)
plot(density(Dulieu$thucte,na.rm=TRUE))
hist(Dulieu$thucte)
length(na.omit(Dulieu$thucte[Dulieu$thucte>mean(Dulieu$thucte,na.rm=TRUE)]))/length(na.omit(Dulieu$thucte))
mean(Dulieu$thucte,na.rm=TRUE)