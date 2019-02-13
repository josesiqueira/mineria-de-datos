##libraries
library(class)

##Load table
df<-read.csv("Crimes.csv", sep=',', row.names=1, na.string="NA")

##First view of data
View(df)
names(df)
############### Variable description ##########
#	1.	COUNTY				(CHARACTER)
#	2.	VICTIM  				(CHARACTER)
#	3.	KILLER  				(CHARACTER)
#	4.	YEAR 					(NUMBER)
#	5.	MONTH					//99. UNKNOWN
#	6.	DAY					//99. UNKNOWN
#	7.	HOUR					(CHARACTER)		
#	8.	WEEKDAY				(CHARACTER)
#	9.  	VICRACE 				(CHARACTER)
#	10.   VICSEX				(CHARACTER)
#	11.   VICAGE 				//99. UNKNOWN
#	12.   VICOCCUP				(CHARACTER)
#	13.  	VICCOND				(CHARACTER)
#	14.  	ACCURACE				(CHARACTER)
#	15.  	ACCUSEX  				(CHARACTER)
#	16.  	ACCUAGE 				//99. UNKNOWN
#	17.  	ACCUOCCUP				(CHARACTER)
#	18.	ACCUCOND				(CHARACTER)
#	19.  	RELATION 				(CHARACTER)
#	20. 	CAUSE					(CHARACTER)
#	21.  	WEAPON				(CHARACTER)
#	22.  	LOCATION				(CHARACTER)
#	23.	POPULATION				(NUMBER)
#	24.	NUM.WOMEN				(NUMBER)
#	25.	NUM.MEN				(NUMBER)
#	26.	LAND.AREA.km2.			(NUMBER)
#	27.	WHITE.ALONE..			(NUMBER)
#	28.	OTHER.RACES..			(NUMBER)
#	29. 	PERCAPITA.MONEY.INCOME..	(NUMBER)
#	30.	HIGH.SCHOOL.GRADUATE..25..	(NUMBER)
###############################################
summary(df)

##Histograms of variables with missing values
##Per veure quin comportament hi ha i posibles anomalies, a més dels missings
par(mfrow=c(2,2))
hist(df[,5], freq=F, main="MONTH HISTOGRAM")
hist(df[,6], freq=F, main="DAY HISTOGRAM")
hist(df[,11], freq=F, main="VICAGE HISTOGRAM")
hist(df[,16], freq=F, main="ACCUAGE HISTOGRAM")

##Number of missing values
table(df[,5]==99)  ##MONTH 	-> 12 Missing
table(df[,6]==99)  ##DAY	-> 41 Missing
table(df[,11]==99) ##VICAGE	-> 584 Missing
table(df[,16]==99) ##ACCUAGE	-> 913 Missing

dim(df)

objects()
attach(df)
objects()

#NA work
##Imput NA
aux = df
dim(aux)

aux[aux[,5]==99,5]<-NA
aux[aux[,6]==99,6]<-NA
aux[aux[,11]==99,11]<-NA
aux[aux[,16]==99,16]<-NA
summary(aux)
table(is.na(aux[,5]))  ##MONTH 	
table(is.na(aux[,6]))  ##DAY	
table(is.na(aux[,11])) ##VICAGE	
table(is.na(aux[,16])) ##ACCUAGE	

## Clustering by POPULATION, LAND.AREA.km2., PERCAPITA.MONEY.INCOME.., HIGH.SCHOOL.GRADUATE..25..
dist<-dist(aux[,c(23,26, 29, 30)], method = "euclidean")
hc<-hclust(dist, method = "ward")
plot(hc,labels=F, hang=0)
#clst<-cutree(hc,k=7)
#aux<-cbind(aux,clst)
#c1<-subset(c, clst==1)
#c2<-subset(c, clst==2)
#c3<-subset(c, clst==3)
#c4<-subset(c, clst==4)
#c5<-subset(c, clst==5)
#c6<-subset(c, clst==6)
#c7<-subset(c, clst==7)
#
#Como hay 7 grupos y estos corresponden al atributo County, descartamos este tipo de clustering
# Ya que es equivalente a hacer los clusters segun la variables County.

##CONCATENACIÓN COUNTY+CAUSE
aux["CONCAT"]<-paste(aux[,1], aux[,20], sep="")

mM<-tapply(aux[,5], aux$CONCAT, mean,na.rm=T)
mD<-tapply(aux[,6], aux$CONCAT, mean,na.rm=T)
mV<-tapply(aux[,11], aux$CONCAT, mean, na.rm=T)
mA<-tapply(aux[,16], aux$CONCAT, mean,na.rm=T)

write.csv(mM, "mM.csv", row.names=T)
mM<-read.csv("mM.csv")
names(mM)<-c("CONCAT", "mMONTH")
write.csv(mD, "mD.csv", row.names=T)
mD<-read.csv("mD.csv")
names(mD)<-c("CONCAT", "mDAY")
write.csv(mV, "mV.csv", row.names=T)
mV<-read.csv("mV.csv")
names(mV)<-c("CONCAT", "mVICAGE")
write.csv(mA, "mA.csv", row.names=T)
mA<-read.csv("mA.csv")
names(mA)<-c("CONCAT", "mACCUAGE")

m1<-merge(mM,mD, "CONCAT")
m2<-merge(mV,mA, "CONCAT")
m<-merge(m1,m2, "CONCAT")
aux<-merge(aux, m, "CONCAT")
View(aux)
names(aux)
#aux[,c(32:35)]<-round(aux[,c(32:35)], digits=0)

for(i in 1:nrow(aux)) {
	if(is.na(aux[i,6])) aux[i,6]<-aux[i,32]
	if(is.na(aux[i,7])) aux[i,7]<-aux[i,33]
	if(is.na(aux[i,12])) aux[i,12]<-aux[i,34]
	if(is.na(aux[i,17])) aux[i,17]<-aux[i,35]
}

table(is.na(aux[,6]))  ##MONTH 	
table(is.na(aux[,7]))  ##DAY	
table(is.na(aux[,12])) ##VICAGE	
table(is.na(aux[,17])) ##ACCUAGE	

##Com que encara queden missings farem la imputació amb el clustering
names(aux)
dist<-dist(aux[,c(24,27, 30, 31)], method = "euclidean")
hc<-hclust(dist, method = "ward")
plot(hc,labels=F, hang=0)
clst<-cutree(hc,k=7)
aux<-cbind(aux,clst)
c1<-subset(aux, clst==1)
c2<-subset(aux, clst==2)
c3<-subset(aux, clst==3)
c4<-subset(aux, clst==4)
c5<-subset(aux, clst==5)
c6<-subset(aux, clst==6)
c7<-subset(aux, clst==7)
names(aux)


aux[is.na(aux[,12]) & aux[,36]==1,12]<-mean(c1$VICAGE, na.rm=T)
aux[is.na(aux[,12]) & aux[,36]==2,12]<-mean(c2$VICAGE, na.rm=T)
aux[is.na(aux[,12]) & aux[,36]==3,12]<-mean(c3$VICAGE, na.rm=T)
aux[is.na(aux[,12]) & aux[,36]==4,12]<-mean(c4$VICAGE, na.rm=T)
aux[is.na(aux[,12]) & aux[,36]==5,12]<-mean(c5$VICAGE, na.rm=T)
aux[is.na(aux[,12]) & aux[,36]==6,12]<-mean(c6$VICAGE, na.rm=T)
aux[is.na(aux[,12]) & aux[,36]==7,12]<-mean(c7$VICAGE, na.rm=T)
aux[is.na(aux[,17]) & aux[,36]==1,17]<-mean(c1$ACCUAGE, na.rm=T)
aux[is.na(aux[,17]) & aux[,36]==2,17]<-mean(c2$ACCUAGE, na.rm=T)
aux[is.na(aux[,17]) & aux[,36]==3,17]<-mean(c3$ACCUAGE, na.rm=T)
aux[is.na(aux[,17]) & aux[,36]==4,17]<-mean(c4$ACCUAGE, na.rm=T)
aux[is.na(aux[,17]) & aux[,36]==5,17]<-mean(c5$ACCUAGE, na.rm=T)
aux[is.na(aux[,17]) & aux[,36]==6,17]<-mean(c6$ACCUAGE, na.rm=T)
aux[is.na(aux[,17]) & aux[,36]==7,17]<-mean(c7$ACCUAGE, na.rm=T)

table(is.na(aux[,6]))  ##MONTH 	
table(is.na(aux[,7]))  ##DAY	
table(is.na(aux[,12])) ##VICAGE	
table(is.na(aux[,17])) ##ACCUAGE

##Ja no hi ha missings!!!

names(aux)
def<-aux[,-c(1,32:36)]
names(def)
dim(def)
dim(df)

table(is.na(def[,5]))  ##MONTH 	
table(is.na(def[,6]))  ##DAY	
table(is.na(def[,11])) ##VICAGE	
table(is.na(def[,16])) ##ACCUAGE
par(mfrow=c(2,2))
hist(def[,5], freq=F, main="MONTH HISTOGRAM")
hist(def[,6], freq=F, main="DAY HISTOGRAM")
hist(def[,11], freq=F, main="VICAGE HISTOGRAM")
hist(def[,16], freq=F, main="ACCUAGE HISTOGRAM")	
summary(def)

write.csv(def, "CrimesClean.csv", row.names=F)
def<-read.csv("CrimesClean.csv")
View(def)

file.remove("mM.csv")
file.remove("mD.csv")
file.remove("mV.csv")
file.remove("mA.csv")