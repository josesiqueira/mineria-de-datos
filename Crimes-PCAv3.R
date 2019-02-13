df<-read.csv("CrimesClean.csv")
View(df)

objects()


#
# VISUALISATION OF DATA
#
# PRINCIPAL COMPONENT ANALYSIS OF CONTINcUOUS VARIABLES, WITH Dictamen PROJECTED AS ILLUSTRATIVE
#

# CREATION OF THE DATA FRAME OF CONTINUOUS VARIABLES

attach(df)
DENSITY = POPULATION/LAND.AREA.km2.

hist(DENSITY)

df$DENSITY=DENSITY

#SELECCIONAR LES VARIABLES PER FER EL PCA
# PRINCIPAL COMPONENT ANALYSIS OF dcon
dcon<-df[,c(4:6,11,16,23:31)] 
pc1 = prcomp(dcon, scale=T)

print(pc1) ##Estos son los componentes principales de cada variable numérica(14)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?

inerProj<- pc1$sdev^2
totalIner<- sum(inerProj)
pinerEix<- 100*inerProj/totalIner ##Porcentaje de inercia en cada subespacio
pinerEix ##Observamos que las primeras 5 dimensiones son las que acumulan un 80%(aprox.) de la inercia
##En consecuencia son las dimensiones que usaremos para realizar el analisis de nuestros datos


#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])
##Representación gráfica del porcentaje de inercia acumulado con las dimensiones

# SELECTION OF THE SIGIFICATIVE DIMENSIONS (LAST ELBOW TECHNIQUE)

nd = 5

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS

lbd = pc1$sdev[1:nd]^2
U = pc1$rotation[,1:nd]
Psi = pc1$x[,1:nd]
View(Psi)
##Coordenadas del PCA por cada uno de los individuos en cada de las 5 dimensiones significatives

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

#################### PLOT OF INDIVIDUALS P1 AND P2 ####################
#######################################################################
names(df)

### Nube de Individuos

plot(Psi[,1],Psi[,2],type="n")
text(Psi[,1],Psi[,2],labels=iden)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
##Observamos heterogeneidad en la nube de individuos ya que estos están agrupados por condados

### PLOT OF VARIABLES

Phi = cor(dcon,Psi)
##xlim=c(-4.25,4.25),ylim=c(-4.25,4.25)
plot(Phi[,1],Phi[,2],type="n",xlim=c(-1.5,1),ylim=c(-0.5,1))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, Phi[,1], Phi[,2], length = 0.07,col="blue")
text(Phi[,1],Phi[,2],labels=etiq,col="darkblue")
##El grafico nos muestra que la relacion entre la densidad de poblacion y otras razas está contrapuesta
##a la relacion que hay entre la raza blanca y el nivel de estudios de la poblacion.
##

######### BIPLOT DE RP
##fm = max(abs(Psi[,c(1,2)]))
##biplot(Psi[,c(1,2)],fm*U[,c(1,2)])
##View(df[c(726,311,889,915,496,86,1012),1]) 

#### NOW WE PROJECT THE CDG OF BOTH LEVELS OF ...

### COUNTY xlim=c(-4,4),ylim=c(-3,3)

fdic1 = tapply(Psi[,1],df[,1],mean)
fdic2 = tapply(Psi[,2],df[,1],mean) 

##lines(fdic1,fdic2,col="green", labels=levels(df[,1]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,1]))
text(fdic1,fdic2,labels=levels(df[,1]),col="green")

### WEEKDAY xlim=c(-1.5,1),ylim=c(-0.5,1)

fdic1 = tapply(Psi[,1],df[,8],mean)
fdic2 = tapply(Psi[,2],df[,8],mean) 
##WEEKDAY<-as.factor(SUNDAY,MONDAY,TUESDAY,WEDNESDAY,THURSDAY,FRIDAY,SATURDAY,UNKNOWN)

lines(fdic1,fdic2,col="red", labels=levels(df[,8]))
points(fdic1,fdic2,pch=16,col="red", labels=levels(df[,8]))
text(fdic1,fdic2,labels=levels(df[,8]),col="red")

### VICRACE xlim=c(-2,2),ylim=c(-1,1)

fdic1 = tapply(Psi[,1],df[,9],mean)
fdic2 = tapply(Psi[,2],df[,9],mean) 

lines(fdic1,fdic2,col="green", labels=levels(df[,9]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,9]))
text(fdic1,fdic2,labels=levels(df[,9]),col="azure")

### VICSEX xlim=c(-1.5,2),ylim=c(-0.5,1)

fdic1 = tapply(Psi[,1],df[,10],mean)
fdic2 = tapply(Psi[,2],df[,10],mean) 

lines(fdic1,fdic2,col="green", labels=levels(df[,10]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,10]))
text(fdic1,fdic2,labels=levels(df[,10]),col="beige")

### VICOCCUP xlim=c(-4,4),ylim=c(-4,4)

fdic1 = tapply(Psi[,1],df[,12],mean)
fdic2 = tapply(Psi[,2],df[,12],mean) 

lines(fdic1,fdic2,col="green", labels=levels(df[,12]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,12]))
text(fdic1,fdic2,labels=levels(df[,12]),col="brown")

### VICCOND xlim=c(-2,2),ylim=c(-3.5,2)

fdic1 = tapply(Psi[,1],df[,13],mean)
fdic2 = tapply(Psi[,2],df[,13],mean) 

lines(fdic1,fdic2,col="green", labels=levels(df[,13]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,13]))
text(fdic1,fdic2,labels=levels(df[,13]),col="pink")

### ACCURACE xlim=c(-2,1),ylim=c(-1,1.5)

fdic1 = tapply(Psi[,1],df[,14],mean)
fdic2 = tapply(Psi[,2],df[,14],mean) 

lines(fdic1,fdic2,col="green", labels=levels(df[,14]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,14]))
text(fdic1,fdic2,labels=levels(df[,14]),col="yellow")

### ACCUSEX xlim=c(-2,2),ylim=c(-1,1)

fdic1 = tapply(Psi[,1],df[,15],mean)
fdic2 = tapply(Psi[,2],df[,15],mean) 

lines(fdic1,fdic2,col="green", labels=levels(df[,15]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,15]))
text(fdic1,fdic2,labels=levels(df[,15]),col="orange")

### ACCUOCCU xlim=c(-4,4),ylim=c(-4,4)
fdic1 = tapply(Psi[,1],df[,17],mean)
fdic2 = tapply(Psi[,2],df[,17],mean) 

lines(fdic1,fdic2,col="green", labels=levels(df[,17]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,17]))
text(fdic1,fdic2,labels=levels(df[,17]),col="coral2")

### ACCUCOND xlim=c(-4,3),ylim=c(-3,3)
fdic1 = tapply(Psi[,1],df[,17],mean)
fdic2 = tapply(Psi[,2],df[,17],mean) 

lines(fdic1,fdic2,col="green", labels=levels(df[,18]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,18]))
text(fdic1,fdic2,labels=levels(df[,18]),col="darkmagenta")

### RELATION xlim=c(-2,2),ylim=c(-1,1)
fdic1 = tapply(Psi[,1],df[,19],mean)
fdic2 = tapply(Psi[,2],df[,19],mean) 

lines(fdic1,fdic2,col="green", labels=levels(df[,19]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,19]))
text(fdic1,fdic2,labels=levels(df[,19]),col="grey")

### CAUSE xlim=c(-2,2),ylim=c(-1,1)
fdic1 = tapply(Psi[,1],df[,20],mean)
fdic2 = tapply(Psi[,2],df[,20],mean) 

lines(fdic1,fdic2,col="green", labels=levels(df[,20]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,20]))
text(fdic1,fdic2,labels=levels(df[,20]),col="gold")

### WEAPON xlim=c(-2,2),ylim=c(-2.5,2.5)
fdic1 = tapply(Psi[,1],df[,21],mean)
fdic2 = tapply(Psi[,2],df[,21],mean) 

lines(fdic1,fdic2,col="green", labels=levels(df[,21]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,21]))
text(fdic1,fdic2,labels=levels(df[,21]),col="mediumspringgreen")
View(df)

### LOCATION xlim=c(-4,3),ylim=c(-3.5,2.5)
fdic1 = tapply(Psi[,1],df[,22],mean)
fdic2 = tapply(Psi[,2],df[,22],mean) 

lines(fdic1,fdic2,col="green", labels=levels(df[,22]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,22]))
text(fdic1,fdic2,labels=levels(df[,22]),col="red")
View(df)


#################### PLOT OF INDIVIDUALS P3 AND P4 ####################
#######################################################################


### Nube Individuos 

plot(Psi[,3],Psi[,4],type="n")
text(Psi[,3],Psi[,4],labels=iden)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

# PLOT OF VARIABLES

Phi = cor(dcon,Psi)
plot(Phi[,3],Phi[,4],type="n",xlim=c(min(Phi[,3],0),max(Phi[,3],0)))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, Phi[,3], Phi[,4], length = 0.07,col="blue")
text(Phi[,3],Phi[,4],labels=etiq,col="darkblue")

## BIPLOT DE RP
##fm = round(max(abs(Psi[,c(3,4)])))
##biplot(Psi[,c(3,4)],fm*U[,c(3,4)])

# NOW WE PROJECT THE CDG OF BOTH LEVELS OF ...
names(df)
fdic1 = tapply(Psi[,3],df[,15],mean)
fdic2 = tapply(Psi[,4],df[,15],mean) 

lines(fdic1,fdic2,pch=16,col="green", labels=levels(df[,15]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,15]))
text(fdic1,fdic2,labels=levels(df[,15]),col="green")

fdic1 = tapply(Psi[,3],df[,7],mean)
fdic2 = tapply(Psi[,4],df[,7],mean) 

lines(fdic1,fdic2,pch=16,col="blue", labels=levels(df[,7]))
points(fdic1,fdic2,pch=16,col="blue", labels=levels(df[,7]))
text(fdic1,fdic2,labels=levels(df[,7]),col="blue")


# PLOT OF INDIVIDUALS P4 AND P5

plot(Psi[,4],Psi[,5],type="n")
text(Psi[,4],Psi[,5],labels=iden)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

# PLOT OF VARIABLES

Phi = cor(dcon,Psi)
plot(Phi[,4],Phi[,5],type="n",xlim=c(min(Phi[,4],0),max(Phi[,4],0)))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, Phi[,4], Phi[,5], length = 0.07,col="blue")
text(Phi[,4],Phi[,5],labels=etiq,col="darkblue")

# BIPLOT DE RP

fm = round(max(abs(Psi[,c(4,5)])))

biplot(Psi[,c(4,5)],fm*U[,c(4,5)])

# NOW WE PROJECT THE CDG OF BOTH LEVELS OF ...
names(df)
fdic1 = tapply(Psi[,4],df[,10],mean)
fdic2 = tapply(Psi[,5],df[,10],mean) 

lines(fdic1,fdic2,pch=16,col="green", labels=levels(df[,10]))
points(fdic1,fdic2,pch=16,col="green", labels=levels(df[,10]))
text(fdic1,fdic2,labels=levels(df[,10]),col="green")

#fm = round(max(abs(Psi[,1]))) # ES UN FACTOR D'ESCALA PER DIBUIXAR LES FLETXES MES VISIBLES EN EL GRAFIC

#plot(Psi[,1],Psi[,2],col="gray")
#arrows(ze, ze, fm*U[,1], fm*U[,2], length = 0.07,col="red")
#text(fm*U[,1],fm*U[,2],labels=etiq,col="red")


# 
# INTERPRET THE TWO FIRST AXES?
#
cor(dcon,Psi) 
##
# CALCULATE THE PROJECTIONS OF THE VARIABLES PHI

U %*% diag(sqrt(lbd))

# WRITE THE FIRST PRINCIPAL COMPONENT FUNCTION OF THE OBSERVED VARIABLES
#

# WRITE THE Ingressos AS FUNCTION OF THE SELECTED FACTORS
# EVERY PRINCIPAL COMPONENT IS A COMBINATION OF THE ORIGINAL VARIABLESS (STANDARDIZED SICE SCALE=T)
# 

Z    = scale(dcon)
reg1 = lm(Psi[,1]~ Z)
print(reg1)
U[,1]

# LIKEWISE EVERY VARIABLE CAN BE EXPLAINED BY THE PRINCIPAL COMPONENTS (LATENT FACTORS) 

reg2 = lm(Z[,1]~ pc1$x[,1:nd])
print(reg2)
U[1,]

# HOWEVER TO IDENTIFY THE LATENT FACTORS IS BETTER TO PERFORM A ROTATION OF THE DIRECTIONS TO FACILITATE THIR INTERPRETABILITY
# RUN VARIMAX ROTATION AND LOOK FOR LOADING (=CORRELATION) > 0.7

pcrot = varimax(Phi)
print(pcrot)

# PROJECTION OF dICATAMEN AS ILLUSTRATIVE

# PROJECCIÓ OF INDIVIDUALS DIFFERENTIATING THE Dictamen
# (we need a numeric Dictamen to color)

idict=df[,1]
plot(Psi[,1],Psi[,2],col=idict)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("topright",levels(df[,1]),pch=1,col=c(1:20))

names(df)
#Dia de la semana
tcon=df[,8]
plot(Psi[,1],Psi[,2],col=tcon)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("topright",levels(df[,8]),pch=1,col=c(1,2,3,4,5,6,7,8))

names(df)
#Raza acusado
tcon=df[,14]
plot(Psi[,1],Psi[,2],col=tcon)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("topright",levels(df[,14]),pch=1,col=c(1:20))

# NOW WE PROJECT THE CDG OF BOTH LEVELS OF Dictament

fdic1 = tapply(Psi[,1],df[,14],mean)
fdic2 = tapply(Psi[,2],df[,14],mean) 

lines(fdic1,fdic2,pch=16,col="green", labels=levels(df[,14]))
text(fdic1,fdic2,labels=levels(df[,14]),col="green")

names(df)
fdic1 = tapply(Psi[,1],ACCURACE,mean)
fdic2 = tapply(Psi[,2],ACCURACE,mean) 

points(fdic1,fdic2,pch=16,col="yellow")
text(fdic1,fdic2,labels=levels(ACCURACE),col="yellow")

#tipus de contrato
#fdic1 = tapply(Psi[,1],Tipo.trabajo,mean)
#fdic2 = tapply(Psi[,2],Tipo.trabajo,mean) 

#points(fdic1,fdic2,pch=16,col="red")
#text(fdic1,fdic2,labels=levels(Tipo.trabajo),col="red")

#########################################
##########################################

dcat <- data.frame(HOUR,WEEKDAY,VICRACE,VICSEX,VICOCCUP,VICCOND,ACCURACE,ACCUSEX,ACCUOCCU,ACCUCOND,RELATION,CAUSE,WEAPON,LOCATION)
# CREATION OF THE DATA FRAME WITH THE ILLUSTRATIVE VARIABLE

dict <- data.frame(COUNTY)
View(dcat)
names(dcat)
table(is.na(dcat, dict))

#for(i in 1:ncol(dcat)) {
#print(table(is.infinite(dcat[,i])))
#}
# LOADING ACM FUNTION

source("acm.r") # LA TROBAREU A LA PAGINA WEB DE L'ASSIGNATURA

ac1 <- acm(dcat,dict)
attributes(ac1)

# HISTOGRAM OF EIGENVALUES OF THE MCA (WHY ARE SO DIFFERENT?)

barplot(ac1$vaps)
View(ac1$cs)
vcol$NAME<-rownames(ac1$cs)
vcol$VAR<-gsub("^[.]?.","",vcol$NAME)
dim(vcol)
# WHAT IS THE SIGNIFICANT DIMENSION IN MCA?

i <- 1

while (ac1$vaps[i] > 1/ncol(dcat)) i = i+1

nd = i-1

# PLOT OF INDIVIDUALS (USELESS)

plot(ac1$rs[,1],ac1$rs[,2],type="n")
text(ac1$rs[,1],ac1$rs[,2],labels=row.names(ac1$rs))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

# PLOT OF MODALITIES (LEVELS OF CATEGORICAL VARIABLES) 

ivar = gl(16,1)

plot(ac1$cs[,1],ac1$cs[,2],type="none")
axis(side=1, pos= 0, labels = F, col="gray")
axis(side=3, pos= 0, labels = F, col="gray")
axis(side=2, pos= 0, labels = F, col="gray")
axis(side=4, pos= 0, labels = F, col="gray")
text(ac1$cs[,1],ac1$cs[,2],labels=row.names(ac1$cs),col=ivar,cex=0.6)


# PLOT OF THE LEVELS OF Dictamen

text(ac1$csup[,1],ac1$csup[,2],labels=row.names(ac1$csup))

# PLOT of INDIVIDUALS ACCODING THE LEVEL OF dICTAMEN
names(df)
plot(ac1$cs[,1],ac1$cs[,2],col=ivar, pch=16)

#lines(ac1$cs[,1],ac1$cs[,2],col="red", labels=levels(df[,c(12)]))

legend("topright",colnames(dcat),pch=16,col=c(1:50))
names(dcat)
View(colors())

index = c(7,8,9,10,12,13,14,15,17,18,19,20,21,22)
for(i in 1:length(index)) {
fdic1 = tapply(ac1$rs[,1],df[,index[i]],mean)
fdic2 = tapply(ac1$rs[,2],df[,index[i]],mean) 

lines(fdic1,fdic2,col=i, labels=levels(df[,index[i]]))
points(fdic1,fdic2,pch=16,col=i, labels=levels(df[,index[i]]))
text(fdic1,fdic2,labels=levels(df[,index[i]]),col=i)
}
names(df)