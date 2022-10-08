install.packages("svDialogs")
install.packages("png")
install.packages("car")

library("svDialogs")
library("png")
library("car")

#No. of ss needed & if want to take help of guide map
choice=dlgInput("Do you want to take help of guide map ? : yes/no", Sys.info()["Requirement"])$res
num_ss=dlgInput("No. of screenshot needed ?", Sys.info()["Requirement"])$res
num=as.numeric(num_ss)

#selecting ss and storing all the landmarks and co-ordinates
list = list(x = c() , y = c())
list_of_places = c()
list_of_ss = c()
cat('\n Select screenshots(s) \n')
for (i in 1:num) {
    ss=readPNG(file.choose())
    plot(1:100,ty="n")
    rasterImage(as.raster(ss),1,1,100,100)
    ans=dlgInput("Do you want to point out any landmark(s) in this ss? : yes/no", Sys.info()["Requirement"])$res
    if (ans == 'yes'){
    	temp = 0
    	cat('\n Select landmark(s) \n')
        while(!is.null(temp)){
        	cat("Left click to store the place \n")
        	temp = locator(1, type = "p", pch = 3, col = "red")
        	place=dlgInput("Enter name of landmark : ")$res
        	list_of_ss = c(list_of_ss,as.character(i))
        	list_of_places = c(list_of_places,tolower(place))
        	list$x = c(list$x,temp$x)
        	list$y = c(list$y,temp$y)
        	
        	cat("The next clicks will store the approx position of current place")
        	cat("\n ESC after you have finished \n")
        	temp = locator(type = "p", pch = 3, col = "blue")
        	len=length(temp$x)
        	list_of_ss = c(list_of_ss,rep( as.character(i) , len) )
        	list_of_places = c(list_of_places,rep(tolower(place) , len))
            list$x = c(list$x,temp$x)
        	list$y = c(list$y,temp$y)
        	
        	cat("\n Esc to move to next SS left click to get more landmark of current ss \n")
        	
        	temp = locator(1, type = "p", pch = 3, col = "red")
        	
        	}}}
        	
#Creating data matrix and carrying out linear model        	
dat2=data.frame("x-coordinate"=list$x,"y-coordinate"=list$y,"Landmark"=list_of_places,"Screenshot.No"=list_of_ss)  
dat1=read.csv(file.choose())
names(dat1)[names(dat1)=="X...x.coordinate"]<-"x.coordinate"
if(choice=="yes"){
alldat=rbind(dat2,dat1)}
if(choice=="no"){
alldat=dat2}	
alldat$Screenshot.No=factor(alldat$Screenshot.No)
num_land=length(unique(alldat$Landmark))
num_ss=length(unique(alldat$Screenshot.No))
fitx=lm(x.coordinate~Landmark+Screenshot.No-1,alldat)
fity=lm(y.coordinate~Landmark+Screenshot.No-1,alldat)
if(fitx$rank<num_land+num_ss-1){
	print("Sorry map is not connected.Try to use guide map.")}


    
    
#Roads
plot(fity$coef~fitx$coef,pch=".",cex=2.5,col=6)
rds=dlgInput("Do you want to see roads ? : yes/no",)$res
if (rds=="yes"){       	
	cat('\n Green line=Lane ; Blue lines= Main Road ; Red lines=Raimohan Banerjee Road ;Yellow lines=Lake View Park Road\n')
#SS1
x1=as.numeric(fitx$coef["LandmarkISI Staff Quarter"])
x2=as.numeric(fitx$coef["LandmarkISI hostel Campus"])
y2=as.numeric(fity$coef["LandmarkISI hostel Campus"])
y1=as.numeric(fity$coef["LandmarkISI Staff Quarter"])
lines(c(x1,x2),c(y1,y2),col=3)
x1=as.numeric(fitx$coef["LandmarkBonhoogly Auto Stand"])
x2=as.numeric(fitx$coef["LandmarkISI hostel Campus"])
y2=as.numeric(fity$coef["LandmarkISI hostel Campus"])
y1=as.numeric(fity$coef["LandmarkBonhoogly Auto Stand"])
lines(c(x1,x2),c(y1,y2),col=4)
x1=as.numeric(fitx$coef["LandmarkMilan Bar & Restaurant"])
x2=as.numeric(fitx$coef["LandmarkLaxmi Bhandar"])
y2=as.numeric(fity$coef["LandmarkLaxmi Bhandar"])
y1=as.numeric(fity$coef["LandmarkMilan Bar & Restaurant"])
lines(c(x1,x2),c(y1,y2),col=4)


#SS2
x1=as.numeric(fitx$coef["LandmarkNew Hotel"])
x2=as.numeric(fitx$coef["LandmarkUpasi Medical Store"])
y2=as.numeric(fity$coef["LandmarkUpasi Medical Store"])
y1=as.numeric(fity$coef["LandmarkNew Hotel"])
lines(c(x1,x2),c(y1,y2),col=3)

#SS3
x1=as.numeric(fitx$coef["LandmarkAnnapurna Chinese"])
x2=as.numeric(fitx$coef["LandmarkUpasi Medical Store"])
y2=as.numeric(fity$coef["LandmarkUpasi Medical Store"])
y1=as.numeric(fity$coef["LandmarkAnnapurna Chinese"])
lines(c(x1,x2),c(y1,y2),col=3)

#SS4
x1=as.numeric(fitx$coef["LandmarkAnnapurna Chinese"])
x2=as.numeric(fitx$coef["LandmarkBaranagar Municipality Health Dept."])
y2=as.numeric(fity$coef["LandmarkBaranagar Municipality Health Dept."])
y1=as.numeric(fity$coef["LandmarkAnnapurna Chinese"])
lines(c(x1,x2),c(y1,y2),col=3)

#SS5
x1=as.numeric(fitx$coef["LandmarkSaaj Sojja"])
x2=as.numeric(fitx$coef["LandmarkBaranagar Municipality Health Dept."])
y2=as.numeric(fity$coef["LandmarkBaranagar Municipality Health Dept."])
y1=as.numeric(fity$coef["LandmarkSaaj Sojja"])
lines(c(x1,x2),c(y1,y2),col=2)

#SS6
x1=as.numeric(fitx$coef["LandmarkSaaj Sojja"])
x2=as.numeric(fitx$coef["LandmarkKrishna Communication"])
y2=as.numeric(fity$coef["LandmarkKrishna Communication"])
y1=as.numeric(fity$coef["LandmarkSaaj Sojja"])
y3=as.numeric(fity$coef["LandmarkGunja Bakery"])
x3=as.numeric(fitx$coef["LandmarkGunja Bakery"])
x4=as.numeric(fitx$coef["LandmarkPaul Enterprise"])
y4=as.numeric(fity$coef["LandmarkPaul Enterprise"])
x5=as.numeric(fitx$coef["LandmarkRikas Rassoi Ghar"])
y5=as.numeric(fity$coef["LandmarkRikas Rassoi Ghar"])
x6=as.numeric(fitx$coef["LandmarkPappu Juice Corner"])
y6=as.numeric(fity$coef["LandmarkPappu Juice Corner"])


lines(c(x1,x2,x3,x4,x5),c(y1,y2,y3,y4,y5),col=2)

lines(c(x5,x6),c(y5,y6),col=7)

#SS7
x1=as.numeric(fitx$coef["LandmarkPappu Juice Corner"])
x2=as.numeric(fitx$coef["LandmarkTym pass cafe"])
y2=as.numeric(fity$coef["LandmarkTym pass cafe"])
y1=as.numeric(fity$coef["LandmarkPappu Juice Corner"])
lines(c(x1,x2),c(y1,y2),col=7)
}

#identify(fity$coef~fitx$coef)
p=identify(fity$coef~fitx$coef)
names(fitx$coef[p])


#Residual analysis
plot(fitx$res)
plot(fity$res)
cor(fitx$res,fitx$fit)
cor(fity$res,fity$fit)


#outlier test
out_x=c()
q1_x=as.numeric(quantile(fitx$res,0.25))
q3_x=as.numeric(quantile(fitx$res,0.75))
ran_x=1.5*IQR(fitx$res)
l=length(fitx$res)
for(i in 1:l){
	if(fitx$res[i]<(q1_x-ran_x)|fitx$res[i]>(q3_x+ran_x)){
	out_x=c(out_x,i)}}
out_y=c()
q1_y=as.numeric(quantile(fity$res,0.25))
q3_y=as.numeric(quantile(fity$res,0.75))
ran_y=1.5*IQR(fity$res)
l=length(fity$res)
for(i in 1:l){
	if(fity$res[i]<(q1_y-ran_y)|fity$res[i]>(q3_y+ran_y)){
	out_y=c(out_y,i)}}

#qqplot
qqPlot(fitx$res)
qqPlot(fity$res)   

#influence diagnosis
infx = influence.measures(fitx)
a1=alldat[which(apply(infx$is.inf,1,sum) > 0),]
infl_pl1=unique(a1[,3])
infy = influence.measures(fity)
a2=alldat[which(apply(infy$is.inf,1,sum) > 0),]
infl_pl2=unique(a2[,3])
     	



        	
        	
        	
        	
        	
        	

            
        	
