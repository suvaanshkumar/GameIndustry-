mydata = read.csv("C:\\Users\\suvaansh\\Desktop\\sem4\\bigdata2\\group project\\vgsales.csv")
##importing csv

##displaying the data
mydata
##creating vectors
namesDataRow=c(mydata$Name)
platformVector=c(mydata$Platform)
genre=c(mydata$Genre)


##matrix
##vector of vector
matrixOfnameplatformgenre=c(platformVector,genre)
matrix1=matrix(matrixOfnameplatformgenre,byrow = FALSE,ncol = 2)
matrix1
##assigning row and columnn names to matrix
colnames(matrix1)<-c("platform","genre")
rownames(matrix1)<-namesDataRow
head(matrix1,10)
##data frames 
DataFrame1=data.frame(namesDataRow,platformVector,genre)
head(DataFrame1)
## Searching which platform provides the most sales in the world
install.packages("dplyr")
library(dplyr)
mydata2 = select(mydata, -c(Year,Other_Sales))
head(mydata2)
mydata2 = select(mydata,Platform,Global_Sales)

head(mydata2)
##mydata3=group_by(mydata2,Platform)
## query to  Search which platform provides the most sales in the world and how much
t = mydata2 %>% 
  group_by(Platform) %>% summarise(Global_Sales=sum(Global_Sales))%>% arrange(desc(Global_Sales))


head(t,1)

## pie chart for sales by platform
##pie(table(t$Global_Sales));
t
pie(t$Global_Sales, main="Sales by platform", col=rainbow(length(t$Platform)), 
    
    labels=t$Platform) 

legend(-1.8, 1.0, t$Platform, cex=0.6,  
       
       fill=rainbow(length(t$Platform))) 
#barplot
barplot(t$Global_Sales, main="sales by platform", xlab="platforms",   
        
        ylab="Total sales", names.arg=t$Platform,  
        
        border="blue") 

##Which platform provide the most games in the world? using data frames
  
mydataquer2 = select(mydata,Platform,Name)
head(mydataquer2)
dfquery3=data.frame(mydataquer2);
##mydata3=group_by(mydata2,Platform)

t = dfquery3 %>% 
  group_by(Platform) %>% count(Platform)%>% arrange(desc(n))


head(t,1)

##Which genre earn the most money in the world?
colnames(mydata)


mydata2 = select(mydata,Genre,Global_Sales)
head(mydata2)
##mydata3=group_by(mydata2,Genre)
## query to  Search which genre provides the most sales in the world and how much
t = mydata2 %>% 
  group_by(Genre) %>% summarise(Global_Sales=sum(Global_Sales))%>% arrange(desc(Global_Sales))


head(t,3)


## sales by time

mydataTime=select(mydata,Year,Global_Sales)
t = mydataTime %>% 
  group_by(Year) %>% summarise(Global_Sales=sum(Global_Sales))%>% arrange((Year))

barplot(t$Global_Sales, main="sales by Year", xlab="Year",   
        
        ylab="Total sales", names.arg=t$Year,  
        
        border="red") 

##sales across regions by category Action



region=c("North America","Europe","Japan")
mydataforAction = filter(mydata, Genre == "Action")
sumOfSalesAcrossRegionforAction=c(sum(mydataforAction$NA_Sales),sum(mydataforAction$EU_Sales),sum(mydataforAction$JP_Sales),sum(mydataforAction$Other_Sales));
##plot(c("na","eu","jp","ot"),sumOfSalesAcrossRegions)
barplot(sumOfSalesAcrossRegionforAction, main="sales by Region for Action", xlab="Regions",   
        
        ylab="Total sales", names.arg=c("North America","Europe","Japan","Others"),  
        
        border="green") 

##sales across regions by category Sports



region=c("North America","Europe","Japan")
mydataforSports = filter(mydata, Genre == "Sports")
sumOfSalesAcrossRegionsForSports=c(sum(mydataforSports$NA_Sales),sum(mydataforSports$EU_Sales),sum(mydataforSports$JP_Sales),sum(mydataforSports$Other_Sales));

barplot(sumOfSalesAcrossRegionsForSports, main="sales by Region for Sports", xlab="Regions",   
        
        ylab="Total sales", names.arg=c("North America","Europe","Japan","Others"),  
        
        border="green") 


##dot chart for sales for top two genre across regions
vectorForsalesForActionAndSports=c(sumOfSalesAcrossRegionforAction,sumOfSalesAcrossRegionsForSports)
matrixSales=matrix(vectorForsalesForActionAndSports,byrow =FALSE,nrow = 4)
rownames(matrixSales)=c("North America","Europe","Japan","Others")
colnames(matrixSales)=c("Action","Sports")

dotchart(matrixSales, color=c("red","blue","darkgreen"), 
         
         main="Dotchart for Sales by genre across regions", cex=0.8) 
 
