#R Assignment 1
#Question 1
n1 <- 1:20
length(n1)
n1

n2 <- 20:1
length(n2)
n2

n3 <- seq(1, 20, 2)
length(n3)
n3

colors <- c("red","blue","green","yellow","black")
length(colors)
colors

claims <- c(TRUE, FALSE)
length(claims)
claims

#Question 2
n1[1]+n2[1]
n1[1]-n2[1]
n1[1]*n2[1]
n1[1]/n2[1]

n1
n2

n1+n2
n1-n2
n1*n2
n1/n2

n1[1]+n2
n1[1]-n2
n1[1]*n2
n1[1]/n2

n1[1]<n2[1]
n1[1]==n2[1]
n1[1]>n2[1]

n1<n2
n1==n2
n1>n2

n1[1]<n2
n1[1]==n2
n1[1]>n2

all(n1<n2)
any(n1<n2)

#The type of operation performed on a vector determines the output of a vector to be a vector or not.
#Example: min(n1) = 1 is a single value while, n1+n2 is a vector with element by element addition.

#Question 3
numbers <- c(n1, n2, n3)
numbers
class(numbers)
length(numbers)

numbers2 <- data.frame(n1, n2, n3)
numbers2
class(numbers2)
nrow(numbers2) 
ncol(numbers2)

#data.frame() combines vectors preserving their sizes as individual columns while
#c() appends vectors together in a single list like dimension. Also data.frame()
#repeats over the elements of smaller vectors to match size of the largest vector.
#This feature of padding smaller vectors by repeating elements will not generate errors
#and can produce faulty calculations.

#Question 4
combined <- c(n1, colors, claims)
combined

#The data type of combined is made char as it can contain all other types of data
#including alphabets and numerics. Vectors are prone to errors when string/char
#data is accidentally added to it.

my.data <- data.frame(n1, colors, claims)
my.data
class(my.data[1,2])
#The name of the middle column is colors and it's type is factor.

names(my.data)

#Question 5
colors.v <- my.data[,2]
colors.v
class(colors.v)

my.data2 <- data.frame(my.data[,2], stringsAsFactors=F)
colors2.v <- my.data2
class(colors2.v)

colors.df <- data.frame(my.data["colors"])
class(colors.df)
colors.df

colors3 <- my.data$colors
colors3
class(colors3)
#colors3 is more like colors.df with a single column
#colors2.v has three columns.

#Question 6
d <- c(Sys.Date(),as.Date("May 20 2016", format="%B %d %Y"))
d
sprintf("%i days",(as.numeric(as.POSIXct(d[1])) + as.numeric(as.POSIXct(d[2])))/(60*60*24))
d[1]-d[2]
d[2]-d[1]

#Question 7
hist(numbers2[,"n1"])
hist(numbers2[,1])
hist(numbers2[,"n1"], breaks=10)
hist(numbers2[,1], breaks=10)

#Uniform Distribution output as each elemsnt has a frequency of one.

#Question 8
plot(numbers)
plot(my.data) #Plot here draws a plot for the entire data frame converting string to values
plot(my.data2) #Plot here considers each string item as an element and plots with their frequency

#Question 9
library(ggplot2)
set.seed(123) 
num <- rnorm(20) 
hist(num, breaks=10) 
my.data2$num <- num  
library("ggplot2") 
g<- ggplot(data=my.data, aes(x=num)) 
g + geom_histogram(aes(fill=colors)) #default position and binwidth 
g + geom_histogram(aes(fill=colors), binwidth=1) #default position is stack 
g + geom_histogram(aes(fill=colors),position = "dodge", binwidth=1) 

#Dodge is easier as the count scale guidelines are easier to follow and the colorschemes of 
#Stack might be difficult to discer between similar colored items at a glance as they're 
#closely stacked/placed.