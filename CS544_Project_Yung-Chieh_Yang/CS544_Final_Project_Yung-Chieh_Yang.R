csv <- read.csv("~/Downloads/world-happiness-report-2021.csv")

# 1.How many countries are in this dataset?
nrow(csv)

# 2.Categorical data by country region
regions <- table(csv[,2])
regions

# 3.Barplot for categorical data by country region
par(mar=c(17,3,2,2))
barplot(regions, col="red", ylab="Numbers", las=2, horiz=F)

# 4.Numerical data by ladder score
ls <- csv[,3]
ls
fivenum(ls)
summary(ls)

# 5.Stem plot, Histogram and Boxplot for numerical data by ladder score
stem(ls)
hist(ls, col="red", xlab="Ladder Score", ylab="Frequency", main="Histogram of Ladder Score")
boxplot(ls, col=hcl(0), xaxt="n", xlab="Ladder Score", horizontal=T)
axis(side=1, at=fivenum(ls), labels=T, las=2)
text(fivenum(ls), rep(1.2,5), srt=90, adj=0, labels=c("Min","Lower Hinge","Median","Upper Hinge","Max"))

# 6.Scatterplot for Two-way table between Ladder score and GDP per capita
plot(csv[,3],csv[,7],xlab="Ladder Score", ylab="GDP per Capita")

# 7.Random samples(sample size = 5, 10, 20) and Central Limit Theorem for 149 Ladder Scores
# Using Discrete Uniform Distribution
lsc <- c(csv[,3])
lsc.sample <- sample(lsc, size=149, replace=T)
table(lsc.sample)
prop.table(table(lsc.sample))
barplot(prop.table(table(lsc.sample)), xlab="Ladder Score", ylab="Proportion")
mean(lsc.sample)
sd(lsc.sample)


xbar <- numeric(149)
par(mfrow=c(1,1))
par(mar=c(5,5,5,5))
for (size in c(5,10,20)){
  for (i in 1:149){
    xbar[i] <- mean(sample(lsc,size=size,replace=T))
  }
  hist(xbar,prob=T,breaks=15,main=paste("Sample Size = ",size),xlab="Ladder Score")
  cat("Sample Size = ",size," Mean = ",mean(xbar)," SD = ",sd(xbar),"\n")
}

# 8.Simple Random Sampling
# (A simple sample random of size 20 is drawn from the ladder score without placement.)
# (The data of the selected sample and the frequency of municipalities in each region are shown below.)
s1 <- srswor(20, nrow(csv))
sample.1 <- csv[s1 != 0, ]
head(sample.1[c(1,2,3,7)])
table(sample.1$Regional.indicator)

# 9.Systematic Sampling
# For a sample of size 20, the data is divided into 7 groups.
# From the first group, a random item is selected.
# The rows of the systematic sample are now computed by taking every 7th item.
# The selected sample is indexed from these rows.
# The frequency of municipalities in each region is shown below.
N1 <- nrow(csv)-9
n1 <- 20
k1 <- ceiling(N1/n1)
k1
r1 <- sample(k1,1)
r1
s2 <- seq(r1,by=k1,length=n1)
sample.2 <- csv[s2, ]
head(sample.2[c(1,2,3,7)])
table(sample.2$Regional.indicator)

# 10.Cluster Sampling
# Cluster of size 4. Clustered by the region.
table(csv$Regional.indicator)
cl <- cluster(csv,c("Regional.indicator"),size=4,method="srswor")
sample.3 <- getdata(csv,cl)
table(sample.3$Regional.indicator)

# 11.Ordering Data
# The dataset is ordered in the descending order by the first letter of the region.
# The sizes are each stratum of the seven regions is explicitly specified in the following.
order.index <- order(csv$Regional.indicator)
data1 <- csv[order.index, ]
head(data1[c(1,2,3,7,8)])
st <- strata(data1,stratanames=c("Regional.indicator"),size=c(17,12,6,20,17,4,7,9,36,21),method="srswor")
sample.4 <- getdata(data1,st)
table(sample.4$Regional.indicator)



