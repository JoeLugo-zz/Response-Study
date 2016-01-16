library(ggplot2)

config.df <- read.table ("/Users/josephlugo/Google Drive/ResponseStudy/ResponseTimeStudyCHRISTELLE_Sheet1.txt", header=T, sep="\t", quote="\"", stringsAsFactors=F)
config.df$Date <- as.POSIXct(config.df$Date.Time, format = "%B %d, %Y at %I:%M%p")

config2.df <- read.table ("/Users/josephlugo/Google Drive/ResponseStudy/ResponseTimeStudyJOE_Sheet1.txt", header=T, sep="\t", quote="\"", stringsAsFactors=F)
config2.df$Date <- as.POSIXct(config2.df$Date.Time, format = "%B %d, %Y at %I:%M%p")

config.df <- config.df[! duplicated (config.df), ]
config2.df <- config2.df[! duplicated (config2.df), ]

config.df <- config.df[rowSums(is.na(config.df)) != ncol(config.df),]
config2.df <- config2.df[rowSums(is.na(config2.df)) != ncol(config2.df),]
j <- rbind (config.df,config2.df)
j <- j[rowSums(is.na(j)) != ncol(j),]
j <- j[with(j, order(Date)), ]
j$Keep <- "YES"
j$blah <-1
j$blah[which(j$Name == "Christelle Mamitag")] <-0

for (i in 1:(nrow(j)-1))
{
	if(j[i,]$Name == j[i+1,]$Name)
	{
		j[i+1,]$Keep <- "NO"
	}
}

keptvals.df <- j[which(j$Keep == "YES"),]
keptvals.df$Diff <- 0
#keptvals.df$Diff <- c(0,diff(keptvals.df$Date))
for (i in 1:nrow(keptvals.df))
{
	keptvals.df[i+1,]$Diff <- difftime(keptvals.df[i+1,]$Date,keptvals.df[i,]$Date, units="mins")
}

# getting month
#keptvals1.df <- keptvals.df[1:(nrow(keptvals.df)),]
dateSplit.ls <- strsplit(keptvals.df$Date.Time," ")
month.ls <- sapply(dateSplit.ls,function(x) x[[1]][1])
keptvals.df$Month <- month.ls

# LOL
textSplit.ls <- strsplit(keptvals.df$Text,"[[:space:]]|(?=[^,'[:^punct:]])", perl=TRUE)
# text.ls <- sapply(textSplit.ls,function (x) "LOL" %in% x || "lol" %in% x || "lool" %in% x || "lmao" %in% x)
text.ls <- sapply(textSplit.ls,function (x) "LOL" %in% x || "LOOL" %in% x)
keptvals.df$lol <- text.ls
keptvals.df$lol[which(keptvals.df$lol == TRUE)] <- "LOL"
keptvals.df$lol[which(keptvals.df$lol == FALSE)] <- "no_LOL"

write.table (keptvals.df, col.names=T, row.names=F, quote=F, sep="\t", file="ResponseTime.txt")

# density plot
d <- density(keptvals.df[which(keptvals.df$Diff < 180),]$Diff)
plot(d, main="SMS Response Time - Joe and Christelle",xlab="Mins")
polygon(d, col="red", border="blue")

# density plot by month
keptvals.df$Month <- factor(keptvals.df$Month,levels = c("January", "February", "March", "April", "May", "June", "July","August","September","October","November"))
g <- ggplot(keptvals.df[which(keptvals.df$Diff < 180),], aes(x = Diff, fill = Name)) + geom_density(alpha = 0.5) + labs(title = "SMS Response Time : Jan-Novemeber 2015",x="Mins",y="Density")
m <- g + facet_grid(. ~ Month)
l <- g + facet_grid(lol ~ Month)

newdata <- keptvals.df[which(keptvals.df$Diff < 200),] 
fit <- lm(newdata$Diff ~ newdata$Name + newdata$Month)
summary(fit)

plot(fit)
# 0.4626 0.3548
