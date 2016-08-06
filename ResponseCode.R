library(ggplot2)


JOE_DF = "/Users/josephlugo/Google Drive/ResponseStudy/ResponseTimeStudyJOE_Sheet1.txt"
CHRISTELLE_DF= "/Users/josephlugo/Google Drive/ResponseStudy/ResponseTimeStudyCHRISTELLE_Sheet1.txt"

load_sms_data <- function(joe_file=JOE_DF, christelle_file=CHRISTELLE_DF){
	joe.df <- read.table(joe_file, header=T, sep="\t", quote="\"", stringsAsFactors=F)
	joe.df$Date <- as.POSIXct(joe.df$Date.Time, format = "%B %d, %Y at %I:%M%p")

	christelle.df <- read.table(christelle_file, header=T, sep="\t", quote="\"", stringsAsFactors=F)
	christelle.df$Date <- as.POSIXct(christelle.df$Date.Time, format = "%B %d, %Y at %I:%M%p")

	dfs = list(joe.df, christelle.df)

	return(dfs)
}

clean_data <- function(joe.df, christelle.df){
	joe.df <- joe.df[! duplicated (joe.df), ]
	christelle.df <- christelle.df[! duplicated (christelle.df), ]
	
	joe.df <- joe.df[rowSums(is.na(joe.df)) != ncol(joe.df),]
	christelle.df <- christelle.df[rowSums(is.na(christelle.df)) != ncol(christelle.df),]

	combined.df <- rbind (joe.df,christelle.df)
	combined.df <- combined.df[rowSums(is.na(combined.df)) != ncol(combined.df),]
	combined.df <- combined.df[with(combined.df, order(Date)), ]
	combined.df$Keep <- "YES"
	combined.df$blah <-1
	combined.df$blah[which(combined.df$Name == "Christelle Mamitag")] <-0
	
	for (i in 1:(nrow(combined.df)-1))
	{
		if(combined.df[i,]$Name == combined.df[i+1,]$Name)
		{
			combined.df[i+1,]$Keep <- "NO"
		}
	}
	
	combined.df <- combined.df[which(combined.df$Keep == "YES"),]
	combined.df$Diff <- 0
	#combined.df$Diff <- c(0,diff(combined.df$Date))
	for (i in 1:nrow(combined.df))
	{
		combined.df[i+1,]$Diff <- difftime(combined.df[i+1,]$Date,combined.df[i,]$Date, units="mins")
	}
	
	# getting month
	#keptvals1.df <- combined.df[1:(nrow(combined.df)),]
	dateSplit.ls <- strsplit(combined.df$Date.Time," ")
	month.ls <- sapply(dateSplit.ls,function(x) x[[1]][1])
	combined.df$Month <- month.ls
	
	# LOL
	textSplit.ls <- strsplit(combined.df$Text,"[[:space:]]|(?=[^,'[:^punct:]])", perl=TRUE)
	# text.ls <- sapply(textSplit.ls,function (x) "LOL" %in% x || "lol" %in% x || "lool" %in% x || "lmao" %in% x)
	text.ls <- sapply(textSplit.ls,function (x) "LOL" %in% x || "LOOL" %in% x)
	combined.df$lol <- text.ls
	combined.df$lol[which(combined.df$lol == TRUE)] <- "LOL"
	combined.df$lol[which(combined.df$lol == FALSE)] <- "no_LOL"
	combined.df$Month <- factor(combined.df$Month,levels = month.name)
	combined.df <- combined.df[! is.na(combined.df$Month),]
	
	# write.table (combined.df, col.names=T, row.names=F, quote=F, sep="\t", file="ResponseTime.txt")

	output.df = combined.df

	return(output.df)
}

plot_data <- function(df, plot = 'month'){
	# density plot
	# density_data <- density(df[which(df$Diff < 180),]$Diff)
	# density_plot <- plot(density_data, main="SMS Response Time - Joe and Christelle",xlab="Mins")
	# polygon(d, col="red", border="blue")

	# density plot by month
	density_plot_month <- ggplot(df[which(df$Diff < 180),], aes(x = Diff, fill = Name)) + geom_density(alpha = 0.5) + labs(title = "SMS Response Time : Jan-Novemeber 2015",x="Mins",y="Density")
	month_plot <- density_plot_month + facet_grid(. ~ Month)
	lol_plot <- density_plot_month + facet_grid(lol ~ Month)

	if (plot == 'month'){
		return(month_plot)
	}

	else if (plot == 'lol'){
		return(lol_plot)
	}

	else {
		return(density_plot_month)
	}
}


main <- function(){
	dfs <- load_sms_data(JOE_DF, CHRISTELLE_DF)
	df <- clean_data(dfs[[1]], dfs[[2]])

	return(df)
}


# import::here(main, plot_data, .from = "ResponseCode.R")
# newdata <- keptvals.df[which(keptvals.df$Diff < 200),] 
# fit <- lm(newdata$Diff ~ newdata$Name + newdata$Month)
# summary(fit)

# plot(fit)
# 0.4626 0.3548
