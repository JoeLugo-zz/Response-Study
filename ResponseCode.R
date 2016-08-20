library(ggplot2)


JOE_DF = "/Users/josephlugo/Google Drive/ResponseStudy/ResponseTimeStudyJOE_Sheet1.txt"
CHRISTELLE_DF= "/Users/josephlugo/Google Drive/ResponseStudy/ResponseTimeStudyCHRISTELLE_Sheet1.txt"

load_sms_data <- function(joe_file=JOE_DF, christelle_file=CHRISTELLE_DF){
	joe.df <- read.table(joe_file, 
						header=T, 
						sep="\t", 
						quote="\"", 
						stringsAsFactors=F)
	joe.df$Date <- as.POSIXct(joe.df$Date.Time, format = "%B %d, %Y at %I:%M%p")

	christelle.df <- read.table(christelle_file, 
								header=T, 
								sep="\t", 
								quote="\"", 
								stringsAsFactors=F)
	christelle.df$Date <- as.POSIXct(christelle.df$Date.Time, format = "%B %d, %Y at %I:%M%p")

	dfs = list(joe.df, christelle.df)

	return(dfs)
}

clean_data <- function(joe.df, christelle.df){
	# Getting rid of any duplicate rows
	joe.df <- joe.df[! duplicated (joe.df), ]
	christelle.df <- christelle.df[! duplicated (christelle.df), ]
	
	# removing any rows with NA values
	joe.df <- joe.df[complete.cases(joe.df),]
	christelle.df <- christelle.df[complete.cases(christelle.df),]

	# Combining both dataframes into one
	combined.df <- rbind(joe.df,christelle.df)

	# Sorting the combined data
	combined.df <- combined.df[with(combined.df, order(Date)), ]
	combined.df$consecutive_texts <- "NO"
	# combined.df$blah <- 1
	# combined.df$blah[which(combined.df$Name == "Christelle Mamitag")] <- 0
	
	for (i in 1:(nrow(combined.df)-1))
	{
		if(combined.df[i,]$Name == combined.df[i+1,]$Name)
		{
			combined.df[i+1,]$consecutive_texts <- "YES"
		}
	}
	
	combined.df <- combined.df[which(combined.df$consecutive_texts == "NO"),]
	combined.df$diff <- 0
	for (i in 1:nrow(combined.df))
	{
		combined.df[i+1,]$diff <- difftime(combined.df[i+1,]$Date,combined.df[i,]$Date, units="mins")
	}
	
	# Getting month
	dateSplit.ls <- strsplit(combined.df$Date.Time," ")
	month.ls <- sapply(dateSplit.ls,function(x) x[[1]][1])
	combined.df$month <- month.ls
	
	# Getting LOL rows
	textSplit.ls <- strsplit(combined.df$Text,"[[:space:]]|(?=[^,'[:^punct:]])", perl=TRUE)
	# text.ls <- sapply(textSplit.ls,function (x) "LOL" %in% x || "lol" %in% x || "lool" %in% x || "lmao" %in% x)
	# TODO: make regex for checking L*L
	text.ls <- sapply(textSplit.ls,function (x) "LOL" %in% x || "LOOL" %in% x)
	combined.df$lol <- text.ls
	combined.df$lol[which(combined.df$lol == TRUE)] <- "LOL"
	combined.df$lol[which(combined.df$lol == FALSE)] <- "no_LOL"
	combined.df$month <- factor(combined.df$month,levels = month.name)
	combined.df <- combined.df[! is.na(combined.df$month),]

	output.df = combined.df

	return(output.df)
}

plot_data <- function(df, plot = 'month'){
	# density plot
	# density_data <- density(df[which(df$diff < 180),]$diff)
	# density_plot <- plot(density_data, main="SMS Response Time - Joe and Christelle",xlab="Mins")
	# polygon(d, col="red", border="blue")

	# density plot by month
	density_plot_month <- ggplot(df[which(df$diff < 180),], aes(x = diff, fill = Name)) 
							+ geom_density(alpha = 0.5) 
							+ labs(title = "SMS Response Time : Jan-Novemeber 2015",x="Mins",y="Density")
	month_plot <- density_plot_month + facet_grid(. ~ month)
	lol_plot <- density_plot_month + facet_grid(lol ~ month)

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


main <- function(save=FALSE){
	dfs <- load_sms_data(JOE_DF, CHRISTELLE_DF)
	df <- clean_data(dfs[[1]], dfs[[2]])

	if save == TRUE{
		write.table (df, col.names=T, row.names=F, quote=F, sep="\t", file="ResponseTime.tsv")
	}

	return(df)
}


# import::here(main, plot_data, .from = "ResponseCode.R")
# newdata <- keptvals.df[which(keptvals.df$diff < 200),] 
# fit <- lm(newdata$diff ~ newdata$Name + newdata$month)
# summary(fit)

# plot(fit)
# 0.4626 0.3548
