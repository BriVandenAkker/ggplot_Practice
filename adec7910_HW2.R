##--------------------------------------------------------------------
##----- Brian VandenAkker
##----- Problem Set 2
##--------------------------------------------------------------------
  
  # Clear the workspace
  rm(list = ls()) # Clear environment
  gc()            # Clear unused memory
  cat("\f")       # Clear the console
  
  # Prepare needed libraries
  library.list <- c("ggplot2"
                    , "lemon"
                    , "gridExtra"
                    , "ggrepel"
                    )
  for (i in 1:length(library.list)) {
    if (!library.list[i] %in% rownames(installed.packages())) {
      install.packages(library.list[i], dependencies = TRUE)
    }
    library(library.list[i], character.only = TRUE)
  }
  rm(library.list, i)
  
  # Set working directory and path to data, if need be
  setwd("C:/Users/brian/Documents/SoftwareTools/HW2/")
  
  # Load data
  sales <- read.csv("iowa.liquor.r.hw2.sales.csv"
  									, check.names = FALSE
  									, stringsAsFactors = FALSE
  									, na.strings = ""
  									)
  items <- read.csv("iowa.liquor.r.hw2.items.csv"
  									, check.names = FALSE
  									, stringsAsFactors = FALSE
  									, na.strings = ""
  									)
  
  # Merge data together
  sales <- merge(items, sales, by.x = "item.id")
  
  # Remove unnecessary variables and objects
  sales$item.id <- NULL
  rm(items)
  # Reorder variables
  var.order <- c("date"
                , "category"
                , "subcategory"
                , "item.name"
                , "volume"
                , "price"
                , "sale.bottles"
                , "sale.volume"
                , "sale.dollars"
                )
  sales <- sales[var.order]
  rm(var.order)
##--------------------------------------------------------------------
##----- Q1
##--------------------------------------------------------------------  
  #Q1 Setup
  str(sales$date)
  #Convert to date data type
  sales$date <- as.Date(sales$date, format = "%Y-%m-%d")
  #Aggregate by total dollar sales for each date
  sales.agg <- aggregate(sale.dollars ~ date, sales, sum)
  #Generate a data frame containing all days in 2015
  sales.dates <- data.frame(sale.date = seq(from = as.Date("2015-01-01"), to = as.Date("2015-12-31"), by = "day"))
  sales.daily <- merge(sales.dates, sales.agg, by.x = "sale.date", by.y = "date", all.x = TRUE)
  rm(sales.agg, sales.dates)
  
  #Generate calendar dimensions
  sales.daily$day <- as.numeric(format(sales.daily$sale.date, "%d"))
  sales.daily$weekday <- factor(substr(weekdays(sales.daily$sale.date),1,3), 
                                levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                ordered = TRUE)
  sales.daily$week <- as.numeric(format(sales.daily$sale.date, "%W")) + 1 
  #But we need week of month not year
  temp <- as.numeric(format(as.Date(cut(sales.daily$sale.date, "month")), "%W"))
  sales.daily$week <- sales.daily$week - temp
  sales.daily$month <- factor(substr(months(sales.daily$sale.date),1,3),
                               levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                               ordered = TRUE)
  sales.daily$quarter <- factor(quarters(sales.daily$sale.date)
                                 , levels = c("Q1", "Q2", "Q3", "Q4")
                                 , labels = c("", "", "", "")
                                 , ordered = TRUE)
  sales.daily$year <- format(sales.daily$sale.date, "%Y")  
  
  #Discrete Scale
  sales.daily$sales.mil <- round(sales.daily$sale.dollars/1000000,1)
  sales.daily$discrete <- cut(sales.daily$sales.mil, c(0 ,1 , 1.2 , 1.4 , 1.6 , 2 , NA ))
  sales.daily$discrete <- gsub("[](]", "", as.character(sales.daily$discrete))
  sales.daily$discrete <- strsplit(as.character(sales.daily$discrete),"[,]" )
  sales.daily$min <- as.numeric(sapply(sales.daily$discrete, function(x) x[1]))
  sales.daily$max <- as.numeric(sapply(sales.daily$discrete, function(x) x[2]))
  sales.daily$discrete <- paste(sales.daily$min, " - ", sales.daily$max, " mln")
  
  count = 0
  for(i in sales.daily$discrete){
    count = count + 1
    if(i == "NA  -  NA  mln"){
      sales.daily$discrete[count] <- NA
    }
  }

  
  # Q1 chart
  ggplot.q1 <- ggplot(sales.daily, aes(x = weekday, y = week, fill = discrete)) +
                      geom_tile(colour = "white") +
                      scale_fill_manual(values = c("purple", "blue", "dark green", "yellow", "orange", "red"), na.value = "gray") +
                      geom_text(aes(label = day)) + 
                      facet_rep_wrap( ~ month , ncol = 3, repeat.tick.labels = TRUE) + 
                      scale_x_discrete(position = "top") +
                      labs(x = "", y = "", title = "Daily Total Dollar Sales: 2015") + 
                      theme(strip.background = element_blank(), strip.placement = "outside", 
                            #Not sure why the legend isn't becoming a (1x6)
                            axis.text.y = element_blank(), legend.position="bottom", legend.direction = "horizontal", legend.title = element_blank(),
                            panel.background = element_rect(fill = NA), 
                            plot.title = element_text(size=15, face="bold", color="dark blue", hjust=0.5, lineheight=1))
                            
                        
  ggplot.q1
  rm(temp, sales.daily, count, i)
  
  # Export Q1 chart
  png(file = "q1.png", width = 1920, height = 1920, res = 180)
  ggplot.q1
  dev.off()

##--------------------------------------------------------------------
##----- Q2
##--------------------------------------------------------------------  
  
  # Q2a chart
  #Generate ppl
  sales$price.per.l <- sales$sale.dollars/sales$sale.volume
  
  #Plot bottom 95% sales
  #Plot
  ggplot.q2a <- ggplot(sales[sales$price.per.l < quantile(sales$price.per.l,prob=.95),], aes(x = category, y = price.per.l, fill = factor(category), outlier.colour = NULL)) +
    geom_boxplot(outlier.shape = 21) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(sales$category))) + 
    scale_y_continuous(breaks = seq(from = 0, to = 40, by = 2), labels = seq(from = 0, to = 40, by = 2)) + 
    labs(title = "Liquor Categories: Price Per Liter", subtitle = "Exluding Top 5% Values", x = "Category", y = "Price per Liter, $") + 
    theme_bw() + 
    theme(legend.title = element_blank(), 
          plot.title = element_text(size=15, face="bold", color="dark blue", hjust=0.5, lineheight=1),
          plot.subtitle = element_text(size=10, face="bold", color="dark blue", hjust=0.5, lineheight=1),
          axis.title.x = element_text(size=10, face="bold", color="dark blue", hjust=0.5, lineheight=1),
          axis.title.y = element_text(size=10, face="bold", color="dark blue", hjust=0.5, lineheight=1))
  
  ggplot.q2a

   # Q2b chart
  
  #Aggregate sales volume by subcategory
  volumebycategory <- aggregate(sale.volume~ subcategory, sales, sum)
  #In thousands
  volumebycategory$sale.volume.agg <- volumebycategory$sale.volume/1000
  #Drop old variable to avoid redundancy in merged dataframe 
  volumebycategory$sale.volume <- NULL
  #Merge aggregated sales with sales data
  sales <- merge(sales, volumebycategory, by = "subcategory")
  #Calculate sale volume weights across subcategory
  sales$weight <- sales$sale.volume/sales$sale.volume.agg
  #Calculate weighted averages across subcategory
  sales$weightedavg <- sales$weight*sales$price.per.l
  #Aggregate weighted averages across subcategory
  aggweight <- aggregate(weightedavg~subcategory + category, sales, sum)
  #Merge weighted averages across subcategories with aggregated sales
  q2bdata <- merge(aggweight, volumebycategory, by = "subcategory")
  #In thousands
  q2bdata$weightedavg <- q2bdata$weightedavg/1000
  
  #Clean up memory
  sales[,c("price.per.l", "sale.volume.agg", "weight", "weightedavg")] <- NULL
  rm(aggweight, volumebycategory)
  
  #Plot
  ggplot.q2b <- ggplot(q2bdata, aes(x = weightedavg, y = sale.volume.agg, color = category, label = subcategory)) +
          geom_point(aes(colour= factor(category)), fill = "white", shape = 1, size = 4, alpha = 1, stroke = 2)+
          labs(title = "Liquor Subcategories", subtitle = "Price vs Quantity", x = "Average Weighted Price per Liter, $", y = "Liters Sold, Thousands")+
          theme_bw() +
          scale_x_continuous(breaks = seq(from = 0, to = 100, by = 5), labels = seq(from = 0, to = 100, by = 5)) +
          scale_y_continuous(breaks = seq(from =0, to = 4000, by = 250), labels = seq(from = 0, to = 4000, by = 250)) +
          geom_text_repel(data = subset(q2bdata, sale.volume.agg > 1250 | weightedavg > 40), hjust = 1) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20, color = "dark blue")
                , plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 12, color = "dark blue")
                , axis.title.x = element_text(face = "bold", size = 10, color = "dark blue")
                , axis.title.y = element_text(face = "bold", size = 10, color = "dark blue")
                , legend.background = element_blank()
                , legend.position = c(.90,.70)  
                , legend.title = element_blank())    

  ggplot.q2b
  
   #Clean up memory
  rm(q2bdata)
  sales[,c("price.per.l", "sale.volume.agg", "weight", "weightedavg")] <- NULL
  

  # Export Q2a chart
  png(file = "q2a.png", width = 2880, height = 1920, res = 180)
    ggplot.q2a
  dev.off()
  
  # Export Q2b chart
  png(file = "q2b.png", width = 2880, height = 1920, res = 180)
    ggplot.q2b
  dev.off()
  
  # Export Q2 chart
  png(file = "q2c.png", width = 2880, height = 1920, res = 180)
    grid.arrange(ggplot.q2a, ggplot.q2b
                 , nrow = 1
                 , ncol = 2
                 )
  dev.off()
  
##--------------------------------------------------------------------
##----- Q3
##--------------------------------------------------------------------
  
  # Q3a chart
  
  #Aggregate sales by month across categories
  sales$month <- factor(substr(months(sales$date),1,3),
                              levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                              ordered = TRUE)
  monthlysales <- aggregate(sale.dollars ~ category + month , sales, sum)
  tot.sales <- aggregate(sale.dollars ~ category, sales, sum)
  colnames(tot.sales)[2] <- "total.sales.category"
  monthlysales <- merge(monthlysales, tot.sales, by = "category")
  
  
  #Aggregate sales by weekday across categories
  sales$weekday <- factor(substr(weekdays(sales$date),1,3),
                          levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                          ordered = TRUE)
  weekdaysales <- aggregate(sale.dollars ~ category + weekday, sales, sum)
  weekdaysales <- merge(weekdaysales, tot.sales, by = "category")
  
  
  #Sales per month/weekday relative to total sales
  monthlysales$month.to.total <- (monthlysales$sale.dollars/monthlysales$total.sales.category) *100
  weekdaysales$weekday.to.total <- (weekdaysales$sale.dollars/weekdaysales$total.sales.category) *100

  ggplot.q3a <- ggplot(monthlysales, aes(x = month, y = month.to.total, group = category , color = category)) +
                geom_line(linetype = "solid", size = 1) +
                labs(title = "Share of Total Sales Per Month, %", y= NULL, x = "Month") + 
                theme_bw() +
                theme(legend.position = "right", legend.title = element_blank(),
                      plot.title = element_text(size=15, face="bold", color="dark blue", hjust=0.5, lineheight=1),
                      axis.title.x = element_text(size=10, face="bold", color="dark blue", hjust=0.5, lineheight=1))
                      
  
  ggplot.q3a
  
  # Q3b chart
  ggplot.q3b <- ggplot(weekdaysales, aes(x = weekday, y = weekday.to.total , group = category, color = category)) +
                geom_line(linetype = "solid", size = 1) +
                labs(title = "Share of Total Sales Per Weekday, %", y = NULL, x = "Weekday") +
                theme_bw() +
                theme(legend.position = "None", 
                      plot.title = element_text(size=15, face="bold", color="dark blue", hjust=0.5, lineheight=1),
                      axis.title.x = element_text(size=10, face="bold", color="dark blue", hjust=0.5, lineheight=1))
  
  ggplot.q3b
  
  rm(monthlysales, weekdaysales, tot.sales)
  
  # Export Q3a chart
  png(file = "q3a.png", width = 2880, height = 1920, res = 180)
    ggplot.q3a
  dev.off()
  
  # Export Q3b chart
  png(file = "q3b.png", width = 2880, height = 1920, res = 180)
    ggplot.q3b
  dev.off()
  
  # Export Q3c chart
  png(file = "q3c.png", width = 2880, height = 1920, res = 180)
    grid.arrange(ggplot.q3a, ggplot.q3b
                            , nrow = 1, ncol = 2
                            , widths = c(2, 1)
                            )
  dev.off()
  
##--------------------------------------------------------------------
##----- Q4
##--------------------------------------------------------------------
  
  # Q4 chart
  
  ranking.dollars <- aggregate(sale.dollars ~ category, sales, sum)
  ranking.dollars$sale.type <- 1
  colnames(ranking.dollars)[2] <- "sales"
  ranking.dollars$rank <- rank(ranking.dollars$sales)
  
  
  ranking.volume <- aggregate(sale.volume ~ category, sales, sum)
  ranking.volume$sale.type <- 2
  colnames(ranking.volume)[2] <- "sales"
  ranking.volume$rank <- rank(ranking.volume$sales)
  
  ranking.bottles <- aggregate(sale.bottles ~ category, sales, sum)
  ranking.bottles$sale.type <- 3
  colnames(ranking.bottles)[2] <- "sales"
  ranking.bottles$rank <- rank(ranking.bottles$sales)
  
  ranking <- rbind(ranking.dollars, ranking.volume, ranking.bottles)
  rm(ranking.dollars, ranking.bottles, ranking.volume)

  
  ggplot.q4 <- ggplot(ranking, aes(x = sale.type, y = rank, group = category, color = category)) +
               geom_line(linetype = "solid", size = 1.2) + 
               geom_vline(aes(xintercept = sale.type), linetype="dashed") + 
               geom_text(data = ranking[ranking$sale.type == min(ranking$sale.type), ] , aes(label = category, y = rank, x = sale.type - 0.05), color = "black", hjust = 1, size = 3) +
               geom_text(data = ranking[ranking$sale.type == max(ranking$sale.type), ] , aes(label = category, y = rank, x = sale.type + 0.05), color = "black", hjust = 0, size = 3) +
               scale_x_continuous(sec.axis = dup_axis(), breaks = c(0:4), labels = c("", "Sales, $", "Sales, liters", "Sales, bottles", "")) +
               coord_cartesian(xlim = c(0:4)) +
               labs(title = "Liquor Category Rankings", x = "", y = "") +
               theme_bw() + 
               theme(legend.position = "none", panel.grid = element_blank(), panel.border = element_blank(),
                     axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
                     plot.title = element_text(hjust = 1/2, size = 15, face = "bold", color= "dark blue", lineheight = 1))
  
  ggplot.q4
  
  rm(ranking)
  
  # Export Q4 chart
  png(file = "q4.png", width = 1920, height = 1920, res = 180)
    ggplot.q4
  dev.off()
