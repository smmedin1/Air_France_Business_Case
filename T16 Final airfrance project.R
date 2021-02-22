################ ---  / \   /\/\    /_\    /\
################# |   \_/  /    \  /   \   \
#                                         \_\
#Data Science: R - DAT-5302 - MsBA1
#Team 16 
#Sarah Medina
#Henrik Bjonness
#Vi Nguyen
#Bogdan Gubarev


#____________________________________________
#installing requiered libraries 
#install.packages('readxl')
#install.packages('tidyverse')
#install.packages('stringr')
#install.packages('dplyr')
#install.packages('qdap')
#install.packages('plotly')
#install.packages('ggplot2')
#install.packages('wordcloud')


####Calling the librariues
library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(qdap)
library(plotly)
library(ggplot2)
library(wordcloud)


#############################################
#Primary Data Massaging
############################################

airfrance_df<- read_excel("Downloads/Air France Case Spreadsheet Supplement.xls", sheet= "DoubleClick")
View(airfrance_df)

#looking at structure of air france
print(nrow(airfrance_df))
str(airfrance_df)
#Copying the dataframe to perform operations 
airfrance <- airfrance_df

#Obtaining the overall columns names and removing unused column in the data
colnames(airfrance)
#Creating a vector for unused columns 
remove_col <- c('Publisher ID', 'Keyword ID', 'Match Type', 'Campaign', 'Keyword Group', 'Category', 'Keyword Type', 'Status', 
                'Total Cost/ Trans.')
#Filtering for the datafame for usefull columns
airfrance <- select(airfrance, -remove_col)

#Checking if any in large numbers or empty string is present in the data 
sum(airfrance== Inf)
sum(airfrance== "")
sum(is.na(airfrance))

#using the sapply function to check for missing values source
print(sapply(airfrance, function(x) sum(is.na(x))))

#Creating a revenue variable in the table
airfrance$revenue <- airfrance$Amount - airfrance$`Total Cost`

#Creating revenue/per booking variable 
airfrance$'Revenue per booking' <- round(airfrance$revenue/airfrance$`Total Volume of Bookings`,2)

#Creating booking probability
airfrance$'Booking Probability' <- round(( airfrance$`Trans. Conv. %` * airfrance$`Engine Click Thru %`) /100,2)
#Creating booking cost 
airfrance$'Cost per booking' <-as.numeric(airfrance$`Total Cost`/ airfrance$`Total Volume of Bookings`)

#turning us-publishers into 1, globals = 0 
airfrance$'US Publisher' <- c()
airfrance$'US Publisher' <- str_detect(airfrance$`Publisher Name`, "US")
airfrance$'US Publisher' <- as.numeric(airfrance$'US Publisher')

#substituting errors in spelling for bid strategy column
unique(airfrance$`Bid Strategy`)
airfrance$`Bid Strategy` <- gsub("Position 1 -2 Target", "Position 1-2 Target", 
                                 airfrance$`Bid Strategy`)
airfrance$`Bid Strategy` <- gsub("Postiion 1-4 Bid Strategy", "Position 1-4 Bid Strategy", 
                                 airfrance$`Bid Strategy`)


#Checking for negative revenues  and creating a separate dataset for analysis 
sum(airfrance$revenue < 0) ## question what should we do with that 
neg_rev_air <- airfrance[which(airfrance$revenue < 0),]
nrow(neg_rev_air)
summary(neg_rev_air)

###############################################
#Descriptive Statistics
###############################################
#creating a new data frame
air_df <- airfrance

#stats for mean, median, sd, min, max of meaningful numeric variables
air_stats <- c("Mean", "Median", "SD", "Min", "Max")
impressions <- round(c(mean(air_df$Impressions),median(air_df$Impressions),sd(air_df$Impressions),min(air_df$Impressions),max(air_df$Impressions)), 2)
num_clicks <- round(c(mean(air_df$Clicks),median(air_df$Clicks),sd(air_df$Clicks),min(air_df$Clicks),max(air_df$Clicks)), 2)
total_amount <- round(c(mean(air_df$Amount),median(air_df$Amount), sd(air_df$Amount),min(air_df$Amount), max(air_df$Amount)), 2)
total_cost <- round(c(mean(air_df$`Total Cost`),median(air_df$`Total Cost`),sd(air_df$`Total Cost`),min(air_df$`Total Cost`),max(air_df$`Total Cost`)), 2)
net_revenue <-round(c(mean(air_df$revenue),median(air_df$revenue),sd(air_df$revenue),min(air_df$revenue),max(air_df$revenue)), 2)
net_rpb <- round(c(mean(air_df$`Revenue per booking`),median(air_df$`Revenue per booking`),sd(air_df$`Revenue per booking`),min(air_df$`Revenue per booking`),max(air_df$`Revenue per booking`)), 2)
net_booking_prob <- round(c(mean(air_df$`Booking Probability`),median(air_df$`Booking Probability`),sd(air_df$`Booking Probability`),min(air_df$`Booking Probability`),max(air_df$`Booking Probability`)), 2)
#creating a dataframe to show the summary of air_stats
air_summ <- as.data.frame(cbind(air_stats, impressions , num_clicks, total_amount, total_cost, net_revenue, net_booking_prob))
air_summ

#Finding unique competitive publishers
publishers <- unique(air_df$`Publisher Name`)
publishers

#Sales descriptive statistics for each  publisher SEM
#starting with an empty vector
total_rev <- c()
total_clicks <- c()
cost_total <- c()
total_book <- c()
total_net_revenue <- c()
avg_positioning <- c()
#creating a for loop to compare publishers
for (i in 1:length(publishers)) {
  total_rev <- c(total_rev,sum(air_df$revenue[which(air_df[,1] == publishers[i])]))
  print(total_rev)
  total_clicks <- c(total_clicks,sum(air_df$Clicks[which(air_df[,1]==publishers[i])]))
  cost_total<- c(cost_total,sum(air_df$`Total Cost`[which(air_df[,1]==publishers[i])]))
  total_book <- c(total_book,sum(air_df$`Total Volume of Bookings`[which(air_df[,1]==publishers[i])]))
  total_net_revenue <- c(total_net_revenue, sum(air_df$revenue[which(air_df[,1]==publishers[i])]))
  avg_positioning <- c(avg_positioning, sum(air_df$`Avg. Pos.`[which(air_df[,1]==publishers[i])]))
  
}#ending loop

#combining Kayak with other publishers for a total amount 
all_publishers <- cbind(c(publishers,"Kayak-US"))
publisher_sales <- as.numeric(cbind(c(total_rev, "233694")))
publisher_clicks<- as.numeric(cbind(c(total_clicks, "2839")))
publisher_costs <- as.numeric(cbind(c(cost_total, "3567.13")))
publisher_books <- cbind(c(total_book, 208))
publisher_net_revenue <- cbind(c(total_net_revenue, 230126.87))
publisher_avg_position <- cbind(c(avg_positioning, NA))

#creating a matrix for overall publishers
my_matrix<- matrix( c(all_publishers, publisher_sales, publisher_clicks, publisher_costs, publisher_books,publisher_net_revenue,  publisher_avg_position), ncol = 7, nrow = 8)
publisher_df<- as.data.frame(my_matrix)
#changing the column names
colnames(x= publisher_df) <- c("publishers", "Total Sales", "Total Clicks", "Total Costs", "Total Bookings", "Net Revenue", "Avg. Position ")

#Changing the type of the variables in the matrix 
publisher_df$`Total Sales` <- round(as.numeric(publisher_df$`Total Sales` ))
publisher_df$`Total Clicks` <- round(as.numeric(publisher_df$`Total Clicks`))
publisher_df$`Total Costs` <- round(as.numeric(publisher_df$`Total Costs`))
publisher_df$`Total Bookings` <- round(as.numeric(publisher_df$`Total Bookings`))
publisher_df$`Net Revenue` <- round(as.numeric(publisher_df$`Net Revenue`))
publisher_df$`Avg. Position ` <- round(as.numeric(publisher_df$`Avg. Position `))
# Adding new columns with usefull information
publisher_df$'Revenue per booking' <- round(publisher_df$`Net Revenue`/publisher_df$`Total Bookings`)
publisher_df$'Cost per booking' <- round(publisher_df$`Total Costs`/publisher_df$`Total Bookings`)
publisher_df$'ROA' <- round((publisher_df$'Revenue per booking'/publisher_df$'Cost per booking'), 2)
publisher_df$'Avg. CPC'<- (publisher_df$'Total Costs'/publisher_df$'Total Clicks')

#empty vector to create 1, 0 in the United States or not
publisher_df$'US based' <- c()
publisher_df$'US based' <- str_detect(publisher_df$`publishers`, "US")
publisher_df$'US based' <- as.character(as.numeric(publisher_df$'US based'))

#Ordering the values based on the ROA
publisher_df <- publisher_df[order(publisher_df$ROA),]

#printing overall publishers and their totals
publisher_df


#############################################
#Logistic Regression 
############################################
#Logistic regression to find if
#there is  a difference of what SEM is doing in the US vs ROW- 
#need to identify which variables need to be better to increase companie's market share in the US. 
air_regression <- airfrance

#initial logistic regression
my_logitair<- glm(airfrance$`US Publisher` ~ airfrance$'Booking Probability' +airfrance$revenue + airfrance$'Amount' + airfrance$'Total Volume of Bookings'+airfrance$'Clicks'+airfrance$'Impressions'+airfrance$'Total Cost'+airfrance$'Avg. Pos.', 
                  data=air_regression, family = "binomial") 
summary(my_logitair)

#Cleaned logistic regression 
cleaned_logitair<- glm(airfrance$`US Publisher` ~  airfrance$'Impressions'+airfrance$'Avg. Pos.' , 
                       data=air_regression, family = "binomial") 
summary(cleaned_logitair)

#creating a pivot table to compare SEMS
air_piv2 <- publisher_df %>% group_by(publishers) %>% summarize(
  overall_records = n(),
  sum_tc = sum(`Total Costs`),
  avg_ROA = mean(ROA),
  avg_cpc = mean(`Avg. CPC`),
  avg_prob = mean(`Total Bookings`),
  avg_pos = mean(`Avg. Position`)
  
)
summary(air_piv2 )

###############################
#Using a bar chart to find highest ROA in relation to publishers
#Kayak has the highest ROA because they are cheap and has more focused audience, while for google it cost high due to publishers utilising the platfornm
#and the broaden customer targeting. 
x <- (publisher_df$publishers)
#where are the Ys coming from
y <- c(publisher_df$ROA)
data <- data.frame(x, y)

data$x <- factor(data$x, levels = data[["x"]])
p <- plot_ly(data, x= ~x, y= ~y, type = "bar", name = "Return on Advertising", colors = "YlOrRd"  , alpha = 0.5) %>%
  layout(title = "Return On Advertising",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
print(p)
########################################
#creating a bubble Chart
#Bubble CHart

p1 <- plot_ly(air_piv2, x = ~sum_tc, y = ~avg_prob,
             textposition = "auto",
             type = 'scatter', 
             mode = 'markers', 
             size = ~avg_cpc, 
             color = ~`publishers`, 
             colors = 'Paired',
             marker = list(opacity = 0.8, sizemode = 'diameter')) %>%
  layout(title = 'Bookings Based on Clicks',
         xaxis = list(title = "Sum of Total Clicks", showgrid = TRUE),
         yaxis = list(title = "Average Amount of Bookings ", showgrid = TRUE),
         showlegend = TRUE)

print(p1)
####Findings from bubble 
#Google US has the highest avg amount of bookings, as well as the most total clicks
#Although this looks great, they are still the most costly publisher in comparison
#Google’s SEM strategy is clearly reaching more people in order for to generate higher avg amount of bookings
#Yahoos click cost is cheaper but there's still a very high amount of bookings

#############################################
#Publisher Analysis 
############################################

#Creating a Stacked Bar Chart
# Stacked Bar Plot with Colors and Legend
#looking at overall publishers
p2 <- ggplot() + geom_bar(aes(y = `Total Costs` , x = `US based`, fill = publishers), data = publisher_df,
                          stat="identity")
print(p2)

### Creating data frame to make publisher analyses

air_pa <- airfrance

#Slicing the data
air_pa$ROA <- air_pa$revenue/air_pa$`Total Cost`
air_pa <- air_pa[which(air_pa$revenue > 0),]

bid_strat <- unique(air_pa$`Bid Strategy`)
print(bid_strat)
#creating empty vectors for publishers
ROA_bid_strat <-c()
Yahoo_US_BS <- c()
MSN_Global_BS <- c()
MSN_US_BS <- c()
Google_Global_BS <- c()
Google_US_BS <- c()
Overture_Global_BS<- c()
Overture_US_BS <- c()

for (i in 1:length(bid_strat)){
  Yahoo_US_BS <- c(Yahoo_US_BS, mean(air_pa$ROA[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                            air_pa$'Publisher Name' == publishers[1])]))
  MSN_Global_BS <- c(MSN_Global_BS, mean(air_pa$ROA[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                                air_pa$'Publisher Name' == publishers[2])]))
  Google_Global_BS <- c(Google_Global_BS, mean(air_pa$ROA[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                                      air_pa$'Publisher Name' == publishers[3])]))
  Overture_Global_BS <- c(Overture_Global_BS, mean(air_pa$ROA[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                                          air_pa$'Publisher Name' == publishers[4])]))
  Google_US_BS <- c(Google_US_BS, mean(air_pa$ROA[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                              air_pa$'Publisher Name' == publishers[5])]))
  Overture_US_BS <- c(Overture_US_BS, mean(air_pa$ROA[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                                  air_pa$'Publisher Name' == publishers[6])]))
  MSN_US_BS <- c(MSN_US_BS, mean(air_pa$ROA[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                        air_pa$'Publisher Name' == publishers[7])]))
}

#creating empty verctors for average positioning
Yahoo_US_AP <- c()
MSN_Global_AP <- c()
MSN_US_AP <- c()
Google_Global_AP <- c()
Google_US_AP <- c()
Overture_Global_AP<- c()
Overture_US_AP <- c()
#looking at average positioning compared to bid strategy
for (i in 1:length(bid_strat)){
  Yahoo_US_AP <- c(Yahoo_US_AP, mean(air_pa$`Avg. Pos.`[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                                air_pa$'Publisher Name' == publishers[1])]))
  MSN_Global_AP <- c(MSN_Global_AP, mean(air_pa$`Avg. Pos.`[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                                    air_pa$'Publisher Name' == publishers[2])]))
  Google_Global_AP <- c(Google_Global_AP, mean(air_pa$`Avg. Pos.`[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                                          air_pa$'Publisher Name' == publishers[3])]))
  Overture_Global_AP <- c(Overture_Global_AP, mean(air_pa$`Avg. Pos.`[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                                              air_pa$'Publisher Name' == publishers[4])]))
  Google_US_AP <- c(Google_US_AP, mean(air_pa$`Avg. Pos.`[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                                  air_pa$'Publisher Name' == publishers[5])]))
  Overture_US_AP <- c(Overture_US_AP, mean(air_pa$`Avg. Pos.`[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                                      air_pa$'Publisher Name' == publishers[6])]))
  MSN_US_AP <- c(MSN_US_AP, mean(air_pa$`Avg. Pos.`[which(air_pa$'Bid Strategy' == bid_strat[i] & 
                                                            air_pa$'Publisher Name' == publishers[7])]))
}

ROA_bid_strat <- cbind(bid_strat, Yahoo_US_BS,Yahoo_US_AP, MSN_Global_BS,MSN_Global_AP, 
                       Google_Global_BS, Google_Global_AP,
                       Overture_Global_BS, Overture_Global_AP, Google_US_BS, Google_US_AP,
                       Overture_US_AP, Overture_US_BS, MSN_US_AP, MSN_US_BS)
View(ROA_bid_strat)


#creating a stacked bar chart to look at bid strategy vs revenue
#all of the global publishers have specific bid strategy, 
#google is around the board but all of their Us publishers dont really
#us comp need specific positions in order to target the market better
p3 <- ggplot() + geom_bar(aes(y = `revenue` , x = `Bid Strategy`, fill = `Publisher Name`), data = air_pa,
                          stat="identity")
p3

##########################################
# MASSAGING text objects in R
#########################################
#Looking for keywords
key_air <- airfrance
View(key_air)
# Filter the Data with ROA higher than 0
key_air$ROA <- as.numeric(key_air$revenue / key_air$`Total Cost`)
#Checking for the campaign performed onlu in us  and creating a seorate dataframe 
key_air_us <- filter(key_air, `US Publisher` == 1)
key_air_us

# creating a separate list with keywords
Keywords_global <- key_air$Keyword
Keywords_US <- key_air_us$Keyword
View(Keywords_global)
View(Keywords_US)

#Creating text vectors to use in corpus matrix
key_gl <- VectorSource(Keywords_global)
key_gl <- VCorpus(key_gl)
key_gl
key_us <- VectorSource(Keywords_US)
key_us <- VCorpus(key_us)
key_us
#Creating a cleaning function for the vcorpus element 
cleaning<- function(x){
  Copr <- tm_map(x, stripWhitespace)
  x <- tm_map(x, removePunctuation)
  x <- tm_map(x, removeNumbers)
  x <- tm_map(x, content_transformer(tolower))
  x <- tm_map(x, removeWords, c(stopwords("en")))
  return(x)
}

#Cleaning the text data for US and World
clean_gl <- cleaning(key_gl)
clean_gl
clean_us <- cleaning(key_us)
clean_us

#creating a term document matrix for keywords
key_tdm_gl <- TermDocumentMatrix(clean_gl)
key_tdm_us<- TermDocumentMatrix(clean_us)
# Printing key_tdm data
head(key_tdm_gl)
head(key_tdm_us)

# Converting to a matrix
gl_matrix<- as.matrix(key_tdm_gl)
us_matrix <-as.matrix(key_tdm_us)

#calculating and sorting for row sums for global and us keyword matrix 
freq_gl <- rowSums(gl_matrix)
freq_gl <- sort(freq_gl, decreasing = T)

freq_us <- rowSums(us_matrix)
freq_us <- sort(freq_us, decreasing = T)

# View the top 10 most common words
freq_gl[1:10]
freq_us[1:10]


#word clouds for global 
Global_terms <- data.frame(
  term = names(freq_gl),
  num = freq_gl
)

#word clouds for us 
Global_terms_us <- data.frame(
  term = names(freq_us),
  num = freq_us
)

#Bar plot looking at global terms
barplot(Global_terms_us[1:10,]$num, las=1, names.arg= Global_terms_us[1:10,]$term, col ="lightblue", main ="Most Frequent Words-US",
        ylab = "Word frequencies-US")

#Creating wordcloud for most frequent global keywords 
wordcloud(Global_terms$term, Global_terms$num,
                max.words = 100, 
                colors = c("grey80", "red", "blue4"))


#Creating wordcloud for most frequent us keywords 
wordcloud(Global_terms_us$term, Global_terms_us$num,
                max.words = 100, 
                colors = c("grey80", "orange", "green"))

#############################################
#Thank you 
############################################
