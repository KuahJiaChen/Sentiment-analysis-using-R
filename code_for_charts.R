setwd("C:/Users/Kuah Jia Chen/Documents/Monash_Resources/Sem 1 2022/FIT3152/Assignment 1")

# import all necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(igraph)
library(igraphdata)
library(tidyr)

# Generate the dataset
rm(list = ls())
set.seed(32286988) # XXXXXXXX = your student ID
webforum <- read.csv("webforum.csv")
webforum <- webforum [sample(nrow(webforum), 20000), ] # 20000 rows

# Study the data set
head(webforum)
summary(webforum)
typeof(webforum)
str(webforum)
n_distinct(webforum$AuthorID)

# Convert the datatype of Date column to Date
webforum$Date = as.Date(webforum$Date,"%Y-%m-%d")
webforum$Date


########################
##### Question a.1 #####
########################

# group them and calculate the number of post for a specific month
date_table <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=n())
# rename the column
date_table = rename(date_table,No_of_post = frequency)
# check the date_table by printing out the first six rows
head(date_table)
# get the mean of No_of_post
mean(date_table$No_of_post)
# check the data type of all columns
str(date_table)

# Figure 1 in the report

# plot the time series graph
ggplot(date_table,aes(x=Date,y=No_of_post,group=1)) + 
  geom_line() + scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) + 
  ggtitle("Number of posts over months and years from 2002 to 2011") + 
  theme(plot.title=element_text(size=15)) + labs(x = "Month/Year") + 
  labs(y="Number of post") + scale_y_continuous(n.breaks=20) + 
  geom_line(size=1)

# pre-process the data so that I can use it to do the hypothesis testing
date_table_before_July_2004 = date_table[as.Date(date_table$Date,"%Y-%m-%d") < as.Date("2004-07-01","%Y-%m-%d"),]
date_table_before_July_2004
date_table_between_July_2004_Jan_2006 = date_table[as.Date(date_table$Date,"%Y-%m-%d") < as.Date("2006-01-01","%Y-%m-%d"),]
date_table_between_July_2004_Jan_2006 = 
  date_table_between_July_2004_Jan_2006[
    as.Date(date_table_between_July_2004_Jan_2006$Date,"%Y-%m-%d") 
    >= as.Date("2004-07-01","%Y-%m-%d"),]
date_table_between_July_2004_Jan_2006

# please check the description of this hypothesis testing in the report
t.test(date_table_between_July_2004_Jan_2006$No_of_post,date_table_before_July_2004$No_of_post,alternative = "less")


# pre-process the data so that I can use it to do the hypothesis testing
date_table_between_Oct_2005_Jan_2006 = date_table[as.Date(date_table$Date,"%Y-%m-%d") < as.Date("2006-01-01","%Y-%m-%d"),]
date_table_between_Oct_2005_Jan_2006 = 
  date_table_between_Oct_2005_Jan_2006[
    as.Date(date_table_between_Oct_2005_Jan_2006$Date,"%Y-%m-%d") 
    >= as.Date("2005-10-01","%Y-%m-%d"),]
date_table_between_Oct_2005_Jan_2006

date_table_between_Jan_2006_July_2009 = date_table[as.Date(date_table$Date,"%Y-%m-%d") < as.Date("2009-07-01","%Y-%m-%d"),]
date_table_between_Jan_2006_July_2009 = 
  date_table_between_Jan_2006_July_2009[
    as.Date(date_table_between_Jan_2006_July_2009$Date,"%Y-%m-%d") 
    >= as.Date("2006-01-01","%Y-%m-%d"),]
date_table_between_Jan_2006_July_2009

# please check the description of this hypothesis testing in the report
t.test(date_table_between_Oct_2005_Jan_2006$No_of_post,date_table_between_Jan_2006_July_2009$No_of_post,alternative = "less")


########################
##### Question a.2 #####
########################

# Figure 13 in the Appendix

# The following piece of code is used to group by the webforum data set based on Date column
# and calculate the mean of that particular column throughout the whole period
webforum_analytic <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(Analytic))
webforum_analytic = rename(webforum_analytic,Average_of_Analytic = frequency)
webforum_clout <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(Clout))
webforum_clout = rename(webforum_clout,Average_of_Clout = frequency)
webforum_authentic <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(Authentic))
webforum_authentic = rename(webforum_authentic,Average_of_Authentic = frequency)
webforum_tone <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(Tone))
webforum_tone = rename(webforum_tone,Average_of_Tone = frequency)
# combine all columns together to create a suitable data frame to plot the time series graph
webforum_four = cbind(webforum_analytic,webforum_clout[2],webforum_authentic[2],webforum_tone[2])
head(webforum_four) # check the data frame
str(webforum_four) # check the data type of columns

colors <- c("Analytic" = "steelblue", "Clout" = "darkred", "Authentic" = "darkmagenta","Tone" = "darkgoldenrod1")
ggplot(webforum_four) + 
  geom_line(aes(x=Date,y=Average_of_Analytic,color="Analytic"),size=1.2) + scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) + geom_line(aes(x=Date,y=Average_of_Clout,color="Clout"),size=1.2) + 
  geom_line(aes(x=Date,y=Average_of_Authentic,color="Authentic"),size=1.2) + 
  geom_line(aes(x=Date,y=Average_of_Tone,color="Tone"),size=1.2) +
  scale_color_manual(values = colors) + 
  labs(y="Average percentile (%)") + labs(x = "Month/Year") +  
  ggtitle("The average percentile of posts expressed on a summary variable over months and years from 2002 to 2011") + 
  theme(plot.title=element_text(size=15)) + scale_y_continuous(n.breaks=20)


# Figure 14 in the Appendix

# The following piece of code is used to group by the webforum data set based on Date column
# and calculate the mean of that particular column throughout the whole period
webforum_ppron <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(ppron))
webforum_ppron = rename(webforum_ppron,Average_of_Ppron = frequency)
webforum_i <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(i))
webforum_i = rename(webforum_i,Average_of_i = frequency)
webforum_we <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(we))
webforum_we = rename(webforum_we,Average_of_We = frequency)
webforum_you <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(you))
webforum_you = rename(webforum_you,Average_of_You = frequency)
webforum_shehe <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(shehe))
webforum_shehe = rename(webforum_shehe,Average_of_Shehe = frequency)
webforum_they <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(they))
webforum_they = rename(webforum_they,Average_of_They = frequency)
# combine all columns together to create a suitable data frame to plot the time series graph
webforum_all_pron = cbind(webforum_ppron,webforum_i[2],webforum_we[2],webforum_you[2],webforum_shehe[2],webforum_they[2])
head(webforum_all_pron) # check the data frame

colors2 <- c("ppron" = "deepskyblue3", "i" = "burlywood", "we" = "chartreuse",
            "you" = "coral","shehe" = "cadetblue","they" = "darkblue")
ggplot(webforum_all_pron) + 
  geom_line(aes(x=Date,y=Average_of_Ppron,color="ppron"),size=1.2) + scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) + geom_line(aes(x=Date,y=Average_of_i,color="i"),size=1.2) + 
  geom_line(aes(x=Date,y=Average_of_We,color="we"),size=1.2) + 
  geom_line(aes(x=Date,y=Average_of_You,color="you"),size=1.2) +
  scale_color_manual(values = colors2) + 
  geom_line(aes(x=Date,y=Average_of_Shehe,color="shehe"),size=1.2) + 
  geom_line(aes(x=Date,y=Average_of_They,color="they"),size=1.2) + 
  labs(y="Average proportion of total word counts (%)") + labs(x = "Month/Year") +  
  ggtitle("The average proportion of all word counts for all pronouns over months and years from 2002 to 2011") + 
  theme(plot.title=element_text(size=15)) + scale_y_continuous(n.breaks=20)
  
# Figure 15 in the Appendix

# The following piece of code is used to group by the webforum data set based on Date column
# and calculate the mean of that particular column throughout the whole period
webforum_posemo <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(posemo))
webforum_posemo = rename(webforum_posemo,Average_of_Posemo = frequency)
webforum_negemo <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(negemo))
webforum_negemo = rename(webforum_negemo,Average_of_Negemo = frequency)
webforum_anx <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(anx))
webforum_anx = rename(webforum_anx,Average_of_Anx = frequency)
webforum_anger <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(anger))
webforum_anger = rename(webforum_anger,Average_of_Anger = frequency)
webforum_sad <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(sad))
webforum_sad = rename(webforum_sad,Average_of_Sad = frequency)
webforum_focuspast <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(focuspast))
webforum_focuspast = rename(webforum_focuspast,Average_of_Focuspast = frequency)
webforum_focuspresent <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(focuspresent))
webforum_focuspresent = rename(webforum_focuspresent,Average_of_Focuspresent = frequency)
webforum_focusfuture <- webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=mean(focusfuture))
webforum_focusfuture = rename(webforum_focusfuture,Average_of_Focusfuture = frequency)
# combine all columns together to create a suitable data frame to plot the time series graph
webforum_all_expression = cbind(webforum_posemo,webforum_negemo[2],webforum_anx[2],
                                webforum_anger[2],webforum_sad[2],webforum_focuspast[2],
                                webforum_focuspresent[2],webforum_focusfuture[2])
head(webforum_all_expression) # check the data frame

colors3 <- c("posemo" = "deepskyblue3", "negemo" = "burlywood", "anx" = "chartreuse",
            "anger" = "coral","sad" = "cadetblue","focuspast" = "darkblue",
            "focuspresent" = "black","focusfuture" = "brown")

ggplot(webforum_all_expression) + 
  geom_line(aes(x=Date,y=Average_of_Posemo,color="posemo"),size=1.2) + scale_x_date(date_breaks = "6 month", date_labels = "%Y-%m") + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) + 
  geom_line(aes(x=Date,y=Average_of_Negemo,color="negemo"),size=1.2) + 
  geom_line(aes(x=Date,y=Average_of_Anx,color="anx"),size=1.2) +
  scale_color_manual(values = colors3) + 
  geom_line(aes(x=Date,y=Average_of_Anger,color="anger"),size=1.2) + 
  geom_line(aes(x=Date,y=Average_of_Sad,color="sad"),size=1.2) + 
  geom_line(aes(x=Date,y=Average_of_Focuspast,color="focuspast"),size=1.2) + 
  geom_line(aes(x=Date,y=Average_of_Focuspresent,color="focuspresent"),size=1.2) + 
  geom_line(aes(x=Date,y=Average_of_Focusfuture,color="focusfuture"),size=1.2) + 
  labs(y="Average proportion of total word counts (%)") + labs(x = "Month/Year") +  
  ggtitle("The average proportion of all word counts for the linguistic variables over months and years from 2002 to 2011") + 
  theme(plot.title=element_text(size=15)) + scale_y_continuous(n.breaks=20)
  


# Figure 15 in the Appendix
# create a heatmap to visualize the correlation between each pair of linguistic variables

# combine all necessary columns into a single data frame
webforum_all_linguistic_variables = cbind(webforum_four,webforum_all_pron[2:7],webforum_all_expression[2:9])
head(webforum_all_linguistic_variables) # check the data frame
# find the correlation
correlation_all = cor(webforum_all_linguistic_variables[2:19])
correlation_all # check the data in correlation_all
# convert correlation_all so that it can be used to create heatmap using melt function
melted_cormat <- melt(correlation_all) 

# plot the heatmap
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme(axis.text.x=element_text(angle = -45, hjust = 0)) + 
  labs(x="Average percentile/proportion for a specific linguistic variable over time") + 
  labs(y="Average percentile/proportion for a specific linguistic variable over time") + 
  ggtitle("The correlation between each pair of linguistic variables over time")


# Figure 2 in the report

# plot a scatter plot
ggplot(webforum_all_linguistic_variables,aes(x=log(Average_of_Negemo),y=log(Average_of_Anger),color=log(Average_of_Tone))) + 
  geom_point(size=5) + geom_smooth(method = "lm", se = FALSE,color="darkred") + 
  ggtitle("The log value of Average_of_Negemo against the log value of Average_of_Anger with a red regression line")

# print the details of the regression line
fitted = lm(log(webforum_all_linguistic_variables$Average_of_Anger)~log(webforum_all_linguistic_variables$Average_of_Negemo))
summary(fitted)


########################
##### Question b.1 #####
########################

# find the number of post for each thread
number_of_post_for_each_thread <- webforum %>% group_by(ThreadID) %>% summarise(frequency=n())
# sort the result data frame to find the ID of top 10 thread with most post
head(number_of_post_for_each_thread[order(number_of_post_for_each_thread$frequency, decreasing=TRUE),],10)

# Figure 3 in the report

# plot the time series graph for the top 4 threads for the average of posemo and negemo variables
# throughout the whole period
color_questionb1 = c("Average_Posemo" = "steelblue","Average_Negemo" = "darkred")
top_4_ID = c(252620,283958,127115,145223)
top_4_thread <- webforum %>% filter(ThreadID %in% top_4_ID) 
# group by the data frame using two attributes, i.e., ThreadID and Date
top_4_thread <- top_4_thread %>% group_by(ThreadID, Date=floor_date(Date, "month")) %>% summarise(Average_Posemo=mean(posemo),Average_Negemo=mean(negemo))
top_4_thread %>% ggplot(aes(x=Date)) + geom_line(aes(y=Average_Posemo,color="Average_Posemo"),size=1) + 
  geom_line(aes(y=Average_Negemo,color="Average_Negemo"),size=1) + facet_wrap(~ThreadID,ncol=2) + 
  labs(y="Average proportion of total word counts (%)") + labs(x = "Month/Year") + 
  ggtitle("The average proportion of total word counts for posemo and negemo over months and years from 2002 to 2011 for the top 4 threads with the most posts")


# Figure 19 in the Appendix

# plot the time series graph for the top 5 to 10 threads for the average of posemo and negemo variables
# throughout the whole period
top_5_to_10_ID = c(472752,309286,532649,296985,191868,773564)
top_5_to_10_thread <- webforum %>% filter(ThreadID %in% top_5_to_10_ID) 
# group by the data frame using two attributes, i.e., ThreadID and Date
top_5_to_10_thread <- top_5_to_10_thread %>% group_by(ThreadID, Date=floor_date(Date, "month")) %>% summarise(Average_Posemo=mean(posemo),Average_Negemo=mean(negemo))
top_5_to_10_thread %>% ggplot(aes(x=Date)) + geom_line(aes(y=Average_Posemo,color="Average_Posemo"),size=1) + 
  geom_line(aes(y=Average_Negemo,color="Average_Negemo"),size=1) + facet_wrap(~ThreadID,ncol=3) + 
  labs(y="Average proportion of total word counts (%)") + labs(x = "Month/Year") + 
  ggtitle("The average proportion of total word counts for posemo and negemo over months and years \n from 2002 to 2011 for the top 5 to 10 threads with the most posts")


# get the average posemo and negemo for the following two threads and do hypothesis testing
# please check the description of the hypothesis testing in the report
average_posemo_negemo_for_283958 = top_4_thread[top_4_thread$ThreadID == 283958,]
average_posemo_negemo_for_472752 = top_5_to_10_thread[top_5_to_10_thread$ThreadID == 472752,]
t.test(average_posemo_negemo_for_283958$Average_Posemo,average_posemo_negemo_for_472752$Average_Posemo,alternative = "greater")
t.test(average_posemo_negemo_for_283958$Average_Negemo,average_posemo_negemo_for_472752$Average_Negemo,alternative = "less")



########################
##### Question c.1 #####
########################

# find the number of posts for each month of a year
find_num_posts_in_one_month = webforum %>% group_by(Date=floor_date(Date, "month")) %>% summarise(frequency=n())
# rename the column
find_num_posts_in_one_month = rename(find_num_posts_in_one_month,No_of_post = frequency)
# sort the data frame in descending order according to the value of No_of_post
find_num_posts_in_one_month = find_num_posts_in_one_month[order(find_num_posts_in_one_month$No_of_post, decreasing=TRUE),]
# I had selected the month that is ranked 98th in the dataset, the reason for choosing this
# particular month has been explained in the report
find_num_posts_in_one_month[98,]

# extract the data needed and study the data frame using count(), n_distinct() and so on to ensure 
# there is at least 30 authors included in the data set
post_in_July_of_2009_all_variables = webforum[as.Date(webforum$Date,"%Y-%m-%d") < as.Date("2009-08-01","%Y-%m-%d"),]
post_in_July_of_2009_all_variables = 
  post_in_July_of_2009_all_variables[
    as.Date(post_in_July_of_2009_all_variables$Date,"%Y-%m-%d") 
    >= as.Date("2009-07-01","%Y-%m-%d"),]
count(post_in_July_of_2009_all_variables)
n_distinct(post_in_July_of_2009_all_variables$AuthorID)
n_distinct(post_in_July_of_2009_all_variables$ThreadID)
nrow(post_in_July_of_2009_all_variables)
post_in_July_of_2009_authorID_threadID = post_in_July_of_2009_all_variables[,c(1,2)]
post_in_July_of_2009_authorID_threadID
# change the data type of the columns
post_in_July_of_2009_authorID_threadID$AuthorID = as.character(post_in_July_of_2009_authorID_threadID$AuthorID)
post_in_July_of_2009_authorID_threadID$ThreadID = as.character(post_in_July_of_2009_authorID_threadID$ThreadID)

# Figure 4 in the report

# Create the social network graph
# Refer to the code given in the lecture slides in week 5

g <- make_empty_graph(directed = FALSE)
# add vertices using "for loop"
for (node in unique(post_in_July_of_2009_authorID_threadID$AuthorID)) {
  g <- add_vertices(g, 1, name = as.character(node))
}
plot(g) # check the current graph

# loop through each group
for (k in unique(post_in_July_of_2009_authorID_threadID$ThreadID)){
  temp = post_in_July_of_2009_authorID_threadID[(post_in_July_of_2009_authorID_threadID$ThreadID == k),]
  # combine each pair of agents to make an edge list
  if (length(temp$ThreadID) > 1){
    Edgelist = as.data.frame(t(combn(temp$AuthorID,2)))
    print(Edgelist)
    colnames(Edgelist) = c("P1", "P2")
    # loop through pairs of edges and add
    for (i in 1 : nrow(Edgelist)) {
      g <- add_edges(g,
                     c(as.character(Edgelist$P1[i]),as.character(Edgelist$P2[i])))
    }
  }
}
plot(g) # check the current graph

# Simplify the graph such that there is no loop and no multi-edges
# please note that I am created an unweighted graph
g = simplify(g)
plot(g, layout = layout.fruchterman.reingold, vertex.size=12, main="The social network of all authors who are posting over July of 2009")
E(g) # check the number of edges
V(g) # check the number of vertices


diameter(g)  # check the diameter of the graph
average.path.length(g) # check the average path length of the graph
graph.density(g) # check the graph density of the graph
transitivity(g) # check the transitivity of the graph
# get.adjacency(g) # the adjacency matrix is too big to show in the console

# Figure 23 in the Appendix

# plot a histogram to check the degree distribution of the network graph
hist(degree(g),breaks=5,col = "grey",main="Degree Distribution of the network graph shown in figure 4",
     xlab="Number of degree")



########################
##### Question c.2 #####
########################

#calculate network centrality measures and combine in a dataframe
d = as.table(degree(g))
b = as.table(betweenness(g))
c = as.table(closeness(g))
e = as.table(evcent(g)$vector)
stats = as.data.frame(rbind(d,b,c,e))
stats = as.data.frame(t(stats))
colnames(stats) = c("degree", "betweenness", "closeness", "eigenvector") 

#sort and explore key nodes
head(stats)
stats[order(-stats$betweenness),]     # top 3 = 170150, 178803, 54960        
stats[order(-stats$closeness),]       # top 3 = 105615, 76919, 188903 (170150 = 8th, 54960 = 9th, 178803 = 10th)
stats[order(-stats$eigenvector),]     # top 3 = 54960, 170150, 180500, 178803
stats[order(-stats$degree),]          # top 3 = 170150 , 180500, 178803, 54960


# get the means of variables for this author based on the posts that he/she posted in July 2009
colMeans(post_in_July_of_2009_all_variables[(post_in_July_of_2009_all_variables$AuthorID == 170150),c(6:23)]) 

# plot histogram to compare the variables of other authors with 170150

# exclude the row for 170150 author
post_in_July_of_2009_all_variables_other_author = post_in_July_of_2009_all_variables[(post_in_July_of_2009_all_variables$AuthorID != 170150),c(6:23)]

# apply the pivot_longer function to convert the data from wide to long format
# so that it can be used to plot multiple histograms in a single figure
data_long <- post_in_July_of_2009_all_variables_other_author %>%                          
  pivot_longer(colnames(post_in_July_of_2009_all_variables_other_author)) %>% 
  as.data.frame()
head(data_long) # check the data frame

# Figure 5 in the report

ggplot(data_long, aes(x=value)) + geom_histogram(bins=50,fill="steelblue") + 
  facet_wrap(~ name, ncol = 3) + 
  ggtitle("The distribution of linguistic variables for all authors posted during July 2009 excluding author 170150") + 
  xlab("Frequency") + ylab("Percentage (%)")

# pre-process the data so that I can use it to do the hypothesis testing
data_for_focus_present_other_author = data_long[(data_long$name=="focuspresent"),2]
data_for_focus_present_other_author
data_for_focus_present_author_170150 = post_in_July_of_2009_all_variables[(post_in_July_of_2009_all_variables$AuthorID == 170150),22]
data_for_focus_present_author_170150

# please check the description of this hypothesis testing in the report
t.test(data_for_focus_present_other_author,data_for_focus_present_author_170150,alternative="two.sided")

# pre-process the data so that I can use it to do the hypothesis testing
data_for_ppron_other_author = data_long[(data_long$name=="ppron"),2]
data_for_ppron_other_author
data_for_ppron_author_170150 = post_in_July_of_2009_all_variables[(post_in_July_of_2009_all_variables$AuthorID == 170150),10]
data_for_ppron_author_170150

# please check the description of this hypothesis testing in the report
t.test(data_for_ppron_other_author,data_for_ppron_author_170150,alternative="two.sided")

