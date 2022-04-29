install.packages("BasketballAnalyzeR")

library(BasketballAnalyzeR)

# Teams
View(Tbox)     # Tbox is the dataset for d teams to be clustered
str(Tbox)        
mydata <- data.frame(Tbox[,-c(1,2)])   # removes 1st and 2nd columns only becuz we use only columns with numeric values for clustering analysis
View(mydata) 

# doing K-means clustering.
set.seed(123)    
kc <- kclustering(mydata)   # In the chart, as Number of clusters values(x-axis) increases, the values in the connected dots increases big at first then starts increasing small small
plot(kc)

set.seed(123)    
kc <- kclustering(mydata, k = 6, labels = Tbox$Team)  
kc      # displays d cluster that each Team column value belongs to
plot(kc, profiles = T)  # creates 6 clusters/groups using all column values(except 1st and 2nd columns) as labels to define each cluster/group 


# doing Hierarchical clustering
set.seed(123) 
hc <- hclustering(mydata)   
plot(hc)


set.seed(123) 
hc <- hclustering(mydata, k = 6, labels = Tbox$Team) 
hc      # displays d cluster that each Team column value belongs to in columns and in dotted lines
plot(hc)


set.seed(123) 
hc <- hclustering(mydata, k = 6, labels = Tbox$Team) 
hc      # displays d cluster that each Team column value belongs to
plot(hc, rect = T, colored.branches = T, cex.labels = 0.9)   



# Players
View(Pbox)   # Pbox displays team and players to be clustered
str(Pbox)
ourdata <- data.frame(Pbox[,-c(1,2)])   
View(ourdata)

yrdata <- subset(ourdata, Pbox$MIN >= 2000)  # using subset() to filter the stated column- MIN by the stated values
ID <- Pbox$Player[Pbox$MIN >= 2000]  # displays Player column values that corresponds to MIN column values of >= 2000


# doing K-means clustering.
set.seed(123)
kc1 <- kclustering(yrdata)
plot(kc1)


set.seed(123)
kc1 <- kclustering(yrdata, k = 4, labels = ID)
kc1          # displays d cluster that each Player column value belongs to in columns and in dotted lines
plot(kc1, profiles = T)   # creates 4 clusters/groups using all column values(except 1st and 2nd columns) as labels to define each cluster/group 


# doing Hierarchical clustering
set.seed(123)
hc1 <- hclustering(yrdata)
plot(hc1)


set.seed(123)
hc1 <- hclustering(yrdata, k = 4, labels = ID)
hc1
plot(hc1, rect = T, colored.branches = T, cex.labels = 0.7)   # creates a dindogram


