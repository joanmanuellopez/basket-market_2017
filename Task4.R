#### Get data and visualize it ####
basket_dataset <- read.transactions(file = "ElectronidexTransactions2017.csv",format = c("basket"), sep=",", cols = NULL)

length (basket_dataset)
basketSize <- size(basket_dataset)
mean(basketSize)
ggplot() + aes(basketSize)+ geom_histogram(binwidth=1, colour="black", fill="white")

itemFrequencyPlot(basket_dataset,type=c("relative"),topN=15, support=0.01)
image(sample(basket_dataset, 10))

# How to use plotting in arules:
# https://rpubs.com/hoakevinquach/Association-Rules

#### Apply Apriori algorithm to get the Association Rules ####
basketRules <- apriori (basket_dataset, parameter = list(supp = 0.0015, conf = 0.85, minlen = 2, maxlen = 6))
# basketRules <- apriori (basket_dataset, parameter = list(supp = 0.001, conf = 0.85, minlen = 2, maxlen = 4), appearance = list(none = c("iMac")), control = list(verbose=F))
# basketRules <- apriori (basket_dataset, parameter = list(supp = 0.0012, conf = 0.85, minlen = 2, maxlen = 8), appearance = list(default="lhs",rhs="ViewSonic Monitor"), control = list(verbose=F))

summary(basketRules)
inspect(basketRules) #How to consider only certain rules?
inspect(sort(basketRules, by = "lift"))

# Convert Association rules into DataFrame !!!!
dfrRules <- DATAFRAME(basketRules, separate = TRUE)

itemBasketRules <- subset(basketRules, items %in% "Samsung Monitor")
is.redundant(basketRules)

# Visualize Rules
plot(basketRules[1:15], method="graph", control=list(type="items"))
plot(sort(basketRules, by = "lift")[3:40], method="graph", control=list(type="items"),interactive = TRUE)

#### How to overcome redundancies ####
subset.matrix <- is.subset(basketRules, basketRules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
basketRules.pruned <- basketRules[!redundant]


#Method 2 to overcome redundancies
subsetRules <- which(colSums(is.subset(basketRules, basketRules)) > 1) # get subset rules in vector
length(subsetRules)  #> 3913
basketRules2 <- basketRules[-subsetRules] # remove subset rules. 


# Very interesting resources for further information
# https://towardsdatascience.com/a-gentle-introduction-on-market-basket-analysis-association-rules-fa4b986a40ce
# http://rstatistics.net/association-mining-with-r/
