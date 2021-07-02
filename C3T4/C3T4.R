#Install and import arules packages
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

#See information about each package
??arules
??arulesViz

#read transactions 
transdata <- read.transactions("ElectronidexTransactions2017edit.csv", 
                               format = "basket", header = FALSE, 
                               sep = ",", rm.duplicates = FALSE)

#summary of transaction data
summary(transdata)

#take a look at the transaction data (lines 1-10)
inspect(transdata[1:10])

#number of transactions
length(transdata)

#number of items per transaction
size(transdata)

#lists transactions by conversion
LIST(transdata[1:10])

#see item labels
itemLabels(transdata)

#visualize item frequency within the transactions as a bar chart
#can also plot based on level of support. ie support = 0.1
itemFrequencyPlot(transdata, topN = 20)
??itemFrequencyPlot

#visualize a sample of the transactions
image(sample(transdata, 20), support = 0.005)

#apply the apriori algorithm with "standard" settings
#this returns 0 rules
test_rules <- apriori(transdata, parameter = list(supp = 0.1, conf = 0.8))


#zero rules were created with the previous setting, so we have to try using different parameters
#lower support and higher confidence generate strong lift (lift ~ 3 - 6)
test_rules2 <- apriori(transdata, parameter = list(supp = 0.0012, conf = 0.93)) 
#16 rules generated

#look at the rules
inspect(test_rules2[1:10]) #looks at first 10 rules in the set
inspect(test_rules2) #loots at all the rules generated

#sort rules in descending order by support
top_supp2 <- sort(test_rules2, decreasing = TRUE, by = "support")
inspect(top_supp2[1:10])

#sort rules in descending order by confidence
top_conf2 <- sort(test_rules2, decreasing = TRUE, by = "confidence")
inspect(top_conf2[1:10])

#sort rules in descending order by lift
top_lift2 <- sort(test_rules2, decreasing = TRUE, by = "lift")
inspect(top_lift2[1:10])

#check for redundant rules
is.redundant(test_rules2) #none are redundant


#test higher support but lower confidence
test_rules3 <- apriori(transdata, parameter = list(supp = 0.01, conf = 0.5))
inspect(test_rules3[1:10])

top_supp3 <- sort(test_rules3, decreasing = TRUE, by = "support")
inspect(top_supp3[1:10])

top_conf3 <- sort(test_rules3, decreasing = TRUE, by = "confidence")
inspect(top_conf3[1:10])

top_lift3 <- sort(test_rules3, decreasing = TRUE, by = "lift")
inspect(top_lift3[1:10])

is.redundant(test_rules3)

#inspect rules pertaining to a particular item
imac_rules <- subset(test_rules3, items %in% "iMac")
inspect(imac_rules)


#test what happens when using minlen parameter
test_rules4a <- apriori(transdata, parameter = list(supp = 0.001, conf = 0.8))

test_rules4b <- apriori(transdata, parameter = list(supp = 0.001, conf = 0.8, minlen = 5))
inspect(test_rules4[1:10])

top_lift4 <- sort(test_rules4, decreasing = TRUE, by = "lift")

inspect(top_lift4[1:10])


#try with higher support
test_rules5 <- apriori(transdata, parameter = list(supp = 0.005, conf = 0.65))
inspect(test_rules5)

top_supp5 <- sort(test_rules5, decreasing = TRUE, by = "support")
inspect(top_supp5[1:9])

top_conf5 <- sort(test_rules5, decreasing = TRUE, by = "confidence")
inspect(top_conf5[1:9])

top_lift5 <- sort(test_rules5, decreasing = TRUE, by = "lift")
inspect(top_lift5[1:9])

is.redundant(test_rules5)

imac_rules5 <- subset(test_rules5, items %in% "iMac")
inspect(imac_rules5)


#look at summary of different rulesets
summary(test_rules2)
summary(test_rules3)
summary(test_rules4)
summary(test_rules5)


#we will use test_rules5 as our final pick of rules d/t manageable rule count,
#good supp/conf values, decent lift, and good count

#plot rules
plot(test_rules5)

plot(test_rules5, method = "graph", control = list(type = "items"))


