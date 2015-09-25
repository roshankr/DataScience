library(arules)
library(arulesViz)
library(datasets)
library(ggplot2)

################################################################
#Market Basket Analysis                                        #
#Used in ,                                                     #
#Product recommendation - like Amazon's "customers who bought  # 
#that, also bought this"                                       #
#Music recommendations - like Last FM's artist recommendations #
#Medical diagnosis - like with diabetes really cool stuff      #
#Content optimisation - like in magazine websites or blogs     #
#                                                              #
#Author - Roshan K R                                           #
################################################################

# Read from Dataset
X <- data(Groceries)

# Create an item frequency plot for the top 20 items
#itemFrequencyPlot(Groceries,topN=20,type="absolute")

# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.7))

# Show the top 5 rules, but only 2 digits
options(digits=2)

rules<-sort(rules, by="confidence", decreasing=TRUE)

#inspect(rules[1:5])

#Rule 4 is perhaps excessively long. Lets say you wanted more 
#concise rules. That is also easy to do by adding a "maxlen" 
#parameter to your apriori function:
#rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))


#eliminate redundant rules
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

inspect(rules[1:5])
