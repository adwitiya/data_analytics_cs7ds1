# Building The Tree Model
library(rpart, quietly=TRUE)
set.seed(3112)
# Build the Decision Tree model.
crs$rpart <- rpart(Attrition ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0))
# Generate a textual view of the Decision Tree model.
print(crs$rpart)
printcp(crs$rpart)
#Draw the Decision Tree
fancyRpartPlot(crs$rpart, main="Decision Tree Employeeattrition.xlsx $ Attrition", sub = "Adwitiya Chakraborty")

#Evaluation of the Moddel
library(ROCR)
# ROC Curve: requires the ggplot2 package.
library(ggplot2, quietly=TRUE)
# Generate an ROC Curve for the rpart model on Employeeattrition.xlsx [test].
crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]
# Remove observations with missing target.
no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Decision Tree Employeeattrition.xlsx [test] Attrition")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.
# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")






# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for rpart model on Employeeattrition.xlsx [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Lift Chart for the rpart model on Employeeattrition.xlsx [train].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])[,2]

#In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$Attrition),"sens", "spec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="rpart-single tree model", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  Employeeattrition.xlsx ",
      sub=paste("Adwitiya Chakraborty", format(Sys.time(), "%Y-%b-%d %H:%M:%S")))
grid()


