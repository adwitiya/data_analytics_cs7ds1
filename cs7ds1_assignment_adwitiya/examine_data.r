suppressMessages(library(ggplot2))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(plyr))
suppressMessages(library(rpart))
suppressMessages(library(rpart.plot))
suppressMessages(library(randomForest))
suppressMessages(library(caret))
suppressMessages(library(gbm))
suppressMessages(library(survival))
suppressMessages(library(pROC))
suppressMessages(library(DMwR))
suppressMessages(library(scales))

g1 <- ggplot(Employee, aes(x = MonthlyIncome, fill = Attrition)) + geom_density(alpha = 0.7) + scale_fill_manual(values = c("#1000ff","#e5ff00"))
g2 <- ggplot(Employee, 
             aes(x = HourlyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#1000ff","#e5ff00"))
g3 <- ggplot(Employee, 
             aes(x = DailyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#1000ff","#e5ff00"))
g4 <- ggplot(Employee, 
             aes(x = MonthlyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("#1000ff","#e5ff00"))
grid.arrange(g1,g2,g3,g4, ncol = 2, nrow = 2)
#Years at Company
ggplot(Employee, 
       aes(y = YearsSinceLastPromotion, x = YearsAtCompany, colour = OverTime)) + 
  geom_jitter(size = 1, alpha = 0.9) + 
  geom_smooth(method = "gam") + 
  facet_wrap(~ Attrition) + 
  ggtitle("Attrition") + 
  scale_colour_manual(values = c("#1000ff","#e28400")) + 
  theme(plot.title = element_text(hjust = 0.5))

#Attrition on Overtime
ggplot(Employee, 
       aes(x = OverTime, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.8) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "OverTime") +
  facet_grid(~Attrition) +
  scale_fill_manual(values = c("#1000ff","#e28400")) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

#Work Life balance
ggplot(Employee, 
       aes(x= WorkLifeBalance, y=DistanceFromHome, group = WorkLifeBalance, fill = WorkLifeBalance)) + 
  geom_boxplot(alpha=0.7) + 
  theme(legend.position="none") + 
  facet_wrap(~ Attrition) + 
  ggtitle("Attrition") + 
  theme(plot.title = element_text(hjust = 0.5))

#Business Travel
ggplot(Employee, 
       aes(x= BusinessTravel,  group=Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill="Business Travel") +
  facet_grid(~Attrition) +
  scale_y_continuous(labels=percent) + 
  scale_fill_manual(values = c("#1000ff","#e28400", "#c40505")) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")
