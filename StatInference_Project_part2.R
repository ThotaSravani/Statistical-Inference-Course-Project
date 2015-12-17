
## Load necesary packages
library(datasets)
library(ggplot2)

## Plot
ggplot(data=ToothGrowth, aes(x=as.factor(dose), y=len, fill=supp)) +
  geom_bar(stat="identity",) +
  facet_grid(. ~ supp) +
  xlab("Dose in Milligrams") +
  ylab("Tooth Length") +
  guides(fill=guide_legend(title="Supplement Type"))

## ------------------------------------------------------------------------
fit <- lm(len ~ dose + supp, data=ToothGrowth)
summary(fit)


## ------------------------------------------------------------------------
confint(fit)

