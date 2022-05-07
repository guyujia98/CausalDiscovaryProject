#-----main-----
source("FMfunctions.R")
set.seed(2022)

data_test <- readRDS("data_test.RDS")
habitqustion <- readRDS("habitq.RDS")
habit <- c(habitqustion$question_id[1:4],"K2.2.D")
#disease <- c("D1.1","D1.2","D1.3","D1.4","D1.5")

sample_1 <- sample_update(data_test, habit=habit,disease ="D1.1")

#comparison between gender
plot_bn(sample_1,"hybrid",agefactor=NULL,gender="男")
plot_bn(sample_1,"hybrid",agefactor=NULL,gender="女")

#comparison across age groups
plot_bn(sample_1,"hybrid",agefactor="25-44",gender="男")
plot_bn(sample_1,"hybrid",agefactor="45-59",gender="男")
plot_bn(sample_1,"hybrid",agefactor="60-89",gender="男")

plot_bn(sample_1,"hybrid",agefactor="25-44",gender="女")
plot_bn(sample_1,"hybrid",agefactor="45-59",gender="女")
plot_bn(sample_1,"hybrid",agefactor="60-89",gender="女")

#method comparison
plot_bn(sample_1,"qnml",agefactor=NULL,gender="男")
plot_bn(sample_1,"pc",agefactor=NULL,gender="男",alpha=0.05)
plot_bn(sample_1,"pc",agefactor=NULL,gender="男",alpha=0.001)

#cv comparison

set.seed(2022)
cv_comparison(sample_1,gender = "男")


#confounders consideration

sample_1 <- sample_update(data_test, habit=habit,disease ="D1.2")
plot_bn(sample_1,"hybrid",agefactor=NULL,gender="男")

sample_1 <- sample_update(data_test, habit=habit,disease =c("D1.1","D1.2"))
plot_bn(sample_1,"hybrid",agefactor=NULL,gender="男")
