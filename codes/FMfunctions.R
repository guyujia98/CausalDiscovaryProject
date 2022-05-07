#-----functions for formal modeling-----
library(bnlearn)
library(dplyr)
library(labelled)
library(Rgraphviz)
library(reshape2)
library(ggplot2)
library(tidyr)

#-----sample update-----
sample_update <- function(data,habit,disease){
  sample_1 <- list()
  sample_1$data <- data %>% select(Gender,A4_F_1,all_of(c(habit,disease))) %>% 
    rename(AgeFactor=A4_F_1)
  sample_1$habitdetail <- labelled::var_label(sample_1$data %>% select(all_of(habit)))%>%unlist()%>%unname() 
  sample_1$diseasedetail <- labelled::var_label(sample_1$data %>% select(all_of(disease)))%>%unlist()%>%unname()
  
  return(sample_1)
}

#-----match name-----
match_name<-function(sample1){
  hfl_e <- c("notReg","notExercise","Drinking","Smoking","BMI")
  dfl_e <- c("hypertension", "diabetes","heartDisease","Apoplexy","RespiratorySD")
  habit <- c(habitqustion$question_id[1:4],"K2.2.D")
  disease <- c("D1.1","D1.2","D1.3","D1.4","D1.5")
  hin <- colnames(sample1$data%>%select(-c(Gender,AgeFactor),-starts_with("D")))
  din <- colnames(sample1$data%>%select(starts_with("D")))
  
  oh <- match(hin,habit)
  od <- match(din,disease)
  
  res <- c(hfl_e[oh],dfl_e[od])
  
  return(res)
}

#-----bayesian network functions-----
plot_bn <- function(sample1 ,method, agefactor=NULL, gender, alpha=0.05){
  df <- sample1$data
  
  if(is.null(agefactor)){
    graph_test <- df%>%filter(Gender==gender) %>%
      select(-c(Gender,AgeFactor))
  } else{
    graph_test <- df%>%filter(Gender==gender, AgeFactor==agefactor) %>%
      select(-c(Gender,AgeFactor))
  }
  
  t <- data.frame(apply(as.matrix(graph_test),2,factor))
  
  res <- switch(method,
         hybrid = h2pc(t),
         pc = pc.stable(t,alpha = alpha),
         qnml = hc(t,score = "qnml"))
  nAttrs <- list()
  eAttrs <- list()
  attrs <- list()
  nm <- nodes(res)
  d <- length(sample1$diseasedetail)
  h <- length(sample1$habitdetail)
  
  z <- match_name(sample1)
  names(z) <- nodes(res)
  nAttrs$label <- z
  fcolor <- c(rep("lightblue",h),rep("red",d))
  names(fcolor) <- nodes(res)
  cl <- fcolor
  attrs$node <- list(shape="ellipse", fixedsize=FALSE)
  
  nAttrs$color <- cl
  nAttrs$fillcolor <- fcolor
  
  dag <- as.graphNEL(res)
  
  plot(dag,nodeAttrs = nAttrs, attrs=attrs)
}


#-----cv comparison-----
cv_comparison <- function(sample1,agefactor=NULL,gender){
  df <- sample1$data
  if(is.null(agefactor)){
    graph_test <- df%>%filter(Gender==gender) %>%
      select(-c(Gender,AgeFactor))
  } else{
    graph_test <- df%>%filter(Gender==gender, AgeFactor==agefactor) %>%
      select(-c(Gender,AgeFactor))
  }
  
  t <- data.frame(apply(as.matrix(graph_test),2,factor))
  
  
  cv.hybrid <- bn.cv(t,bn="h2pc",runs=10)
  cv.qnml <- bn.cv(t,bn="hc",runs=10,algorithm.args = list(score="qnml"))
  cv.pc <- bn.cv(t,bn="pc.stable",runs=10,algorithm.args = list(alpha=0.05))
  cv.pc2 <- bn.cv(t,bn="pc.stable",runs=10,algorithm.args = list(alpha=0.001))
  
  plot(cv.hybrid, cv.qnml, cv.pc,cv.pc2,xlab = c("hybrid", "qnml","pc0.05","pc0.001"))
  
}