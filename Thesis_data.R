###############################
###############################
#####simulated data set 1######
###############################

set.seed(098)
eps_c<-rnorm(500)
v1<-runif(500,min=0,max=10)
v2<-runif(500,min=0,max=10)
v3<-runif(500,min=0,max=10)
v4<-runif(500,min=0,max=10)
data_c<-data.frame(v1,v2,v3,v4)
names(data_c)<-c("x1","x2","x3","x4")

ml<-function(x1,x2,x3,x4) {10*sin((3<x1&x1<=10)*1)+20*(1*(3<x2&x2<=10)-0.5)^2+
    3*(1*(0<x3&x3<=2)-1*(0<x4&x4<=3))}
y<-ml(data_c[,1],data_c[,2],data_c[,3],data_c[,4])
data_c$y<-y+eps_c

####create a learner(under designed ml) that can be used by Predictor$new()
lerner<-function(){
  value<-list(ml=ml)
  attr(value,"class")<-"ml"
  value
}
lrn<-lerner()
predict.ml<-function(x,newdata,type,...){
  x$ml(newdata$x1,newdata$x2,newdata$x3,newdata$x4)
}
model_type.ml=function(x){
  "regression"
}

####turn ml to our needed predictor
predictor_exm=Predictor$new(lrn,data=data_c[1:4])

##################################
##################################
#####simulated data set 2#########
##################################

set.seed(987)
eps_c<-rnorm(500)
v1<-runif(500,min=0,max=10)
v2<-runif(500,min=0,max=10)
v3<-runif(500,min=0,max=10)
v4<-runif(500,min=0,max=10)
data_c<-data.frame(v1,v2,v3,v4)
names(data_c)<-c("x1","x2","x3","x4")

ml<-function(x1,x2,x3,x4) {10*sin((1.5<x1&x1<=10)*1)+20*(1*(1.5<x2&x2<=10)-0.5)^2+
    3*(1*(0<x3&x3<=1)-1*(0<x4&x4<=1.5))}
y<-ml(data_c[,1],data_c[,2],data_c[,3],data_c[,4])
data_c$y<-y+eps_c

lerner<-function(){
  value<-list(ml=ml)
  attr(value,"class")<-"ml"
  value
}
lrn<-lerner()
predict.ml<-function(x,newdata,type,...){
  x$ml(newdata$x1,newdata$x2,newdata$x3,newdata$x4)
}
model_type.ml=function(x){
  "regression"
}
predictor_exm=Predictor$new(lrn,data=data_c[1:4])




####chosen observations as point of interest are:
####For data set 1####
####(0,0,1.5,2)
####(2,1,1.5,2)
####(0,1,2.5,2)
####(1,2,2.5,2)
####(3,3,2,2)
####(3,1,2,2)
####(0.5,3,3,1)
####(2,3,2,3)
####(1.2,0.5,1.5,3)
####(0.5,1,0.2,0.1)
####(3,0,2,3)
####(2,1.5,3,0)
####(2.8,0.5,2,1)
####(1,2.5,2.5,0)
####(2.5,1.5,0.5,1)
####(0.5,0.5,0.5,3)
####(0.2,2.8,0,0.2)
####(0.1,0.3,2.8,0.2)
####(2.5,0.1,1,1)
####(2,0.2,2.8,0.1)
####For data set 2####
####(0.2,0.8,1,1)
####(1.3,0.3,0.2,0)
####(0.1,1.3,1,1)
####(0.1,0.3,0.4,1.2)
####(0.4,1.2,0.2,0.2)
####(0.6,0.5,1,0.1)
####(0.2,0.3,0.2,0.1)
####(0.1,0.6,1,0.8)
####(0.3,1.3,0.2,0.1)
####(0.5,1.3,0.6,0.5)
####(0.5,0.5,0.8,0.4)
####(0.2,1,0.8,1)
####(0.1,0.2,0.3,0.9)
####(0.4,0.6,1,0.2)
####(0.2,1,0.9,1.4)
####(0.7,0.1,1,0.1)
####(0.4,1.3,0.6,0.1)
####(0.3,1.4,0.5,0.5)
####(0.3,0.1,0.9,0.1)
####(1.5,0.4,0.3,0)


############################
############################
###Real data set ###########
############################

####get the data from OpenML
task=getOMLTask(task.id = 23L)
exa_com<-task$input$data.set$data
task_com<-makeClassifTask(target="Contraceptive_method_used", data=exa_com)

####set the learner and tune the optimal "ntree"
learner_com = makeLearner("classif.randomForest",predict.type = "prob")
tune_ps_com=makeParamSet(makeNumericParam("ntree",lower=1,upper=1000))
ctrl<-makeTuneControlRandom(maxit = 300)
rdesc=makeResampleDesc("CV",iters=5L)
ml_model=tuneParams(learner_com,task = task_com,resampling = rdesc,par.set = tune_ps_com,control = ctrl)
lrn_opt<-makeLearner("classif.randomForest",predict.type = "prob",ntree=655)
####trian random forest with optimal "ntree"
set.seed(987)
ml_opt<-train(lrn_opt,task_com)

exa_com_x<-exa_com[,1:9]
####the 10th observation as point of interest
x_interest_com<-exa_com_x[10,]
####define Predictor and its class/task based on the highest predictive probability 
####for point of interest
predictor_ml<-Predictor$new(ml_opt,data=exa_com_x)
predictor_ml$class<-"1"
predictor_ml$task<-"classification"



