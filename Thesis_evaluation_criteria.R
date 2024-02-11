#####################################
#####################################
###Linear volume coverage(Data 1)####
#####################################
size<-1
for(j in 1:length(data_c[,1:4]) ){
  
  if(primb$box$upper[[j]]<=3){
    x<-(primb$box$upper[[j]]-primb$box$lower[[j]])/3
  }
  else if(primb$box$upper[[j]]>3&primb$box$lower[[j]]<3){
    x<-(3-primb$box$lower[[j]])/3
  }
  else if(primb$box$lower[[j]]>=3){
    x<-0
  }
  
  size<-size*x
}
size

#################################################
#################################################
###Box volume coverage(for simulated data set)###
#################################################
for(j in 1:4 ){
  if(class(data_c[,j])=="numeric"){
    x<-(primb$box$upper[[j]]-primb$box$lower[[j]])/(
      primb$full_range$upper[[j]]-primb$full_range$lower[[j]])
  }
  else{
    x<-length(primb.o$box$levels[[j]])/primb.o$full_range$nlevels[[j]]
  }
  size=size*x
}

size

###########################################
###########################################
###Fidelity of box(data 1&method2)#########
###########################################

sample_range = function(data,resulted_box) {
  param_list = lapply(names(data), function(col_name){
    
    if (is.double(data[[col_name]])){
      lb = resulted_box$lower[[col_name]]
      ub = resulted_box$upper[[col_name]]
      param = ParamDbl$new(col_name, lower = lb, upper = ub)
    } 
    if(is.factor(data[[col_name]])){
      lev=levels(data[[col_name]])
      param = ParamFct$new(col_name, levels = lev)
    }
    else if (is.integer(data[[col_name]])) {
      lb = resulted_box$lower[[col_name]]
      ub = resulted_box$upper[[col_name]]
      param = ParamInt$new(col_name, lower = lb, upper = ub)
    }
    
    param})
  ps = ParamSet$new(param_list)
  return(ps)
}
box_samples<-vector("list",length = 1)

  range<-sample_range(data_c[,1:4],method2$box$box)
  box_samples<-SamplerUnif$new(range)$sample(n = 100)$data

predicts<-vector("list",length = 1)
ml<-vector("list",length = 1)
MSE_within<-vector("numeric",length=1)

  for (n in names(data_c[,1:4])){
    if (is.numeric(box_samples[[n]])){
      box_samples[[n]]<-box_samples[[n]]
    }
    else if (!is.numeric(box_samples[[n]])){
      box_samples[[n]]<-factor(box_samples[[n]],levels = levels(exa_com_x[[n]]))
    }
  } 


  predicts<-predict(method2$model,box_samples)%>%unname()
  ml<-predictor_exm$predict(box_samples)
  MSE_within<-sum((predicts-ml)^2)/100

######################################
######################################
####global fidelity(data 1&method2)###
######################################

predicts<-predict(method2$model,data_c[,1:4])%>%unname()%>%unlist()
ml<-predictor_exm$predict(data_c[,1:4])[[1]]
we<-1-gower_dist(data.frame(x1=0.5,x2=0.5,x3=0.8,x4=0.4),data_c[,1:4])
MSE_weighted_whole<-sum(we*((predicts-ml)^2))/nrow(data_c)
