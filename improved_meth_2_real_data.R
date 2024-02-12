improved_method2<-function(predictor,sigma,t,x_interest_x,data_x,desired_range){
  sizes<-numeric()
  surros<-list()
  boxes<-list()
  weights_original<-kernel(sigma,x_interest_x,data_x)
  predi<-predictor$predict(data_x)
  new_df<-cbind(data_x,predi)
  names(new_df)[length(names(new_df))]<-"y"
  set.seed(789)
  ##find first surrogate model and its corresponding box using original weights
  fitted_model_cv.1<-glmnetUtils::cv.glmnet(y~.,data=new_df,weights = weights_original)
  surro<-glmnetUtils::glmnet(y~.,data=new_df,
                             weights = weights_original,lambda=fitted_model_cv.1$lambda.min)
  surros[[1]]<-surro
  surro_1<-Predictor$new(surro,data=new_df,y="y")
  prim1=Prim$new(predictor = surro_1,predictor_2 = predictor)
  primb1=prim1$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range))
  post<-PostProcessing$new(predictor=surro_1,predictor_2 = predictor)
  post.1<-post$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range),box_init = primb1$box)
  boxes[[1]]<-post.1
  
  b.size<-0
  for(j in names(data_x)){
    if(class(data_x[,j])=="numeric"){
      x<-(post.1$box$upper[[j]]-post.1$box$lower[[j]])/(
        post.1$full_range[id==j]$upper-post.1$full_range[id==j]$lower)
      b.size=b.size+x
    }
    else{
      x<-length(post.1$box$levels[[j]])/(post.1$full_range[id==j]$nlevels)
      b.size=b.size+x
    }
  }
  k<-1
  sizes[k]<-b.size
  
  rownames(data_x)<-NULL
  box<-data_x
  for(i in colnames(box)) {
    if(class(box[,i])=="numeric"){
      box<-box[between(box[,i],post.1$box$lower[[i]],post.1$box$upper[[i]]),]}
    else{
      box<-box[box[,i]%in%post.1$box$levels[[i]],]
    }
  }
  ##find weights for obs within first box
  inbox_weights<-weights_original[as.numeric(rownames(box))]
  ##do the loop, stop if the there's no ob whose weight is < t
  while (!min(inbox_weights)>=t|length(inbox_weights)==1){
    k<-k+1
    ##iteratively increase sigma and re-calculate weights using new sigma
    sigma<-sigma+0.01
    weights_original<-kernel(sigma,x_interest_x ,data_x)
    ##get new surrogate model and its box
    glm<-glmnetUtils::cv.glmnet(y~.,data = new_df,weights = weights_original)
    surro<-glmnetUtils::glmnet(y~.,data=new_df,weights = weights_original,
                               lambda=glm$lambda.min)
    surros[[k]]<-surro
    surro.p<-Predictor$new(surro,data=new_df,y="y")
    prim=Prim$new(predictor =surro.p,predictor_2 = predictor)
    primb=prim$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range))
    post.m<-PostProcessing$new(predictor=surro.p,predictor_2 = predictor)
    post.1<-post.m$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range),box_init = primb$box)
    boxes[[k]]<-post.1
    
    b.size<-0
    for(j in names(data_x)){
      if(class(data_x[,j])=="numeric"){
        x<-(post.1$box$upper[[j]]-post.1$box$lower[[j]])/(
          post.1$full_range[id==j]$upper-post.1$full_range[id==j]$lower)
        b.size=b.size+x
      }
      else{
        x<-length(post.1$box$levels[[j]])/(post.1$full_range[id==j]$nlevels)
        b.size=b.size+x
      }
    }
  
    sizes[k]<-b.size
    rownames(data_x)<-NULL
    box<-data_x
    for(j in colnames(box)) {
      if(class(box[,i])=="numeric"){
        box<-box[between(box[,i],post.1$box$lower[[i]],post.1$box$upper[[i]]),]}
      else{
        box<-box[box[,i]%in%post.1$box$levels[[i]],]
      }
    }
    ##get reweighted-weights for obs within new box 
    inbox_weights<-weights_original[as.numeric(rownames(box))]
  }
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
  box_samples<-vector("list",length = length(sizes))
  for (i in 1:length(sizes)){
    range<-sample_range(data_x,boxes[[i]]$box)
    box_samples[[i]]<-SamplerUnif$new(range)$sample(n = 100)$data
  }
  predicts<-vector("list",length = length(sizes))
  ml<-vector("list",length = length(sizes))
  MSE_within<-vector("numeric",length=length(sizes))
  
  for (i in 1:length(sizes)){
    for (n in names(data_x)){
      if (is.numeric(box_samples[[i]][[n]])){
        box_samples[[i]][[n]]<-box_samples[[i]][[n]]
      }
      else if (!is.numeric(box_samples[[i]][[n]])){
        box_samples[[i]][[n]]<-factor(box_samples[[i]][[n]],levels = levels(exa_com_x[[n]]))
      }
    } 
  }
  
  for (i in 1:length(sizes)) {
    predicts[[i]]<-predict(surros[[i]],box_samples[[i]])%>%unname()
    ml[[i]]<-predictor$predict(box_samples[[i]])
    MSE_within[i]<-sum((predicts[[i]]-ml[[i]])^2)/nrow(predicts[[i]]-ml[[i]])
  }
  criterion<-sizes-MSE_within
  return(list(best.surro=surros[[which.max(criterion)]],best.box=boxes[[which.max(criterion)]]))
}



imp<-improved_method2(predictor_ml,0.1,0.2,x_interest_com,exa_com_x,0.4)
####box volume coverage
size<-1
for(j in names(exa_com_x)){
  if(class(exa_com_x[,j])=="numeric"){
    x<-(imp$best.box$box$upper[[j]]-imp$best.box$box$lower[[j]])/(
      imp$best.box$full_range[id==j]$upper-imp$best.box$full_range[id==j]$lower)
  }
  else{
    x<-length(imp$best.box$box$levels[[j]])/(imp$best.box$full_range[id==j]$nlevels)
  }
  size<-size*x
}
size

####fidelity of box
box_samples<-vector("list",length = 1)

  range<-sample_range(exa_com_x,imp$best.box$box)
  box_samples<-SamplerUnif$new(range)$sample(n = 100)$data

predicts<-vector("list",length =1)
ml<-vector("list",length = 1)
MSE_within<-vector("numeric",length=1)

  predicts<-predict(imp$best.surro,box_samples)%>%unname()
  ml<-predictor_ml$predict(box_samples)
  MSE_within<-sum((predicts-ml)^2)/100

MSE_within
