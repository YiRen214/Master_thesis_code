#############################
#############################
######Method I###############--->categorical features in dataset must be changed into factor at first step; 
#############################--->predictor$task shall either be "regression" or "classification"; 

method1<-function(predictor,x_interest_x,data_x,desired_range,alpha=0.5,iterations)
  ##predictor:A predictor object that contains trained ml model to be explained
  ##and data to be used for analyzing the model.(obtains using Predictor$new from iml package) 
  ##Its class(for classification task) should be same as of x_interest.
  ##x_interest_x: observation that's of interest,without target variable values.
  ##data_x: dataset without target column.
  ##desired_range: desired largest discrepancy between prediction of ml and surrogate model in found hyperbox.
  ##alpha: the rate for observations out of box to decline.
  ##iterations:number of iterations for reweighting
{
  ####calculate original weights for all instances
  weights_original<-1-gower_dist(x_interest_x,data_x)
  
  predi<-predictor$predict(data_x)
  new_df<-cbind(data_x,predi)
  names(new_df)[length(names(new_df))]<-"y"
  ####find first surrogate model using original weights
  glm.1<-glmnetUtils::cv.glmnet(y~.,data = new_df,weights = weights_original)
  glm_1<-glmnetUtils::glmnet(y~.,data=new_df,weights = weights_original,lambda=glm.1$lambda.min)
  ####find its corresponding box of desired range
  surro_1<-Predictor$new(glm_1,data=new_df,y="y")
  prim.1=Prim$new(predictor =surro_1,predictor_2 = predictor)
  primb.1=prim.1$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range))
  post<-PostProcessing$new(predictor=surro_1,predictor_2 = predictor)
  post.1<-post$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range),box_init = primb.1$box)
  #############################################calculate new weights for all onservations besed on resulted box
  rownames(data_x)<-NULL
  box<-data_x
  ####find obs within the first box
  for(i in colnames(box)) {
    if(class(box[,i])=="numeric"){
      box<-box[between(box[,i],post.1$box$lower[[i]],post.1$box$upper[[i]]),]}
    else{
      box<-box[box[,i]%in%post.1$box$levels[[i]],]
    }
  }
  ####find obs-ids outside of box
  out_index<-setdiff(rownames(data_x),rownames(box))
  
  weights<-data.frame(matrix(NA,nrow=nrow(data_x),ncol=3))
  colnames(weights)<-c("original","outside","inside")
  weights$original<-1-(gower_dist(x_interest_x,data_x))
  
  alpha<-0.5
  weights_o<-character(length = nrow(weights))
  ####decline weights of obs outside of the box with rate of alpha 
  for (i in as.numeric(out_index)){
    weights_o[i]<-weights[i,"original"]*alpha   
  }
  ####calculate the sum of original weights for obs within box
  box_origin_sum<-0
  for (i in as.numeric(rownames(box))){
    j<-weights[i,"original"]
    box_origin_sum<-box_origin_sum+j
  }
  ####calculate beta, by which obs inside box will be multiplied
  beta<-(sum(weights$original)-sum(na.omit(as.numeric(weights_o))))/box_origin_sum
  ####multiply weights of obs inside box by beta
  weights_i<-character(length = nrow(weights))
  for (i in as.numeric(rownames(box))){
    weights_i[i]<-weights[i,"original"]*beta 
  }
  ####renew weights matrix
  weights$outside<-as.numeric(weights_o)
  weights$inside<-as.numeric(weights_i)
  ####have an additional vector for re-weighted weights of all 
  weights_rec<-weights$outside
  for (i in which(is.na(weights$outside))){
    weights_rec[i]<-weights$inside[i]
  }
  ###############################################Begin of the loop
  
  model<-vector("list",length=iterations)
  primb<-vector("list",length=iterations)
  weights_ml<-vector("list",length = iterations)
  ####inherit from first result
  model[[1]]<-glm_1
  primb[[1]]<-primb.1
  weights_ml[[1]]<-weights_original
  weights_ml[[2]]<-weights_rec
  weights<-weights
  ####iteratively find the next surrogate model and its box using re-weighted weights
  for (i in 2:iterations) {
    model.cv<-glmnetUtils::cv.glmnet(y~.,data=new_df,weights = weights_ml[[i]])
    model[[i]]<-glmnetUtils::glmnet(y~.,data=new_df,weights = weights_ml[[i]],
                                    lambda=model.cv$lambda.min)
    ######################################
    surro=Predictor$new(model[[i]],data=new_df,y="y")
    prim=Prim$new(predictor = surro,predictor_2 = predictor)
    primbox<-prim$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range))
    postb<-PostProcessing$new(predictor=surro,predictor_2 = predictor)
    primb[[i]]<-postb$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range),box_init = primbox$box)
    ############################the re-weighting process is similar to the process described above
    rownames(data_x)<-NULL
    box<-data_x
    for(j in colnames(box)) {
      if(class(box[,j])=="numeric"){
        box<-box[between(box[,j],primb[[i]]$box$lower[[j]],primb[[i]]$box$upper[[j]]),]}
      else{
        box<-box[box[,j]%in%primb[[i]]$box$levels[[j]],]
      }
    }
    out_index<-setdiff(rownames(data_x),rownames(box))
    ####calculate the sum of weights for obs within new box
    weight_in_sum<-0
    for (m in as.numeric(rownames(box))){
      if (is.na(weights[m,"inside"])==FALSE){
        j<-weights[m,"inside"]
      }
      else{
        j<-weights[m,"outside"]
      }
      weight_in_sum=weight_in_sum+j
    }
    ####reweight the obs outside box using alpha
    weights_o<-character(length = nrow(weights))
    for (n in as.numeric(out_index)){
      if (is.na(weights[n,"outside"])==FALSE){
        weights_o[n]<-weights[n,"outside"]*alpha   
      }
      else {
        weights_o[n]<-weights[n,"inside"]*alpha 
      }
    }
    beta<-(sum(weights$original)-sum(na.omit(as.numeric(weights_o))))/weight_in_sum
    weights_in<-character(length = nrow(weights))
    for (k in as.numeric(rownames(box))){
      if (is.na(weights[k,"inside"])==FALSE){
        weights_in[k]<-weights[k,"inside"]*beta    
      }
      else {
        weights_in[k]<-weights[k,"outside"]*beta
      }
    }
    weights$outside<-as.numeric(weights_o)
    weights$inside<-as.numeric(weights_in)
    ###assign the changed weights to next list-element that is used to find new surrogate model
    weights_ml[[i+1]]<-weights$outside
    for (p in which(is.na(weights$outside))){
      weights_ml[[i+1]][p]<-weights$inside[p]
    }
  }
  return(list(models=model,boxes=primb,reweights=weights_ml))
}



#############################
#############################
######Method II###############--->categorical features in dataset must be changed into factor at first step; 
#############################--->predictor$task shall either be "regression" or "classification"; 

####Definition of a kernel
kernel<-function(sigma,x_interest,data){
  exp(-(gower_dist(x_interest,data))^2/(sigma^2))
}

method2<-function(predictor,sigma,t,x_interest_x,data_x,desired_range){
  ####sigma: strating sigma value
  ####other parameters are same as in method1
  weights_original<-kernel(sigma,x_interest_x,data_x)
  predi<-predictor$predict(data_x)
  new_df<-cbind(data_x,predi)
  names(new_df)[length(names(new_df))]<-"y"
  set.seed(789)
  ####find first surrogate model and its corresponding box using original weights
  fitted_model_cv.1<-glmnetUtils::cv.glmnet(y~.,data=new_df,weights = weights_original)
  surro<-glmnetUtils::glmnet(y~.,data=new_df,
                             weights = weights_original,lambda=fitted_model_cv.1$lambda.min)
  
  surro_1<-Predictor$new(surro,data=new_df,y="y")
  prim1=Prim$new(predictor = surro_1,predictor_2 = predictor)
  primb1=prim1$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range))
  post<-PostProcessing$new(predictor=surro_1,predictor_2 = predictor)
  post.1<-post$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range),box_init = primb1$box)
  
  rownames(data_x)<-NULL
  box<-data_x
  for(i in colnames(box)) {
    if(class(box[,i])=="numeric"){
      box<-box[between(box[,i],post.1$box$lower[[i]],post.1$box$upper[[i]]),]}
    else{
      box<-box[box[,i]%in%post.1$box$levels[[i]],]
    }
  }
  ####find weights for obs within first box
  inbox_weights<-weights_original[as.numeric(rownames(box))]
  ####do the loop, stop if the there's no ob whose weight is < t
  while (!min(inbox_weights)>=t|length(inbox_weights)==1){
    ####iteratively increase sigma and re-calculate weights using new sigma
    sigma<-sigma+0.01
    weights_original<-kernel(sigma,x_interest_x ,data_x)
    ####get new surrogate model and its box
    glm<-glmnetUtils::cv.glmnet(y~.,data = new_df,weights = weights_original)
    surro<-glmnetUtils::glmnet(y~.,data=new_df,weights = weights_original,
                               lambda=glm$lambda.min)
    surro.p<-Predictor$new(surro,data=new_df,y="y")
    prim=Prim$new(predictor =surro.p,predictor_2 = predictor)
    primb=prim$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range))
    post.m<-PostProcessing$new(predictor=surro.p,predictor_2 = predictor)
    post.1<-post.m$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range),box_init = primb$box)
    rownames(data_x)<-NULL
    box<-data_x
    for(j in colnames(box)) {
      if(class(box[,i])=="numeric"){
        box<-box[between(box[,i],post.1$box$lower[[i]],post.1$box$upper[[i]]),]}
      else{
        box<-box[box[,i]%in%post.1$box$levels[[i]],]
      }
    }
    ####get recalculated-weights for obs within new box 
    inbox_weights<-weights_original[as.numeric(rownames(box))]
  }
  return(list(sigma=sigma,model=surro,box=post.1))
}


#############################
#############################
######Method III###############--->categorical features in dataset must be changed into factor at first step; 
#############################--->predictor$task shall either be "regression" or "classification"; 

method3<-function(predictor_ml,x_interest_x,data_x,desired_range,n_parts,t){
  ####n_parts: Number of parts that you want to cut the data set into
  ####t:threshold for change of box size
  ####other parameters stay the same
  
  data_x$weights_original<-1-(gower_dist(x_interest_x,data_x))
  ####rearrange the data set according to distance to point of interest
  data_x_rearranged<-data_x%>%arrange(desc(weights_original))
  predi<-predictor_ml$predict(data_x_rearranged[,1:(ncol(data_x_rearranged)-1)])
  new_df<-cbind(data_x_rearranged[,1:(ncol(data_x_rearranged)-1)],predi)
  names(new_df)[length(names(new_df))]<-"y"
  ##fit first surrogate model using whole data set with original weights
  glm.1<-glmnetUtils::cv.glmnet(y~.,data = new_df,weights = data_x_rearranged$weights_original)
  glm_1<-glmnetUtils::glmnet(y~.,data=new_df,weights = data_x_rearranged$weights_original,
                             lambda=glm.1$lambda.min)
  ##find its corresponding box
  surro_1<-Predictor$new(glm_1,data=new_df,y="y")
  prim.1=Prim$new(predictor =surro_1,predictor_2 = predictor_ml)
  primb.1=prim.1$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range))
  post<-PostProcessing$new(predictor=surro_1,predictor_2 = predictor_ml)
  post.1<-post$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range),box_init = primb.1$box)
  
  size.1<-0
  ##calculate box size for first box
  for(j in names(data_x_rearranged[,1:(ncol(data_x_rearranged)-1)])){
    if(class(data_x_rearranged[,j])=="numeric"){
      x<-(post.1$box$upper[[j]]-post.1$box$lower[[j]])/(
        post.1$full_range[id==j]$upper-post.1$full_range[id==j]$lower)
      size.1=size.1+x
    }
    else{
      x<-length(post.1$box$levels[[j]])/(post.1$full_range[id==j]$nlevels)
      size.1=size.1+x
    }
  }
  
  ##separate data set into n_part parts
  dis<-nrow(data_x_rearranged)/n_parts
  size.2<-0
  ##change weights of second part to 0
  weight_enhenced<-data_x_rearranged$weights_original
  weight_enhenced[(dis*(2-1)+1):(dis*2)]<-0
  ##fit second surrogate model with new weights and corresponding hyperbox
  glm.2<-glmnetUtils::cv.glmnet(y~.,data = new_df,weights = weight_enhenced)
  glm_2<-glmnetUtils::glmnet(y~.,data=new_df,weights = weight_enhenced,
                             lambda=glm.1$lambda.min)
  
  surro_2<-Predictor$new(glm_2,data=new_df,y="y")
  prim.2=Prim$new(predictor =surro_2,predictor_2 = predictor_ml)
  primb.2=prim.2$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range))
  post_2<-PostProcessing$new(predictor=surro_2,predictor_2 = predictor_ml)
  post.2<-post_2$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range),box_init = primb.2$box)
  ######calculate second box size
  for(j in names(data_x_rearranged[,1:(ncol(data_x_rearranged)-1)])){
    if(class(data_x_rearranged[,j])=="numeric"){
      x<-(post.2$box$upper[[j]]-post.2$box$lower[[j]])/(
        post.2$full_range[id==j]$upper-post.2$full_range[id==j]$lower)
      size.2=size.2+x
    }
    else{
      x<-length(post.2$box$levels[[j]])/(post.2$full_range[id==j]$nlevels)
      size.2=size.2+x
    }
  }
  ##compare change of box size 
  diff=abs(size.2-size.1)
  
  ####stop until change of box size is greater than threshold t
  i<-3
  while(diff<t&i<=n_parts){
    ##re-weight by multiplying with (1+(1/n_parts)*(i-2))
    weight_enhenced<-weight_enhenced*(1+(1/n_parts)*(i-2))
    ##find next surrogate model and its box using new weights
    glm.3<-glmnetUtils::cv.glmnet(y~.,data = new_df,weights = weight_enhenced)
    glm_3<-glmnetUtils::glmnet(y~.,data=new_df,weights = weight_enhenced,
                               lambda=glm.3$lambda.min)
    
    surro_3<-Predictor$new(glm_3,data=new_df,y="y")
    
    prim.3=Prim$new(predictor =surro_3,predictor_2 = predictor_ml)
    primb.3=prim.3$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range))
    post_3<-PostProcessing$new(predictor=surro_3,predictor_2 = predictor_ml)
    post.3<-post_3$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range),box_init = primb.3$box)
    ##set weights for part i(under new weights)=0,find surrogate model and its corresponding box 
    weight_enhenced[(dis*(i-1)+1):(dis*i)]<-0
    
    glm.4<-glmnetUtils::cv.glmnet(y~.,data = new_df,weights = weight_enhenced)
    glm_4<-glmnetUtils::glmnet(y~.,data=new_df,weights = weight_enhenced,
                               lambda=glm.4$lambda.min)
    
    surro_4<-Predictor$new(glm_4,data=new_df,y="y")
    prim.4=Prim$new(predictor =surro_4,predictor_2 = predictor_ml)
    primb.4=prim.4$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range))
    post_4<-PostProcessing$new(predictor=surro_4,predictor_2 = predictor_ml)
    post.4<-post_4$find_box(x_interest = x_interest_x,desired_range = c(0,desired_range),box_init = primb.4$box)
    ##compare box size change
    size.3<-0
    for(j in names(data_x_rearranged[,1:(ncol(data_x_rearranged)-1)])){
      if(class(data_x_rearranged[,j])=="numeric"){
        x<-(post.3$box$upper[[j]]-post.3$box$lower[[j]])/(
          post.3$full_range[id==j]$upper-post.3$full_range[id==j]$lower)
        size.3=size.3+x
      }
      else{
        x<-length(post.3$box$levels[[j]])/(post.3$full_range[id==j]$nlevels)
        size.3=size.3+x
      }
    }
    
    size.4<-0
    for(j in names(data_x_rearranged[,1:(ncol(data_x_rearranged)-1)])){
      if(class(data_x_rearranged[,j])=="numeric"){
        x<-(post.4$box$upper[[j]]-post.4$box$lower[[j]])/(
          post.4$full_range[id==j]$upper-post.4$full_range[id==j]$lower)
        size.4=size.4+x
      }
      else{
        x<-length(post.4$box$levels[[j]])/(post.4$full_range[id==j]$nlevels)
        size.4=size.4+x
      }
    }
    diff<-abs(size.4-size.3)
    ##move to next part
    i<-i+1
  }
  return(list(returned_data=data_x_rearranged[1:(dis*(i-2)),],newdf=new_df))
}


