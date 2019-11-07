# Does the minimum distance replication of BPP

require(numDeriv)
require(MCMCpack)

implied_cov_BPP <-function(params, ma, taste, T, perm_shk_params, tran_shk_params, perm_ins_params,tran_ins_params, meas_error_params) {
    if (ma==1) {
        teta <- params[1] 
    } else {
        teta <- 0.0
    }
    if (taste) {
        varcsi <- params[1+ma] 
    } else {
        varcsi <- 0.0
    }
    var_perm <- params[(1+ma+taste):(ma+taste+perm_shk_params)] 
    var_tran <- params[(1+ma+taste+perm_shk_params):(ma+taste+perm_shk_params+tran_shk_params)] 
    ins_perm <- params[(1+ma+taste+perm_shk_params+tran_shk_params):(ma+taste+perm_shk_params+tran_shk_params+perm_ins_params)] 
    ins_tran <- params[(1+ma+taste+perm_shk_params+tran_shk_params+perm_ins_params):(ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params)] 
    var_c_error <- params[(1+ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params):(ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params+meas_error_params)] 

    dify  <- matrix(0,nrow=T,ncol=T) #/* Income */
    difcd <- matrix(0,nrow=T,ncol=T) #/* Consumption */
    difc  <- matrix(0,nrow=T,ncol=T) #/* Consumption */
    difcme<- matrix(0,nrow=T,ncol=T) #/* Measurement error of consumption */
    difyc <- matrix(0,nrow=T,ncol=T) #/* Cov Income Consumption */
    dif   <- matrix(0,nrow=2*T,ncol=2*T)

    #/* This is the variance of Income */
    dify[1,1]<-var_perm[1]+var_tran[1]+(1-teta)^2*var_tran[1]+teta^2*var_tran[1]
    dify[2,2]<-var_perm[1]+var_tran[2]+(1-teta)^2*var_tran[1]+teta^2*var_tran[1]
    dify[3,3]<-var_perm[1]+var_tran[3]+(1-teta)^2*var_tran[2]+teta^2*var_tran[1]
    for (j in 4:(T-3)){
        dify[j,j]<-var_perm[j-2]+var_tran[j]+(1-teta)^2*var_tran[j-1]+teta^2*var_tran[j-2] 
    }
    dify[T-2,T-2]<-var_perm[T-4]+var_tran[T-2]+(1-teta)^2*var_tran[T-3]+teta^2*var_tran[T-4]
    dify[T-1,T-1]<-var_perm[T-4]+var_tran[T-2]+(1-teta)^2*var_tran[T-2]+teta^2*var_tran[T-3]
    dify[T,T]    <-var_perm[T-4]+var_tran[T-2]+(1-teta)^2*var_tran[T-2]+teta^2*var_tran[T-2]
    
    dify[1,2]<- -(1-teta)^2*var_tran[1]
    ###############This should go up to  np.array(range(T-2))+2
    ###############Followine line also increased by 1 index
    for (j in 3:(T-1)){
        dify[j-1,j]<- -(1-teta)*var_tran[j-1]+teta*(1-teta)*var_tran[j-2]
    }

    dify[T-1,T-2]<- -(1-teta)^2*var_tran[T-2]
    
    for (j in 3:T){
        dify[j-2,j]<- -teta*var_tran[j-2]
    }
    
    for (i in 2:T){
        for (j in i:T){
            dify[j,i-1]<- dify[i-1,j]
        }
    }
            
    #/* This is the variance of Consumption */
    difcd[1,1]<-ins_perm[1]^2*var_perm[1]+ins_tran[1]^2*var_tran[1]+varcsi
    difcd[2,2]<-ins_perm[1]^2*var_perm[1]+ins_tran[1]^2*var_tran[2]+varcsi
    difcd[3,3]<-ins_perm[1]^2*var_perm[1]+ins_tran[1]^2*var_tran[3]+varcsi
    for (j in 4:(T-3)){
        difcd[j,j]<-ins_perm[1]^2*var_perm[j-2]+ins_tran[1]^2*var_tran[j]+varcsi
    }


    difcd[T-2,T-2]<-ins_perm[1]^2*var_perm[T-4]+ins_tran[1]^2*var_tran[T-2]+varcsi
    difcd[T-1,T-1]<-ins_perm[1]^2*var_perm[T-4]+ins_tran[1]^2*var_tran[T-2]+varcsi
    difcd[T,T]    <-ins_perm[1]^2*var_perm[T-4]+ins_tran[1]^2*var_tran[T-2]+varcsi

    difcme[1,1]<-2*var_c_error[1]
    for (j in 2:(T-1)){
        difcme[j,j]<-var_c_error[j]+var_c_error[j-1]
    }
    difcme[T,T]<-2*var_c_error[T-1]

    for (j in 1:(T-1)){
        difcme[j,j+1]<- -var_c_error[j]
    }

    difc<-difcme+difcd
    
    for (i in 2:T){
        for (j in i:T){
            difc[j,i-1]<-difc[i-1,j]
        }
    }

    #/* This is the Covariance of Income and Consumption */
    
    difyc[1,1]<-ins_perm[1]*var_perm[1]+ins_tran[1]*var_tran[1]
    difyc[2,2]<-ins_perm[1]*var_perm[1]+ins_tran[1]*var_tran[2]
    difyc[3,3]<-ins_perm[1]*var_perm[1]+ins_tran[1]*var_tran[3]

    for (j in 4:(T-3)){
        difyc[j,j]<-ins_perm[1]*var_perm[j-2]+ins_tran[1]*var_tran[j]
    }
    difyc[T-2,T-2]<-ins_perm[1]*var_perm[T-4]+ins_tran[1]*var_tran[T-2]
    difyc[T-3,T-1]<-ins_perm[1]*var_perm[T-4]+ins_tran[1]*var_tran[T-2]
    difyc[T,T]    <-ins_perm[1]*var_perm[T-4]+ins_tran[1]*var_tran[T-2]

    for (j in 2:(T-1)){
        difyc[j-1,j]<- -(1-teta)*ins_tran[1]*var_tran[j-1]
    }

    difyc[T-1,T]<- -(1-teta)*ins_tran[1]*var_tran[T-2]

    for (j in 3:(T-1)){
        difyc[j-2,j]<- -teta*ins_tran[1]*var_tran[j-2]
    }
    #/* Final matrix */
    
    dif[1:T,1:T]            <-difc
    dif[(T+1):(2*T),1:T]        <-difyc
    dif[1:T,(T+1):(2*T)]        <- t(difyc)
    dif[(T+1):(2*T),(T+1):(2*T)]    <-dify
    
#    difa1 <- rbind(dif[1:8,],dif[12:(2*T),])
#    difa2 <- cbind(difa1[,1:8],difa1[,12:(2*T)])
    
    fm<-vech(dif)

    return (fm)
}
BPP_parameter_estimation <- function(c_vector, omega, T, ma=1, taste=1){

    # get number of parameters of each type
    perm_shk_params <- T-4  # time varying permanent shock variance
    tran_shk_params <- T-2
    perm_ins_params <- 1
    tran_ins_params <- 1
    meas_error_params <- T-1   #time-varying measurement error variance

    num_params <- ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params+meas_error_params
    
    init_params <- matrix(0,nrow=num_params,ncol=1)

    if (ma==1){
        init_params[1] <- 0.1   #teta, ma component of income process
    }
    if (taste){
        init_params[1+ma] <- 0.01  #variance of taste shocks
    }
    init_params[(1+ma+taste):(ma+taste+perm_shk_params)] <- matrix(0.003,nrow=perm_shk_params,ncol=1) 
    init_params[(1+ma+taste+perm_shk_params):(ma+taste+perm_shk_params+tran_shk_params)] <- matrix(0.003,nrow=tran_shk_params,ncol=1)
    init_params[(1+ma+taste+perm_shk_params+tran_shk_params):(ma+taste+perm_shk_params+tran_shk_params+perm_ins_params)] <- matrix(1,nrow=perm_ins_params,ncol=1) 
    init_params[(1+ma+taste+perm_shk_params+tran_shk_params+perm_ins_params):(ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params)] <- matrix(0.3,nrow=tran_ins_params,ncol=1) 
    init_params[(1+ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params):(ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params+meas_error_params)] <- matrix(0.06,nrow=meas_error_params,ncol=1) 
    
    objectiveFun <-function(params, ma, taste, T, empirical_cov, weight_matrix){
        model_cov <- implied_cov_BPP(params, ma, taste,T, perm_shk_params, tran_shk_params, perm_ins_params,tran_ins_params, meas_error_params)
        distance <- (model_cov-empirical_cov) %*% weight_matrix %*% (model_cov-empirical_cov)
        return (distance)
    }

    # Define the weight matrix as Equal Weight Minimum Distance
    weight_matrix <- diag(diag(omega)^(-1))

    ret <- objectiveFun(init_params, ma, taste, T, c_vector, weight_matrix)

    solved_objective <- nlm(objectiveFun, init_params, ma, taste, T, c_vector, weight_matrix, iterlim = 1000)
    solved_params <- solved_objective$estimate
    jacob <- jacobian(implied_cov_BPP, solved_params,  ma=ma, taste=taste,T=T, perm_shk_params=perm_shk_params, tran_shk_params=tran_shk_params, perm_ins_params= perm_ins_params,tran_ins_params=tran_ins_params, meas_error_params=meas_error_params)
   
    Sandwich1 <- solve(t(jacob) %*% weight_matrix %*% jacob)
    Sandwich2 <- t(jacob) %*% weight_matrix %*% omega %*% weight_matrix %*% jacob
    cov_params <- Sandwich1 %*% Sandwich2 %*% Sandwich1
    standard_errors <- diag(cov_params)^0.5

    if (ma==1){
        teta <- solved_params[1] 
        teta_se <- standard_errors[1] 
    } else{
        teta <- 0.0
        teta_se <- 0.0
    }
    if (taste){
        varcsi <- solved_params[1+ma] 
        varcsi_se <- standard_errors[1+ma] 
    } else {
        varcsi <- 0.0
        varcsi_se <- 0.0 
    }
    var_perm <- solved_params[(1+ma+taste):(ma+taste+perm_shk_params)] 
    var_tran <- solved_params[(1+ma+taste+perm_shk_params):(ma+taste+perm_shk_params+tran_shk_params)] 
    ins_perm <- solved_params[(1+ma+taste+perm_shk_params+tran_shk_params):(ma+taste+perm_shk_params+tran_shk_params+perm_ins_params)] 
    ins_tran <- solved_params[(1+ma+taste+perm_shk_params+tran_shk_params+perm_ins_params):(ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params)] 
    var_c_error <- solved_params[(1+ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params):(ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params+meas_error_params)] 
    
    var_perm_se <- standard_errors[(1+ma+taste):(ma+taste+perm_shk_params)] 
    var_tran_se <- standard_errors[(1+ma+taste+perm_shk_params):(ma+taste+perm_shk_params+tran_shk_params)] 
    ins_perm_se <- standard_errors[(1+ma+taste+perm_shk_params+tran_shk_params):(ma+taste+perm_shk_params+tran_shk_params+perm_ins_params)] 
    ins_tran_se <- standard_errors[(1+ma+taste+perm_shk_params+tran_shk_params+perm_ins_params):(ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params)] 
    var_c_error_se <- standard_errors[(1+ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params):(ma+taste+perm_shk_params+tran_shk_params+perm_ins_params+tran_ins_params+meas_error_params)] 
    
    output = list("var_perm"=var_perm, "var_perm_se"=var_perm_se, "var_tran"=var_tran, "var_tran_se"=var_tran_se, "ins_perm"=ins_perm, "ins_perm_se"=ins_perm_se, "ins_tran"=ins_tran, "ins_tran_se"=ins_tran_se, "var_c_error"=var_c_error,"var_c_error_se"=var_c_error_se,"teta"= teta, "teta_se"=teta_se, " varcsi"=varcsi, "varcsi_se"=varcsi_se)
    return (output)
}