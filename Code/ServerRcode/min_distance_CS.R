# Carroll Samwick Parameter estimation


require(numDeriv)

implied_cov_CS <-function(params,T, diff_to_use=3:5) {
    var_perm <- params[1] 
    var_tran <- params[2]
    ins_perm <- params[3] 
    ins_tran <- params[4] 

    num_diffs=length(diff_to_use)
    max_diff = max(diff_to_use)
    diff_cols = sum(max_diff - diff_to_use+1)
    diff_rows = T- max_diff+1
    moment_y2 = array(0.0, dim=c(diff_rows,diff_cols))
    moment_cy = array(0.0, dim=c(diff_rows,diff_cols))
    
    for (t in (1:diff_rows+max_diff-1)){
      this_col=1
      for (j in 1:num_diffs){
        n = diff_to_use[j]
        moment_y2[,this_col:(this_col+max_diff-n)] = (n-1.0/3.0)*var_perm + 2.0*var_tran
        moment_cy[,this_col:(this_col+max_diff-n)] = (n-1.0/3.0)*ins_perm*var_perm + 2.0*ins_tran*var_tran
        this_col=this_col+max_diff-n+1
      }
    }
    implied_cov = c(as.vector(t(moment_y2)),as.vector(t(moment_cy)))

        return (implied_cov)
}
CS_parameter_estimation <- function(c_vector, omega,T,diff_to_use=3:5){

    init_params <- matrix(0,nrow=4,ncol=1)

    init_params[1] <- 0.03 
    init_params[2] <-0.03
    init_params[3] <-0.7
    init_params[4] <-0.7

    objectiveFun <-function(params, empirical_cov, weight_matrix,T,diff_to_use=3:5){
        model_cov <- implied_cov_CS(params,T,diff_to_use)
        distance <- (model_cov-empirical_cov) %*% weight_matrix %*% (model_cov-empirical_cov)
        return (distance)
    }

    # Define the weight matrix as Equal Weight Minimum Distance
    weight_matrix <- diag(diag(omega)^(-1))

    ret <- objectiveFun(init_params, c_vector, weight_matrix,T)

    solved_objective <- nlm(objectiveFun, init_params, c_vector, weight_matrix,T, iterlim = 1000, steptol=1e-12)
    solved_params <- solved_objective$estimate
    
    jacobFun <-function(params){
      return (implied_cov_CS(params,T,diff_to_use))
    }    
    
    jacob <- jacobian(jacobFun, solved_params)
   
    Sandwich1 <- solve(t(jacob) %*% weight_matrix %*% jacob)
    Sandwich2 <- t(jacob) %*% weight_matrix %*% omega %*% weight_matrix %*% jacob
    cov_params <- Sandwich1 %*% Sandwich2 %*% Sandwich1
    standard_errors <- diag(cov_params)^0.5

    var_perm <- solved_params[1] 
    var_tran <- solved_params[2] 
    ins_perm <- solved_params[3] 
    ins_tran <- solved_params[4] 

    var_perm_se <- standard_errors[1] 
    var_tran_se <- standard_errors[2] 
    ins_perm_se <- standard_errors[3] 
    ins_tran_se <- standard_errors[4] 

    output = list("var_perm"=var_perm, "var_perm_se"=var_perm_se, "var_tran"=var_tran, "var_tran_se"=var_tran_se, "ins_perm"=ins_perm, "ins_perm_se"=ins_perm_se, "ins_tran"=ins_tran, "ins_tran_se"=ins_tran_se, "implied_cov"=(implied_cov_CS(solved_params,T,diff_to_use)))
    return (output)
}