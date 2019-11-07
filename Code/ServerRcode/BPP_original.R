# Create moments needed for the BPP method

require(MCMCpack)

create_moments_BPP <- function(all_data) {
  
  col_id     <- 1                      #/*colum where id is */ 
  col_year   <- 2                      #/*column where year is*/
  coly_dif   <- 3                      #/*column where residuals in FD of income are*/
  coldy_dif  <- 4                      #/*column where dummies for no-missing residuals in FD are*/
  colc_dif   <- 5                      #/*column where residuals in FD of consumption are*/
  coldc_dif  <- 6                      #/*column where dummies for no-missing residuals in FD are*/
  
  # length of panel. Assumes all individuals appear in all years
  T  <- max(all_data[,col_year])-min(all_data[,col_year])+1 
  y  <- nrow(all_data) 
  
  #Although in Stata we get residuals of income, when we select on some aspects of the data
  #the mean changes in income and consumption may no longer be zero. This can introduce bias.
  #Here we recenter the sample being examined year by year
  recenter_means=TRUE
  mean_dy = matrix(0.0,y)
  mean_dc = matrix(0.0,y)
  for (i in 1:T){
    index = i + (0:(y/T -1))*T
    dy_with_na = all_data[,coly_dif]
    dy_with_na[all_data[,coldy_dif]==0] = NA
    dc_with_na = all_data[,colc_dif]
    dc_with_na[all_data[,coldc_dif]==0] = NA
    mean_dy[index] = mean(dy_with_na[index],na.rm=TRUE)
    mean_dc[index] = mean(dc_with_na[index],na.rm=TRUE)
  }
  #take out means if required
  if (recenter_means==TRUE) {
    all_data[,coly_dif] = all_data[,coly_dif] - mean_dy
    all_data[,coly_dif][all_data[,coldy_dif]==0] = 0
    all_data[,colc_dif] = all_data[,colc_dif] - mean_dc
    all_data[,colc_dif][all_data[,coldc_dif]==0] = 0
    #store the size of mean removed
  }
  mean_dy = mean_dy[1:T]
  mean_dc = mean_dc[1:T]
  
  
  
  # dif 
  dif    <- array(1:T^2, dim=c(T,T))*0.0
  d_dif  <- array(1:T^2, dim=c(T,T))*0.0

  for (k in 0:((y/T)-1)){
    i<- k*T+1
    dif_j <-  all_data[i:(i+T-1),coly_dif] %o% all_data[i:(i+T-1),coly_dif]
    d_dif_j<- all_data[i:(i+T-1),coldy_dif] %o% all_data[i:(i+T-1),coldy_dif]
    dif <-  dif+dif_j
    d_dif <-  d_dif+d_dif_j
  }
  dif <-  dif/d_dif
  income_cov <-  dif
  income_var <-  diag(dif)
  
  # create vector from the lower triangular elements of the covariance matrix (it is a symetric matrix)
  c_matrix <- dif
  c_vector <- vech(c_matrix)
  
  dim <- nrow(c_matrix)*(nrow(c_matrix)+1)/2
  
  # create variance matrix for all these moments
  omega  <- array(1:dim^2, dim=c(dim,dim))*0.0
  d_matrix=d_dif
  d_vector=vech(d_matrix)
  for (k in 0:((y/T)-1)){
    i=k*T+1
    dif_j  <- all_data[i:(i+T-1),coly_dif] %o% all_data[i:(i+T-1),coly_dif]
    d_dif_j <- all_data[i:(i+T-1),coldy_dif] %o% all_data[i:(i+T-1),coldy_dif]
    c_matrix_j <-dif_j
    d_matrix_j <- d_dif_j
    c_vector_j <- vech(c_matrix_j)
    d_vector_j <- vech(d_matrix_j)
    omega <- omega+((c_vector_j-c_vector) %o% (c_vector_j-c_vector))*((d_vector_j %o% d_vector_j))
  }    
  c_vector_income_only = c_vector
  omega_income_only=omega/(d_vector %o% d_vector)
  
  
  #**********Now create both income and consumption moments
  
  # stack consumption and income
  consumption_rows <- cbind(all_data[,c(colc_dif,coldc_dif,col_id,col_year)], all_data[,1]*0+1)
  income_rows =       cbind(all_data[,c(coly_dif,coldy_dif,col_id,col_year)],2*(all_data[,1]*0+1))
  
  colnames(income_rows) <- colnames(consumption_rows)
  c_y_rows = rbind(consumption_rows,income_rows)
  # sort by id, cons/income, year (do backwards and make sure sorts are stable)
  c_y_rows <- c_y_rows[order(c_y_rows[,3],c_y_rows[,5],c_y_rows[,4]),]
  
  initial_year = min(all_data[,col_year])
  final_year = max(all_data[,col_year])
  T = final_year-initial_year+1 
  T1= T+T
  
  dif    <- array(1:T1^2, dim=c(T1,T1))*0.0
  d_dif  <- array(1:T1^2, dim=c(T1,T1))*0.0
  y      <- nrow(c_y_rows) 
  
  for (k in 0:((y/T1)-1) ){
    i=k*T1+1
    dif_j = c_y_rows[i:(i+T1-1),1] %o% c_y_rows[i:(i+T1-1),1]
    d_dif_j = c_y_rows[i:(i+T1-1),2] %o% c_y_rows[i:(i+T1-1),2]
    dif = dif+dif_j
    d_dif = d_dif+d_dif_j
  }
  dif = dif/d_dif
  # create vector from the lower triangular elements of the covariance matrix (it is a symetric matrix)
  c_matrix=dif
  c_vector= vech(c_matrix)
  dim = (nrow(c_matrix)*(nrow(c_matrix)+1))/2
  # create variance matrix for all these moments
  omega  = array(1:dim^2, dim=c(dim,dim))*0.0
  d_matrix=d_dif
  d_vector=vech(d_matrix)
  
  for (k in 0:((y/T1)-1) ){
    i=k*T1+1
    dif_j  =c_y_rows[i:(i+T1-1),1] %o% c_y_rows[i:(i+T1-1),1]
    d_dif_j=c_y_rows[i:(i+T1-1),2] %o% c_y_rows[i:(i+T1-1),2]
    c_matrix_j=dif_j
    d_matrix_j=d_dif_j
    c_vector_j= vech(c_matrix_j)
    d_vector_j=vech(d_matrix_j)
    omega=omega+(((c_vector_j-c_vector) %o% (c_vector_j-c_vector))*((d_vector_j %o% d_vector_j)))
  }
  c_vector_both = c_vector
  omega_both=omega/(d_vector %o% d_vector)
  
  only_income = FALSE
  if (only_income) {
    output <- list("c_vector" = c_vector_income_only, "omega" = omega_income_only, "d_dif"=d_dif, "mean_dy"=mean_dy, "mean_dc"=mean_dc)
  }  else {
    output <- list("c_vector" = c_vector_both, "omega" = omega_both, "d_dif"=d_dif, "mean_dy"=mean_dy,"mean_dc"=mean_dc)
  }
  return (output) }