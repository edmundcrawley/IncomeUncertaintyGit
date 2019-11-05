# Use longer moments

require(MCMCpack)

create_moments_CS <- function(all_data, diff_to_use=3:5) {
  
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


    #matrix containing the 3/4/5th (or diff_to_use) differences
    #First half of the columns are income growth, second half are consumption growth
    #diff_matrix_exist contains 1 if that element of the matrix has good data, 0 otherise
    num_diffs=length(diff_to_use)
    diff_matrix = array(0.0, dim=c(y,2*num_diffs))
    diff_matrix_exist = array(0, dim=c(y,2*num_diffs))
    for (k in 0:((y/T)-1)){
  	i <- k*T
    	for (j in 1:num_diffs){
    	  n=diff_to_use[j]
      	for (m in n:T){
      		diff_matrix[i+m,j] = sum(all_data[(i+m-n+1):(i+m),coly_dif])
      		diff_matrix[i+m,j+num_diffs] = sum(all_data[(i+m-n+1):(i+m),colc_dif])
      		diff_matrix_exist[i+m,j] = min(all_data[(i+m-n+1):(i+m),coldy_dif])
      		diff_matrix_exist[i+m,j+num_diffs] = min(all_data[(i+m-n+1):(i+m),coldc_dif])
      	}
    	}
    }
    diff_matrix = diff_matrix*diff_matrix_exist

    #create matrix with one row per year for which the 5th (or max(diff_to_use)) difference exists
    #Each row has 5-n+1 entries for the nth difference variance, one for each nth difference fitting into the 5 year period
    max_diff = max(diff_to_use)
    diff_cols = sum(max_diff - diff_to_use+1)
    diff_rows = T- max_diff+1
    moment_y2 = array(0.0, dim=c(diff_rows,diff_cols))
    moment_cy = array(0.0, dim=c(diff_rows,diff_cols))
    d_dif = array(0.0, dim=c(diff_rows,diff_cols))
    for (k in 0:((y/T)-1)){
      i <- k*T
      moment_y2_j = array(0.0, dim=c(diff_rows,diff_cols))
      moment_cy_j = array(0.0, dim=c(diff_rows,diff_cols))
      d_dif_j = array(0.0, dim=c(diff_rows,diff_cols))
      for (t in (1:diff_rows+max_diff-1)){
        #first check if there is data for both c and y for the max diff in this year, if not move on to the next
        if (diff_matrix_exist[i+t,num_diffs]!=0 & diff_matrix_exist[i+t,2*num_diffs]!=0){
          this_col=1
          for (j in 1:num_diffs){
            n = diff_to_use[j]
            moment_y2_j[(t- max_diff+1),this_col:(this_col+max_diff-n)] = diff_matrix[(i+t-(max_diff-n)):(i+t),j]^2
            moment_cy_j[(t- max_diff+1),this_col:(this_col+max_diff-n)] = diff_matrix[(i+t-(max_diff-n)):(i+t),j]*diff_matrix[(i+t-(max_diff-n)):(i+t),j+num_diffs]
            d_dif_j[(t- max_diff+1),this_col:(this_col+max_diff-n)] = 1
            this_col=this_col+max_diff-n+1
          }
        }
      }
      moment_y2 = moment_y2+moment_y2_j
      moment_cy = moment_cy+moment_cy_j
      d_dif = d_dif+d_dif_j
    }
    moment_y2 = moment_y2/d_dif
    moment_cy = moment_cy/d_dif
    c_vector = c(as.vector(t(moment_y2)),as.vector(t(moment_cy)))
    d_vector = c(as.vector(t(d_dif)),as.vector(t(d_dif)))
    dim = length(c_vector)
    omega = array(1:dim, dim=c(dim,dim))*0.0

    for (k in 0:((y/T)-1)){
      i <- k*T
      moment_y2_j = array(0.0, dim=c(diff_rows,diff_cols))
      moment_cy_j = array(0.0, dim=c(diff_rows,diff_cols))
      d_dif_j = array(0.0, dim=c(diff_rows,diff_cols))
      for (t in (1:diff_rows+max_diff-1)){
        #first check if there is data for both c and y for the max diff in this year, if not move on to the next
        if (diff_matrix_exist[i+t,num_diffs]!=0 & diff_matrix_exist[i+t,2*num_diffs]!=0){
          this_col=1
          for (j in 1:num_diffs){
            n = diff_to_use[j]
            moment_y2_j[(t- max_diff+1),this_col:(this_col+max_diff-n)] = diff_matrix[(i+t-(max_diff-n)):(i+t),j]^2
            moment_cy_j[(t- max_diff+1),this_col:(this_col+max_diff-n)] = diff_matrix[(i+t-(max_diff-n)):(i+t),j]*diff_matrix[(i+t-(max_diff-n)):(i+t),j+num_diffs]
            d_dif_j[(t- max_diff+1),this_col:(this_col+max_diff-n)] = 1
            this_col=this_col+max_diff-n+1
          }
        }
      }
        c_vector_j = c(as.vector(t(moment_y2_j)),as.vector(t(moment_cy_j)))
        d_vector_j = c(as.vector(t(d_dif_j)),as.vector(t(d_dif_j)))
        omega=omega+(((c_vector_j-c_vector) %o% (c_vector_j-c_vector))*((d_vector_j %o% d_vector_j)))
    }
    omega = omega/(d_vector %o% d_vector)

    
    #Also calculate first difference income variance
    delta_y_var = sum((all_data[,coly_dif]^2)*diff_matrix_exist[,1])/sum(diff_matrix_exist[,1])
    #And regression coefficients of consumption change vs income change for different time periods
    reg_coef = moment_cy/moment_y2

    output <- list("c_vector" = c_vector, "omega" = omega, "T" = T, "delta_y_var"=delta_y_var,"reg_coef"=reg_coef,"moment_y2"=moment_y2,"moment_cy"=moment_cy,"d_dif"=d_dif,"mean_dy"=mean_dy,"mean_dc"=mean_dc)
    return (output)
}




