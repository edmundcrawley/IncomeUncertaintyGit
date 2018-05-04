#Shows the assumption that the transitory shock is over
#after 2 years is valid by showing a clear linear relation
#between

max_diff=10
#moments_all <- create_moments_CS(all_data,1:max_diff)
this_moments = paste('moments_',max_diff,sep='')
moments_all = moments_loop[[this_moments]]

y2_diff = array(0.0, dim=c(max_diff,1))
cy_diff = array(0.0, dim=c(max_diff,1))
regcoef_diff = array(0.0, dim=c(max_diff,1))

#To be a fair judge of the accuracy we need to include the same moments in the 
#parameter estimation as we do when we take the mean moments
#Therefore we stick in the mean to the parameter estimation
smoothed_moment_y2=moments_all$moment_y2*0.0
smoothed_moment_cy=moments_all$moment_cy*0.0

this_col=1
for (i in 1:max_diff){
  y2_diff[i] = mean(moments_all$moment_y2[,(this_col):(this_col+max_diff-i)])
  cy_diff[i] = mean(moments_all$moment_cy[,(this_col):(this_col+max_diff-i)])
  regcoef_diff[i] = mean(moments_all$reg_coef[,(this_col):(this_col+max_diff-i)])
  
  smoothed_moment_y2[,(this_col):(this_col+max_diff-i)]=mean(moments_all$moment_y2[,(this_col):(this_col+max_diff-i)])
  smoothed_moment_cy[,(this_col):(this_col+max_diff-i)]=mean(moments_all$moment_cy[,(this_col):(this_col+max_diff-i)])
  
  this_col = this_col+max_diff-i+1
}

#############################
n1=3
n2=5
diff_to_use = c(n1,n2)
cols_per_diff = max_diff-diff_to_use+1
moments_used =c()
j=1
k=1
for (i in 1:max_diff){
  if (i==diff_to_use[j]){
    moments_used = c(moments_used, k:(k+max_diff-i))
    j = j+1
    if (j>2){
      break
    }
  }
  k=k+max_diff-i+1
}
moments_used_all=c()
for (i in 0:(2*(T-max_diff+1)-1)){
  moments_used_all = c(moments_used_all,moments_used+i*(max_diff*(max_diff+1))/2)
}
c_vector_sub = moments_all$c_vector[moments_used_all]
omega_sub    = moments_all$omega[moments_used_all,][,moments_used_all]
CS_output_sub = CS_parameter_estimation(c_vector_sub, omega_sub, T-(max_diff-diff_to_use[-1]),diff_to_use,cols_per_diff) 

#############################
# #create 3:5 moments from this sample that only contains households that last 7 years
# cols_to_keep = c(16,17,18,21,22,25)-1
# cols_to_keep_full=cols_to_keep
# for (i in 1:(2*(T-max_diff+1)-1)){
#   cols_to_keep_full = c(cols_to_keep_full,cols_to_keep+i*28)
# }
# #c_vector_sub = moments_all$c_vector[cols_to_keep_full]
# c_vector_sub =c(as.vector(t(smoothed_moment_y2)),as.vector(t(smoothed_moment_cy)))[cols_to_keep_full]
# omega_sub = moments_all$omega[cols_to_keep_full,][,cols_to_keep_full]
# CS_output_sub = CS_parameter_estimation(c_vector_sub, omega_sub, T-2,3:5) 
#####################################


figures_dir = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/figures/"
#png(paste(figures_dir, "IncreasingDiff.png",sep=""))
plot(1:max_diff,y2_diff,ylim=c(0,1.2*max(y2_diff)),xlim=c(0,max_diff),
     main="Covariance with Increasing Difference Operator",xlab="n",ylab="variance/covariance")
lines(1:max_diff,y2_diff)
points(1:max_diff,cy_diff)
lines(1:max_diff,cy_diff,lty="dashed")
lines(0:max_diff,(0:max_diff-1.0/3.0)*CS_output_sub$var_perm + 2*CS_output_sub$var_tran, col="red")
lines(0:max_diff,(0:max_diff-1.0/3.0)*CS_output_sub$ins_perm*CS_output_sub$var_perm + 2*CS_output_sub$ins_tran*CS_output_sub$var_tran, col="green")
legend(0.25, 0.05, legend=c(expression(paste("var(",Delta^n,"y) Empirical"),paste("var(",Delta^n,"y) matched to n=3,4,5"), paste("cov(",Delta^n,"y,",Delta^n,"c) Empirical"),paste("cov(",Delta^n,"y,",Delta^n,"c) matched to n=3,4,5"))),lty=c("solid","solid","dashed","solid"),col=c("black","red","black","green"))
#dev.off()

# dev.new()
# plot(1:max_diff,regcoef_diff,ylim=c(min(regcoef_diff)/1.2,max(regcoef_diff)*1.2))
# lines(1:max_diff,regcoef_diff)





