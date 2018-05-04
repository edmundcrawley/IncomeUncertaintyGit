

empirical_input_file="Input_for_R_fullsample.csv"
#empirical_input_file="input_for_R_5percentsample.csv"
#load the data
raw_data = read.csv(paste(empirical_input_folder,empirical_input_file,sep=""), sep=",")
year_col = 2
#format input data as a martrix
all_data<- as.matrix(raw_data[,1:9]) 

this_data = all_data[age_range,]
moments_loop = list()
for (max_diff in 4:10){
  this_moments = paste('moments_',max_diff,sep='')
  moments_loop[[this_moments]]=create_moments_CS(this_data,1:max_diff)
}

params_loop = list()
for (max_diff in 4:10){
  this_moments = paste('moments_',max_diff,sep='')
  moments_all = moments_loop[[this_moments]]
  
  var_tran_array = array(0.0, dim=c(max_diff,max_diff))
  var_perm_array = array(0.0, dim=c(max_diff,max_diff))
  ins_tran_array = array(0.0, dim=c(max_diff,max_diff))
  ins_perm_array = array(0.0, dim=c(max_diff,max_diff))
  
  var_tran_array_se = array(0.0, dim=c(max_diff,max_diff))
  var_perm_array_se = array(0.0, dim=c(max_diff,max_diff))
  ins_tran_array_se = array(0.0, dim=c(max_diff,max_diff))
  ins_perm_array_se = array(0.0, dim=c(max_diff,max_diff))
  
  for (n1 in 1:(max_diff-1)){
    for (n2 in (n1+1):max_diff){
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
      var_perm_array[n1,n2] = CS_output_sub$var_perm
      var_tran_array[n1,n2] = CS_output_sub$var_tran
      ins_perm_array[n1,n2] = CS_output_sub$ins_perm
      ins_tran_array[n1,n2] = CS_output_sub$ins_tran
      
      var_perm_array_se[n1,n2] = CS_output_sub$var_perm_se
      var_tran_array_se[n1,n2] = CS_output_sub$var_tran_se
      ins_perm_array_se[n1,n2] = CS_output_sub$ins_perm_se
      ins_tran_array_se[n1,n2] = CS_output_sub$ins_tran_se
    }
  }
  params_loop[[paste('var_perm_array_',max_diff,sep='')]] =  var_perm_array
  params_loop[[paste('var_tran_array_',max_diff,sep='')]] =  var_tran_array
  params_loop[[paste('ins_perm_array_',max_diff,sep='')]] =  ins_perm_array
  params_loop[[paste('ins_tran_array_',max_diff,sep='')]] =  ins_tran_array
  
  params_loop[[paste('var_perm_array_se_',max_diff,sep='')]] =  var_perm_array_se
  params_loop[[paste('var_tran_array_se_',max_diff,sep='')]] =  var_tran_array_se
  params_loop[[paste('ins_perm_array_se_',max_diff,sep='')]] =  ins_perm_array_se
  params_loop[[paste('ins_tran_array_se_',max_diff,sep='')]] =  ins_tran_array_se
}

to_plot = 'var_tran_array_'

plot(diag(params_loop[[paste(to_plot,10,sep='')]][,-(1:2)]))
for (max_diff in 4:10){
  points(diag(params_loop[[paste(to_plot,max_diff,sep='')]][,-(1:2)]))
  lines(diag(params_loop[[paste(to_plot,max_diff,sep='')]][,-(1:2)]))
}
lines(diag(params_loop[[paste(to_plot,7,sep='')]][,-(1:2)]+1.96*params_loop[[paste(to_plot,'se_',7,sep='')]][,-(1:2)]),lty='dashed')
lines(diag(params_loop[[paste(to_plot,7,sep='')]][,-(1:2)]-1.96*params_loop[[paste(to_plot,'se_',7,sep='')]][,-(1:2)]),lty='dashed')
lines(diag(params_loop[[paste(to_plot,10,sep='')]][,-(1:2)]+1.96*params_loop[[paste(to_plot,'se_',10,sep='')]][,-(1:2)]),lty='dashed')
lines(diag(params_loop[[paste(to_plot,10,sep='')]][,-(1:2)]-1.96*params_loop[[paste(to_plot,'se_',10,sep='')]][,-(1:2)]),lty='dashed')

save_dir = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/save/"
save(moments_loop,params_loop,file=paste(save_dir,'moments',sep=''))


for (max_diff in 4:10){
  this_moments = paste('moments_',max_diff,sep='')
  moments_all = moments_loop[[this_moments]]
  
}