# Testing how transition between quintiles will bias results


type = "liquid"

if (type=="liquid"){
  input_file=paste(txt_dir,"/transition_liquid.csv",    sep="")
  filename = "transitioning_liquid"
  title_string = "Transitory MPX by Liquid Wealth"
  ###############################################################################
  # load liquid weath quintile data
  load(paste(moments_dir,'moments_by_liquid_wealth_quantile',tag,'.RData',sep=''))
  num_quantiles = 5
  round_digits = -3
  wealth_quantile_set = as.character(1:num_quantiles)
  output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
  wealth_quantile_output=output
  wealth_quantile_params = output$category_params
  ###############################################################################
}

num_subperiods = 100
years=12
T=years
ignore_periods = 2
num_agents = 1000

prob_matrix = as.matrix(read.csv(file=input_file, header=FALSE, sep=","))

perm_var = wealth_quantile_params[,1]
tran_var = wealth_quantile_params[,2] 

phi_empirical = wealth_quantile_params[,3]
psi_empirical = wealth_quantile_params[,4]

phi = solve(prob_matrix/100) %*% phi_empirical
psi = solve(prob_matrix/100) %*% psi_empirical

set.seed(8)
perm_shocks_raw = t(perm_var**0.5*replicate(num_subperiods*(years+ignore_periods), rnorm(num_agents)) )/num_subperiods**1.5
tran_shocks_raw = t(tran_var**0.5*replicate(num_subperiods*(years+ignore_periods), rnorm(num_agents)) )/num_subperiods**0.5
quantile_shock = replicate(num_subperiods*(years+ignore_periods), runif(num_agents))

perm_var_compare = matrix(0,5,2)
tran_var_compare = matrix(0,5,2)
perm_MPX_compare = matrix(0,5,2)
tran_MPX_compare = matrix(0,5,2)

for (quantile in 1:5){
  quantile_realized = array(1, dim=c(num_subperiods*(years+ignore_periods),num_agents))
  for (i in 1:4){
      quantile_realized[quantile_shock>sum(prob_matrix[quantile,1:i]/100)] = i+1
  }
  perm_shocks = perm_shocks_raw*perm_var[quantile_realized]**0.5
  tran_shocks = tran_shocks_raw*tran_var[quantile_realized]**0.5
  tran_y = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
  perm_y = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
  tran_c = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
  perm_c = matrix(0,num_subperiods*(years+ignore_periods),num_agents)
  for (i in 1:(num_subperiods*(years+ignore_periods)-1)){
    tran_y[i+1,] = tran_shocks[i+1,]
    perm_y[i+1,] = perm_y[i,] + perm_shocks[i+1,]
    tran_c[i+1,] = psi[quantile_realized[i+1,]]*tran_shocks[i+1,]
    perm_c[i+1,] = perm_c[i,] + phi[quantile_realized[i+1,]]*perm_shocks[i+1,]
  }
  y = tran_y + perm_y 
  c = tran_c + perm_c 
  y_annual = matrix(0,years,num_agents)
  c_annual = matrix(0,years,num_agents)
  for (year in (1:years)){
    if (num_subperiods==1){
      y_annual[year,] = (y[(num_subperiods*(year+ignore_periods-1)+1):(num_subperiods*(year+ignore_periods)),])
      c_annual[year,] = (c[(num_subperiods*(year+ignore_periods-1)+1):(num_subperiods*(year+ignore_periods)),])
    }else{
      y_annual[year,] = colSums(y[(num_subperiods*(year+ignore_periods-1)+1):(num_subperiods*(year+ignore_periods)),])
      c_annual[year,] = colSums(c[(num_subperiods*(year+ignore_periods-1)+1):(num_subperiods*(year+ignore_periods)),])
    }
    c_annual[year,] = c_annual[year,] 
  }
  
  delta_y = (y_annual[2:nrow(y_annual),]-y_annual[1:(nrow(y_annual)-1),])
  delta_c = (c_annual[2:nrow(c_annual),]-c_annual[1:(nrow(c_annual)-1),])
  all_data = matrix(0,nrow(delta_y)*ncol(delta_y), 6)
  for (i in (1:ncol(delta_y))){
    all_data[((i-1)*nrow(delta_y)+1):(i*nrow(delta_y)),1] = i   # id column
    all_data[((i-1)*nrow(delta_y)+1):(i*nrow(delta_y)),2] = 1:nrow(delta_y)   # year column
    all_data[((i-1)*nrow(delta_y)+1):(i*nrow(delta_y)),3] = delta_y[,i]   # id column
    all_data[((i-1)*nrow(delta_y)+1):(i*nrow(delta_y)),4] = 1   # id column
    all_data[((i-1)*nrow(delta_y)+1):(i*nrow(delta_y)),5] = delta_c[,i]   # id column
    all_data[((i-1)*nrow(delta_y)+1):(i*nrow(delta_y)),6] = 1   # id column
  }
  moments_all <- create_moments_CS(all_data)
  c_vector = moments_all$c_vector
  omega = moments_all$omega
  #T = moments_all$T
  CS_output = CS_parameter_estimation(c_vector, omega,T-1) 
  
  perm_var_compare[quantile,2] = CS_output$var_perm
  tran_var_compare[quantile,2] = CS_output$var_tran
  perm_MPX_compare[quantile,2] = CS_output$ins_perm
  tran_MPX_compare[quantile,2] = CS_output$ins_tran
  
  perm_var_compare[quantile,1] = perm_var[quantile]
  tran_var_compare[quantile,1] = tran_var[quantile]
  perm_MPX_compare[quantile,1] = phi_empirical[quantile]
  tran_MPX_compare[quantile,1] = psi_empirical[quantile]
}

params = tran_MPX_compare

params = matrix(0,5,2)
params[,1] = psi[]
params[,2] = tran_MPX_compare[,2]


quantile_labels = 1:5
this_colors = c('#fb8072','#bebada','#ffffb3','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#8dd3c7')
this_colors = this_colors[1:2]
axis_string = "MPX"
xlabel_pos = 0.5
tag_list_legend = c("Short-term Quintile","Long-term Quintile")
x_label = "Qunitile"
legend_xpos = 7.5

pdf(paste(figures_dir, filename,".pdf",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plotTop = max(max(params),1.0)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_labels,
                      cex.names=0.75,
                      beside=TRUE,col=this_colors,
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=title_string,
                      ylab = "",
                      xlab = x_label,
                      border="black",
                      axes=TRUE)
mtext(text = axis_string,
      side = 2, #side 2 = left
      line = 4,cex=1.5)
text(x=barCenters[1,]+xlabel_pos, y =-plotTop*0.02,srt=0, adj=1, labels=quantile_labels,xpd=TRUE)
legend(legend_xpos, plotTop, legend=tag_list_legend, fill=this_colors,bty="n")
dev.off()

