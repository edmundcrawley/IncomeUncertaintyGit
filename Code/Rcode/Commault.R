# This file does estimation using Commault's robust estimator

######################################################################################

tag = ""
title_string = "MPX"
axis_string = "MPX"

require(ks)


base_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/"
R_code_folder =  "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode"
moments_BPP_dir = paste(base_dir,"Code/ServerRcode/ServerOutput/AEJ_revision/TxtFilesFromAndreas/",sep='')
moments_dir = paste(base_dir,"Code/ServerRcode/ServerOutput/AEJ_revision/",sep='')
figures_dir = paste(base_dir,"Code/Rcode/Figures/AEJ_revision/",sep='')
tables_dir = paste(base_dir,"Code/Rcode/Tables/AEJ_revision/",sep='')


###############################################################################
# Full sample estimation
#First load the moments
c_vector = as.vector(t(read.csv(file=paste(moments_BPP_dir,"/BPP_moments_all_c_vector.txt", sep=""), header=FALSE, sep=",")))
omega =    as.matrix(read.csv(file=paste(moments_BPP_dir,"/BPP_moments_all_omega.txt",    sep=""), header=FALSE, sep=","))
T=12
c_matrix = invvech(c_vector)
covy = c_matrix[13:24,13:24]
covcy = c_matrix[0:12,13:24]
#BPP
#i=2 BPP, i=3 Commault
i=2
mean(diag(covcy[1:(12-i+1),i:12]))
mean(diag(covy[1:(12-i+1),i:12]))
mean(diag(covcy[1:(12-i+1),i:12]))/mean(diag(covy[1:(12-i+1),i:12]))


  select_moments = array(FALSE,dim=c(24,24))
  for (k in 1:(12-j+1)){
    select_moments[12+k-1+j,k] = TRUE
    select_moments[12+k-1+j,12+k] = TRUE
    select = vech(select_moments)
  }
  c_vector[select]
  omega[select,select]



###############################################################################
estimation_commault<- function(moments_BPP_dir,moments_stub,category_set, T=12,num_lags=5) {
  category_params = array(0, dim=c(length(category_set),num_lags))
  category_params_se = array(0, dim=c(length(category_set),num_lags))
  covy_all = array(0, dim=c(length(category_set),num_lags))
  covy_se = array(0, dim=c(length(category_set),num_lags))
  covcy_all = array(0, dim=c(length(category_set),num_lags))
  covcy_se = array(0, dim=c(length(category_set),num_lags))
  for (i in 1:length(category_set)){
    this_category = as.character(category_set[i])
    this_c_vector = as.vector(t(read.csv(file=paste(moments_BPP_dir,"/",moments_stub,i,"c_vector.txt", sep=""), header=FALSE, sep=",")))
    this_omega = as.matrix(read.csv(file=paste(moments_BPP_dir,"/",moments_stub,i,"_omega.txt", sep=""), header=FALSE, sep=","))
    c_matrix = invvech(this_c_vector)
    covy = c_matrix[13:24,13:24]
    covcy = c_matrix[0:12,13:24]
    for (j in 1:num_lags){
      select_moments = array(FALSE,dim=c(24,24))
      select_moments_covcy = array(FALSE,dim=c(24,24))
      select_moments_covy = array(FALSE,dim=c(24,24))
      for (k in 1:(12-j+1)){
        select_moments[12+k-1+j,k] = TRUE
        select_moments_covcy[12+k-1+j,k] = TRUE
        select_moments[12+k-1+j,12+k] = TRUE
        select_moments_covy[12+k-1+j,12+k] = TRUE
        select = vech(select_moments)
        select_cy = vech(select_moments_covcy)
        select_y = vech(select_moments_covy)
      }
      
      covcy_all[i,j] = mean(diag(covcy[1:(12-j+1),j:12]))
      covy_all[i,j] = mean(diag(covy[1:(12-j+1),j:12]))
      category_params[i,j] = mean(diag(covcy[1:(12-j+1),j:12]))/mean(diag(covy[1:(12-j+1),j:12]))
     deriv = matrix(0,(2*(12-j+1)))
     deriv[0:(12-j+1)] = 1/((12-j+1)*mean(diag(covy[1:(12-j+1),j:12])))
     deriv[(12-j+2):(2*(12-j+1))] = mean(diag(covcy[1:(12-j+1),j:12]))/((12-j+1)*mean(diag(covy[1:(12-j+1),j:12]))**2)
     category_params_se[i,j] = (t(deriv) %*% this_omega[select,select] %*% deriv)^0.5
     
     deriv = matrix(0,(12-j+1))
     deriv[0:(12-j+1)] = 1/(12-j+1)
     covcy_se[i,j] = (t(deriv) %*% this_omega[select_cy,select_cy] %*% deriv)^0.5
     covy_se[i,j] = (t(deriv) %*% this_omega[select_y,select_y] %*% deriv)^0.5
    }
  }
  commault = list()
  commault$category_params = category_params
  commault$category_params_se = category_params_se
  commault$covy_all = covy_all
  commault$covy_se = covy_se
  commault$covcy_all = covcy_all
  commault$covcy_se = covcy_se
  return (commault)
}

###############################################################################
# load liquid weath quintile data and create graph
moments_stub = "BPP_moments_by_liquid_wealth_quantile"
num_quantiles = 5
wealth_quantile_set = as.character(1:num_quantiles)
commault_output =estimation_commault(moments_BPP_dir,moments_stub, make.names(wealth_quantile_set), T=12,num_lags=5)
#now recalculate standard solution
load(paste(moments_dir,'moments_by_liquid_wealth_quantile',tag,'.RData',sep=''))
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
output =estimation_by_category(moments_by_liquid_wealth_quantile, make.names(wealth_quantile_set))
wealth_quantile_output=output
wealth_quantile_params = output$category_params
wealth_quantile_se = output$category_se
wealth_quantile_obs = output$category_obs
wealth_quantile_total_var = output$category_total_var
wealth_quantile_set = c(paste('$0-',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[1]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[2]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('$',format(round(moments_by_liquid_wealth_quantile$quantiles[[3]],round_digits),big.mark=",", trim=TRUE),'-',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))
wealth_quantile_set = c(wealth_quantile_set,paste('> $',format(round(moments_by_liquid_wealth_quantile$quantiles[[4]],round_digits),big.mark=",", trim=TRUE),sep=''))

for_commault_plot = array(0,dim=c(5,3))
for_commault_plot[,1] = wealth_quantile_output$category_params[,4]
for_commault_plot[,2:3] = commault_output$category_params[,3:4]
for_commault_plot_se = array(0,dim=c(5,3))
for_commault_plot_se[,1] = wealth_quantile_output$category_se[,4]
for_commault_plot_se[,2:3] = commault_output$category_params_se[,3:4]

#for_commault_plot = for_commault_plot[,1:2]
#for_commault_plot_se = for_commault_plot_se[,1:2]


pdf(paste(figures_dir, "Commault.pdf",sep=""))
  this_legend=c("Baseline","Commault Robust Estimator, t=1","Commault Robust Estimator, t=2")
  xlabel_pos = 2
  this_colors=c(colors[1:3])
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plotTop = max(max(for_commault_plot),1.0)
barCenters <- barplot(height=t(for_commault_plot),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=this_colors,
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("MPX by Liquid Wealth"),
                      ylab = "MPX", border="black", axes=TRUE)
text(x=barCenters[1,]+xlabel_pos, y =-plotTop*0.02,srt=45, adj=1, labels=wealth_quantile_set,xpd=TRUE)
segments(barCenters, t(for_commault_plot-for_commault_plot_se*1.96),
         barCenters,
         t(for_commault_plot+for_commault_plot_se*1.96), lwd=1.5)
arrows(barCenters, t(for_commault_plot-for_commault_plot_se*1.96),
       barCenters,
       t(for_commault_plot+for_commault_plot_se*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
legend(6, plotTop, legend=this_legend, fill=this_colors,bty="n")
#dev.copy(png, paste(figures_dir, "MPXBy",category_for_save,tag,".png",sep=""))
#dev.copy(pdf, paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "MPXBy",category_for_save,tag,".svg",sep=""))
dev.off()


###############################################################################

###############################################################################
# load liquid weath quintile data and create graph
moments_stub = "BPP_moments_by_liquid_wealth_quantile"
num_quantiles = 5
round_digits = -3
wealth_quantile_set = as.character(1:num_quantiles)
BPP_output =estimation_by_category_BPP(moments_BPP_dir,moments_stub, make.names(wealth_quantile_set), T=12)
###############################################################################
for_commault_BPP_plot = array(0,dim=c(5,3))
for_commault_BPP_plot[,1:2] = for_commault_plot[,1:2]
for_commault_BPP_plot[,3] = BPP_output$category_params[,4]

for_commault_BPP_plot_se = array(0,dim=c(5,3))
for_commault_BPP_plot_se[,1:2] = for_commault_plot_se[,1:2]
for_commault_BPP_plot_se[,3] = BPP_output$category_se[,4]

pdf(paste(figures_dir, "Commault_BPP.pdf",sep=""))
this_legend=c("Baseline","Commault Robust Estimator", "BPP")
xlabel_pos = 2
this_colors=c(colors[1:3])
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
plotTop = max(max(for_commault_BPP_plot),1.0)
barCenters <- barplot(height=t(for_commault_BPP_plot),
                      names.arg=wealth_quantile_set,
                      cex.names=0.75,
                      beside=TRUE,col=this_colors,
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Transitory MPX by Liquid Wealth"),
                      ylab = "MPX", border="black", axes=TRUE)
text(x=barCenters[1,]+xlabel_pos, y =-plotTop*0.02,srt=45, adj=1, labels=wealth_quantile_set,xpd=TRUE)
segments(barCenters, t(for_commault_BPP_plot-for_commault_BPP_plot_se*1.96),
         barCenters,
         t(for_commault_BPP_plot+for_commault_BPP_plot_se*1.96), lwd=1.5)
arrows(barCenters, t(for_commault_BPP_plot-for_commault_BPP_plot_se*1.96),
       barCenters,
       t(for_commault_BPP_plot+for_commault_BPP_plot_se*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
legend(8, plotTop, legend=this_legend, fill=this_colors,bty="n")
#dev.copy(png, paste(figures_dir, "MPXBy",category_for_save,tag,".png",sep=""))
#dev.copy(pdf, paste(figures_dir, "MPXBy",category_for_save,tag,".pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "MPXBy",category_for_save,tag,".svg",sep=""))
dev.off()