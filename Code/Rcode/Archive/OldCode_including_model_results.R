#reload liquid wealth data
###############################################################################
# load liquid weath quintile data and create graph
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


################################################################################
# Get CSTW estimated data from Python
benchmark_results <- read.csv(paste(PythonResults_folder,'benchmark_liquidwealth.txt',sep=''), sep=" ",header=FALSE)
prefshock_results <- read.csv(paste(PythonResults_folder,'prefshock_liquidwealth.txt',sep=''), sep=" ",header=FALSE)

#plot transitory model results
#dev.new()
pdf(paste(figures_dir, "benchmark_tran_denmark.pdf",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
quantile_names = c("1","2","3","4","5")
params = matrix(c(wealth_quantile_params[,4],benchmark_results[,4]),nrow=5,ncol=2)
se = matrix(c(wealth_quantile_se[,4],0,0,0,0,0),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.2)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[2],colors[3]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Transitory MPX by Liquid Wealth Quantile: Model vs Data"),
                      ylab = axis_string, xlab = "Liquid Wealth Quintile", border="black", axes=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02, adj=1, labels=quantile_names,xpd=TRUE)
segments(barCenters, t(params-se*1.96),
         barCenters,
         t(params+se*1.96), lwd=1.5)
arrows(barCenters, t(params-se*1.96),
       barCenters,
       t(params+se*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
legend(2, plotTop, legend=c(expression(paste("Data")),expression(paste("Model"))), fill=c(colors[2],colors[3]),bty="n")
#dev.copy(png, paste(figures_dir, "benchmark_tran_denmark.png",sep=""))
#dev.copy(pdf, paste(figures_dir, "benchmark_tran_denmark.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "benchmark_tran_denmark.svg",sep=""))
dev.off()

#plot transitory model results with preference shock model
#dev.new()
pdf(paste(figures_dir, "prefshock_tran_denmark.pdf",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
quantile_names = c("1","2","3","4","5")
params = matrix(c(wealth_quantile_params[,4],prefshock_results[,4]),nrow=5,ncol=2)
se = matrix(c(wealth_quantile_se[,4],0,0,0,0,0),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.2)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[2],"grey85"),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Transitory MPX by Liquid Wealth Quantile: Model vs Data"),
                      ylab = axis_string, xlab = "Liquid Wealth Quintile", border=NA, axes=TRUE)
params = matrix(c(wealth_quantile_params[,4],benchmark_results[,4]),nrow=5,ncol=2)
se = matrix(c(wealth_quantile_se[,4],0,0,0,0,0),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.2)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[2],colors[3]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Transitory MPX by Liquid Wealth Quantile: Model vs Data"),
                      ylab = axis_string, xlab = "Liquid Wealth Quintile", border="black", axes=TRUE, add=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02, adj=1, labels=quantile_names,xpd=TRUE)
segments(barCenters, t(params-se*1.96),
         barCenters,
         t(params+se*1.96), lwd=1.5)
arrows(barCenters, t(params-se*1.96),
       barCenters,
       t(params+se*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
legend(2, plotTop, legend=c(expression(paste("Data")),expression(paste("Model")),"Model with Preference Shocks"), fill=c(colors[2],colors[3],"grey85"),bty="n")
#dev.copy(png, paste(figures_dir, "prefshock_tran_denmark.png",sep=""))
#dev.copy(pdf, paste(figures_dir, "prefshock_tran_denmark.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "prefshock_tran_denmark.svg",sep=""))
dev.off()


#plot permanent model results
#dev.new()
pdf(paste(figures_dir, "benchmark_perm_denmark.pdf",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
quantile_names = c("1","2","3","4","5")
params = matrix(c(wealth_quantile_params[,3],benchmark_results[,3]),nrow=5,ncol=2)
se = matrix(c(wealth_quantile_se[,3],0,0,0,0,0),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.2)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[1],colors[3]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Permanent MPX by Liquid Wealth Quantile: Model vs Data"),
                      ylab = axis_string, xlab = "Liquid Wealth Quintile", border="black", axes=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02, adj=1, labels=quantile_names,xpd=TRUE)
segments(barCenters, t(params-se*1.96),
         barCenters,
         t(params+se*1.96), lwd=1.5)
arrows(barCenters, t(params-se*1.96),
       barCenters,
       t(params+se*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
legend(2, plotTop, legend=c(expression(paste("Data")),expression(paste("Model"))), fill=c(colors[1],colors[3]),bty="n")
#dev.copy(png, paste(figures_dir, "benchmark_perm_denmark.png",sep=""))
#dev.copy(pdf, paste(figures_dir, "benchmark_perm_denmark.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "benchmark_perm_denmark.svg",sep=""))
dev.off()

#plot permanent model results
#dev.new()
pdf(paste(figures_dir, "prefshock_perm_denmark.pdf",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
quantile_names = c("1","2","3","4","5")
params = matrix(c(wealth_quantile_params[,3],prefshock_results[,3]),nrow=5,ncol=2)
se = matrix(c(wealth_quantile_se[,3],0,0,0,0,0),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.2)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[1],colors[3],"grey85"),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Permanent MPX by Liquid Wealth Quantile: Model vs Data"),
                      ylab = axis_string, xlab = "Liquid Wealth Quintile", border=NA, axes=TRUE)
params = matrix(c(wealth_quantile_params[,3],benchmark_results[,3]),nrow=5,ncol=2)
se = matrix(c(wealth_quantile_se[,3],0,0,0,0,0),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.2)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[1],colors[3]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Permanent MPX by Liquid Wealth Quantile: Model vs Data"),
                      ylab = axis_string, xlab = "Liquid Wealth Quintile", border="black", axes=TRUE, add=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02, adj=1, labels=quantile_names,xpd=TRUE)
segments(barCenters, t(params-se*1.96),
         barCenters,
         t(params+se*1.96), lwd=1.5)
arrows(barCenters, t(params-se*1.96),
       barCenters,
       t(params+se*1.96), lwd=1.5,
       angle=90,code=3, length=0.05)
legend(2, plotTop, legend=c(expression(paste("Data")),expression(paste("Model"))), fill=c(colors[1],colors[3]),bty="n")
#dev.copy(png, paste(figures_dir, "prefshock_perm_denmark.png",sep=""))
#dev.copy(pdf, paste(figures_dir, "prefshock_perm_denmark.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "prefshock_perm_denmark.svg",sep=""))
dev.off()


#plot data compared to model MPC
#dev.new()
pdf(paste(figures_dir, "MPC_accuracy.pdf",sep=""))
par(mar=c(8,7,4,5)+0.1,cex.axis=1.2,cex.lab=1.5)
quantile_names = c("1","2","3","4","5")
params = matrix(c(prefshock_results[,4],prefshock_results[,5]),nrow=5,ncol=2)
plotTop = max(max(wealth_quantile_params[,3:4]), 1.0)
barCenters <- barplot(height=t(params),
                      names.arg=quantile_names,
                      cex.names=0.75,
                      beside=TRUE,col=c(colors[2],colors[3]),
                      las=2,ylim=c(0,plotTop), xaxt="n",
                      main=paste("Empirical Estimates and Model Partial Derivatives"),
                      ylab = axis_string, border="black", axes=TRUE)
text(x=barCenters[1,]+1, y =-plotTop*0.02, adj=1, labels=quantile_names,xpd=TRUE)
legend(2, plotTop, legend=c(expression(paste("Empirical Method")),expression(paste("Model Partial Derivative (6m MPC)"))), fill=c(colors[2],colors[3]),bty="n")
#dev.copy(png, paste(figures_dir, "MPC_accuracy.png",sep=""))
#dev.copy(pdf, paste(figures_dir, "MPC_accuracy.pdf",sep=""))
#dev.copy(svg, paste(figures_dir, "MPC_accuracy.svg",sep=""))
dev.off()


# Unused Robustness
#Net wealth
num_quantiles = 5
#transitory
robustness_plot(tag_list, "moments_by_net_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Transitory MPX by Net Wealth Quintile", "Robust_tranMPX_netwealth", param_col=4, x_label="Quintile")
#permanent
robustness_plot(tag_list, "moments_by_net_wealth_quantile", as.character(1:num_quantiles), tag_list_legend, "Permanent MPX by Net Wealth Quintile", "Robust_permMPX_netwealth", param_col=3, x_label="Quintile")


#NNP
#transitory
robustness_plot(tag_list, "moments_by_NNP_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Transitory MPX by NNP Decile", "Robust_tranMPX_NNP", param_col=4,legend_xpos = 2, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_NNP_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Permanent MPX by NNP Decile", "Robust_permMPX_NNP", param_col=3,legend_xpos = 2, x_label="Decile")

#Income
num_quantiles = 10
#transitory
robustness_plot(tag_list, "moments_by_Income_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Transitory MPX by Income Decile", "Robust_tranMPX_Income", param_col=4, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_Income_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Permanent MPX by Income Decile", "Robust_permMPX_Income", param_col=3,legend_xpos = 6, x_label="Decile")


#MeanCons
num_quantiles = 10
#transitory
robustness_plot(tag_list, "moments_by_MeanCons_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Transitory MPX by Consumption Decile", "Robust_tranMPX_MeanCons", param_col=4, x_label="Decile")
#permanent
robustness_plot(tag_list, "moments_by_MeanCons_quantile", as.character(c(1,4,7,10)), tag_list_legend, "Permanent MPX by Consumption Decile", "Robust_permMPX_MeanCons", param_col=3,legend_xpos = 6, x_label="Decile")

