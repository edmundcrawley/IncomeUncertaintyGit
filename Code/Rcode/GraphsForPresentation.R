
#################################################################################
# 
# This file makes graphs for the presentation
# Nicer colors and larger fonts
#
# 
###############################################################################

# Set folders
moments_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/ServerRcode/ServerOutput/"
figures_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode/Figures/"
PythonResults_folder = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/PrefShockModel/Results/"

tag = "_level_lincome_head"

thickness = 2
plot_width = 9.5
right_mar = 17
left_mar = 7
col_complete = "cyan"
col_solow = "blue"
col_bs = "red"


###############################################################################
# Pull in consumption saving numbers from Python output
FromPython <- scan(paste(PythonResults_folder,'basic_regressions.txt',sep=''), what=double(), sep=",")
FromPython <- scan(paste(PythonResults_folder,'benchmark_br_all.txt',sep=''), what=double(), sep=",")
solow_spending = 0.75
# Now draw graph
#png(paste(figures_dir, "basic_regression_complete_slides",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_complete_slides",tag,".pdf",sep=""),width=plot_width)
#svg(paste(figures_dir, "basic_regression_complete_slides",tag,".svg",sep=""))
par(mar=c(8,left_mar,4,right_mar)+0.1,cex.axis=1.2,cex.lab=1.5,xpd=TRUE)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid', lwd=thickness,col=col_complete,type='o', pch=15, las=1,xlab='N, Years of Growth',ylab='',main='Regressing Consumption Growth on Income Growth')
mtext(TeX('$\\beta^N$'),side=2,las=1,line=4, cex = 1.5)
legend("right", inset=c(-0.6,0), legend=c("Complete Markets","","",""), col=c(col_complete,col_solow,col_bs,"black"),lty=c("solid","solid","solid","solid"),lwd=c(thickness,thickness,thickness,thickness), pch=c(15,16,17,15), pt.cex = 1, cex = 1.3, y.intersp=1.3,bty="n")
dev.off()

#png(paste(figures_dir, "basic_regression_solow_slides",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_solow_slides",tag,".pdf",sep=""),width=plot_width)
#svg(paste(figures_dir, "basic_regression_solow_slides",tag,".svg",sep=""))
par(mar=c(8,left_mar,4,right_mar)+0.1,cex.axis=1.2,cex.lab=1.5,xpd=TRUE)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid', lwd=thickness,col=col_complete,type='o', pch=15, las=1, xlab='N, Years of Growth',ylab='',main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid', lwd=thickness,col=col_solow,type='o', pch=16)
mtext(TeX('$\\beta^N$'),side=2,las=1,line=4, cex = 1.5)
legend("right", inset=c(-0.6,0), legend=c("Complete Markets","Solow","",""), col=c(col_complete,col_solow,col_bs,"black"),lty=c("solid","solid","solid","solid"),lwd=c(thickness,thickness,thickness,thickness), pch=c(15,16,17,15), pt.cex = 1, cex = 1.3, y.intersp=1.3,bty="n")
dev.off()

#png(paste(figures_dir, "basic_regression_BS_slides",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_BS_slides",tag,".pdf",sep=""),width=plot_width)
#svg(paste(figures_dir, "basic_regression_BS_slides",tag,".svg",sep=""))
par(mar=c(8,left_mar,4,right_mar)+0.1,cex.axis=1.2,cex.lab=1.5,xpd=TRUE)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid', lwd=thickness,col=col_complete,type='o', pch=15, las=1, xlab='N, Years of Growth',ylab='',main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid', lwd=thickness,col=col_solow,type='o', pch=16)
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid',col=col_bs, lwd=thickness,type='o', pch=17)
mtext(TeX('$\\beta^N$'),side=2,las=1,line=4, cex = 1.5)
legend("right", inset=c(-0.6,0),legend=c("Complete Markets","Solow","Buffer-Stock",""), col=c(col_complete,col_solow,col_bs,"black"),lty=c("solid","solid","solid","solid"),lwd=c(thickness,thickness,thickness,thickness), pch=c(15,16,17,15), pt.cex = 1, cex = 1.3, y.intersp=1.3,bty="n")
arrows(2, 0.12, 9, 0.12)
arrows(9, 0.12, 2, 0.12)
text(3.5, 0.22, labels = "Relatively more \n transitory variance")
text(7.5, 0.22, labels = "Relatively more \n permanent variance")
dev.off()

#png(paste(figures_dir, "basic_regression_slides",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_slides",tag,".pdf",sep=""),width=plot_width)
#svg(paste(figures_dir, "basic_regression_slides",tag,".svg",sep=""))
par(mar=c(8,left_mar,4,right_mar)+0.1,cex.axis=1.2,cex.lab=1.5,xpd=TRUE)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid', lwd=thickness,col=col_complete,type='o', pch=15, las=1, xlab='N, Years of Growth',ylab='',main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid',col=col_solow, lwd=thickness,type='o', pch=16)
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid',lwd=thickness,col=col_bs,type='o', pch=17)
points(reg_coefs, pch=15,lwd=thickness)
lines(reg_coefs)
lines(reg_coefs+1.96*std_errors,lty='dashed')
lines(reg_coefs-1.96*std_errors,lty='dashed')
mtext(TeX('$\\beta^N$'),side=2,las=1,line=4, cex = 1.5)
legend("right", inset=c(-0.6,0), legend=c("Complete Markets","Solow","Buffer-Stock","Data"), col=c(col_complete,col_solow,col_bs,"black"),lty=c("solid","solid","solid","solid"),lwd=c(thickness,thickness,thickness,thickness), pch=c(15,16,17,15), pt.cex = 1, cex = 1.3, y.intersp=1.3,bty="n")
arrows(2, 0.12, 9, 0.12)
arrows(9, 0.12, 2, 0.12)
text(3.5, 0.22, labels = "Relatively more \n transitory variance")
text(7.5, 0.22, labels = "Relatively more \n permanent variance")
dev.off()

#png(paste(figures_dir, "basic_regression_liquid_wealth_slides",tag,".png",sep=""))
pdf(paste(figures_dir, "basic_regression_liquid_wealth_slides",tag,".pdf",sep=""),width=plot_width)
#svg(paste(figures_dir, "basic_regression_liquid_wealth",tag,".svg",sep=""))
par(mar=c(8,left_mar,4,right_mar)+0.1,cex.axis=1.2,cex.lab=1.5,xpd=TRUE)
plot(array(0.0,dim=dim(reg_coefs)), ylim=c(0,1),lty='solid', lwd=thickness,col=col_complete,type='o', pch=15, las=1, xlab='N, Years of Growth',ylab='',main='Regressing Consumption Growth on Income Growth')
lines(array(solow_spending,dim=dim(reg_coefs)),lty='solid', lwd=thickness,col=col_solow,type='o', pch=16)
lines(FromPython[1:dim(reg_coefs)[1]],lty='solid', lwd=thickness,col=col_bs,type='o', pch=17)
points(reg_coefs,col='gray', pch=15,lwd=thickness)
lines(reg_coefs,col='gray')
lines(reg_coefs+1.96*std_errors,lty='dashed',col='gray')
lines(reg_coefs-1.96*std_errors,lty='dashed',col='gray')
#Add high and low liquid quantiles
points(reg_coefs_liquid_wealth$X1, pch=15,lwd=thickness)
lines(reg_coefs_liquid_wealth$X1)
lines(reg_coefs_liquid_wealth$X1+1.96*std_errors_liquid_wealth$X1,lty='dashed')
lines(reg_coefs_liquid_wealth$X1-1.96*std_errors_liquid_wealth$X1,lty='dashed')
points(reg_coefs_liquid_wealth$X5, pch=15,lwd=thickness)
lines(reg_coefs_liquid_wealth$X5)
lines(reg_coefs_liquid_wealth$X5+1.96*std_errors_liquid_wealth$X5,lty='dashed')
lines(reg_coefs_liquid_wealth$X5-1.96*std_errors_liquid_wealth$X5,lty='dashed')
mtext(TeX('$\\beta^N$'),side=2,las=1,line=4, cex = 1.5)
legend("right", inset=c(-0.6,0), legend=c("Complete Markets","Solow","Buffer-Stock","Data"), col=c(col_complete,col_solow,col_bs,"black"),lty=c("solid","solid","solid","solid"),lwd=c(thickness,thickness,thickness,thickness), pch=c(15,16,17,15), pt.cex = 1, cex = 1.3, y.intersp=1.3,bty="n")
arrows(2, 0.12, 9, 0.12)
arrows(9, 0.12, 2, 0.12)
text(3.5, 0.22, labels = "Relatively more \n transitory variance")
text(7.5, 0.22, labels = "Relatively more \n permanent variance")
text(3.0, 0.92, labels = "Least Liquid", cex=1.3)
text(6, 0.47, labels = "Most Liquid",cex=1.3)
dev.off()

##############################################################################
