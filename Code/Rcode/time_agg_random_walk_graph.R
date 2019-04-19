# Draw a time aggregated random walk (4 subperiods)

library(ggplot2)
library(gridExtra)

figures_dir = "C:/Users/edmun/OneDrive/Documents/Research/Denmark/IncomeUncertaintyGit/Code/Rcode/Figures/"
par(cex.axis=1.2,cex.lab=1.5)
# 
# N_sub =50
# N_period =7
# N_subperiods =N_sub*N_period
# time = (1:(N_subperiods))/N_sub
# #shocks = rnorm(N_subperiods,0,1)
# underlying = cumsum(shocks)-mean(cumsum(shocks))
# time_agg = underlying*0.0
# for (T in 1:N_period){
#   t = (T-1)*N_sub+1
#   time_agg[t:(t+N_sub-1)]=mean(underlying[t:(t+N_sub-1)])
# }
# 
# plot(time, underlying, type="l")
# plot(time, time_agg)
# 
# time_agg_func=stepfun(time[1:(N_subperiods-1)], time_agg)
# underlying_func=stepfun(time[1:(N_subperiods-1)], underlying)
# 
# plot(time_agg_func, time, xlim= c(0,N_period),ylim=c(min(underlying),max(underlying)),col="red", col.points=FALSE, verticals=TRUE,lwd=4,xlab="Time",ylab="Income",main="Time Aggregated Random Walk")
# lines(underlying_func, time, xlim= c(0,N_period), col.points=FALSE, verticals=TRUE)
# dev.print(pdf, paste(figures_dir,'timeagg_rw.pdf'))

# #Now do MA(2) plot
# theta1 = 0.35
# theta2 = 0.1
# 
# impulse_response_MA2=stepfun(0:4, c(0,1,theta1,theta2,0,0))
# plot(impulse_response_MA2, 0:4, xlim= c(0,5),ylim=c(0,1.2),col="red", col.points=FALSE, verticals=TRUE,lwd=4,xlab="Time",ylab="Income",main="MA(2) Impulse Response")
# dev.print(pdf, paste(figures_dir,'MA2.pdf',sep=""))

x = (0:1000)/1000*5
y = 1.2/16.0*(pmin(x-2,0))^4
c = 0.5*0.25*(pmin(x-2,0))^2
cBPP = c*0.0 + 0.2

x=c((-20:0)/(20.0*5),x)
y=c((-20:0)*0.0,y)
c=c((-20:0)*0.0,c)
cBPP=c((-20:0)*0.0,cBPP)

pdf(paste(figures_dir,'GenericTransitory.pdf',sep=""), width=7, height=3)
par(mar=c(5,5,3,3))
plot(x,y, xlim= c(0,5),ylim=c(0,1.2),col="red",type="l",lwd=4,xlab="Time",ylab="Income",main="Generic Transitory Impulse Response, f(t)")
dev.off()
#dev.print(pdf,  paste(figures_dir,'GenericTransitory.pdf',sep=""))

pdf(paste(figures_dir,'GenericTransitoryConsumption.pdf',sep=""), width=7, height=3)
par(mar=c(5,5,3,3))
plot(x,y, xlim= c(0,5),ylim=c(0,1.2),col="red",type="l",lwd=4,xlab="Time",ylab="Income/Consumption",main="Generic Transitory Impulse Responses, f(t) and g(t)")
lines(x,c, xlim= c(0,5),ylim=c(0,1.2),col="black",type="l",lwd=4,xlab="Time",ylab="Income/Consumption",main="Generic Transitory Impulse Response, f(t)")
legend(2.0,1.0, legend=c("Income f(t)", "Consumption g(t)",""),col=c("red","black","green"),lty=1,lwd=4,bty="n")
dev.off()
#dev.print(pdf,  paste(figures_dir,'GenericTransitoryConsumption.pdf',sep=""))

pdf(paste(figures_dir,'GenericTransitoryConsumptionWithBPP.pdf',sep=""), width=7, height=3)
par(mar=c(5,5,3,3))
plot(x,y, xlim= c(0,5),ylim=c(0,1.2),col="red",type="l",lwd=4,xlab="Time",ylab="Income/Consumption",main="Generic Transitory Impulse Responses, f(t) and g(t)")
lines(x,c, xlim= c(0,5),ylim=c(0,1.2),col="black",type="l",lwd=4,xlab="Time",ylab="Income/Consumption",main="Generic Transitory Impulse Response, f(t)")
lines(x,cBPP, xlim= c(0,5),ylim=c(0,1.2),col="green",type="l",lwd=4,xlab="Time",ylab="Income/Consumption",main="Generic Transitory Impulse Response, f(t)")
legend(2.0,1.0, legend=c("Income f(t)", "Consumption g(t)","BPP Random Walk"),col=c("red","black","green"),lty=1,lwd=4,bty="n")
dev.off()
#dev.print(pdf,  paste(figures_dir,'GenericTransitoryConsumptionWithBPP.pdf',sep=""))

#plots for the slides
pdf(paste(figures_dir,'GenericTransitory_slides.pdf',sep=""), width=7, height=3)
par(mar=c(5,5,3,3))
plot(x,y, xlim= c(0,5),ylim=c(0,1.2),col="black",type="l",lwd=4,xlab="Years",ylab="Income",yaxt='n')
legend(1.0,1.0, legend=c("f(t),  Path of Transitory Income Shock", ""),col=c("black","white"),lty=1,lwd=4,bty="n",cex=1)
dev.off()
#dev.print(pdf,  paste(figures_dir,'GenericTransitory_slides.pdf',sep=""))

pdf(paste(figures_dir,'GenericTransitoryConsumption_slides.pdf',sep=""), width=7, height=3)
par(mar=c(5,5,3,3))
plot(x,y, xlim= c(0,5),ylim=c(0,1.2),col="black",type="l",lwd=4,xlab="Years",ylab="Income/Consumption",yaxt='n')
lines(x,c, xlim= c(0,5),ylim=c(0,1.2),col="red",type="l",lwd=4,xlab="Time",ylab="Income/Consumption",yaxt='n')
legend(1.0,1.0, legend=c("f(t),  Path of Transitory Income Shock", "g(t), Path of Consuption Response"),col=c("black","red"),lty=1,lwd=4,bty="n", cex=1)
dev.off()
#dev.print(pdf,  paste(figures_dir,'GenericTransitoryConsumption_slides.pdf',sep=""))

####################################
# plot time aggregation stuff
N_sub =2
N_period =3
N_subperiods =N_sub*N_period
time = (1:(N_subperiods))/N_sub
shocks = c(0,0,0,1,0,0)
underlying = 50000+50000*cumsum(shocks)
time_agg = underlying*0.0
for (T in 1:N_period){
  t = (T-1)*N_sub+1
  time_agg[t:(t+N_sub-1)]=mean(underlying[t:(t+N_sub-1)])
}

time_agg_func=stepfun(time[1:(N_subperiods-1)], time_agg)
underlying_func=stepfun(time[1:(N_subperiods-1)], underlying)

#dev.new()
pdf(paste(figures_dir, "TimeAggExample1_slides.pdf",sep=""))
par(mar=c(5,7,2,2), mfcol=c(1,1),cex.axis=1.5,cex.lab=2)
plot(underlying_func, time, xlim= c(0,N_period),ylim=c(0,110000),col="black",lty="solid",lwd=4, col.points=FALSE, verticals=TRUE,xlab="Time",main="",ylab="",yaxt = "n",las=1)
axis(side = 2, at = c(0,25000,50000,75000,100000), las=1,label=c("$0","$25,000","$50,000","$75,000","$100,000"))
legend(0.3,40000,legend=c("Permanent Income Flow",""),lty=c("solid","dashed"),lwd=c(4,4),col=c("black","white"),bty = "n", cex=1.7)
#dev.copy(pdf, paste(figures_dir, "TimeAggExample1_slides.pdf",sep=""))
dev.off()

#dev.new()
pdf(paste(figures_dir, "TimeAggExample2_slides.pdf",sep=""))
par(mar=c(5,7,2,2), mfcol=c(1,1),cex.axis=1.5,cex.lab=2)
plot(underlying_func, time, xlim= c(0,N_period),ylim=c(0,110000),col="black",lty="solid",lwd=4, col.points=FALSE, verticals=TRUE,xlab="Time",main="",ylab="",yaxt = "n",las=1)
axis(side = 2, at = c(0,25000,50000,75000,100000), las=1,label=c("$0","$25,000","$50,000","$75,000","$100,000"))
lines(time_agg_func, time, xlim= c(0,N_period),ylim=c(0,110000),col="blue",lty="dashed",lwd=4, verticals=TRUE ,col.points=FALSE)
legend(0.3,40000,legend=c("Permanent Income Flow","Observed Annual Income"),lty=c("solid","dashed"),lwd=c(4,4),col=c("black","blue"),bty = "n", cex=1.7)
#dev.copy(pdf, paste(figures_dir, "TimeAggExample2_slides.pdf",sep=""))
dev.off()


######################################################################
# Plot showing how delayed durable response changes with the length of delay
num_points = 1000
x = c(1:num_points)/num_points
y = (2*x-x^2)
x = c(x,x+1)
y = c(y, y*0+1)
y2 = c(y*0+1)
pdf(paste(figures_dir,'DurableBias.pdf',sep=""),width=7, height=3)
par(mar=c(5,5,3,3))
plot(x*12,y,type="l",xlab="Delay, Months",ylab="Bias",xaxt="n",yaxt="n",main=expression(paste("Bias in ",psi," vs Durable Delay")))
lines(x*12,y2,lty="dashed")
axis(side = 1, at = c(0,6,12,18,24))
axis(side = 2, at = c(0,1), labels = c("0",expression(paste(frac(sigma[p]^2,2*sigma[q]^2),phi[d]))), las=1)
#dev.copy(png, paste(figures_dir,'DurableBias.png',sep=""))
dev.off()
#dev.print(pdf,  paste(figures_dir,'DurableBias.pdf',sep=""))



