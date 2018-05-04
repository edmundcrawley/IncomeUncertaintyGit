
###############################################################################
#Set folders and load code
Rcode_folder = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/Rcode/BPPLikeCarrollSamwick/"
empirical_input_folder = "E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/save/"
library(zoo)
source(paste(Rcode_folder,"BPPLikeCarrollSamwick.r",sep=""))
source(paste(Rcode_folder,"min_distance_CS.r",sep=""))

# Choose which input data to use
#empirical_input_file="Input_for_R_fullsample.csv"
empirical_input_file="input_for_R_5percentsample.csv"

#load the data
raw_data = read.csv(paste(empirical_input_folder,empirical_input_file,sep=""), sep=",")
year_col = 2
#drop years 2014 and 2015 if required
# raw_data = raw_data[raw_data[,year_col]<2014,]
#format input data as a martrix
all_data<- as.matrix(raw_data[,1:9]) 

#create moments
moments <- create_moments_CS(all_data)
c_vector <- moments[["c_vector"]]
omega <- moments[["omega"]]

###############################################################################

age_col = 10
T  <- max(all_data[,year_col])-min(all_data[,year_col])+1 
y      <-nrow(all_data) 

#Next replicate BPP
CS_output = CS_parameter_estimation(c_vector, omega, T) 

#Now repeat excercise by age
#First set age constant as value in 2010
age2010 = array(1:y, dim=c(y,1))*0.0
for (k in 0:((y/T)-1)){
	i <- k*T
	age2010[(i+1):(i+T)] = raw_data[(i+7),age_col]
    }
age2010[is.na(age2010)] = 0

age_set = 25:70
age_params = array(0, dim=c(length(age_set),4))
age_se = array(0, dim=c(length(age_set),4))
age_obs = array(0, dim=c(length(age_set)))
age_total_var = array(0, dim=c(length(age_set)))
for (i in 1:length(age_set)){
	this_age = age_set[i]
	this_data = all_data[age2010==this_age,]
	this_moments <- create_moments_CS(this_data)
	this_c_vector <- this_moments[["c_vector"]]
	this_omega <- this_moments[["omega"]]
	this_CS_output = CS_parameter_estimation(this_c_vector, this_omega, T) 
	age_params[i,1] = this_CS_output$var_perm
	age_params[i,2] = this_CS_output$var_tran
	age_params[i,3] = this_CS_output$ins_perm
	age_params[i,4] = this_CS_output$ins_tran
	age_se[i,1] = this_CS_output$var_perm_se
	age_se[i,2] = this_CS_output$var_tran_se
	age_se[i,3] = this_CS_output$ins_perm_se
	age_se[i,4] = this_CS_output$ins_tran_se
	age_obs[i] = nrow(this_data)/T
	age_total_var[i] = this_moments$delta_y_var
}

png(filename="E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/figures/VarianceByAge.png")
plot(age_set, age_params[,1],col="green",main="Permanent and Transitory Variance by Age",xlab="Age",ylab="Shock Variance",ylim=c(0,0.04))
points(age_set, age_params[,2],col="red")
points(age_set, age_total_var,col="black")
lines(age_set, rollmean(age_params[,1],5,fill=NA), col="green")
lines(age_set, rollmean(age_params[,2],5,fill=NA), col="red")
lines(age_set, rollmean(age_total_var,5,fill=NA), col="black")
lines(age_set, rollmean(2.0/3.0*age_params[,1]+2.0*age_params[,2],5,fill=NA), col="black",lty="dashed")
legend(40, 0.035, legend=c("Permanent Var", "Transitory Var", expression(paste("var(",Delta,"y)")),expression(paste("Model implied var(",Delta,"y)"))), col=c("green","red","black","black"),lty=c("solid","solid","solid","dashed"))
dev.off()

png(filename="E:/ProjektDB/706172/Workdata/706172/Husholdningsprojekt/Precautionary saving with time varying risk/Edmund/BPP/figures/MPXByAge.png")
plot(age_set, age_params[,3],col="green",main="Permanent and Transitory MPX by Age",xlab="Age",ylab="MPX",ylim=c(0,1))
points(age_set, age_params[,4],col="red")
lines(age_set, rollmean(age_params[,3],5,fill=NA), col="green")
lines(age_set, rollmean(age_params[,4],5,fill=NA), col="red")
legend(38, 0.24, legend=c("Permanent MPX", "Transitory MPX"), col=c("green","red"),lty=c("solid","solid"))
dev.off()


