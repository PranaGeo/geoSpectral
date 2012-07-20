# TODO: Add comment
# 
# Author: acizmeli
###############################################################################

#Function Implementing the OC3M version 6 (operational MODIS algorithm) 
OC3Mv6_fcn = function(X)
{
	X = log10(X)
	a = c(0.2424, -2.7423, 1.8017, 0.0015, -1.2280)
	Y = 10^(a[1] + a[2]*X + a[3]*X^2 + a[4]*X^3 + a[5]*X^4)
	
	return(Y)
}
#Function Implementing the OC3M version 6 (operational MODIS algorithm) 
OC3Mv6 = function(lw443, lw489, lw510, lw555)
{
	X = pmax(lw443, lw489, lw510)/lw555
	OC3Mv6 = OC3Mv6_fcn(X)
	
	return(OC3Mv6)
}