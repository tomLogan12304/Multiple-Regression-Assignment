#Logan Tom
#Geo 411
#Faithwin Gbadamosi 

getwd()
dir()

sales <- read.csv("homeSales (1).csv")
names(sales)

base.model <- lm(price ~ bedrooms + bathrooms + sqft_living + yr_built + HS + SC, data = sales)

summary(base.model)

plot(base.model)

ResidualVsXPlots <- function(mod.in){
  var.names <- names(mod.in$coefficients)
  n.x.vars <- length(var.names)
  mod.e <- residuals(mod.in)
  
  for (i in 2:n.x.vars){
    plot (mod.in$model[,var.names[i]], mod.e, xlab = var.names[i], ylab = "residuals")
    lines(lowess(mod.in$model[,var.names[i]],mod.e, f=3/4), col="red")
    locator(1)
  }
}  

ResidualVsXPlots(base.model)

XScaleLocationPlots <- function(mod.in){
  var.names <- names(mod.in$coefficients)
  n.x.vars <- length(var.names)
  std.residuals <- sqrt(abs(rstandard(mod.in)))
  
  for (i in 2:n.x.vars){
    plot (mod.in$model[,var.names[i]], std.residuals, xlab = var.names[i], 
          ylab = "Square root of Absolute Standardized Residuals")
    lines(lowess(mod.in$model[,var.names[i]], std.residuals, f=3/4), col="red")
    locator(1)
  }
}

XScaleLocationPlots(base.model)

library(car)

vif(base.model)

