## Import Federal and State tax bracket information 
## (working directory must be set accordingly)

fedTax <- read.csv("FedTaxRates.csv")
njTax <- read.csv("NJTaxRates.csv")


## Define affodability function

afford <- function(inc, expenses, homepay) {
        
        ## inc = Annual Income
        ## expenses = monthly expenses, excluding housing
        ## homepay = monthly home expenses, e.g. mortgate + property taxes + 
        ##      HOA + insurance
        
        if (inc > fedTax[1,1] & inc < fedTax[2,1]) {
                fed <- fedTax[1,3] + inc * fedTax[1,4]
        } else if (inc > fedTax[2,1] & inc < fedTax[3,1]) {
                fed <- fedTax[2,3] + (inc - fedTax[2,1]) * fedTax[2,4]
        } else if (inc > fedTax[3,1] & inc < fedTax[4,1]) {
                fed <- fedTax[3,3] + (inc - fedTax[3,1]) * fedTax[3,4]
        } else if (inc > fedTax[4,1] & inc < fedTax[5,1]) {
                fed <- fedTax[4,3] + (inc - fedTax[4,1]) * fedTax[4,4]
        } else if (inc > fedTax[5,1] & inc < fedTax[6,1]) {
                fed <- fedTax[5,3] + (inc - fedTax[5,1]) * fedTax[5,4]
        } else if (inc > fedTax[6,1] & inc < fedTax[7,1]) {
                fed <- fedTax[6,3] + (inc - fedTax[6,1]) * fedTax[6,4]
        } else if (inc > fedTax[7,1]) {
                fed <- fedTax[7,3] + (inc - fedTax[7,1]) * fedTax[7,4]
        }
         
        
        if (inc > njTax[1,1] & inc < njTax[2,1]) {
                state <- njTax[1,3] + inc * njTax[1,4]
        } else if (inc > njTax[2,1] & inc < njTax[3,1]) {
                state <- njTax[2,3] + (inc - njTax[2,1]) * njTax[2,4]
        } else if (inc > njTax[3,1] & inc < njTax[4,1]) {
                state <- njTax[3,3] + (inc - njTax[3,1]) * njTax[3,4]
        } else if (inc > njTax[4,1] & inc < njTax[5,1]) {
                state <- njTax[4,3] + (inc - njTax[4,1]) * njTax[4,4]
        } else if (inc > njTax[5,1] & inc < njTax[6,1]) {
                state <- njTax[5,3] + (inc - njTax[5,1]) * njTax[5,4]
        } else if (inc > njTax[6,1]) {
                state <- njTax[6,3] + (inc - njTax[6,1]) * njTax[6,4]
        } 
        
        
        save <- inc * .1
        retire <- inc * .1
        
        num <- expenses + homepay + fed/12 + state/12 + save/12 + retire/12
        
        den <- inc/12
        
        resvec <- c(num/den, inc/12, homepay, expenses, fed/12, state/12, 
                    save/12, retire/12)
        
        roundvec <- function(x) (as.numeric(round(x, 2)))
        
        resvec <- sapply(resvec, roundvec)
        
        labvec <- c("Net Affordability", "Monthly Income", "Monthly Home", 
                    "Monthly Expenses", "Monthly Fed Tax", "Monthly State Tax", 
                    "Monthly Savings", "Monthly Retirement")
        
        res <- as.data.frame(cbind(labvec, resvec))
        names(res) <- c("Variable", "Value")
        
        res
}