payment <- function(p, mortgage, tax=p*.014, hoa=300, ins=80, pmi=.005) {
        
        ## tax = annual tax
        ## hoa = HomeOwners Association fees, monthly
        ## ins = HomeOwners Insurance, monthly
        ## pmi = Property Mortgage Insurance, monthly
        
        mortgage <- function(p, int = 0.04, term = 30, compound = 12, down = .20) {
                
                ## mortgage calculates the smoothed monthly mortgage payment based on
                ## the variables below
                
                ## p = present value purchase price
                ## int = interest rate
                ## term = duration of loan repayment in years
                ## compound = compounding factor
                ## down = % of purchase price as down payment
                
                loan <- p - (p * down)
                
                res <- (loan * (int*(1 + (int/compound))^(compound * term)) / 
                                ((1 + (int/compound))^(compound * term) - 1)) / 12
        }
        
        tot <- mortgage + tax/12 + hoa + ins + pmi*loan
        
        tot
}