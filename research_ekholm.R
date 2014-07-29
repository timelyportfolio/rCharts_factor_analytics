require(gdata)
require(magrittr)

#http://www.andersekholm.fi/selection_timing/
#this is the excel file provided as an illustration  
test_data <- read.xls(
  "Ekholm_Selectivity_Timing.xlsx"
  ,sheet = 4
  ,pattern = "RND*"
) %>%
  #in my Excel sheet I removed % format to increase # of decimals
  #but if you have not done this then you might
  #want to remove the % and divide by 100 to make numeric
  lapply(
    FUN=function(col){
      if(col[1] %>% grepl( x = ., pattern = "%")){
        gsub(col,pattern="%",replacement="") %>%
          as.numeric / 100
      } else col
    }
  ) %>%
  do.call( cbind, .) %>%
  data.frame

# let's just get the columns we need
# to confirm that we can perform the calculation in R
return_data <- test_data[, c(2,7)] 


jensen_lm <- lm( Rp.Rf ~ Rm.Rf, data = return_data )
# so looks like the basic linear model matches
# Alpha -0.009565 in cell E4
# Beta - 0.987492 in cell E5
jensen_lm %>% coefficients

# now time to check the new calculations
jensen_lm %>%
  .$residuals %>%
  . ^ 2 %>%
  data.frame(
    return_data[,1:2]
    , fitted_sq = .
    , Rm.Rf_sq = return_data[,1] ^ 2
  ) %T>% assign(x = "return_data", value = ., pos = .GlobalEnv) %>%
  lm( fitted_sq ~ Rm.Rf_sq , data = . ) %>%
  coefficients %>% . ^ (1/2) %>%
  t %>%
  data.frame %>% 
  set_colnames(c("ActiveAlpha", "ActiveBeta")) %>%
  data.frame(
    .
    , "SelectionShare" = .$ActiveAlpha ^ 2 / (var(return_data$Rp.Rf) * (nrow(return_data) - 1) / nrow(return_data))
    , "TimingShare" = .$ActiveBeta ^ 2* mean( return_data$Rm.Rf_sq ) / (var(return_data$Rp.Rf) * (nrow(return_data) - 1) / nrow(return_data))
    
  ) -> ekholm

# then to make sure R2 + SelectionShare + TimingShare = 100%
summary(jensen_lm)$r.squared + ekholm[1,3] + ekholm[1,4]
