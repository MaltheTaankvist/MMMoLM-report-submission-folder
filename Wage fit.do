clear
cls

/********************************************************************
  Pipeline:  use logw1  →  fit log-normal  →  map 99-th percentile to 40
********************************************************************/

*--------------------------------------------------------------*
* 1.  LOAD the data that hold  logw1                            *
*--------------------------------------------------------------*
cd "C:/Users/Malth/OneDrive/Dokumenter/GitHub/Malthes_Micro-and-macro_folder/Project"
use "BM_data_USoc.dta", clear

*--------------------------------------------------------------*
* 2.  Keep valid observations                                  *
*--------------------------------------------------------------*
drop if missing(logw1)				//Drop missing values
* (optional) drop extreme wages after exponentiating, e.g. > 2e5 DKK
gen double w_raw = exp(logw1)		//Generate values in original format (unlogging)

*--------------------------------------------------------------*
* 3.  Fit Normal(μ,σ²)  directly to  logw1                     *
*--------------------------------------------------------------*
quietly summarize logw1, detail					//Computes sample moment
scalar mu_hat    = r(mean)						//Extract the mean
scalar N         = r(N)							//Extract sample size (not needed for now)
scalar sigma_hat = r(sd)*sqrt((N-1)/N)          //Sample → MLE correction

*--------------------------------------------------------------*
* 4.  Pick scaling  s  so that 99-th pct = 40                  *
*--------------------------------------------------------------*
scalar z99 = invnormal(.99)                     // 99-th-pct z-score ≈ 2.32635 (one-sided)
scalar p99 = exp(mu_hat + sigma_hat*z99)        // wage at empirical 99-th pct
scalar s   = p99 / 40                           // uniform shrink factor

* Parameters on the [0,40] scale
scalar mu_model    = mu_hat - ln(s)				
scalar sigma_model = sigma_hat

* Display the values
display "---------------------------------------------------------"
display "μ̂  (logw1)       = " %9.4f mu_hat
display "σ̂  (logw1)       = " %9.4f sigma_hat
display "scaling  s        = " %9.4f s
display "μ* (model scale)  = " %9.4f mu_model
display "σ* (model scale)  = " %9.4f sigma_model
display "---------------------------------------------------------"

*--------------------------------------------------------------*
* 5.  Histogram + fitted log-normal on the [0,40] scale        *
*--------------------------------------------------------------*
cap drop w_model						// Avoids error if w_model already exists (just to enable running multiple times)
gen double w_model = exp(logw1) / s		// Generates my wage on the new 0..40 scale

local mu  = mu_model					//Create macro to input in the functino in the plot
local sig = sigma_model					//Create macro to input in the function in the plot

*Plots the histogram of the data, and draws f(x) for all x'es 0..40 and plots them in a red line

twoway ///
    (histogram w_model, density start(0) width(0.5) ///
        fcolor(eltblue) lcolor(none)) ///
    (function exp(-(ln(x)-`mu')^2/(2*`sig'^2)) ///
              /(sqrt(2*c(pi))*`sig'*x), ///
        range(0.001 40) lwidth(thick) lcolor(cranberry)) ///
    , ytitle("Density") xtitle("wage (model scale)") ///
      title("Scaled wages with fitted log-normal overlay") ///
      legend(off)
