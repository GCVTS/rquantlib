VanillaSwap <- function(ts, swap, fixedRate) {
    UseMethod("VanillaSwap")
}

VanillaSwap.default  <- function(
    ts,
    swap = list(
        iborIndex = list(
            class = "Euribor",  # or "USDLibor"
            tenor = 6 * Period$Months
        ),
        fixedLeg = list(
            tenor = NULL,   # set according to ibor currency and swap tenor
            dayCount = NULL # or "Actual360", "Thirty360", "Actual365Fixed"
        ),
        floatingLeg = list(
            tenor = NULL,   # set according to ibor currency and swap tenor
            dayCount = NULL # or "Actual360", "Thirty360", "Actual365Fixed"
        ),
        pricingEngine = list(
            class = "DiscountingSwapEngine"
        ),
        tenor = 10 * Period$Years,
        effectiveDate = NULL,
        receiveFixed = TRUE
    ),
    fixedRate
){
    if(class(ts)!="DiscountCurve"){
        stop("'ts' must be DiscountCurve", call. = FALSE)
    }

    if (!is.list(swap) || length(swap) == 0) {
        stop("'swap' must be a non-empty list", call. = FALSE)
    }
    if (!is.list(swap$iborIndex)) {
        swap$iborIndex = list()
    }
    if(is.null(swap$iborIndex$class)){
        swap$iborIndex$class = "Euribor"
        warning("'swap$iborIndex$class' not set, defaulting to \"Euribor\"", call. = FALSE)
    }
    if(!(swap$iborIndex$class %in% c("Euribor", "USDLibor"))){
        stop("'swap$iborIndex$class' must be \"Euribor\" or \"USDLibor\"", call. = FALSE)
    }
    if(is.null(swap$iborIndex$tenor)){
        swap$iborIndex$tenor = 6 * Period$Months
        warning("'swap$iborIndex$tenor' not set, defaulting to 6 months", call. = FALSE)
    }
    if(!is.numeric(swap$iborIndex$tenor)){
        stop("'swap$iborIndex$tenor' must be numeric", call. = FALSE)
    }
    if(!is.list(swap$fixedLeg)){
        swap$fixedLeg = list()
    }
    if(!("tenor" %in% names(swap$fixedLeg))){
        swap$fixedLeg <- c(swap$fixedLeg, list(tenor = NULL))
    }
    if(!is.null(swap$fixedLeg$tenor) && !is.numeric(swap$fixedLeg$tenor)){
        stop("'swap$fixedLeg$tenor' must be numeric", call. = FALSE)
    }
    if(!("dayCount" %in% names(swap$fixedLeg))){
        swap$fixedLeg <- c(swap$fixedLeg, list(dayCount = NULL))
    }
    if(!is.null(swap$fixedLeg$dayCount) && !(swap$fixedLeg$dayCount %in% c("Actual360", "Thirty360", "Actual365Fixed"))){
        stop("'swap$fixedLeg$dayCount' must be \"Actual360\", \"Thirty360\" or \"Actual365Fixed\"", call. = FALSE)
    }
    if(!is.list(swap$floatingLeg)){
        swap$floatingLeg = list()
    }
    if(!("tenor" %in% names(swap$floatingLeg))){
        swap$floatingLeg <- c(swap$floatingLeg, list(tenor = NULL))
    }
    if(!is.null(swap$floatingLeg$tenor) && !is.numeric(swap$floatingLeg$tenor)){
        stop("'swap$floatingLeg$tenor' must be numeric", call. = FALSE)
    }
    if(!("dayCount" %in% names(swap$floatingLeg))){
        swap$floatingLeg <- c(swap$floatingLeg, list(dayCount = NULL))
    }
    if(!is.null(swap$floatingLeg$dayCount) && !(swap$floatingLeg$dayCount %in% c("Actual360", "Thirty360", "Actual365Fixed"))){
        stop("'swap$floatingLeg$dayCount' must be \"Actual360\", \"Thirty360\" or \"Actual365Fixed\"", call. = FALSE)
    }
    if (!is.list(swap$pricingEngine)) {
        swap$pricingEngine = list()
    }
    if(is.null(swap$pricingEngine$class)){
        swap$pricingEngine$class = "DiscountingSwapEngine"
        warning("'swap$pricingEngine$class' not set, defaulting to \"DiscountingSwapEngine\"", call. = FALSE)
    }
    if(!(swap$pricingEngine$class %in% c("DiscountingSwapEngine"))){
        stop("'swap$pricingEngine$class' must be \"DiscountingSwapEngine\"", call. = FALSE)
    }
    if(is.null(swap$tenor)){
        swap$tenor = 10 * Period$Years
        warning("'swap$tenor' not set, defaulting to 10 years", call. = FALSE)
    }
    if(!is.numeric(swap$tenor)){
        stop("'swap$tenor' must be numeric", call. = FALSE)
    }
    if(is.null(swap$effectiveDate)){
        swap$effectiveDate = advance("UnitedStates", getEvaluationDate(), 1, 3)
        warning("'swap$effectiveDate' not set, defaulting to 1 year from evaluation date using US calendar", call. = FALSE)
    }
    if(class(swap$effectiveDate) != "Date"){
        stop("'swap$effectiveDate' must be Date", call. = FALSE)
    }
    if(is.null(swap$receiveFixed)){
        swap$receiveFixed = TRUE
        warning("'swap$receiveFixed' not set, defaulting to TRUE", call. = FALSE)
    }
    if(!is.logical(swap$receiveFixed)){
        stop("'swap$receiveFixed' must be logical", call. = FALSE)
    }

    if(!is.numeric(fixedRate)){
        stop("'fixedRate' must be numeric", call. = FALSE)
    }
    
    val <- vanilla_swap(swap, fixedRate, ts$table$date, ts$table$zeroRates) 
    val$params = list(
        ts = ts,
        swap = swap,
        fixedRate = fixedRate
    )
    class(val) <- "VanillaSwap"
    val
}
