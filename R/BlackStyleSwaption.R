BlackStyleSwaption <- function(ts, call, put, exercise, strike, vol, volType) {
    UseMethod("BlackStyleSwaption")
}

BlackStyleSwaption.default  <- function(
    ts,
    call = list(
        class = "VanillaSwap",
        iborIndex = list(
            class = "Euribor",  # or "USDLibor"
            tenor = 6 * Period$Months
        ),
        fixedLeg = list(
            tenor = NULL  # set according to ibor currency and swap tenor
        ),
        floatingLeg = list(
            tenor = NULL  # set according to ibor currency and swap tenor
        ),
        pricingEngine = list(
            class = "DiscountingSwapEngine"
        ),
        tenor = 10 * Period$Years,
        effectiveDate,
        receiveFixed = FALSE
    ),
    put = list(
        class = "VanillaSwap",
        iborIndex = list(
            class = "Euribor",  # or "USDLibor"
            tenor = 6 * Period$Months
        ),
        fixedLeg = list(
            tenor = NULL  # set according to ibor currency and swap tenor
        ),
        floatingLeg = list(
            tenor = NULL  # set according to ibor currency and swap tenor
        ),
        pricingEngine = list(
            class = "DiscountingSwapEngine"
        ),
        tenor = 10 * Period$Years,
        effectiveDate,
        receiveFixed = TRUE
    ),
    exercise = list(
        class = "EuropeanExercise",
        date
    ),
    strike,
    vol,
    volType = "ShiftedLognormal"  # or "Normal"
){
    if(class(ts)!="DiscountCurve"){
        stop("'ts' must be DiscountCurve", call. = FALSE)
    }

    if (!is.list(call) || length(call) == 0) {
        stop("'call' must be a non-empty list", call. = FALSE)
    }
    if(is.null(call$class)){
        call$class = "VanillaSwap"
        warning("'call$class' not set, defaulting to \"VanillaSwap\"", call. = FALSE)
    }
    if(!(call$class %in% c("VanillaSwap"))){
        stop("'call$class' must be \"VanillaSwap\"", call. = FALSE)
    }
    if (!is.list(call$iborIndex)) {
        call$iborIndex = list()
    }
    if(is.null(call$iborIndex$class)){
        call$iborIndex$class = "Euribor"
        warning("'call$iborIndex$class' not set, defaulting to \"Euribor\"", call. = FALSE)
    }
    if(!(call$iborIndex$class %in% c("Euribor", "USDLibor"))){
        stop("'call$iborIndex$class' must be \"Euribor\" or \"USDLibor\"", call. = FALSE)
    }
    if(is.null(call$iborIndex$tenor)){
        call$iborIndex$tenor = 6 * Period$Months
        warning("'call$iborIndex$tenor' not set, defaulting to 6 months", call. = FALSE)
    }
    if(!is.numeric(call$iborIndex$tenor)){
        stop("'call$iborIndex$tenor' must be numeric", call. = FALSE)
    }
    if(!is.list(call$fixedLeg)){
        call$fixedLeg = list()
    }
    if(!("tenor" %in% names(call$fixedLeg))){
        call$fixedLeg <- c(call$fixedLeg, list(tenor = NULL))
    }
    if(!is.list(call$floatingLeg)){
        call$floatingLeg = list()
    }
    if(!("tenor" %in% names(call$floatingLeg))){
        call$floatingLeg <- c(call$floatingLeg, list(tenor = NULL))
    }
    if (!is.list(call$pricingEngine)) {
        call$pricingEngine = list()
    }
    if(is.null(call$pricingEngine$class)){
        call$pricingEngine$class = "DiscountingSwapEngine"
        warning("'call$pricingEngine$class' not set, defaulting to \"DiscountingSwapEngine\"", call. = FALSE)
    }
    if(!(call$pricingEngine$class %in% c("DiscountingSwapEngine"))){
        stop("'call$pricingEngine$class' must be \"DiscountingSwapEngine\"", call. = FALSE)
    }
    if(is.null(call$tenor)){
        call$tenor = 10 * Period$Years
        warning("'call$tenor' not set, defaulting to 10 years", call. = FALSE)
    }
    if(!is.numeric(call$tenor)){
        stop("'call$tenor' must be numeric", call. = FALSE)
    }
    if(is.null(call$effectiveDate)){
        stop("'call$effectiveDate' not set", call. = FALSE)
    }
    if(class(call$effectiveDate) != "Date"){
        stop("'call$effectiveDate' must be Date", call. = FALSE)
    }
    if(is.null(call$receiveFixed)){
        call$receiveFixed = FALSE
        warning("'call$receiveFixed' not set, defaulting to FALSE", call. = FALSE)
    }
    if(!is.logical(call$receiveFixed)){
        stop("'call$receiveFixed' must be logical", call. = FALSE)
    }

    if (!is.list(put) || length(put) == 0) {
        stop("'put' must be a non-empty list", call. = FALSE)
    }
    if(is.null(put$class)){
        put$class = "VanillaSwap"
        warning("'put$class' not set, defaulting to \"VanillaSwap\"", call. = FALSE)
    }
    if(!(put$class %in% c("VanillaSwap"))){
        stop("'put$class' must be \"VanillaSwap\"", call. = FALSE)
    }
    if (!is.list(put$iborIndex)) {
        put$iborIndex = list()
    }
    if(is.null(put$iborIndex$class)){
        put$iborIndex$class = "Euribor"
        warning("'put$iborIndex$class' not set, defaulting to \"Euribor\"", call. = FALSE)
    }
    if(!(put$iborIndex$class %in% c("Euribor", "USDLibor"))){
        stop("'put$iborIndex$class' must be \"Euribor\" or \"USDLibor\"", call. = FALSE)
    }
    if(is.null(put$iborIndex$tenor)){
        put$iborIndex$tenor = 6 * Period$Months
        warning("'put$iborIndex$tenor' not set, defaulting to 6 months", call. = FALSE)
    }
    if(!is.numeric(put$iborIndex$tenor)){
        stop("'put$iborIndex$tenor' must be numeric", call. = FALSE)
    }
    if(!is.list(put$fixedLeg)){
        put$fixedLeg = list()
    }
    if(!("tenor" %in% names(put$fixedLeg))){
        put$fixedLeg <- c(put$fixedLeg, list(tenor = NULL))
    }
    if(!is.list(put$floatingLeg)){
        put$floatingLeg = list()
    }
    if(!("tenor" %in% names(put$floatingLeg))){
        put$floatingLeg <- c(put$floatingLeg, list(tenor = NULL))
    }
    if (!is.list(put$pricingEngine)) {
        put$pricingEngine = list()
    }
    if(is.null(put$pricingEngine$class)){
        put$pricingEngine$class = "DiscountingSwapEngine"
        warning("'put$pricingEngine$class' not set, defaulting to \"DiscountingSwapEngine\"", call. = FALSE)
    }
    if(!(put$pricingEngine$class %in% c("DiscountingSwapEngine"))){
        stop("'put$pricingEngine$class' must be \"DiscountingSwapEngine\"", call. = FALSE)
    }
    if(is.null(put$tenor)){
        put$tenor = 10 * Period$Years
        warning("'put$tenor' not set, defaulting to 10 years", call. = FALSE)
    }
    if(!is.numeric(put$tenor)){
        stop("'put$tenor' must be numeric", call. = FALSE)
    }
    if(is.null(put$effectiveDate)){
        stop("'put$effectiveDate' not set", call. = FALSE)
    }
    if(class(put$effectiveDate) != "Date"){
        stop("'put$effectiveDate' must be Date", call. = FALSE)
    }
    if(is.null(put$receiveFixed)){
        put$receiveFixed = TRUE
        warning("'put$receiveFixed' not set, defaulting to TRUE", call. = FALSE)
    }
    if(!is.logical(put$receiveFixed)){
        stop("'put$receiveFixed' must be logical", call. = FALSE)
    }

    if (!is.list(exercise)) {
        exercise = list()
    }
    if(is.null(exercise$class)){
        exercise$class = "EuropeanExercise"
        warning("'exercise$class' not set, defaulting to \"EuropeanExercise\"", call. = FALSE)
    }
    if(!(exercise$class %in% c("EuropeanExercise"))){
        stop("'exercise$class' must be \"EuropeanExercise\"", call. = FALSE)
    }
    if(is.null(exercise$date)){
        stop("'exercise$date' not set", call. = FALSE)
    }
    if(class(exercise$date) != "Date"){
        stop("'exercise$date' must be Date", call. = FALSE)
    }
   
    if(!is.numeric(strike)){
        stop("'strike' must be numeric", call. = FALSE)
    }
    
    if(!is.numeric(vol)){
        stop("'vol' must be numeric", call. = FALSE)
    }
    
    if(!(volType %in% c("ShiftedLognormal", "Normal"))){
        stop("'volType' must be \"ShiftedLognormal\" or \"Normal\"", call. = FALSE)
    }
    
    val <- black_style_swaption(call, put, exercise, strike, vol, volType,
                                ts$table$date, ts$table$zeroRates) 
    val$params = list(
        ts = ts,
        call = call,
        put = put,
        exercise = exercise,
        strike = strike,
        vol = vol,
        volType = volType
    )
    class(val) <- "BlackStyleSwaption"
    val
}
