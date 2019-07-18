BlackStyleSwaption <- function(ts, leg1, leg2, exercise, strike, vol, volType) {
    UseMethod("BlackStyleSwaption")
}

BlackStyleSwaption.default  <- function(
    ts,
    leg1 = list(
        class = "VanillaSwap",
        iborIndex = list(
            class = "Euribor",  # or "USDLibor"
            tenor = 6 * Period$Months
        ),
        pricingEngine = list(
            class = "DiscountingSwapEngine"
        ),
        tenor = 10 * Period$Years,
        effectiveDate,
        receiveFixed = FALSE
    ),
    leg2 = list(
        class = "VanillaSwap",
        iborIndex = list(
            class = "Euribor",  # or "USDLibor"
            tenor = 6 * Period$Months
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
        stop("'ts' must be DiscountCurve")
    }

    if (!is.list(leg1) || length(leg1) == 0) {
        stop("'leg1' must be a non-empty list")
    }
    if(is.null(leg1$class)){
        leg1$class = "VanillaSwap"
        warning("'leg1$class' not set, defaulting to \"VanillaSwap\"")
    }
    if(!(leg1$class %in% c("VanillaSwap"))){
        stop("'leg1$class' must be \"VanillaSwap\"")
    }
    if (!is.list(leg1$iborIndex) || length(leg1$iborIndex) == 0) {
        stop("'leg1$iborIndex' must be a non-empty list")
    }
    if(is.null(leg1$iborIndex$class)){
        leg1$iborIndex$class = "Euribor"
        warning("'leg1$iborIndex$class' not set, defaulting to \"Euribor\"")
    }
    if(!(leg1$iborIndex$class %in% c("Euribor", "USDLibor"))){
        stop("'leg1$iborIndex$class' must be \"Euribor\" or \"USDLibor\"")
    }
    if(is.null(leg1$iborIndex$tenor)){
        leg1$iborIndex$tenor = 6 * Period$Months
        warning("'leg1$iborIndex$tenor' not set, defaulting to 6 months")
    }
    if(!is.numeric(leg1$iborIndex$tenor)){
        stop("'leg1$iborIndex$tenor' must be numeric")
    }
    if (!is.list(leg1$pricingEngine) || length(leg1$pricingEngine) == 0) {
        stop("'leg1$pricingEngine' must be a non-empty list")
    }
    if(is.null(leg1$pricingEngine$class)){
        leg1$pricingEngine$class = "DiscountingSwapEngine"
        warning("'leg1$pricingEngine$class' not set, defaulting to \"DiscountingSwapEngine\"")
    }
    if(!(leg1$pricingEngine$class %in% c("DiscountingSwapEngine"))){
        stop("'leg1$pricingEngine$class' must be \"DiscountingSwapEngine\"")
    }
    if(is.null(leg1$tenor)){
        leg1$tenor = 10 * Period$Years
        warning("'leg1$tenor' not set, defaulting to 10 years")
    }
    if(!is.numeric(leg1$tenor)){
        stop("'leg1$tenor' must be numeric")
    }
    if(is.null(leg1$effectiveDate)){
        stop("'leg1$effectiveDate' not set")
    }
    if(class(leg1$effectiveDate) != "Date"){
        stop("'leg1$effectiveDate' must be Date")
    }
    if(is.null(leg1$receiveFixed)){
        leg1$receiveFixed = FALSE
        warning("'leg1$receiveFixed' not set, defaulting to FALSE")
    }
    if(!is.logical(leg1$receiveFixed)){
        stop("'leg1$receiveFixed' must be logical")
    }

    if (!is.list(leg2) || length(leg2) == 0) {
        stop("'leg2' must be a non-empty list")
    }
    if(is.null(leg2$class)){
        leg2$class = "VanillaSwap"
        warning("'leg2$class' not set, defaulting to \"VanillaSwap\"")
    }
    if(!(leg2$class %in% c("VanillaSwap"))){
        stop("'leg2$class' must be \"VanillaSwap\"")
    }
    if (!is.list(leg2$iborIndex) || length(leg2$iborIndex) == 0) {
        stop("'leg2$iborIndex' must be a non-empty list")
    }
    if(is.null(leg2$iborIndex$class)){
        leg2$iborIndex$class = "Euribor"
        warning("'leg2$iborIndex$class' not set, defaulting to \"Euribor\"")
    }
    if(!(leg2$iborIndex$class %in% c("Euribor", "USDLibor"))){
        stop("'leg2$iborIndex$class' must be \"Euribor\" or \"USDLibor\"")
    }
    if(is.null(leg2$iborIndex$tenor)){
        leg2$iborIndex$tenor = 6 * Period$Months
        warning("'leg2$iborIndex$tenor' not set, defaulting to 6 months")
    }
    if(!is.numeric(leg2$iborIndex$tenor)){
        stop("'leg2$iborIndex$tenor' must be numeric")
    }
    if (!is.list(leg2$pricingEngine) || length(leg2$pricingEngine) == 0) {
        stop("'leg2$pricingEngine' must be a non-empty list")
    }
    if(is.null(leg2$pricingEngine$class)){
        leg2$pricingEngine$class = "DiscountingSwapEngine"
        warning("'leg2$pricingEngine$class' not set, defaulting to \"DiscountingSwapEngine\"")
    }
    if(!(leg2$pricingEngine$class %in% c("DiscountingSwapEngine"))){
        stop("'leg2$pricingEngine$class' must be \"DiscountingSwapEngine\"")
    }
    if(is.null(leg2$tenor)){
        leg2$tenor = 10 * Period$Years
        warning("'leg2$tenor' not set, defaulting to 10 years")
    }
    if(!is.numeric(leg2$tenor)){
        stop("'leg2$tenor' must be numeric")
    }
    if(is.null(leg2$effectiveDate)){
        stop("'leg2$effectiveDate' not set")
    }
    if(class(leg2$effectiveDate) != "Date"){
        stop("'leg2$effectiveDate' must be Date")
    }
    if(is.null(leg2$receiveFixed)){
        leg2$receiveFixed = TRUE
        warning("'leg2$receiveFixed' not set, defaulting to TRUE")
    }
    if(!is.logical(leg2$receiveFixed)){
        stop("'leg2$receiveFixed' must be logical")
    }

    if (!is.list(exercise) || length(exercise) == 0) {
        stop("'exercise' must be a non-empty list")
    }
    if(is.null(exercise$class)){
        exercise$class = "EuropeanExercise"
        warning("'exercise$class' not set, defaulting to \"EuropeanExercise\"")
    }
    if(!(exercise$class %in% c("EuropeanExercise"))){
        stop("'exercise$class' must be \"EuropeanExercise\"")
    }
    if(is.null(exercise$date)){
        stop("'exercise$date' not set")
    }
    if(class(exercise$date) != "Date"){
        stop("'exercise$date' must be Date")
    }
   
    if(!is.numeric(strike)){
        stop("'strike' must be numeric")
    }
    
    if(!is.numeric(vol)){
        stop("'vol' must be numeric")
    }
    
    if(!(volType %in% c("ShiftedLognormal", "Normal"))){
        stop("'volType' must be \"ShiftedLognormal\" or \"Normal\"")
    }
    
    val <- black_style_swaption(leg1, leg2, exercise, strike, vol, volType,
                                ts$table$date, ts$table$zeroRates) 
    val$params = list(
        ts = ts,
        leg1 = leg1,
        leg2 = leg2,
        exercise = exercise,
        strike = strike,
        vol = vol,
        volType = volType
    )
    class(val) <- "BlackStyleSwaption"
    val
}
