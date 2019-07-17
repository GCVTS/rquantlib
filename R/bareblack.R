BareBlackSwaption <- function(params,
                         ts, 
                         legparams=list(floatFreq="Semiannual")) {

    UseMethod("BareBlackSwaption")
}

BareBlackSwaption.default <- function(params, 
                                 ts,
                                 legparams=list(floatFreq="Semiannual")) {
    # Check that params list names

    if (!is.list(params) || length(params) == 0) {
        stop("The params parameter must be a non-empty list", call.=FALSE)
    }
    if(is.null(params$tradeDate)){
        stop("swaption trade date not set", call.=FALSE)
    }
    if(is.null(params$startDate)){
        params$startDate=advance("UnitedStates",params$tradeDate, 1, 3)
        warning("swaption start date not set, defaulting to 1 year from trade date using US calendar")
    }
    if(is.null(params$expiryDate)){
        params$expiryDate=params$startDate
        warning("swaption expiry date not set, defaulting to 1 year from trade date using US calendar")
    }
    if(is.null(params$maturity)){
        params$maturity=advance("UnitedStates",params$startDate, 5, 3)
        warning("swaption maturity not set, defaulting to 5 years from startDate using US calendar")
    }
    if(is.null(params$strike)){
        stop("swaption strike not set", call.=FALSE)
    }
    if(is.null(params$vol)){
        stop("swaption vol not set", call.=FALSE)
    }
    if(is.null(params$volType)){
        params$volType="ShiftedLognormal"
        warning("swaption volType not set, defaulting to \"ShiftedLognormal\"")
    }
    if(!(params$volType %in% c("Normal", "ShiftedLognormal"))){
        stop("swaption volType must be \"Normal\" or \"ShiftedLognormal\"", call.=FALSE)
    }
    

    if(class(ts)=="DiscountCurve"){
        matchlegs<-matchParams(legparams)
        val <- bareblackengine(params, matchlegs, c(ts$table$date), ts$table$zeroRates) 
    } else{
        stop("DiscountCurve class term structure required", call.=FALSE)
        
    }
    val$params=params
    val$atmRate=as.numeric(val$atmRate)
    class(val) <- "BareBlackSwaption"
    summary(val)
    val
}

