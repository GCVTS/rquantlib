#include <rquantlib_internal.h>

using namespace QuantLib;


QuantLib::Period tenor(double x) {
    if (x < 0.08333333){
        int n = static_cast<int>(365 * x);
        return Days * n;
    } else if (x < 1.0){
        int n = static_cast<int>(12 * x);
        return Months * n;
    } else {
        int n = static_cast<int>(x);
        return Years * n;
    }
}

QuantLib::Period tenor(Rcpp::List& l, std::string s = "tenor") {
    if (l[s] == R_NilValue) {
        return QuantLib::Period();
    } else {
        return tenor(Rcpp::as<double>(l[s]));
    }
}

QuantLib::DayCounter day_count(std::string s) {
    if (s == "Actual360") {
        return Actual360();
    } else if (s == "Thirty360") {
        return Thirty360(Thirty360::BondBasis);
    } else if (s == "Actual365Fixed") {
        return Actual365Fixed();
    } else {
        Rcpp::stop("'dayCount' must be \"Actual360\", \"Thirty360\" or \"Actual365Fixed\"");
    }
}

QuantLib::DayCounter day_count(Rcpp::List& l, std::string s = "dayCount") {
    if (l[s] == R_NilValue) {
        return QuantLib::DayCounter();
    } else {
        return day_count(Rcpp::as<std::string>(l[s]));
    }
}


// [[Rcpp::export]]
Rcpp::List black_style_swaption(Rcpp::List call,
                                Rcpp::List put,
                                Rcpp::List exercise,
                                double strike,
                                double vol,
                                std::string volType,
                                std::vector<QuantLib::Date> dateVec,
                                std::vector<double> zeroVec){

    Rcpp::List call_iborIndex = Rcpp::as<Rcpp::List>(call["iborIndex"]);
    Rcpp::List call_fixedLeg = Rcpp::as<Rcpp::List>(call["fixedLeg"]);
    Rcpp::List call_floatingLeg = Rcpp::as<Rcpp::List>(call["floatingLeg"]);
    Rcpp::List call_pricingEngine = Rcpp::as<Rcpp::List>(call["pricingEngine"]);
    Rcpp::List put_iborIndex = Rcpp::as<Rcpp::List>(put["iborIndex"]);
    Rcpp::List put_fixedLeg = Rcpp::as<Rcpp::List>(call["fixedLeg"]);
    Rcpp::List put_floatingLeg = Rcpp::as<Rcpp::List>(call["floatingLeg"]);
    Rcpp::List put_pricingEngine = Rcpp::as<Rcpp::List>(put["pricingEngine"]);
    
    QuantLib::Handle<QuantLib::YieldTermStructure> yldCrv(rebuildCurveFromZeroRates(dateVec, zeroVec));

    // Create IborIndex objects for swaps
    IborIndex* iborIndex1;
    if (Rcpp::as<std::string>(call_iborIndex["class"]) == "Euribor") {
        iborIndex1 = new Euribor(tenor(call_iborIndex), yldCrv);
    } else if (Rcpp::as<std::string>(call_iborIndex["class"]) == "USDLibor") {
        iborIndex1 = new USDLibor(tenor(call_iborIndex), yldCrv);
    } else {
        Rcpp::stop("'call$iborIndex$class' must be \"Euribor\" or \"USDLibor\"");
    }
    QuantLib::ext::shared_ptr<IborIndex> iborIndex1_ptr(iborIndex1);

    IborIndex* iborIndex2;
    if (Rcpp::as<std::string>(put_iborIndex["class"]) == "Euribor") {
        iborIndex2 = new Euribor(tenor(put_iborIndex), yldCrv);
    } else if (Rcpp::as<std::string>(put_iborIndex["class"]) == "USDLibor") {
        iborIndex2 = new USDLibor(tenor(put_iborIndex), yldCrv);
    } else {
        Rcpp::stop("'put$iborIndex$class' must be \"Euribor\" or \"USDLibor\"");
    }
    QuantLib::ext::shared_ptr<IborIndex> iborIndex2_ptr(iborIndex2);

    // Create swaps
    QuantLib::ext::shared_ptr<VanillaSwap> underlyingCall =
        MakeVanillaSwap(tenor(call), iborIndex1_ptr, strike)
        .withEffectiveDate(Rcpp::as<QuantLib::Date>(call["effectiveDate"]))
        .withFixedLegTenor(tenor(call_fixedLeg))
        .withFixedLegDayCount(day_count(call_fixedLeg))
        .withFloatingLegTenor(tenor(call_floatingLeg))
        .withFloatingLegDayCount(day_count(call_floatingLeg))
        .receiveFixed(Rcpp::as<bool>(call["receiveFixed"]));

    QuantLib::ext::shared_ptr<VanillaSwap> underlyingPut =
        MakeVanillaSwap(tenor(put), iborIndex2_ptr, strike)
        .withEffectiveDate(Rcpp::as<QuantLib::Date>(put["effectiveDate"]))
        .withFixedLegTenor(tenor(put_fixedLeg))
        .withFixedLegDayCount(day_count(put_fixedLeg))
        .withFloatingLegTenor(tenor(put_floatingLeg))
        .withFloatingLegDayCount(day_count(put_floatingLeg))
        .receiveFixed(Rcpp::as<bool>(put["receiveFixed"]));

    // Create pricing engines for swaps
    PricingEngine* swapEngine1;
    if (Rcpp::as<std::string>(call_pricingEngine["class"]) == "DiscountingSwapEngine") {
        swapEngine1 = new DiscountingSwapEngine(yldCrv);
    } else {
        Rcpp::stop("'call$pricingEngine$class' must be \"DiscountingSwapEngine\"");
    }
    QuantLib::ext::shared_ptr<PricingEngine> swapEngine1_ptr(swapEngine1);

    PricingEngine* swapEngine2;
    if (Rcpp::as<std::string>(put_pricingEngine["class"]) == "DiscountingSwapEngine") {
        swapEngine2 = new DiscountingSwapEngine(yldCrv);
    } else {
        Rcpp::stop("'put$pricingEngine$class' must be \"DiscountingSwapEngine\"");
    }
    QuantLib::ext::shared_ptr<PricingEngine> swapEngine2_ptr(swapEngine2);

    // Get atm fwd rate
    underlyingCall->setPricingEngine(swapEngine1_ptr);
    underlyingPut->setPricingEngine(swapEngine2_ptr);

    Real rate = underlyingCall->fairRate();
    
    // Create Exercise object for swaptions
    Exercise* exercise1;
    if (Rcpp::as<std::string>(exercise["class"]) == "EuropeanExercise") {
        exercise1 = new EuropeanExercise(Rcpp::as<QuantLib::Date>(exercise["date"]));
    } else {
        Rcpp::stop("'exercise$class' must be \"EuropeanExercise\"");
    }
    QuantLib::ext::shared_ptr<Exercise> exercise1_ptr(exercise1);

    // Create swaptions
    Swaption swaptionC(underlyingCall, exercise1_ptr);
    Swaption swaptionP(underlyingPut, exercise1_ptr);

    // Create pricing engine for swaptions
    PricingEngine* swaptionEngine1;
    if (volType == "ShiftedLognormal") {
        swaptionEngine1 = new BlackSwaptionEngine(yldCrv, vol);
    } else if (volType == "Normal") {
        swaptionEngine1 = new BachelierSwaptionEngine(yldCrv, vol);
    } else {
        Rcpp::stop("'volType' must be \"ShiftedLognormal\" or \"Normal\"");
    }
    QuantLib::ext::shared_ptr<PricingEngine> swaptionEngine1_ptr(swaptionEngine1);

    // Get prices
    swaptionC.setPricingEngine(swaptionEngine1_ptr);
    swaptionP.setPricingEngine(swaptionEngine1_ptr);

    Real pricePay = swaptionC.NPV();
    Real priceRcv = swaptionP.NPV();

    return Rcpp::List::create(Rcpp::Named("pay") = pricePay,
                              Rcpp::Named("rcv") = priceRcv,
                              Rcpp::Named("sigma") = vol,
                              Rcpp::Named("atmRate") = rate);
}
