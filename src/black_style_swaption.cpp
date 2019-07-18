#include <rquantlib_internal.h>

using namespace QuantLib;


// [[Rcpp::export]]
Rcpp::List black_style_swaption(Rcpp::List leg1,
                                Rcpp::List leg2,
                                Rcpp::List exercise,
                                double strike,
                                double vol,
                                std::string volType,
                                std::vector<QuantLib::Date> dateVec,
                                std::vector<double> zeroVec){

    Rcpp::List leg1_iborIndex = Rcpp::as<Rcpp::List>(leg1["iborIndex"]);
    Rcpp::List leg1_pricingEngine = Rcpp::as<Rcpp::List>(leg1["pricingEngine"]);
    Rcpp::List leg2_iborIndex = Rcpp::as<Rcpp::List>(leg2["iborIndex"]);
    Rcpp::List leg2_pricingEngine = Rcpp::as<Rcpp::List>(leg2["pricingEngine"]);
    
    QuantLib::Handle<QuantLib::YieldTermStructure> yldCrv(rebuildCurveFromZeroRates(dateVec, zeroVec));

    // Create IborIndex objects for swaps
    IborIndex* iborIndex1;
    if (Rcpp::as<std::string>(leg1_iborIndex["class"]) == "Euribor") {
        iborIndex1 = new Euribor(Rcpp::as<double>(leg1_iborIndex["tenor"]) * Years, yldCrv);
    } else if (Rcpp::as<std::string>(leg1_iborIndex["class"]) == "USDLibor") {
        iborIndex1 = new USDLibor(Rcpp::as<double>(leg1_iborIndex["tenor"]) * Years, yldCrv);
    } else {
        Rcpp::stop("'leg1$iborIndex$class' must be \"Euribor\" or \"USDLibor\"");
    }
    QuantLib::ext::shared_ptr<IborIndex> iborIndex1_ptr(iborIndex1);

    IborIndex* iborIndex2;
    if (Rcpp::as<std::string>(leg2_iborIndex["class"]) == "Euribor") {
        iborIndex2 = new Euribor(Rcpp::as<double>(leg2_iborIndex["tenor"]) * Years, yldCrv);
    } else if (Rcpp::as<std::string>(leg2_iborIndex["class"]) == "USDLibor") {
        iborIndex2 = new USDLibor(Rcpp::as<double>(leg2_iborIndex["tenor"]) * Years, yldCrv);
    } else {
        Rcpp::stop("'leg2$iborIndex$class' must be \"Euribor\" or \"USDLibor\"");
    }
    QuantLib::ext::shared_ptr<IborIndex> iborIndex2_ptr(iborIndex2);

    // Create swaps
    QuantLib::ext::shared_ptr<VanillaSwap> underlyingCall =
        MakeVanillaSwap(Rcpp::as<double>(leg1["tenor"]) * Years, iborIndex1_ptr, strike)
        .withEffectiveDate(Rcpp::as<QuantLib::Date>(leg1["effectiveDate"]))
        .receiveFixed(Rcpp::as<bool>(leg1["receiveFixed"]));

    QuantLib::ext::shared_ptr<VanillaSwap> underlyingPut =
        MakeVanillaSwap(Rcpp::as<double>(leg2["tenor"]) * Years, iborIndex2_ptr, strike)
        .withEffectiveDate(Rcpp::as<QuantLib::Date>(leg2["effectiveDate"]))
        .receiveFixed(Rcpp::as<bool>(leg2["receiveFixed"]));

    // Create pricing engines for swaps
    PricingEngine* swapEngine1;
    if (Rcpp::as<std::string>(leg1_pricingEngine["class"]) == "DiscountingSwapEngine") {
        swapEngine1 = new DiscountingSwapEngine(yldCrv);
    } else {
        Rcpp::stop("'leg1$pricingEngine$class' must be \"DiscountingSwapEngine\"");
    }
    QuantLib::ext::shared_ptr<PricingEngine> swapEngine1_ptr(swapEngine1);

    PricingEngine* swapEngine2;
    if (Rcpp::as<std::string>(leg2_pricingEngine["class"]) == "DiscountingSwapEngine") {
        swapEngine2 = new DiscountingSwapEngine(yldCrv);
    } else {
        Rcpp::stop("'leg2$pricingEngine$class' must be \"DiscountingSwapEngine\"");
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
