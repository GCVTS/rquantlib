#include <rquantlib_internal.h>

using namespace QuantLib;


QuantLib::Period tenor(double x);
QuantLib::Period tenor(Rcpp::List& l, QuantLib::Period p = QuantLib::Period(), std::string s = "tenor");

QuantLib::DayCounter day_count(std::string s);
QuantLib::DayCounter day_count(Rcpp::List& l, QuantLib::DayCounter dc = QuantLib::DayCounter(), std::string s = "dayCount");

QuantLib::Rate rate(SEXP& x) {
    if (x != R_NilValue) {
        return Rcpp::as<QuantLib::Rate>(x);
    } else {
        return QuantLib::Null<QuantLib::Rate>();
    }
}


// [[Rcpp::export]]
Rcpp::List vanilla_swap(Rcpp::List swap,
                        SEXP fixedRate,
                        std::vector<QuantLib::Date> dateVec,
                        std::vector<double> zeroVec){

    Rcpp::List swap_iborIndex = Rcpp::as<Rcpp::List>(swap["iborIndex"]);
    Rcpp::List swap_fixedLeg = Rcpp::as<Rcpp::List>(swap["fixedLeg"]);
    Rcpp::List swap_floatingLeg = Rcpp::as<Rcpp::List>(swap["floatingLeg"]);
    Rcpp::List swap_pricingEngine = Rcpp::as<Rcpp::List>(swap["pricingEngine"]);

    // Rebuild yield curve, see Prototypes#139 for hacks
    YieldTermStructure* rebuiltCurve;
    if (dateVec.size() == 1 && zeroVec.size() == 1) {
        rebuiltCurve = new FlatForward(dateVec[0], zeroVec[0], QuantLib::ActualActual());
    } else {
        rebuiltCurve = new InterpolatedZeroCurve<Linear>(dateVec, zeroVec, QuantLib::ActualActual());
    }
    QuantLib::ext::shared_ptr<YieldTermStructure> rebuiltCurve_ptr(rebuiltCurve);
    QuantLib::Handle<YieldTermStructure> yldCrv(rebuiltCurve_ptr);

    // Create IborIndex object for swap
    IborIndex* iborIndex1;
    if (Rcpp::as<std::string>(swap_iborIndex["class"]) == "Euribor") {
        iborIndex1 = new Euribor(tenor(swap_iborIndex), yldCrv);
    } else if (Rcpp::as<std::string>(swap_iborIndex["class"]) == "USDLibor") {
        iborIndex1 = new USDLibor(tenor(swap_iborIndex), yldCrv);
    } else if (Rcpp::as<std::string>(swap_iborIndex["class"]) == "GBPLibor") {
        iborIndex1 = new GBPLibor(tenor(swap_iborIndex), yldCrv);
    } else if (Rcpp::as<std::string>(swap_iborIndex["class"]) == "JPYLibor") {
        iborIndex1 = new JPYLibor(tenor(swap_iborIndex), yldCrv);
    } else if (Rcpp::as<std::string>(swap_iborIndex["class"]) == "Bbsw") {
        iborIndex1 = new Bbsw(tenor(swap_iborIndex), yldCrv);
    } else {
        Rcpp::stop("'swap$iborIndex$class' must be \"Euribor\", \"USDLibor\", \"GBPLibor\", \"JPYLibor\" or \"Bbsw\"");
    }
    QuantLib::ext::shared_ptr<IborIndex> iborIndex1_ptr(iborIndex1);

    // Create swap
    QuantLib::ext::shared_ptr<VanillaSwap> swap1 =
        MakeVanillaSwap(tenor(swap), iborIndex1_ptr, rate(fixedRate))
        .withEffectiveDate(Rcpp::as<QuantLib::Date>(swap["effectiveDate"]))
        .withFixedLegTenor(tenor(swap_fixedLeg))
        .withFixedLegDayCount(day_count(swap_fixedLeg))
        .withFloatingLegTenor(tenor(swap_floatingLeg, iborIndex1_ptr->tenor()))
        .withFloatingLegDayCount(day_count(swap_floatingLeg, iborIndex1_ptr->dayCounter()))
        .receiveFixed(Rcpp::as<bool>(swap["receiveFixed"]));

    // Create pricing engine for swap
    PricingEngine* swapEngine1;
    if (Rcpp::as<std::string>(swap_pricingEngine["class"]) == "DiscountingSwapEngine") {
        swapEngine1 = new DiscountingSwapEngine(yldCrv);
    } else {
        Rcpp::stop("'swap$pricingEngine$class' must be \"DiscountingSwapEngine\"");
    }
    QuantLib::ext::shared_ptr<PricingEngine> swapEngine1_ptr(swapEngine1);


    // Get price
    swap1->setPricingEngine(swapEngine1_ptr);

    Real fixedLegBPS = swap1->fixedLegBPS();
    Real fixedLegNPV = swap1->fixedLegNPV();
    Rate fairRate = swap1->fairRate();
    Real floatingLegBPS = swap1->floatingLegBPS();
    Real floatingLegNPV = swap1->floatingLegNPV();
    Spread fairSpread = swap1->fairSpread();

    return Rcpp::List::create(Rcpp::Named("fixedLegBPS") = fixedLegBPS,
                              Rcpp::Named("fixedLegNPV") = fixedLegNPV,
                              Rcpp::Named("fairRate") = fairRate,
                              Rcpp::Named("floatingLegBPS") = floatingLegBPS,
                              Rcpp::Named("floatingLegNPV") = floatingLegNPV,
                              Rcpp::Named("fairSpread") = fairSpread);
}
