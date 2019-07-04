#include <rquantlib_internal.h>

using namespace QuantLib;
//using namespace boost::unit_test_framework;

using std::fabs;


// [[Rcpp::export]]
Rcpp::List bareblackengine(Rcpp::List rparam,
                      Rcpp::List legParams,
                      std::vector<QuantLib::Date> dateVec,
                      std::vector<double> zeroVec){

    QuantLib::Date tradeDate(Rcpp::as<QuantLib::Date>(rparam["tradeDate"]));
    QuantLib::Date startDate(Rcpp::as<QuantLib::Date>(rparam["startDate"]));
    QuantLib::Date expiryDate(Rcpp::as<QuantLib::Date>(rparam["expiryDate"]));
    QuantLib::Date maturity(Rcpp::as<QuantLib::Date>(rparam["maturity"]));
    double strike = Rcpp::as<double>(rparam["strike"]);
    double vol = Rcpp::as<double>(rparam["vol"]);
    int floatFreq = Rcpp::as<int>(legParams["floatFreq"]);

    // BOOST_TEST_MESSAGE("Testing Markov functional vanilla engines...");

    Settings::instance().evaluationDate() = tradeDate;

    QuantLib::Handle<QuantLib::YieldTermStructure> yldCrv(rebuildCurveFromZeroRates(dateVec, zeroVec));

    QuantLib::ext::shared_ptr<IborIndex> iborIndex1(new Euribor(floatFreq * Months, yldCrv));

    // create swaps for european swaption here to get atm fwd rate, these are ignored for bermudan  //
    QuantLib::ext::shared_ptr<VanillaSwap> underlyingCall =
        MakeVanillaSwap(Years*(((maturity-expiryDate)/365.0)), iborIndex1, strike)
        .withEffectiveDate(expiryDate)
        .receiveFixed(false);

    QuantLib::ext::shared_ptr<VanillaSwap> underlyingPut =
        MakeVanillaSwap(Years*((maturity-expiryDate)/365.0), iborIndex1, strike)
        .withEffectiveDate(expiryDate)
        .receiveFixed(true);
    QuantLib::ext::shared_ptr<PricingEngine> swapEngine(new DiscountingSwapEngine(yldCrv));

    underlyingCall->setPricingEngine(swapEngine);
    underlyingPut->setPricingEngine(swapEngine);

    Real pricePay,priceRcv,rate;
    rate=underlyingCall->fairRate();
    
    // calculate european here //
    QuantLib::ext::shared_ptr<BlackSwaptionEngine> blackSwaptionEngine1(new BlackSwaptionEngine(yldCrv, vol));

    QuantLib::ext::shared_ptr<Exercise> exercise(new EuropeanExercise(startDate));

    //     Rprintf("%d %d %d\n",outputs1.expiries_[i].dayOfMonth(),outputs1.expiries_[i].month(),outputs1.expiries_[i].year());

    Swaption swaptionC(underlyingCall, exercise);
    Swaption swaptionP(underlyingPut, exercise);
    swaptionC.setPricingEngine(blackSwaptionEngine1);
    swaptionP.setPricingEngine(blackSwaptionEngine1);
    pricePay = swaptionC.NPV();
    priceRcv = swaptionP.NPV();

    ////////men at work/////////////////

    return Rcpp::List::create(Rcpp::Named("pay") = pricePay,
                              Rcpp::Named("rcv") = priceRcv,
                              Rcpp::Named("sigma") = vol,
                              Rcpp::Named("atmRate") = rate);
}
