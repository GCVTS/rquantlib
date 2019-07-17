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
    std::string volType = Rcpp::as<std::string>(rparam["volType"]);
    int floatFreq = Rcpp::as<int>(legParams["floatFreq"]);

    // BOOST_TEST_MESSAGE("Testing Markov functional vanilla engines...");

    Settings::instance().evaluationDate() = tradeDate;

    QuantLib::Handle<QuantLib::YieldTermStructure> yldCrv(rebuildCurveFromZeroRates(dateVec, zeroVec));

    QuantLib::ext::shared_ptr<IborIndex> iborIndex1(new Euribor(floatFreq * Months, yldCrv));

    // create swaps for european swaption and get atm fwd rate //
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

    Real rate=underlyingCall->fairRate();
    
    // calculate european //
    QuantLib::ext::shared_ptr<Exercise> exercise(new EuropeanExercise(startDate));

    Swaption swaptionC(underlyingCall, exercise);
    Swaption swaptionP(underlyingPut, exercise);

    if (volType == "ShiftedLognormal") {
        QuantLib::ext::shared_ptr<BlackSwaptionEngine> blackSwaptionEngine1(new BlackSwaptionEngine(yldCrv, vol));
        swaptionC.setPricingEngine(blackSwaptionEngine1);
        swaptionP.setPricingEngine(blackSwaptionEngine1);
    } else if (volType == "Normal") {
        QuantLib::ext::shared_ptr<BachelierSwaptionEngine> bachelierSwaptionEngine1(new BachelierSwaptionEngine(yldCrv, vol));
        swaptionC.setPricingEngine(bachelierSwaptionEngine1);
        swaptionP.setPricingEngine(bachelierSwaptionEngine1);
    } else {
        Rcpp::stop("swaption volType must be \"Normal\" or \"ShiftedLognormal\"");
    }

    Real pricePay = swaptionC.NPV();
    Real priceRcv = swaptionP.NPV();

    ////////men at work/////////////////

    return Rcpp::List::create(Rcpp::Named("pay") = pricePay,
                              Rcpp::Named("rcv") = priceRcv,
                              Rcpp::Named("sigma") = vol,
                              Rcpp::Named("atmRate") = rate);
}
