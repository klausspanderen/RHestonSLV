//  Copyright (C) 2016 Klaus Spanderen
//
//  This file is part of RHestonSLV.
//
//  RHestonSLV is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, version 2 of the License
//
//  RHestonSLV is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with RHestonSLV.  If not, see <http://www.gnu.org/licenses/>.

#include <ql/quotes/simplequote.hpp>
#include <ql/time/calendars/nullcalendar.hpp>
#include <ql/time/daycounters/actual365fixed.hpp>
#include <ql/instruments/vanillaoption.hpp>
#include <ql/instruments/barriertype.hpp>
#include <ql/instruments/barrieroption.hpp>
#include <ql/instruments/forwardvanillaoption.hpp>
#include <ql/instruments/impliedvolatility.hpp>
#include <ql/processes/blackscholesprocess.hpp>
#include <ql/math/randomnumbers/sobolbrownianbridgersg.hpp>
#include <ql/math/statistics/generalstatistics.hpp>
#include <ql/pricingengines/barrier/fdhestonbarrierengine.hpp>
#include <ql/pricingengines/vanilla/fdhestonvanillaengine.hpp>
#include <ql/pricingengines/vanilla/analyticeuropeanengine.hpp>
#include <ql/pricingengines/forward/forwardengine.hpp>
#include <ql/methods/montecarlo/multipathgenerator.hpp>
#include <ql/termstructures/yield/flatforward.hpp>
#include <ql/termstructures/volatility/equityfx/blackconstantvol.hpp>
#include <ql/termstructures/volatility/equityfx/localvoltermstructure.hpp>
#include <ql/termstructures/volatility/equityfx/hestonblackvolsurface.hpp>
#include <ql/termstructures/volatility/equityfx/noexceptlocalvolsurface.hpp>
#include <ql/models/marketmodels/browniangenerators/mtbrowniangenerator.hpp>
#include <ql/models/marketmodels/browniangenerators/sobolbrowniangenerator.hpp>

#include <ql/experimental/models/hestonslvmcmodel.hpp>
#include <ql/experimental/models/hestonslvfdmmodel.hpp>
#include <ql/experimental/barrieroption/doublebarriertype.hpp>
#include <ql/experimental/barrieroption/doublebarrieroption.hpp>
#include <ql/experimental/processes/hestonslvprocess.hpp>
#include <ql/experimental/finitedifferences/fdhestondoublebarrierengine.hpp>
#include <ql/experimental/barrieroption/analyticdoublebarrierengine.hpp>
#include <ql/methods/finitedifferences/utilities/localvolrndcalculator.hpp>
#include <ql/experimental/barrieroption/analyticdoublebarrierbinaryengine.hpp>

#include <boost/scoped_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/algorithm/string.hpp>

#include <Rcpp.h>
#include <limits>

using namespace Rcpp;

using QuantLib::Real;
using QuantLib::Time;
using QuantLib::Handle;
using QuantLib::Quote;
using QuantLib::TimeGrid;
using QuantLib::SimpleQuote;
using QuantLib::Volatility;
using QuantLib::DayCounter;
using QuantLib::TimeGrid;
using QuantLib::DiscountFactor;
using QuantLib::YieldTermStructure;
using QuantLib::LocalVolTermStructure;
using QuantLib::HestonModel;
using QuantLib::HestonProcess;
using QuantLib::GeneralizedBlackScholesProcess;
using QuantLib::BrownianGeneratorFactory;
using QuantLib::MTBrownianGeneratorFactory;
using QuantLib::SobolBrownianGeneratorFactory;
using QuantLib::HestonSLVFokkerPlanckFdmParams;
using QuantLib::PricingEngine;
using QuantLib::FdmHestonGreensFct;
using QuantLib::FdmSquareRootFwdOp;
using QuantLib::FdmSchemeDesc;

namespace Rcpp {
  static const unsigned int QLtoJan1970Offset = 25569;
  template <> QuantLib::Date as(SEXP dtsexp) {
    Date dt(dtsexp);
    return QuantLib::Date(static_cast<int>(dt.getDate()) + QLtoJan1970Offset);
  }
}

namespace {
  class ZeroYieldTermStructure : public YieldTermStructure {
  public:
    ZeroYieldTermStructure(QuantLib::Date refDate, Function r)
    : YieldTermStructure(
        refDate,
        QuantLib::NullCalendar(),
        QuantLib::Actual365Fixed()),
      r_(r){}

    QuantLib::Date maxDate() const { return QuantLib::Date::maxDate(); }

  protected:
    DiscountFactor discountImpl(Time t) const {
      return std::exp(-as<Real>(r_(t))*t);
    }

  private:
    const Function r_;
  };

  class LocalVolSurface : public LocalVolTermStructure {
  public:
    LocalVolSurface(QuantLib::Date refDate, const Function& f)
    : LocalVolTermStructure(
        refDate,
        QuantLib::NullCalendar(),
        QuantLib::Following,
        QuantLib::Actual365Fixed()),
      f_(f){}

    Real minStrike() const { return QL_EPSILON; }
    Real maxStrike() const { return QL_MAX_REAL; }

    QuantLib::Date maxDate() const { return QuantLib::Date::maxDate(); }

  protected:
    Volatility localVolImpl(Time t, Real strike) const {
      return as<Real>(f_(t, strike));
    }

  private:
    const Function f_;
  };

  boost::shared_ptr<HestonModel> buildHestonModel(
    QuantLib::Date refDate, S4 hestonProcess) {

    if (!hestonProcess.is("HestonProcess"))
      stop("Second parameter needs to be of type HestonProcess");

    const Function r = as<Function>(hestonProcess.slot("r"));
    const Function q = as<Function>(hestonProcess.slot("q"));

    const Real spot  = as<Real>(hestonProcess.slot("spot"));
    const Real kappa = as<Real>(hestonProcess.slot("kappa"));
    const Real theta = as<Real>(hestonProcess.slot("theta"));
    const Real sigma = as<Real>(hestonProcess.slot("sigma"));
    const Real rho   = as<Real>(hestonProcess.slot("rho"));
    const Real v0    = as<Real>(hestonProcess.slot("v0"));

    return boost::make_shared<HestonModel>(
        boost::make_shared<HestonProcess>(
          Handle<YieldTermStructure>(
            boost::make_shared<ZeroYieldTermStructure>(refDate, r)),
          Handle<YieldTermStructure>(
            boost::make_shared<ZeroYieldTermStructure>(refDate, q)),
          Handle<QuantLib::Quote>(
            boost::make_shared<QuantLib::SimpleQuote>(spot)),
          v0, kappa, theta, sigma, rho)
      );
  }

  boost::shared_ptr<LocalVolTermStructure> getFixedLocalVolFromHeston(
      const boost::shared_ptr<HestonModel>& hestonModel,
      const boost::shared_ptr<TimeGrid>& timeGrid) {

    const Handle<QuantLib::BlackVolTermStructure> trueImpliedVolSurf(
        boost::make_shared<QuantLib::HestonBlackVolSurface>(
          Handle<HestonModel>(hestonModel)));

    const boost::shared_ptr<HestonProcess> hestonProcess
      = hestonModel->process();

    const boost::shared_ptr<LocalVolTermStructure> localVol(
        boost::make_shared<QuantLib::NoExceptLocalVolSurface>(
          trueImpliedVolSurf,
          hestonProcess->riskFreeRate(),
          hestonProcess->dividendYield(),
          hestonProcess->s0(),
          std::sqrt(hestonProcess->theta())));


    const boost::shared_ptr<QuantLib::LocalVolRNDCalculator> localVolRND(
        boost::make_shared<QuantLib::LocalVolRNDCalculator>(
          hestonProcess->s0().currentLink(),
          hestonProcess->riskFreeRate().currentLink(),
          hestonProcess->dividendYield().currentLink(),
          localVol,
          timeGrid));

    std::vector<boost::shared_ptr<std::vector<Real> > > strikes;
    for (unsigned i=1; i < timeGrid->size(); ++i) {
      const Time t = timeGrid->at(i);
      const boost::shared_ptr<QuantLib::Fdm1dMesher> fdm1dMesher
        = localVolRND->mesher(t);

      const std::vector<Real>& logStrikes = fdm1dMesher->locations();
      const boost::shared_ptr<std::vector<Real> > strikeSlice(
          boost::make_shared<std::vector<Real> >(logStrikes.size()));

      for (unsigned j=0; j < logStrikes.size(); ++j) {
        (*strikeSlice)[j] = std::exp(logStrikes[j]);
      }

      strikes.push_back(strikeSlice);
    }

    const unsigned nStrikes = strikes.front()->size();
    const boost::shared_ptr<QuantLib::Matrix> localVolMatrix(
        boost::make_shared<QuantLib::Matrix>(nStrikes, timeGrid->size()-1));
    for (unsigned i=1; i < timeGrid->size(); ++i) {
      const Time t = timeGrid->at(i);
      const boost::shared_ptr<std::vector<Real> > strikeSlice
        = strikes[i-1];

      for (unsigned j=0; j < nStrikes; ++j) {
        const Real s = (*strikeSlice)[j];
        (*localVolMatrix)[j][i-1] = localVol->localVol(t, s, true);
      }
    }

    const QuantLib::Date todaysDate
      = hestonProcess->riskFreeRate()->referenceDate();
    const DayCounter dc = hestonProcess->riskFreeRate()->dayCounter();

    const std::vector<Time> expiries(
        timeGrid->begin()+1, timeGrid->end());

    return boost::make_shared<QuantLib::FixedLocalVolSurface>(
      todaysDate, expiries, strikes, localVolMatrix, dc);
  }


  class HestonLocalVolSurface : public LocalVolTermStructure {
  public:
    HestonLocalVolSurface(QuantLib::Date refDate,
                          QuantLib::Date maturityDate,
                          S4 hestonProcess)
      : LocalVolTermStructure(
          refDate,
          QuantLib::NullCalendar(),
          QuantLib::Following,
          QuantLib::Actual365Fixed()),
          localVol_(getFixedLocalVolFromHeston(
            buildHestonModel(refDate, hestonProcess),
            boost::make_shared<TimeGrid>(
              QuantLib::Actual365Fixed().yearFraction(refDate, maturityDate),
              unsigned(QuantLib::Actual365Fixed().yearFraction(refDate, maturityDate)*52))
          )) {}

    Real minStrike() const { return QL_EPSILON; }
    Real maxStrike() const { return QL_MAX_REAL; }

    QuantLib::Date maxDate() const { return QuantLib::Date::maxDate(); }

    Volatility localVol(Time t, Real strike) const {
      return localVolImpl(t, strike);
    }
  protected:
    Volatility localVolImpl(Time t, Real strike) const {
      return localVol_->localVol(t, strike, true);
    }

  private:
    const boost::shared_ptr<LocalVolTermStructure> localVol_;
  };


  boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess>
  getBlackScholesFromHestonProcess(const boost::shared_ptr<HestonProcess>& process) {
    return boost::make_shared<QuantLib::GeneralizedBlackScholesProcess>(
      process->s0(),
      process->dividendYield(),
      process->riskFreeRate(),
      Handle<QuantLib::BlackVolTermStructure>(
        boost::make_shared<QuantLib::BlackConstantVol>(
          process->riskFreeRate()->referenceDate(),
          QuantLib::NullCalendar(),
          Handle<QuantLib::Quote>(
            boost::make_shared<QuantLib::SimpleQuote>(0.25)),
            process->riskFreeRate()->dayCounter())));
  }

  FdmSchemeDesc getFdmSchemeDesc(const std::string& schemeDescStr) {
    return  (schemeDescStr == "ModifiedCraigSneyd") ? FdmSchemeDesc::ModifiedCraigSneyd()
          : (schemeDescStr == "CraigSneyd") ? FdmSchemeDesc::CraigSneyd()
          : (schemeDescStr == "Hundsdorfer") ? FdmSchemeDesc::Hundsdorfer()
          : (schemeDescStr == "ModifiedHundsdorfer") ? FdmSchemeDesc::ModifiedHundsdorfer()
          : (schemeDescStr == "ImplicitEuler") ? FdmSchemeDesc::ImplicitEuler()
          : (schemeDescStr == "ExplicitEuler") ? FdmSchemeDesc::ExplicitEuler()
          : (schemeDescStr == "Douglas") ? FdmSchemeDesc::Douglas()
          : (stop("unknown scheme type"), FdmSchemeDesc::ExplicitEuler());
  }

  QuantLib::Option::Type getOptionType(const std::string& optionTypeStr) {
    const std::string optionTypeU = boost::to_upper_copy<std::string>(optionTypeStr);

    return (optionTypeU =="CALL") ? QuantLib::Option::Call
         : (optionTypeU =="PUT") ? QuantLib::Option::Put
         : (stop("unknown option type"), QuantLib::Option::Call);
  }

  boost::shared_ptr<QuantLib::StrikedTypePayoff> getPayoff(
      const std::string& optionTypeStr,
      const std::string& payoffTypeStr,
      Real strike) {
    const std::string payoffTypeU = boost::to_upper_copy<std::string>(payoffTypeStr);

    if (payoffTypeU =="PLAINVANILLA")
      return boost::make_shared<QuantLib::PlainVanillaPayoff>(
        getOptionType(optionTypeStr), strike);
    else if (payoffTypeU =="CASHORNOTHING")
      return boost::make_shared<QuantLib::CashOrNothingPayoff>(
        getOptionType(optionTypeStr), strike, 1.0);
    else if (payoffTypeU =="ASSETORNOTHING")
      return boost::make_shared<QuantLib::AssetOrNothingPayoff>(
        getOptionType(optionTypeStr), strike);
    else {
      stop("unknown payoff type");
      return boost::shared_ptr<QuantLib::StrikedTypePayoff>();
    }
  }

  boost::shared_ptr<QuantLib::Exercise> getExercise(
    const std::string& exerciseStr, const QuantLib::Date maturityDate) {
    const std::string exerciseStrU = boost::to_upper_copy<std::string>(exerciseStr);

    return (exerciseStrU == "EUROPEAN") ?
        boost::make_shared<QuantLib::EuropeanExercise>(maturityDate)
      :  (exerciseStrU == "AMERICAN") ?
        boost::make_shared<QuantLib::AmericanExercise>(maturityDate)
      : (stop("unknown exercise type"), boost::shared_ptr<QuantLib::Exercise>());
  }

  QuantLib::Barrier::Type getBarrierType(const std::string& barrierTypeStr) {
    const std::string barrierTypeU = boost::to_upper_copy<std::string>(barrierTypeStr);

    return (barrierTypeU == "DOWNIN") ? QuantLib::Barrier::DownIn
         : (barrierTypeU == "UPIN") ? QuantLib::Barrier::UpIn
         : (barrierTypeU == "DOWNOUT") ? QuantLib::Barrier::DownOut
         : (barrierTypeU == "UPOUT") ? QuantLib::Barrier::UpOut
         : (stop("unknown barrier type"), QuantLib::Barrier::UpOut);
  }

  QuantLib::DoubleBarrier::Type getDoubleBarrierType(const std::string& barrierTypeStr) {
    const std::string barrierTypeU = boost::to_upper_copy<std::string>(barrierTypeStr);

    return (barrierTypeU == "KNOCKIN") ? QuantLib::DoubleBarrier::KnockIn
      : (barrierTypeU == "KNOCKOUT") ? QuantLib::DoubleBarrier::KnockOut
      : (barrierTypeU == "KIKO") ? QuantLib::DoubleBarrier::KIKO
      : (barrierTypeU == "KOKI") ? QuantLib::DoubleBarrier::KOKI
      : (stop("unknown barrier type"), QuantLib::DoubleBarrier::KnockIn);
  }

}

class HestonSLVMCModel {
public:
  HestonSLVMCModel(QuantLib::Date referenceDate,
                   QuantLib::Date maxDate,
                   Function localVol,
                   S4 hestonProcess,
                   S4 hestonSLVMCParams) {

    if (!hestonSLVMCParams.is("HestonSLVMCParams"))
      stop("Last parameter needs to be of type HestonSLVMCParams");

    const bool sobol = as<bool>(hestonSLVMCParams.slot("qmc"));
    const unsigned timeStepsPerYear = as<unsigned>(hestonSLVMCParams.slot("timeStepsPerYear"));
    const unsigned nBins = as<unsigned>(hestonSLVMCParams.slot("nBins"));
    const unsigned calibrationPaths = as<unsigned>(hestonSLVMCParams.slot("calibrationPaths"));

    model_.reset(new QuantLib::HestonSLVMCModel(
        Handle<LocalVolTermStructure>(
          boost::make_shared<LocalVolSurface>(referenceDate, localVol)),
        Handle<HestonModel>(
          buildHestonModel(referenceDate, hestonProcess)),
        sobol ? boost::shared_ptr<BrownianGeneratorFactory>(
                  new SobolBrownianGeneratorFactory(
                      QuantLib::SobolBrownianGenerator::Diagonal,
                      1234ul, QuantLib::SobolRsg::JoeKuoD7))
              : boost::shared_ptr<BrownianGeneratorFactory>(
                  new MTBrownianGeneratorFactory(1234ul)),
        maxDate, timeStepsPerYear, nBins, calibrationPaths));
  }

  Real leverageFunction(Time t, Real strike) const {
    try {
      return model_->leverageFunction()->localVol(t, strike, true);
    }
    catch (QuantLib::Error& ex) {
      QL_REQUIRE( std::numeric_limits<Real>::has_quiet_NaN,
                  "quit NaN are not supported");
      return std::numeric_limits<Real>::quiet_NaN();
    }
  }

private:
  boost::scoped_ptr<QuantLib::HestonSLVMCModel> model_;
};


class HestonSLVFDMModel {
public:
  HestonSLVFDMModel(QuantLib::Date referenceDate,
                    QuantLib::Date maxDate,
                    Function localVol,
                    S4 hestonProcess,
                    S4 fdmParams) {
    if (!fdmParams.is("HestonSLVFDMParams"))
      stop("Last parameter needs to be of type HestonSLVFDMParams");

    const std::string greensAlgoStr
      = as<std::string>(fdmParams.slot("greensAlgorithm"));

    const FdmHestonGreensFct::Algorithm greensAlgorithm =
        (greensAlgoStr == "Gaussian") ? FdmHestonGreensFct::Gaussian
      : (greensAlgoStr == "ZeroCorrelation") ? FdmHestonGreensFct::ZeroCorrelation
      : (greensAlgoStr == "SemiAnalytical") ? FdmHestonGreensFct::SemiAnalytical
      : (stop("unknown Greens function type"), FdmHestonGreensFct::SemiAnalytical);

    const std::string trafoTypeStr
      = as<std::string>(fdmParams.slot("transformationType"));

    const FdmSquareRootFwdOp::TransformationType transformationType =
        (trafoTypeStr == "Plain") ? FdmSquareRootFwdOp::Plain
      : (trafoTypeStr == "Power") ? FdmSquareRootFwdOp::Power
      : (trafoTypeStr == "Log") ? FdmSquareRootFwdOp::Log
      : (stop("unknown transformation type"), FdmSquareRootFwdOp::Log);

    const std::string schemeDescStr
      = as<std::string>(fdmParams.slot("fdmSchemeType"));

    const FdmSchemeDesc schemeDesc = getFdmSchemeDesc(schemeDescStr);

    const HestonSLVFokkerPlanckFdmParams params = {
      as<unsigned>(fdmParams.slot("xGrid")),
      as<unsigned>(fdmParams.slot("vGrid")),
      as<unsigned>(fdmParams.slot("tMaxStepsPerYear")),
      as<unsigned>(fdmParams.slot("tMinStepsPerYear")),
      as<Real>(fdmParams.slot("tStepNumberDecay")),
      as<unsigned>(fdmParams.slot("nRannacherTimeSteps")),
      as<unsigned>(fdmParams.slot("predictionCorrectionSteps")),
      as<Real>(fdmParams.slot("x0Density")),
      as<Real>(fdmParams.slot("localVolEpsProb")),
      as<unsigned>(fdmParams.slot("maxIntegrationIterations")),
      as<Real>(fdmParams.slot("vLowerEps")),
      as<Real>(fdmParams.slot("vUpperEps")),
      as<Real>(fdmParams.slot("vMin")),
      as<Real>(fdmParams.slot("v0Density")),
      as<Real>(fdmParams.slot("vLowerBoundDensity")),
      as<Real>(fdmParams.slot("vUpperBoundDensity")),
      as<Real>(fdmParams.slot("leverageFctPropEps")),
      greensAlgorithm,
      transformationType,
      schemeDesc
    };

    model_.reset(new QuantLib::HestonSLVFDMModel(
        Handle<LocalVolTermStructure>(
          boost::make_shared<LocalVolSurface>(referenceDate, localVol)),
        Handle<HestonModel>(
          buildHestonModel(referenceDate, hestonProcess)),
        maxDate,
        params));
  }

  Real leverageFunction(Time t, Real strike) const {
    try {
      return model_->leverageFunction()->localVol(t, strike, true);
    }
    catch (QuantLib::Error& ex) {
      QL_REQUIRE( std::numeric_limits<Real>::has_quiet_NaN,
                  "quit NaN are not supported");
      return std::numeric_limits<Real>::quiet_NaN();
    }
  }

private:
  boost::scoped_ptr<QuantLib::HestonSLVFDMModel> model_;
};


List hestonSLVOptionPricer(QuantLib::Date referenceDate,
                           Real strike,
                           const std::string& optionType,
                           const std::string& exerciseType,
                           QuantLib::Date maturityDate,
                           S4 hestonProcess,
                           Function leverageFct,
                           unsigned tGrid=51, unsigned xGrid=201, unsigned vGrid=51, unsigned dampingSteps=0,
                           const std::string& fdmScheme = "ModifiedCraigSneyd") {

  QuantLib::VanillaOption option(
      boost::make_shared<QuantLib::PlainVanillaPayoff>(
        getOptionType(optionType), strike),
        getExercise(exerciseType, maturityDate));

  option.setPricingEngine(
      boost::make_shared<QuantLib::FdHestonVanillaEngine>(
        buildHestonModel(referenceDate, hestonProcess),
        tGrid, xGrid, vGrid, dampingSteps,
        getFdmSchemeDesc(fdmScheme),
        boost::make_shared<LocalVolSurface>(referenceDate, leverageFct)));

  const Real npv = option.NPV();
  const Real delta = option.delta();
  const Real gamma = option.gamma();
  const Real theta = option.theta();

  const boost::shared_ptr<HestonProcess> process
      = buildHestonModel(referenceDate, hestonProcess)->process();

  QL_REQUIRE( std::numeric_limits<Real>::has_quiet_NaN,
                "quit NaN are not supported");

  Volatility v = std::numeric_limits<Real>::quiet_NaN();
  try {
    v = option.impliedVolatility(
      npv, getBlackScholesFromHestonProcess(process), 1e-8, 1000);
  }
  catch (QuantLib::Error&) {}

  return List::create(
    Named("value") = npv,
    Named("delta") = delta,
    Named("gamma") = gamma,
    Named("theta") = theta,
    Named("impliedVol") = v
  );
};


List hestonSLVForwardOptionPricer(QuantLib::Date referenceDate,
                                  Real strike,
                                  QuantLib::Date resetDate,
                                  const std::string& optionType,
                                  QuantLib::Date maturityDate,
                                  unsigned nSimulations,
                                  S4 hestonProcessParams,
                                  Function leverageFct) {

  const boost::shared_ptr<HestonProcess> hestonProcess(
    buildHestonModel(referenceDate, hestonProcessParams)->process());

  const boost::shared_ptr<LocalVolSurface> leverageFunction(
    boost::make_shared<LocalVolSurface>(referenceDate, leverageFct));

  const boost::shared_ptr<QuantLib::HestonSLVProcess> slvProcess(
    boost::make_shared<QuantLib::HestonSLVProcess>(hestonProcess,leverageFunction));

  const Handle<YieldTermStructure> rTS = hestonProcess->riskFreeRate();
  const Handle<YieldTermStructure> qTS = hestonProcess->dividendYield();

  const DayCounter dc = rTS->dayCounter();
  const Time resetTime = dc.yearFraction(referenceDate, resetDate);
  const Time maturityTime = dc.yearFraction(referenceDate, maturityDate);

  std::vector<Time> mandatoryTimes;
  mandatoryTimes.push_back(resetTime);
  mandatoryTimes.push_back(maturityTime);

  const TimeGrid grid(mandatoryTimes.begin(), mandatoryTimes.end(), unsigned(51*maturityTime));
  const unsigned tSteps = grid.size()-1;
  const unsigned resetIndex = grid.closestIndex(resetTime);

  typedef QuantLib::SobolBrownianBridgeRsg rsg_type;
  typedef QuantLib::MultiPathGenerator<rsg_type>::sample_type sample_type;

  const unsigned factors = slvProcess->factors();
  const boost::shared_ptr<QuantLib::MultiPathGenerator<rsg_type> > pathGen(
    boost::make_shared<QuantLib::MultiPathGenerator<rsg_type> >(
        slvProcess, grid, rsg_type(factors, tSteps), false));

  const boost::shared_ptr<QuantLib::StrikedTypePayoff> payoff(
    boost::make_shared<QuantLib::PlainVanillaPayoff>(
      getOptionType(optionType), strike));

  QuantLib::GeneralStatistics stat;

  for (unsigned i=0; i< nSimulations; ++i) {
    const sample_type& path = pathGen->next();

    const Real S_t1 = path.value[0][resetIndex-1];
    const Real S_T1 = path.value[0][tSteps-1];

    const sample_type& antiPath = pathGen->antithetic();
    const Real S_t2 = antiPath.value[0][resetIndex-1];
    const Real S_T2 = antiPath.value[0][tSteps-1];

    stat.add(0.5*S_t1*payoff->operator()(S_T1/S_t1)
           + 0.5*S_t2*payoff->operator()(S_T2/S_t2));
  }
  const DiscountFactor df = rTS->discount(grid.back());
  const Real npv = stat.mean()*df;

  const boost::shared_ptr<SimpleQuote> vol(boost::make_shared<SimpleQuote>(0.2));

  const Handle<QuantLib::BlackVolTermStructure> volTS(
    boost::make_shared<QuantLib::BlackConstantVol>(
      referenceDate,
      QuantLib::NullCalendar(),
      Handle<Quote>(vol),
      dc));

  const boost::shared_ptr<GeneralizedBlackScholesProcess> bsProcess(
    boost::make_shared<GeneralizedBlackScholesProcess>(
      slvProcess->s0(), qTS, rTS, volTS));

  const boost::shared_ptr<PricingEngine> fwdEngine(
      boost::make_shared<QuantLib::ForwardVanillaEngine<
        QuantLib::AnalyticEuropeanEngine> >(bsProcess));

  const QuantLib::ForwardVanillaOption fwdOption(
    strike, resetDate, payoff,
    boost::make_shared<QuantLib::EuropeanExercise>(maturityDate));

  Volatility implVol = std::numeric_limits<Real>::quiet_NaN();
  try {
    implVol = QuantLib::detail::ImpliedVolatilityHelper::calculate(
      fwdOption, *fwdEngine, *vol, npv, 1e-8, 200, 1e-4, 2.0);
  }
  catch (QuantLib::Error&) {}

  return List::create(
    Named("value") = npv,
    Named("impliedVol") = implVol
  );
}


List hestonSLVBarrierPricer(QuantLib::Date referenceDate,
                            Real barrier,
                            Real rebate,
                            const std::string& barrierType,
                            Real strike,
                            const std::string& optionType,
                            QuantLib::Date maturityDate,
                            S4 hestonProcess,
                            Function leverageFct,
                            unsigned tGrid=51, unsigned xGrid=201, unsigned vGrid=51, unsigned dampingSteps=0,
                            const std::string& fdmScheme = "ModifiedCraigSneyd") {

  QuantLib::BarrierOption option(
      getBarrierType(barrierType),
      barrier,
      rebate,
      boost::make_shared<QuantLib::PlainVanillaPayoff>(
        getOptionType(optionType), strike),
      boost::make_shared<QuantLib::EuropeanExercise>(maturityDate));

  option.setPricingEngine(
    boost::make_shared<QuantLib::FdHestonBarrierEngine>(
      buildHestonModel(referenceDate, hestonProcess),
      tGrid, xGrid, vGrid, dampingSteps,
      getFdmSchemeDesc(fdmScheme),
      boost::make_shared<LocalVolSurface>(referenceDate, leverageFct)));

  const Real npv = option.NPV();
  const Real delta = option.delta();
  const Real gamma = option.gamma();
  const Real theta = option.theta();

  const boost::shared_ptr<HestonProcess> process
    = buildHestonModel(referenceDate, hestonProcess)->process();

  QL_REQUIRE( std::numeric_limits<Real>::has_quiet_NaN,
            "quit NaN are not supported");

  Volatility v = std::numeric_limits<Real>::quiet_NaN();
  for (Real vStart =0.0001; vStart < 2.0 && !(v == v); vStart+=0.01)
    try {
      v = option.impliedVolatility(
        npv, getBlackScholesFromHestonProcess(process), 1e-8, 1000, vStart, vStart+0.01);
    }
    catch (QuantLib::Error& e) {
    }

  return List::create(
    Named("value") = npv,
    Named("delta") = delta,
    Named("gamma") = gamma,
    Named("theta") = theta,
    Named("impliedVol") = v
  );
};


List hestonSLVDoubleNoTouchBarrierPricer(
    QuantLib::Date referenceDate,
    Real barrier_lo,
    Real barrier_hi,
    Real rebate,
    const std::string& barrierType,
    Real strike,
    const std::string& optionType,
    const std::string& payoffType,
    QuantLib::Date maturityDate,
    S4 hestonProcess,
    Function leverageFct,
    unsigned tGrid=51, unsigned xGrid=201, unsigned vGrid=51, unsigned dampingSteps=0,
    const std::string& fdmScheme = "ModifiedCraigSneyd") {

  QuantLib::DoubleBarrierOption option(
      getDoubleBarrierType(barrierType),
      barrier_lo,
      barrier_hi,
      rebate,
      getPayoff(optionType, payoffType, strike),
      boost::make_shared<QuantLib::EuropeanExercise>(maturityDate));

  option.setPricingEngine(
    boost::make_shared<QuantLib::FdHestonDoubleBarrierEngine>(
      buildHestonModel(referenceDate, hestonProcess),
      tGrid, xGrid, vGrid, dampingSteps,
      getFdmSchemeDesc(fdmScheme),
      boost::make_shared<LocalVolSurface>(referenceDate, leverageFct)));

  const Real npv = option.NPV();

  return List::create(Named("value") = npv);
};

List bsDoubleNoTouchBarrierPricer(
    QuantLib::Date referenceDate,
    Real barrier_lo,
    Real barrier_hi,
    Real rebate,
    const std::string& barrierType,
    Real strike,
    const std::string& optionType,
    const std::string& payoffType,
    QuantLib::Date maturityDate,
    Real underlying,
    Real dividendYield,
    Real riskFreeRate,
    Real volatility) {

  const boost::shared_ptr<QuantLib::StrikedTypePayoff> payoff(
      getPayoff(optionType, payoffType, strike));

  QuantLib::DoubleBarrierOption option(
    getDoubleBarrierType(barrierType),
    barrier_lo, barrier_hi, rebate,
    payoff,
    boost::make_shared<QuantLib::EuropeanExercise>(maturityDate));

  boost::shared_ptr<QuantLib::GeneralizedBlackScholesProcess> bsProcess(
    boost::make_shared<QuantLib::GeneralizedBlackScholesProcess>(
      Handle<Quote>(boost::make_shared<SimpleQuote>(underlying)),
      Handle<YieldTermStructure>(
        boost::make_shared<QuantLib::FlatForward>(
          referenceDate, dividendYield, QuantLib::Actual365Fixed())),
      Handle<YieldTermStructure>(
        boost::make_shared<QuantLib::FlatForward>(
          referenceDate, riskFreeRate, QuantLib::Actual365Fixed())),
      Handle<QuantLib::BlackVolTermStructure>(
        boost::make_shared<QuantLib::BlackConstantVol>(
          referenceDate,
          QuantLib::NullCalendar(),
          Handle<QuantLib::Quote>(
            boost::make_shared<SimpleQuote>(volatility)),
            QuantLib::Actual365Fixed()))));

  if (boost::dynamic_pointer_cast<QuantLib::PlainVanillaPayoff>(payoff))
    option.setPricingEngine(
      boost::make_shared<QuantLib::AnalyticDoubleBarrierEngine>(bsProcess));
  else option.setPricingEngine(
      boost::make_shared<QuantLib::AnalyticDoubleBarrierBinaryEngine>(bsProcess));

  const Real npv = option.NPV();

  return List::create(Named("value") = npv);
}

RCPP_MODULE(HestonSLVModule) {
  class_<HestonSLVMCModel>( "HestonSLVMCModel" )

  .constructor<QuantLib::Date, QuantLib::Date, Function, S4, S4>()
  .method("leverageFunction",&HestonSLVMCModel::leverageFunction)
  ;

  class_<HestonSLVFDMModel>( "HestonSLVFDMModel" )

  .constructor<QuantLib::Date, QuantLib::Date, Function, S4, S4>()
  .method("leverageFunction",&HestonSLVFDMModel::leverageFunction)
  ;

  class_<HestonLocalVolSurface>( "HestonLocalVolSurface" )

  .constructor<QuantLib::Date, QuantLib::Date, S4>()
  .method("localVol",&HestonLocalVolSurface::localVol)
  ;


  function("hestonSLVOptionPricer_", &hestonSLVOptionPricer);
  function("hestonSLVForwardOptionPricer", &hestonSLVForwardOptionPricer);
  function("hestonSLVBarrierPricer_", &hestonSLVBarrierPricer);
  function("hestonSLVDoubleNoTouchBarrierPricer_",
           &hestonSLVDoubleNoTouchBarrierPricer);
  function("bsDoubleNoTouchBarrierPricer",
           &bsDoubleNoTouchBarrierPricer);
}
