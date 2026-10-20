# Empirical analysis: counterfactuals of the CPI for mobile telephone services

Econometric models to estimate counterfactual scenarios, following the methodology developed by Aradillas (2019), to analyze the impact of the 2013 telecommunications reform and the entry of AT&T in 2015.

<img src="https://github.com/ivang019/Impact-of-the-reform-and-the-entry-of-ATT-on-mobile-phone-service-prices-in-Mexico/blob/main/INPC_Movil_Contrafactual.png?raw=true" alt="Counterfactuals of the mobile services CPI" width="75%">

## Literature review

Several studies have estimated the ex post effects of regulatory events on the mobile telecommunications sector, using causal inference models that compare treated and untreated units. In the Mexican case, Ovando and Olivera (2018) use a difference-in-differences (DiD) model with household data to estimate the impact of the telecommunications reform on fixed internet adoption between 2010 and 2015. They find that the probability of subscribing increased by 66%, with stronger effects in the lower income deciles.

The IFT study (2021) uses the synthetic control method to build a counterfactual from a combination of variables of interest in ten countries. The goal is to evaluate the impact of the asymmetric regulation imposed on the AEP (preponderant economic agent) since 2014. The results suggest a fall in ARPU of 43.35%, an increase in service quality of 30.1% and an increase in coverage of 10.22%.

Aradillas (2019) proposes a model with a lag to estimate counterfactuals that identify the effect of the 2013 reform on several variables of the sector. His findings show statistically significant reductions in prices and in market concentration. However, he points out that América Móvil has kept price margins above competitive levels, even after the reform.

This document adopts the methodology of Aradillas (2019) with some changes. His model is a good fit because it can build counterfactuals from one single time series per variable. However, unlike the original study, this document considers two relevant events: the 2013 reform and the entry of AT&T in 2015, because both changed the dynamics of the mobile services market. These methodological differences do not allow a direct comparison with the results of Aradillas.

## Data

The model uses the CPI of mobile telephone services (STM), at national level, with biweekly observations from 2011 to 2024.

| Variable | Unit | Level | Period | Frequency |
|---|---|---|---|---|
| Mobile CPI | Number | National | 2011-2024 | Biweekly |

Descriptive statistics:

| Variable | Observations | Min | Max | Mean | Median | Standard deviation |
|---|---|---|---|---|---|---|
| Mobile CPI (MXN) | 327 | 88.67 | 247 | 126.92 | 100.12 | 41.56 |

## Model

The model of Aradillas (2019) was adapted. It allows simple statistical models with a good fit to the data. It is used to evaluate the effect of two sequential events in the Mexican mobile market: the constitutional reform in telecommunications of 2013 and the entry of AT&T in 2015. For the reform, we estimated how the variable would have evolved without it, using only data from before the reform. For the entry of AT&T, the counterfactual was built with the dynamics observed after the reform but before AT&T entered, that is, between 2013 and 2015.

### Model based on Aradillas (2019)

Aradillas (2019) does an econometric analysis of the impact of the 2013 telecommunications reform on the telecommunications sector. To measure the effect, the author builds a counterfactual for time series of several variables. Time series are written as $Y_t$, where $Y$ is the variable and $t$ is the time period. Each variable can be written as a function of its lag, that is, as an autoregressive AR(1) process:

$$Y_t = \beta_0^{AR} + \beta_1^{AR} Y_{t-1} + e_t \quad (1)$$

$$Y_t = \beta_0^{PR} + \beta_1^{PR} Y_{t-1} + \epsilon_t \quad (2)$$

Where $\beta_0^{AR}$ and $\beta_1^{AR}$ are the coefficients to estimate before the reform, and $e_t$ is its residual. And $\beta_0^{PR}$ and $\beta_1^{PR}$ are the coefficients to estimate with data after the reform, and $\epsilon_t$ is its residual.

So, equation (1) is the regression for the period before the reform, and (2) is for the period after. "AR" is a superscript on the betas that means "before reform" (*antes de reforma*) and "PR" means "post reform" (*post reforma*). The author defines as dynamic equations: $\beta_0^{AR} + \beta_1^{AR} Y_{t-1}$ and $\beta_0^{PR} + \beta_1^{PR} Y_{t-1}$. The counterfactuals are obtained under the assumption that the reform changed the dynamic equation in a structural way, and that the residual $\epsilon_t$ captures the variation that can come from other factors (economic, legal, regulatory, technological, etc.). So, the counterfactuals for the period after the reform were built in this way:

$$\hat{Y}_t^{R} = \hat{\beta}_0^{AR} + \hat{\beta}_1^{AR} Y_{t-1} + \hat{\epsilon}_t \quad (3)$$

Where:

$$\hat{\epsilon}_t = Y_t - \hat{\beta}_0^{PR} - \hat{\beta}_1^{PR} Y_{t-1}$$

Equation (3) estimates the counterfactual of the Telecommunications Reform. That is, the coefficients that capture the market dynamics before the reform are combined with the residuals (shocks) after the reform to build the counterfactual.

For the purpose of this document, we propose a model based on Aradillas, with two events instead of one, that had a strong effect on the variables of the STM market. The second event is the entry of AT&T into the Mexican market in October 2015. The counterfactual for the reform is built in the same way as Aradillas (2019).

For the AT&T counterfactual, we basically follow the same methodology used for the reform counterfactual. The main difference is the time window used to calculate the AR(1) coefficients. Instead of using all the pre-event observations, that is, all the observations from the start of the series until AT&T entered in 2015, we use the observations after the reform but before the entry of AT&T. The main reason is that the Aradillas model assumes that the reform changes the dynamics of the variables. So, the AT&T counterfactual must be calculated with the last observed dynamics, which is from the reform until the arrival of AT&T.

The shocks are calculated with the errors $\hat{\epsilon}_t$ of the AR(1), using the observations after October 2015. So, the functional form of the AT&T counterfactual is:

$$\hat{Y}_t^{ATT} = \hat{\beta}_0^{A\_ATT} + \hat{\beta}_1^{A\_ATT} Y_{t-1} + \hat{\epsilon}_t \quad (4)$$

In equation (4), ATT stands for AT&T.

Considering these two events, the interpretation of the variable and its two possible counterfactuals is:

- **Observed variable** $`= Y_t \equiv`$ Observed value of variable "y" in a given period, given that the reform happened and AT&T entered the market.
- **Reform counterfactual** $`= \hat{Y}_t^{R} \equiv`$ Estimated value of variable "y" in a given period, given that the reform did not happen and AT&T did not enter the market.
- **AT&T counterfactual** $`= \hat{Y}_t^{ATT} \equiv`$ Estimated value of variable "y" in a given period, given that the reform did happen and AT&T did not enter the market.

Because each series implies a particular situation, it is not possible to estimate the effect of each event by itself. What the comparison between these series says is how variable "y" would have behaved, given its particular scenario.

## Estimation

Equations (3) and (4) are estimated to build the counterfactuals of the CPI. The counterfactuals show how the variable would have behaved if the 2013 reform had not been enacted; and, if the reform had been enacted, if AT&T had not entered the market. The following table shows the estimated coefficients.

| Variable | Intercept Pre-Reform | Coefficient Pre-Reform | Intercept Pre-AT&T | Coefficient Pre-AT&T |
|---|---|---|---|---|
| Mobile CPI (MXN) | 6.1247 | 0.9634 | 4.4648 | 0.9662 |

We compare the fit of the AR(1) before the event with the observed series before the event. A good fit means that the counterfactual estimates after the event are reliable.

## Results

The results of the model confirm that both the reform and the entry of AT&T put downward pressure on mobile service prices. For 2024, without both events, the STM CPI would have stayed at levels 75% higher than the observed ones (≈175 MXN vs. ≈100 MXN). If only AT&T is excluded, prices would have been 40% higher (≈140 MXN). This shows that, although the reform set the competitive base, the entry of operators sped up the fall in prices.

An important finding is the different trends: without the reform and without AT&T, prices would show an upward path since 2022. In the scenario without AT&T, a downward gap would remain, but it would not be enough to reach the current values.

In summary, we proposed a model with a lag to estimate the impact of the 2013 reform, and of the later entry of AT&T, on the CPI of the mobile services market, following the methodology of Aradillas (2019). The main result of this econometric model is that, without these two events, the price index would have been at least 40% higher than the observed one.

Comparing the results of each counterfactual scenario, one point stands out: even with the approval of the 2013 reform, the entry of AT&T into the market sped up the fall in the prices of these services considerably. This suggests that the entry of new operators, together with a strong regulatory framework, can have a positive impact on companies and users of these mobile services.

## Conclusions

The econometric analysis confirms that the 2013 reform and the entry of AT&T were key catalysts to improve competition conditions. The counterfactual scenarios show that, without these events, prices would be 75% higher. These changes brought real benefits for consumers, such as the 49.3% reduction in prices (2013-2024).

## Reproducibility

```
├── INPC_MOVIL.csv                 # Biweekly CPI series, subindex 237 Mobile telephone service
├── contrafactual_inpc.R           # Cleaning, estimation and plot
├── INPC_Movil_Contrafactual.png   # Results plot
└── README.md
```

R packages: `readr`, `dplyr`, `sandwich`, `lmtest`, `ggplot2`, `estimatr`.

## References

- Aradillas (2019).
- IFT (2021).
- Ovando and Olivera (2018).
