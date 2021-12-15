---
lang: en-EN
bibliography: C:/Users/adrien.tableau/Dropbox/JabRef/Library_OFB.bib
csl: https://www.zotero.org/styles/apa
link-citations: true
title: "Recruitment rate estimation in waterfowl using a non-invasive method"
author: 
- Adrien Tableau^[Office Français de la Biodiversité, France - adrien.tableau@ofb.gouv.fr]
# - Alain Caizergues^[Office Français de la Biodiversité, France - alain.caizergues@ofb.gouv.fr]
- Iain Henderson^[Animal and Plant Health Agency, United Kingdom - iain.henderson@apha.gov.uk]
- Sébastien Reeber^[Société Nationale pour la Protection de la Nature, France - sebastien.reeber@snpn.fr]
# - Matthieu Guillemain^[Office Français de la Biodiversité, France -  matthieu.guillemain@ofb.gouv.fr]
date: "15 décembre, 2021"
abstract: |
  The response of a waterfowl population to a harvest pressure depends on its capacity to renew. The recruitment, i.e. the number of young adults reproducing for the first time, is a key indicator to describe the renewal of a population, and therefore an essential tool in population management. The recruitment rate, defined inhere as the number of recruits produced per breeder, is even more informative because it is independent of the breeding population size and allows comparisons over time and between species. The proportion of young adult in a waterfowl population, which is a main step in estimating the recruitment rate, is often estimated from game-hunting samples. However, this proportion is not accessible in years without harvest, or with a low harvest rate. Moreover, the age-structure in the harvest samples does not necessarily reflect the underlying age-structure of the population. It is often skewed towards immatures and can lead to an overestimation of the recruitment rate. In waterfowl, adult males usually display brighter colours than females and immatures from both sexes. This dichromatism can be characterized and monitored from count surveys, a non-invasive method. Such information can be used to infer the proportion of immatures, and consequently the recruitment rate. In using two populations of ruddy duck, *Oxyura oxyura*, this study introduces a model to estimate the recruitment rate from count data. To test the accuracy of the method, the results are compared to recruitment rate estimates from samples, and their consistency to support the observed population growth rates is also tested. The results suggest that the counting method is a versatile tool to estimate the recruitment rate if the species is monitored during the appropriate time window. Authors argue for considering a two-category count protocol in winter surveys of waterfowl species for which adult males can be differentiated from other individuals.   
  
  **Keywords**: fecundity - productivity - reproductive success - age ratio - juvenile - duck
output: 
    bookdown::pdf_document2:
      latex_engine: xelatex
      toc: false
    bookdown::word_document2:
      toc: false
header-includes:
  \usepackage{float}
---



\newpage

# Introduction

*Definitions*

One defines the recruitment rate of a breeding population as the average number of recruits produced per each breeder [@Flint2015]. This parameter might sometimes be called *annual fertility* [@Koons2014], *fecundity* [@Arnold2018], *reproductive rate* [@Cooch2014], *reproductive success* [@Etterson2011], or *productivity* [@Johnson1987; @Hagen2008]. If one considers a species reaching its sexual maturity as early as its first year, all individuals are mature at the reproduction season. The recruitment rate is thus the ratio of the number of young adults at the reproduction season of year $t$, i.e. the recruits, to the number of all individuals at the reproduction season of the previous year, $t - 1$. The declination for species with delayed maturity follows the same process [see @Robertson2008].

The recruitment rate parameter is complex because it is composed of two main sub-parameters: the fecundity followed by the juvenile survival [@Etterson2011]. Both sub-parameters can be split into other sub-parameters. The fecundity can be defined by the product of the nesting success, the average clutch size, and the hatching success. The juvenile survival can be defined as the product of the post-hatching survival and the post-fledging survival, both critical life periods [@Hill2008; @Blums2004]. The recruitment rate as defined in the present study is thus a parameter integrating multiple life steps from the egg to the young adult attempting to breed for its first time [@Koons2017]. 

*The use of recruitment rate in population management*

Population conservation, harvesting, and control require tools in population management in order to reach quantitative targets, such as a population size for example [@Shea1998; @Johnson2021]. To do so, a first blind strategy is to test iteratively management rules to reach progressively an optimum. Such approach can be time-consuming given the intrinsic variability of the population response to its changing environment. It might also suffer from a rejection of the stakeholders because of the lack of transparency of an arbitrary process [@Williams2013]. Another approach is to try to predict the growth rate of the population as a function of the harvest rate and other environment inputs. Such modelling approach can provide clues on the recovering time of a population upon a variety of management rules [@Otis2006]. These decision-support tools are then key inputs to status on the best management rules to implement. The growth rate of a population depends on the adult survival and the recruitment rate, see Equation \@ref(eq:GR) in Section \@ref(PGR). The adult survival is generally estimated by mark recapture/recovery methods [@Lebreton2001]. If the harvest pressure behaves as an additive mortality to the natural mortality for adults [e.g. @Iverson2013], it is quite straightforward to predict its impact on the adult survival. 

According to @Blums2004, the recruitment for diving ducks is poorly related to fecundity, but depends mostly on juvenile survival, which is driven by weather conditions. Studies on other birds concludes also on the weak correlation between fecundity and recruitment [e.g. @Murray2000]. The difficulty to track juvenile survival [@Schmidt2008] demonstrates the interest to focus on the recruitment rate when accessible. The key to predict the growth rate of a population is then to estimate the value of the recruitment rate and its variability, potentially influenced by the harvest pressure [@Anders2005]. 

*The variability of the recruitment rate*

A waterfowl species that is released in a favourable habitat will naturally expand [@Malthus1872]. The growth of such a population reflects that the recruitment rate parameter is higher than the adult mortality rate. This expansion is limited by the carrying capacity of the habitat [@Sayre2008]. When a waterfowl species is endemic of an ecosystem and evolves in stable environmental conditions, the population size varies around the carrying capacity because the individuals compete for space or/and food [@Nummi2015], which prevents the population from expanding. This competition commonly induces a lower fecundity or/and a higher juvenile mortality. The population renewal is slowed down because the recruitment rate parameter is affected by density-dependent effects [@Gunnarsson2013]. For an endemic species evolving in stable environmental conditions, the recruitment rate compensates on average the adult mortality rate, which explains the stabilization of the number of individuals [@Flint2015].

Hunting waterfowl induces adult and juvenile mortality [@Bellrose1980]. Intuitively, one expects that such a hunted population would observe lower adult survival and recruitment rate and then would rapidly deplete to its extinction. However, hunting waterfowl has a long history, and most of these species have persisted over time [@Cooch2014]. So paradoxically, hunting immature individuals does not always induce a decrease of recruitment rate. The heterogeneity of quality among immature individuals might explain this by inducing a compensation process if the harvest selects mostly individuals with a low reproductive potential [@Lindberg2013; @Gimenez2017]. The recruitment rate can even increase because the harvest pressure reduces the densities and potentially the competition for space and food [@Nummi2015; @Peron2012]. In conclusion, a limited exploitation of a population observing heterogeneity and/or density-dependent process might paradoxically increase or at least not affect the recruitment rate. Under moderate exploitation, a new balance between recruitment rate and adult mortality can be theoretically reached [@Tsikliras2018]. The size of a newly exploited population in stable environmental conditions should thus reach a different equilibrium, which is expected to be lower than the size without exploitation. This process explains why the exploitation of many waterfowl species is sustainable and has been lasting over a long time. 

The fecundity and the juvenile survival, the two components of the recruitment rate, are very sensitive to weather conditions [@Blums2004; @Folliot2017]. The environment conditions are thus an additional source of variability of the recruitment rate. The response of the recruitment rate to a harvest pressure is thus not straightforward. It is noisy because of its sensitivity to environmental factors. It also depends directly on the intensity of harvest on the immature individuals and indirectly on the mitigation of the density-dependent effects.

*What part of the recruitment rate gradient is necessary for population management?* 

In population management, a common question for a threaten or an invasive species is to determine the measure that will allow to reach a targeted population size/density in a limited time period [@Shea1998]. For such question, the carrying capacity of the habitat is theoretically not reached. So a manager should mostly focus on collecting knowledge on the maximum recruitment rate and its variability by removing the potential density-dependent effects occurring only for the highest theoretical densities [@Peron2013; @Eraud2021].

*What is the ideal biological model to explore the gradient of the recruitment rate?*

Hunting waterfowl has a long history, but the biological effects of long-term moratorium have been poorly surveyed [@MartinezAbrain2013]. So there are few opportunities to explore the response of the recruitment rate to a gradient of pressure for such species. Alien vertebrate species are commonly introduced during release events [@Saul2016]. They usually observe a first period of colonisation on new territories without management measures. Since its presence might impact the balance of the colonized ecosystem, it happens that managers take strong control measures to restrict the growth of these populations or to eradicate them [@Oficialdegui2020]. Such species are then a good biological model to overcome density-dependent effects and explore the effects of a harvest gradient on recruitment rate.

*How to estimate recruitment rate?*

The recruitment rate parameter of a population can be assessed first by evaluating the fecundity of the breeding population and then by monitoring the survival from the egg life stage to the first-breeding event. To do so, it is necessary to monitor nests in habitats representative of the considered population [@Blums2004]. The survival of a chick to its recruitment can be done by capture mark recapture method quite tricky to set up [@Blums2004; @Schmidt2008; @Arnold2018]. Even if such approach provides valuable information on the dynamics of the recruitment and can highlight the bottlenecks, the fieldwork required to estimate both components is too time-consuming to generalize such approach for waterfowl population management on a long-term horizon. 

In waterfowl, it is generally impossible to differentiate immatures from adults without examining birds in hand. Aging individuals from samples coming from hunting bags is thus another opportunity to estimate the recruit proportion. Such approach suffers from two caveats. First, it is impossible to assess the recruitment rate when there is no harvest whereas it is necessary to predict what would happen if a moratorium seems required to allow a population to recover, or to calibrate a priori the harvest effort necessary to control an invasive species [@Smith2005]. Secondly, like any predation action, hunting is selective and provides a biased picture of the population age structure in favour of the immatures [@Bellrose1980; @Fox2014]. The hunting bag analysis might then lead to overestimate the recruitment rate and consequently the maximum sustainable harvest rate, with adverse consequences on population conservation [@Fox2014]. Inferring recruitment rate from the proportion of immature birds trapped during tagging studies is also promising [@Arnold2018], but the author admits that juveniles might be more naïve and are then more likely caught in traps [@RguibiIdrissi2003].

In waterfowl, it is common that adult males display brighter colours than adult females and immatures of both sexes, which display cover-up plumage similar to adult females [@Johnson1999]. This discrepancy is valuable because it can be observed and quantified from distance. Knowing adult sex ratio, such approach is promising because it requires only to count the proportion of birds of the two plumage kinds to infer the recruitment rate [@Smith2001]. Unlike the two previous methods, this one is non-invasive and requires an acceptable time investment, which allows to collect long time series and consequently to track recruitment rate variability.

*Methodology of this study*

From two populations of ruddy duck, a species introduced in Europe in the 40's, this study introduces a simple Bayesian model to infer recruitment rate from count data. A large part of this study focuses on assessing the viability of such approach. The comparison between two populations is a first step in testing the consistency of the method. A comparison to the common approach using samples from hunting bags aims at checking if the recruitment rate variability is well tracked and if the hypothesis stating that the recruitment rate is overestimated when using sample data is consistent with the results. Using two other approaches using population growth rates, one testes the consistency of the order of magnitude of the recruitment rate estimated from the counting method. Studying two populations is also an opportunity to discuss the response of the recruitment rate to a gradient of harvest pressure.

# Materials & methods {#MM}

## Biological model

The ruddy duck is a diving duck species introduced in the United Kingdom in the 40's [@GutierrezExposito2020]. The first reproduction was observed in the 60's. The population rapidly grew to reach more than 5000 individuals spread over the entire country. A new population set up in France from the 90's, likely because of an arrival of a few individuals from the United Kingdom (UK). Unlike the UK population, the French one set up around a single wintering spot, the Grand Lieu Lake (47.09°N, 1.67°W), which facilitates the population monitoring. Since this species is considered as a threat to the white-headed duck because of the risk of a genetic introgression [@MunozFuentes2007], an European plan of eradication has been adopted and control measures were taken in both countries [@GutierrezExposito2020]. 

Just before the reproduction period, one can distinguish from shore individuals with male-like plumage from female-like individuals on wintering spots (Figure \@ref(fig:flock)). It is noticeable that the apparent proportion of males in the counts from shore is far below the proportion of males in the samples from hunting bags (Figure \@ref(fig:pmal)). 

\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../Output/flock} 

}

\caption{(ref:flock)}(\#fig:flock)
\end{figure}

\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../Output/plot_1_saved} 

}

\caption{(ref:pmal)}(\#fig:pmal)
\end{figure}

Assuming sex identification of samples is very accurate, such observation demonstrates that the counting method misses the identification of a significant part of males in the population. In many waterfowl species, immature individuals of both sexes display cover-up plumage similar to adult females [@Reeber2015]. Consequently, the counts in winter allows adult males to be distinguished from all other individuals (adult females, immature females, and immature males). This feature can be valuable to estimate the recruitment with two conditions. The season when adult males can be distinguished from all other individuals is sufficiently close to the breeding period to consider the age structure picture of the population to be representative of the age structure of the breeding population. The adult sex ratio is known and stable over the considered time period. 

The demography of both ruddy duck populations has been tracked thanks to exhaustive counts on the wintering spots during the period between December 1 and January 31. Controlled individuals between the counting date and the beginning of the reproduction period (set up to May 1 upon field observations) were subtracted from the winter count to get a proxy of the breeding population size. Time series show that the two populations follow similar demographic histories (Figure \@ref(fig:count)). They grew freely in both countries until 1999. Control measures have been then applied from 1999 to 2005 in the UK and from 1999 to 2018 in France. In both countries, these management measures led to stop the expansion of the ruddy ducks. From 2005 in the UK and from 2018 in France, higher control effort led to a rapid depletion in both populations. This history underlines that both populations observed a large spectrum of harvest effort, from no pressure to very high pressure. For sake of interpretation, the time period 2001-2004 was classified in the *no harvest* category in France because the harvest pressure applied over this period was very limited with no effect on the population growth.

\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../Output/plot_2} 

}

\caption{(ref:count)}(\#fig:count)
\end{figure}

The counting dataset, which differentiates the male-like from the female-like individuals in winter, covers 7 years and 19 years in UK and France respectively (Figure \@ref(fig:count)). Combined together, the counting datasets cover different population dynamics corresponding to a gradient of harvest pressure. In France, data before 2004 correspond to a period of population growth with (almost) no harvest pressure, whereas data in the following years correspond to a stabilized population size with a significant level of harvest pressure (Figure \@ref(fig:count)). In UK, the counting data correspond to a quick population depletion associated to a high level of harvest pressure, especially before the reproduction period. In parallel, the sampling dataset, which corresponds to individuals shot in winter during control operations, covers 9 years only in UK, with 5 years in common with the corresponding counting time series.

## Recruitment rate inference from count data

Adult sex ratio in waterfowl is generally biased towards males (Figure \@ref(fig:pmal), @Wood2021). As a consequence, deducing the immature proportion from the adult male proportion is not straightforward because the adult proportion twice as much as the male proportion. Even if the adult sex ratio may vary over a long time range, it is relatively stable over a few years [@Wood2021]. Sex identification in adult samples from hunting bags was available for both ruddy duck populations. However, the small population size in France prevents from providing precise sex proportion estimates. A comparison over months in UK showed no difference in male proportion, so adult samples collected over the whole year were used to estimate the male proportion. A comparison of male proportion among years with more 500 samples did not show significant interannual differences. As a consequence, all adult samples were pooled to estimate the adult male proportion, see Equation \@ref(eq:pm). 

Assuming the additive property of the binomial distribution, the proportion of immature individuals can be deduced from the cumulated counts of male-like individuals in the wintering population, see Equation \@ref(eq:pic). Only years with more than 100 individuals were selected. From this proportion and abundance index of breeding population sizes, the recruitment rate is straightforward, see Equation \@ref(eq:r). If the absolute value of the breeding population size is accessible, the recruitment can be estimated, see Equation \@ref(eq:R).

|Name|Class|Description|
|:---|:----|:----------------------------|
|$SAM$|Data|Total number of adult males in samples|
|$SAF$|Data|Total number of adult females in samples|
|$p(m|a)$|Parameter|Proportion of males in adults (or probability to be an adult male)|
|$CAM_{i, t}$|Data|Cumulated number of type-male individuals counted in population $i$ in year $t$, which are assumed to be adult males|
|$C_{i, t}$|Data|Cumulated number of individuals counted in population $i$ in year $t$|
|$p(a \cap m)_{i, t}$|Parameter|Proportion of adult males in population $i$ in year $t$|
|$p(a)_{i, t}$|Parameter|Proportion adults in population $i$ in year $t$|
|$p(i)_{i, t}$|Parameter|Proportion of immatures in population $i$ in year $t$|
|$N_{i, t}$|Data|Proxy of the size of the breeding population $i$ in year $t$ (maximum number of individuals counted in the wintering population minus removals before reproduction)|
|$r_{i, t}$|Parameter|Recruitment rate, i.e. number of recruits in population $i$ in year $t$ per breeder in year $t - 1$|
|$R_{i, t}$|Parameter|Number of recruits in population $i$ in year $t$|

\begin{equation}
p(m|a) \sim {\sf Beta}(SAM, SAF)
(\#eq:pm)
\end{equation}

\begin{align}
CAM_{i, t} & \sim {\sf Binom}(p(a \cap m)_{i, t}, C_{i, t}) \notag \\
& \sim {\sf Binom}(p(m|a).p(a)_{i, t}, C_{i, t}) \notag \\
& \sim {\sf Binom}(p(m|a).(1 - p(i)_{i, t}), C_{i, t})
(\#eq:pic)
\end{align}

\begin{equation}
r_{i, t} = \frac{p(i)_{i, t}.N_{i, t}}{N_{i, t - 1}}
(\#eq:r)
\end{equation}

\begin{equation}
R_{i, t} = p(i)_{i, t}.N_{i, t}
(\#eq:R)
\end{equation}

## Comparison with the sampling method

A direct estimation of the immature proportion can be made using samples from hunting bags, see Equation \@ref(eq:pis). By combining estimates of immature proportion and Equation \@ref(eq:r), recruitment rates from samples were estimated to check the variability consistency of the counting method. One selected only years with more than 100 individuals controlled in winter, which correspond to a 9-year period in UK. This sampling time series covers 5 years of the UK counting time series. 

|Name|Class|Description|
|:---|:----|:----------------------------|
|$SI_{i, t}$|Data|Number of immatures sampled in the wintering population $i$ in year $t$|
|$S_{i, t}$|Data|Number of individuals sampled in the wintering population $i$ in year $t$|

\begin{equation}
SI_{i, t} \sim {\sf Binom}(p(i)_{i, t}, S_{i, t})
(\#eq:pis)
\end{equation}

## Result validation using population growth rates {#PGR}

The simple relationship (a bit more complex if a species with delayed maturity is considered, see @Robertson2008) between population growth rate, adult survival, and recruitment rate can be used to test the accuracy of the order of magnitude of recruitment rates estimated from the counting and the sampling methods, see Equation \@ref(eq:GR). This relationship comes from a simple reasoning for a closed population: the population size in year $t$ is equal to the number of breeders that survived over year $t - 1$ plus the offspring produced in year $t - 1$ that survived until the reproduction period of year $t$, i.e. the recruitment in year $t$. The growth rate of a population is thus the sum of the adult survival rate and the recruitment rate [@Flint2015].

|Name|Description|
|:--|:----------------------------|
|$N_{t}$|Number of breeders in year $t$|
|$D_{t}$|Number of breeders dead after reproduction in year $t$|
|$R_{t}$|Number of recruits in year $t$|
|$s_{t}$|Adult survival rate, i.e. proportion of breeders in year $t-1$ still alive in year $t$|
|$r_{t}$|Recruitment rate, i.e. number of recruits in year $t$ produced per breeder in year $t-1$|
|$\lambda_{t}$|Growth rate of the population between the reproduction periods of year $t-1$ and year $t$|

\begin{align} 
N_{t} & = N_{t-1} - D_{t-1} + R_{t} \notag \\
N_{t} & = N_{t-1} - (1 - S_{t}).N_{t-1} + r_{t}.N_{t-1} \notag \\
N_{t} & = S_{t}.N_{t-1} + r_{t}.N_{t-1} \notag \\
\frac{N_{t}}{N_{t-1}} & = s_{t} + r_{t} \notag \\
\lambda_{t} & = s_{t} + r_{t} 
(\#eq:GR)
\end{align}

From the evolution of the breeding population size, population growth rate can be assessed every year to catch the variability range \@ref(eq:GR), or can be smoothed over a consistent time period to provide average population growth rate, see Equation \@ref(eq:L), which is a linear regression on the logarithm scale. 

|Name|Class|Description|
|:--|:----|:----------------------------|
|$N0_{i, J}$|Parameter|Intercept of the regression model|
|$\lambda_{i, J}$|Parameter|Average population growth rate over a restricted time interval $J$ for a population $i$ (in $year^{-1}$)|
|$t \in J$|Index|Year index within the time interval $J$| 
|$\sigma_{i, J}$|Parameter|Standard deviation of the regression model|

\begin{equation}
log(N_{i, t}) \sim {\sf Norm}(N0_{i, J} + log(\lambda_{i, J}).t, \sigma_{i, J})
(\#eq:L)
\end{equation}

### Analysis of the adult survival 

The adult survival is commonly assessed by capture mark recapture [@Lebreton2001]. Combined to annual population growth rate, adult survival rate allows recruitment rate to be estimated each year, and then compared with the outputs of the counting method. Unfortunately, no tagging study was performed on the two ruddy duck populations. However, the relationship \@ref(eq:GR) could still produce annual adult survival estimates that can be discussed. To do so, annual population growth rates overlapping the counting time series were estimated. 

Inconsistency on recruitment rate output, which is defined on $[0;\infty]$, would either lead to survival output $<0$, which points out an overestimation of the recruitment rate, or $>1$, which reflects that the recruitment rate cannot support the observed population growth and is thus underestimated. In between the two limit values, one still discuss the survival outputs by comparison with survival values published in the literature. 

### Comparison to a proxy of maximum recruitment rate

When population growth is not limited by a harvest pressure and/or by density-dependent effects, both recruitment rate and adult survival are close to their maximum potential. Given a conservative assumption on the maximum adult survival of ruddy ducks and maximum population growth rate estimated during no harvest time period, one can provide a proxy of the maximum recruitment rate. If the recruitment rate outputs of the model are significantly superior to this proxy, it means that the method overestimates the recruitment rate, which may reflect that some adult males are not detected during count surveys. By splitting the recruitment rate outputs upon population and harvest category, one can discuss of the impact of the harvest on the recruitment rate. 

To do so, one estimates maximum population growth rates over time periods observing no harvest using Equation \@ref(eq:L). Since the estimate of the UK population size is noisy below 500 individuals, one rejected data before 1972 (Figure \@ref(fig:grts)). After reaching the threshold of 1000 individuals, the UK population growth observed a strong inflexion whereas no harvest pressure was applied. This observation led to split the analysis into two time series for the time periods used in the maximum growth rate inference in UK (Figure \@ref(fig:grts)). 

\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../Output/plot_3} 

}

\caption{(ref:grts)}(\#fig:grts)
\end{figure}

To get proxies of maximum recruitment rate from Equation \@ref(eq:GR), one uses a very conservative prior on the adult survival by setting a uniform distribution on $[0.7; 0.9]$ [the lower limit corresponds to the upper range of survival rates observed in literature for species of similar weight, the upper limit corresponds to the range of survival rates of long-life species, @Nichols1997; @Krementz1997; @Buxton2004]. The average recruitment rates from the count data are estimated for each population and for each harvest category and compared to the maximum recruitment rates. 

For all the sub-models in Section \@ref(MM), the Bayesian framework was used for its efficiency and simplicity to propagate error through the parameters. One generated three chains of length 500000, with a thinning of 10 to avoid autocorrelation in the samples, and one discarded the first 2000 samples as burn-in. Chain convergence was assessed using the Gelman and Rubin convergence diagnostic (R<1.1, @Gelman1992). One fit the models using NIMBLE [@Valpine2017] run from R [@RCT2020]. Data and code are available here: https://github.com/adri-tab/Ruddy_duck_recruitment_rate. The values *X[Y; Z]* reported in results are the medians and the associated boundaries of the 95% confidence interval of posterior distributions. The median was preferred to the mean because of its robustness to skewed distribution. 

# Results

## Recruitment rate variability

The adult male proportion, which is necessary to infer on the recruitment rate from count data, is estimated at 0.60 [0.59; 0.61]. The counting method provides estimations of the recruitment rate, see Figure \@ref(fig:rrts)A. In UK, values vary from 0.06 [0.05; 0.07] to 0.27 [0.20; 0.34] recruits per breeder, and in France, 0.08 [0.04; 0.12] to 0.76 [0.64; 0.88] recruits per breeder. There are significant year-to-year variations with a maximum amplitude of 0.68 recruits per breeder in France (9.5-time variability). On the available period, recruitment rate in UK is smaller than in France. Recruitment rates are estimated in both populations over 5 years, and no correlation is observed between the two populations. The lowest recruitment rate observed in France, which is in 2019, corresponds to the start of the depletion of the population. It is noticeable that it is in the same range than the UK values, also estimated during a strong population depletion.

Recruitment rates estimated from UK sample data are consistently higher than estimates from the corresponding count data, Figure \@ref(fig:rrts)A. The correlation between the two methods is strong and points out that the sampling method overestimates the recruitment rate by 44% compared to the counting method, Figure \@ref(fig:rrts)B. Estimation precision is similar for the two methods. The UK population started to deplete from 2006, but no significant shift is observed between the two periods. 
 
\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../Output/plot_4} 

}

\caption{(ref:rrts)}(\#fig:rrts)
\end{figure}

## Consistency of the corresponding adult survival

Adult survival rates were deduced from the recruitment rate estimates and the observed population growth rates, see Figure \@ref(fig:srts)A. In UK, values that are obtained from the counting method ranged from 0.21 [0.20; 0.22] to 0.68 [0.65; 0.72]. In France, survival rates vary from 0.39 [0.34; 0.44] to 1.08 [0.98; 1.17]. Like recruitment rates, there is significant year-to-year variability without clear temporal structure in both populations with a maximum amplitude of 0.69 in France (2.8-time variability). Unlike the recruitment rate, the survival rate in 2019 in France is not a low outlier.

As expected, survival rates estimated from UK sample data are consistently lower than estimates from the corresponding count data, Figure \@ref(fig:srts)A. The correlation between the two methods points out that the sampling method underestimates the survival rate by 32% compared to the counting method, Figure \@ref(fig:srts)B. Estimation precision is similar for the two methods. The sample time series shows that the adult survival is significantly lower after 2006, which corresponds to the start of the depletion of the UK population.

\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../Output/plot_5} 

}

\caption{(ref:srts)}(\#fig:srts)
\end{figure}

## Comparison to a proxy of maximum recruitment rate

Maximum population growth rates are similar for the two populations, 1.45 [1.36; 1.55] and 1.47 [1.29; 1.68] for UK and France respectively, which corresponds to an increase of about 45% per year (Figure \@ref(fig:mgr)). After reaching a certain size (~ 1000 individuals), the UK population growth rate falls to 1.06 [1.04; 1.07], which corresponds to an increase of 6% per year, even though no harvest pressure was applied on it. 

\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../Output/plot_6} 

}

\caption{(ref:mgr)}(\#fig:mgr)
\end{figure}

Upon the conservative assumption of a maximum adult survival uniformly distributed over 0.70 to 0.90, the maximum recruitment rate reaches 0.65 [0.51; 0.75] and 0.66 [0.45; 0.90] recruits per breeder for UK and France respectively (Figure \@ref(fig:grcomp)). When there is no harvest, the estimated average recruitment rate of the French population is in the range of the proxy of the maximum recruitment rate, 0.51 [0.46; 0.57] recruits per breeder. When there is harvest, the average recruitment rates are 0.14 [0.12; 0.16] and 0.33 [0.31; 0.36] recruits per breeder for the UK and France respectively. Both values are significantly lower than the maximum recruitment rate, and it is noticeable that the average recruitment rate is lower for the UK population, which is in depletion over the count time series, than for the French population, which of the dynamic is mostly stable over the monitored period. 

\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../Output/plot_7} 

}

\caption{(ref:grcomp)}(\#fig:grcomp)
\end{figure}

# Discussion

Count data in waterfowl that differentiate adult males from other individuals are successfully used in this study to infer estimations of recruitment rates. In the following section, one checks the accuracy of the method from indicators developed for this purpose. From the outputs on the two populations of ruddy duck, one discusses about the effects of the harvest on the recruitment rate and its implication on waterfowl management.

## Accuracy of the counting method

The recruitment rates from the counting method vary in the same range for the two populations, which demonstrate a certain robustness of the method. The higher variability observed in France is not unexpected because the French time series is longer and covers a larger harvest spectrum than in UK. There is no temporal correlation between the two populations, so it suggests that the recruitment rate fluctuations are more likely influenced by regional-scale factors than large-scale ones. This is consistent with the literature because both nesting/hatching success and juvenile survival are conditional to the onset of lay, which is tightly related to local weather parameters, e.g. the spring temperature, the cumulative rainfall, and the water levels [@Folliot2017; @Blums2004; @Dzus1998].

Assuming that the sample data provide a good picture of the interannual variability of the recruitment rate, the good correlation to the outputs of the counting method demonstrates the ability of this method to track the recruitment rate variability. This result is quite robust because the correlation is obvious with only a five-year time series. However, this conclusion might be overrated because two successive population size values are part of the recruitment rate, which mechanically scale the outputs. It explains why the correlation on the immature proportion, the other component of the recruitment rate, is a bit more noisy than for the recruitment rate (Figure \@ref(fig:sup)A). The uncertainty of the estimates is very close to the outputs from the sampling method. Even though the counting method is more indirect to infer on the recruitment rate, it performs to provide a good confidence of the outputs which is satisfying. 

A significant change in adult sex ratio would bias the estimation of recruitment rate. However, since this ratio integrates many age-cohorts, the temporal autocorrelation is structurally strong. Significant variations can then occur only every multiple years, especially if it is a long-living species. Consequently, it is not necessary to monitor and update the adult sex ratio on a yearly base.  

The counting method always provides recruitment rates significantly lower than the sampling method by factor close to 2 on average, a result that is in line with the hypothesis that the harvest is generally selective towards immature individuals in waterfowl because they are more vulnerable to hunters [@Bellrose1980; @Fox2014]. This is a robust result because the approaches used to control ruddy ducks differ strongly from usual hunting practices and are apriori less selective. It is especially clear in the UK, for which control operations occur in winter using long-range rifles that prevents the bird from escaping from the threat. The assumption on the accuracy of the counting method is then not excluded from the consistency of this result. 

Annual adult survival rates deduced from observed population growth rates and recruitment estimates are on average in line with the literature **ref required**. which confirms that the counting method is poorly biased if there is some bias.

survival in the range of the expected values -> so no apparent issue of scaling issue, 
so we the hypothesis that male-like individuals corresponds only to adult male is validated.

*average recruitment rate compared to a proxy of maximum recruitment rate*

always below the maximum recruitment rate so satisfying. 

overall, even if we don't have access to the true value of the recruitment rate to test the accuracy of the counting method, a beam of arguments support that the method is not heavily biased, unlike the sampling method, as demonstrated in @Fox2014, even if one considers a picture limited to the end of the hunting season [@Fox2016].

## Influence of harvest on the recruitment rate

Variability within harvest category represents  

## Implication in waterfowl management

Survival higher than one but warning, observation error, so annual population growth rate might be inaccurate on some years. Also, we assumed a closed population, but it is possible that years with adult survival higher than one corresponds to an arrival of birds from the other population. As a consequence, the recruitment rate on these years may also be overestimated. 

No impact on the adult survival in France, but it seems that there is an impact in UK.
Why, because harvest occurs mostly in summer in France, and in winter in UK.

The harvest pressure, simplified into a two-category variable, explains already 19.3% and 4.6% of the variance of the recruitment rate and the adult survival rate of the French population. The harvest pressure described as a continuous variable is expected to explain even more the recruitment rate variability. The harvest pressure applied in France is thus a factor that likely induces a strong immature mortality, but a moderate one on the adults. 



Impact on the recruitment rate in France, and no impact in UK.


Overall, the harvest seems to impact differently France and UK, likely because the harvest mortality occurs mostly in summer in France, and in winter in UK. 

But caution, a population model linking the harvest rate to state variables describing the population is necessary to check precisely the impact of the harvest pressure, and test hypothesis on the functioning of harvest mortality.

Recruitment rate decrease with the harvest for the French population, and the UK population recruitment rate is also below the French one. 
it seems quite clear that the harvest affects the recruitment rate. 

*reference to introduction*

It is quite consistent with the assumption in introduction. Since studied populations are in colonization of their habitat, maximum densities are not reached, and then no or limited density dependent effect occur. 

If counting method acceptable, moderate harvest pressure does not reduce the recruitment rate as expected, maybe due to density dependent mortality for juvenile but it affects it if strong harvest rate. That is very interesting bro but warning, these interpretation holds on weak estimations and adult survival equal to 1!!!

it may be possible that first sight of density dependence occur in UK from 1000 individuals, but was not big enough to fully curb the population growth. Since there is a strong correlation between the number of birds observed and the number of sites occupied by the birds, this inflexion may be simply due to a limited number of new favourable sites to be colonized.

What about heterogeneity, figure 8 -> One said that recruitment rate can increase if harvest because of heterogeneity and density dependence, but it is not what we observe in France, likely because the exploitation is high inhere and consequently impact. 

## Use of recruitment rate

*better than sex ratio*
a same value of age ratio can correspond to different fecundity levels, so warning. 
a same value of age ratio for a population in depletion and a population stabilized are not similary productive, the first one is less productive than the second one ! 
so warning, focus on recruitment rate and not on age ratio if the dynamics of the population varies! recruitment proportion is commonly used in to discuss the fecundity of a population, but unlike the recruitment rate, this indicator does not consider the dynamics of the population and does reflect fecundity only on stable growth period

the consistency between the two max growth rate strenghtens that seems to be a biological limit for this species, at least in the western Palearctic. 

As described before, recruitment rate over the studied time period is far lower for the UK than France (2.5 factor). 
Underlined adult survival are comprised between 0 and 1, whatever the method. So we cannot conclude if one can be rejected. 

France affected the recruitment, UK affected adult survival for a similar result -> depletion

whereas adult survival more consistent between the tow (.5 factor), meaning there was more variability in recruitment rate than in survival rate. growth rate difference is thus more explained by recruitment rate fluctuation than adult survival fluctuation, consistent with the demographic buffering, or canalization hypothesis [@Gaillard2003; @Lenzi2021] 

survival values compared to literature? s

Abundance index are considered exact, which should not be true. and lead to structural error in the estimates of annual growth rate. 

@Arnold2018 underlines the lack of data on recruitment rate for modellers (in discussion) and that new methods are required to track this variable that often drive the population growth rate

Recruitment rate drives lambda? see @Arnold2018

 (only X years), meaning no large scale factor drive the fluctuations of productivity. 

adult sex ratio in line with other duck species [@Wood2021]

This time lag might be not a issue if the survival of immature is close to the adult survival between the two periods. winter count and breeding season

According to @Blums2004, the recruitment for diving ducks is not related to the fecundity, but depends mostly on the juvenile survival, which is driven by weather conditions. Studies on other birds also conclude of the poor correlation between fecundity and recruitment [e.g. @Murray2000]. 

No correlation between the two populations on recruitment rate, so maybe no large scale drivers of recruitment rate variability. 

age proportion -> [0, 1]
recruitment rate [0, +inf] donc plus sensitive

recruitment rate the most sensitive parameter to density dependent effect [@Koons2014]

Hypothesis of time invariant sex ratios in immature and adult: limits

For hunted duck species in North America, the mortality rate on first-year immatures compared to adults is higher by a factor ranging from 1.4 to 2 (Bellrose 1980). 

recruitment rate: 
Stabilité grosse pop, 
variabilité petite pop. 

Au dela du fait que la croissance de la pop est la somme de la survie et de la recruitment rate, 
intéressant de parler de la variability de la survie vs variabilité de la recruitment rate 
pour parler de risk management.

recruitment rate: drop from 2009

one considers harvest as an impact only adult survival, not on age structure, but we should, oversimplistics

blabla

@Nichols2006 -> il faut compter de la façon dont on demande

Taper des jeunes c'est pas forcément très utile car density dependence

If the maximum recruitment rate is not reached under optimal conditions (maximum population size not reached, and limited harvest pressure), it is a signal that one should explore the intermediate life stages before recruitment to provide a diagnostic on the changes of the environment conditions that have affected the recruitment rate. 

Condition of application to other species. 

Message de l'article

# Acknowledgments {-}

This work was partly funded by the LIFE Oxyura project (LIFE17 NAT/FR/000942) through the LIFE program. This work was carried on with the impulse of Jean-François Maillard, and Jean-Baptiste Mouronval from the *Office Français de la Biodiversité*, and Jean-Marc Gillier from the *Société Nationale pour la Protection de la Nature*. The authors acknowledge all the contributors of the data collection in the UK and in France, especially: **UK names**, Vincent Fontaine, Denis Lacourpaille, Justin Potier, Alexis Laroche, Médéric Lortion, Jules Joly, and Valentin Boniface.

# References {-}

<div id="refs"></div>

\newpage

# (APPENDIX) Supplement {-} 

# Supplement

\begin{figure}[H]

{\centering \includegraphics[width=1\linewidth]{../Output/plot_8} 

}

\caption{(ref:sup)}(\#fig:sup)
\end{figure}

(ref:flock) Typical observation of a ruddy duck flock in winter: 4 male-like individuals with a white cheek and a black cap, 10 female-like individuals with a whitish cheek and a dark stripe across it, 3 unidentified individuals © Jay McGowan - 3 February 2013 - Tompkins, New York, United States

(ref:pmal) Apparent male proportion in winter counts vs male proportion in samples; for the first category, one point corresponds to the proportion of male-like plumage in a population counted in winter; for the second category, the male proportion is estimated from all ruddy ducks sampled over one year

(ref:count) Evolution of the size of the two ruddy duck populations in light of the harvest pressure and the data time series

(ref:grts) Evolution of the size of the two ruddy duck populations in light of the harvest pressure; on time periods without harvest, maximum population growth rates can be estimated (slopes in red for the UK and in blue for France)

(ref:rrts) (A) Estimates of recruitment rate, i.e. average number of recruits produced per breeder; (B) Comparison of the recruitment rate values between the two estimation methods; bars define the 95% confidence intervals

(ref:srts) (A) Estimates of adult survival rate using population growth rates; (B) Comparison of the survival rate values between the two estimation methods; bars define the 95% confidence intervals

(ref:mgr) (A) Estimates of the maximum population growth without harvest $(\lambda - 1)$; two estimates were produced for the UK population because the growth rate dropped significantly when its size reached more than 1000 individuals; (B) Growth rate estimates; bars define the 95% confidence intervals

(ref:grcomp) Comparison between proxies of maximum recruitment rate and average recruitment rates estimated with and without harvest (*counting* method only); proxies are estimated by using the maximum growth rates; bars define the 95% confidence intervals

(ref:sup) (A1) Estimates of the proportion of the immature individuals in the wintering population; (B1) estimates of the number of recruits, i.e. the young adults reproducing for their first time; (A2) & (B2) comparison of the values between the two estimation methods; bars define the 95% confidence intervals
