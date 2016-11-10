#' ---
#' title: "Modeling the noise: probability basics and the bestiary of distributions"
#' author: "Aaron A. King"
#' output:
#'   html_document:
#'     toc: yes
#'     toc_depth: 4
#' bibliography: ../course.bib
#' csl: ../plos.csl
#' ---
#' 
#' \newcommand\prob[1]{\mathbb{P}\left[{#1}\right]}
#' \newcommand\expect[1]{\mathbb{E}\left[{#1}\right]}
#' \newcommand\var[1]{\mathrm{Var}\left[{#1}\right]}
#' \newcommand\cov[1]{\mathrm{Cov}\left[{#1}\right]}
#' \newcommand\dist[2]{\mathrm{#1}\left(#2\right)}
#' \newcommand\dlta[1]{{\Delta}{#1}}
#' \newcommand{\dd}[1]{\mathrm{d}{#1}}
#' \newcommand{\transpose}{\mathrm{T}}
#' \newcommand\lik{\mathcal{L}}
#' \newcommand\loglik{\ell}
#' \newcommand{\scinot}[2]{#1{\times}10^{#2}}
#' \newcommand{\pd}[3][]{\frac{\partial^{#1}{#2}}{\partial{#3}^{#1}}}
#' \newcommand{\deriv}[3][]{\frac{\mathrm{d}^{#1}{#2}}{\mathrm{d}{#3}^{#1}}}
#' 
#' &copy; `r format(Sys.Date(),"%Y")` Aaron A. King.  
#' 
#' 
## ----echo=FALSE,results='hide'-------------------------------------------
library(plyr)
library(ggplot2)
library(reshape2)
library(magrittr)

#' 
#' ## Why variability matters.
#' 
#' * Signal vs noise
#' * The *shape* of the noise can be interesting and important
#' * Ecological variability can be important in itself
#' 	- Probability of extinction
#' 	- Speed of biological invasion
#' 	- Genetic drift
#' * Noise can reveal signal
#' * Noise as a fact vs noise as a modeling tool
#' 
#' ## Process noise vs measurement error
#' 
#' * Measurement error
#' * Process noise
#' * *Parus major* example
#' 
#' ## Crash course in probability
#' 
#' ### Axioms of probability
#' 
#' * Set of *elementary events*, $\Omega$
#' * Probability measure, $\mathbb{P}$
#' * $\prob{A} \ge 0$  
#' * $\prob{\Omega} = 1$  
#' * representation via Venn diagrams
#' * if $A \cap B = \emptyset$, then $\prob{A \cup B} = \prob{A}+\prob{B}$
#' * implications:
#' 	- $\prob{\emptyset} = 0$
#' 	- $\prob{\neg A} = 1-\prob{A}$
#' 
#' ### Conditional probability and independence
#' 
#' * $\prob{A|B} = \frac{\prob{A \cap B}}{\prob{B}}$
#' * $A$ and $B$ are independent if and only if $\prob{A \cap B}=\prob{A}\,\prob{B}$.
#' * Alternatively, $A$ and $B$ are independent if and only if $\prob{B|A}=\prob{B}$.
#' 
#' ### Bayes' rule
#' 
#' * $\prob{B|A} = \frac{\prob{A|B}\,\prob{B}}{\prob{A}}$
#' * Bayes' rule vs Bayesian statistics
#' * Example: the base rate fallacy
#' 
#' ### Random variables
#' 
#' - A function $X:\Omega -> U$ is a random variable.
#' - $U$ is called the *range* of $X$.
#' - It is important to distinguish between *discrete* and *continuous* random variables.
#' 
#' #### Cumulative probability distribution function (c.d.f.)
#' 
#' For random variable (r.v.) $X$, the c.d.f., $F_X(x) = \prob{X\le x}$.
#' 
## ----cdf,echo=FALSE------------------------------------------------------
data.frame(x=seq(0,10,length=100)) %>%
    mutate(F=pgamma(q=x,shape=3,scale=1)) %>%
    ggplot(aes(x=x,y=F))+geom_line()+
    labs(y=expression(F[X](x)))+
    scale_x_continuous(breaks=seq(0,10))+
    theme_bw()

data.frame(x=seq(0,10,length=100)) %>%
    mutate(F=pbinom(q=x,size=10,prob=0.3)) %>%
    ggplot(aes(x=x,y=F))+geom_step()+
    labs(y=expression(F[X](x)))+
    scale_x_continuous(breaks=seq(0,10))+
    theme_bw()

#' 
#' #### Quantile function
#' 
#' The quantile function $F_X^{-1}$ is the inverse of the c.d.f.
#' That is,
#' $$F_X(F_X^{-1}(p)) = p \qquad \text{and} \qquad F_X^{-1}(F_X(x)) = x.$$
#' 
#' If $0\le p \le 1$, then $F_X^{-1}(p)$ is the $p$-th quantile of $X$.
#' 
## ----qfun,echo=FALSE-----------------------------------------------------
data.frame(p=seq(0,1,length=1000)) %>%
    mutate(q=qgamma(p=p,shape=3,scale=1)) %>%
    ggplot(aes(x=p,y=q))+geom_line()+
    labs(y=expression({F^{-1}}[X](p)))+
    scale_x_continuous(breaks=seq(0,1,by=0.1))+
    theme_bw()

data.frame(p=seq(0,1,length=1000)) %>%
    mutate(q=qbinom(p=p,size=10,prob=0.3)) %>%
    ggplot(aes(x=p,y=q))+geom_step()+
    labs(y=expression({F^{-1}}[X](p)))+
    scale_x_continuous(breaks=seq(0,1,by=0.1))+
    theme_bw()

#' 
#' 
#' #### Probability density function (p.d.f.)
#' 
#' For continuous r.v. $X$, the p.d.f. of $X$, $f_X$, is a function such that, for any $a$, $b$ in the range of $X$,
#' $$\prob{a \le X < b} = \int_a^b\!f_X(x)\,\dd{x}.$$
#' From this definition, it's an easy consequence of the fundamental theorem of calculus that, if $f_X$ is continuous, then the p.d.f. is the derivative of the c.d.f.:
#' $$f_X(x) = \deriv{F_X}{x}.$$
#' 
## ----pdf-c,echo=FALSE----------------------------------------------------
data.frame(x=seq(0,10,length=1000)) %>%
    mutate(f=dgamma(x=x,shape=3,scale=1)) %>%
    ggplot(aes(x=x,y=f))+geom_line()+
    labs(y=expression(f[X](x)))+
    scale_x_continuous(breaks=seq(0,10))+
    theme_bw()

data.frame(x=seq(0,10,by=1)) %>%
    mutate(f=dbinom(x=x,size=10,prob=0.3)) %>%
    ggplot(aes(x=x,y=f))+geom_bar(stat="identity")+
    labs(y=expression(f[X](x)))+
    scale_x_continuous(breaks=seq(0,10))+
    theme_bw()

#' 
#' The discrete r.v., the p.d.f. is sometimes called the *probability mass function*.
#' For each value, $x$, in the range of $X$,
#' $$f_X(x)=\prob{X=x}.$$
#' 
#' 
#' #### Expectation and moments
#' 
#' * Expectation
#' * Mean
#' * Variance
#' * Higher moments
#' * Generalized moments
#' * Median & quantiles
#' 
#' #### Simulating random deviates
#' 
#' * Pseudorandom number generators
#' 
#' 
#' ----------------------------
#' 
#' ## A bestiary of probability distributions
#' 
#' **R** contains a great deal of distilled knowledge about probability distributions.
#' In particular, for each of a large class of important distributions, methods to compute probability distribution functions (p.d.f., i.e., density or mass functions), cumulative distribution functions (c.d.f.), and quantile functions are available, as are methods for simulating these distributions (i.e., drawing random deviates with the desired distribution).
#' Conveniently, these are all named using the same scheme:
#' 
#' &nbsp; | &nbsp;
#' --------------------------|----------------------------
#' `dxxx(x, ...)` | probability distribution function
#' `pxxx(q, ...)` | cumulative distribution function
#' `qxxx(p, ...)` | quantile function (i.e., inverse of `pxxx`)
#' `rxxx(n, ...)` | simulator
#' 
#' In the above `xxx` stands for the abbreviated name of the specific distribution.
#' In each case, the `...` indicates that additional, distribution-specific, parameters are to be supplied.
#' A complete list of distributions provided by the base **stats** package can be viewed by executing `?Distributions`.
#' 
#' ## Discrete probability distributions
#' 
#' ### Bernoulli
#' 
#' The Bernoulli random variable describes the outcome of a single binary trial.
#' It has only one parameter: the probability of a "success".
#' We write
#' $$X \sim \dist{Bernoulli}{p}$$
#' to indicate that $X$ can take two values (success/failure, yes/no, true/false, 1/0) and that it has value "success" with probability $p$.
#' 
#' A simple way to simulate Bernoulli random deviates in **R** is to generate uniform deviates and test these against $p$.
#' Thus, to generate 10 $\dist{Bernoulli}{p=0.3}$ random deviates, we can do
## ------------------------------------------------------------------------
x <- runif(n=10)<0.3; x

#' Since the Bernoulli distribution is a special case of the binomial distribution (i.e., with `size=1`, see below), a more **R**-ish way of doing the above would be
## ------------------------------------------------------------------------
rbinom(n=10,size=1,prob=0.3)

#' 
#' ### Binomial
#' 
#' 
#' A binomial r.v. is the sum of independent Bernoulli r.v.
#' In other words, a binomial r.v. represents the number of successes in some number, $n$, independent Bernoulli trials, each of which has the same probability of success, $p$.
#' If $X$ is a binomial r.v., we write
#' $$X \sim \dist{Binomial}{n,p}$$
#' and say that "$X$ is binomially distributed with *size* $n$ and *probability* $p$".
#' 
#' The following plots the p.d.f. of a binomial r.v.
## ----binomial-pdf-cdf,results='markup',echo=TRUE-------------------------
n <- 50
p <- 0.3

x <- seq.int(-2,n+2)
pdf <- dbinom(x=x,size=n,prob=p)

library(ggplot2)
ggplot(data=data.frame(x=x,pdf=pdf),mapping=aes(x=x))+
  geom_bar(aes(y=pdf),stat='identity',color=NA)+
  theme_bw()

#' 
#' 
#' --------------------
#' 
#' ##### Exercise
#' 
#' Write codes to plot the c.d.f. of the binomial distribution.
#' Verify the relationship between the p.d.f. and the c.d.f.
#' 
#' ------------------------
#' 
#' 
#' 
#' The binomial p.d.f. has the formula
#' $$\mathbb{P}[X=x] = \frac{n!}{x!\,(n-x)!}\,p^x\,(1-p)^{n-x} = \binom{n}{x}\,p^x\,(1-p)^{n-x}.$$
#' We often abbreviate the expression on the right-hand size of the above as $\dist{Binomial}{n,p}$.
#' 
#' If $X \sim \dist{Binomial}{n,p}$, then $\expect{X}=n\,p$ and $\var{X}=n\,p\,(1-p)$.
#' 
#' --------------------
#' 
#' ##### Exercise*
#' 
#' A stock of glass test tubes is kept in a research laboratory.
#' Unfortunately, the lab PI loses his funding, so that he can no longer afford to replace broken test tubes.
#' Assuming that, initially, there are 100 test tubes and that each test tube has a probability $p=0.05$ of being broken each day, write **R** code that simulates the size of the stock as a function of time.
#' Through simulation, estimate the expected time to loss of the entire stock and the variance in this time.
#' Also, plot the estimated distribution of times to this day of reckoning.
#' 
#' 
#' ------------------------
#' 
#' 
#' 
#' ### Poisson
#' 
#' 
#' The Poisson distribution describes the number of independent random events occurring in a given interval of time or across a particular region of space.
#' Examples include lightning strikes and settlement events (bacterial colonization, seed settlement).
#' Thus, for example, if lightning strikes occur completely at random at a rate of $\mu$ strikes per km$^{2}$ per yr, then the number, $X$, of strikes occurring in a given zone of area $A$ km$^2$ in an interval of duration $T$ yr is a Poisson r.v. with parameter $\lambda=\mu A T$.
#' In general, the p.d.f. for a $\dist{Poisson}{\lambda}$ r.v. is
#' $$\prob{X=x} = \dist{Poisson}{\lambda} = \frac{\lambda^x}{x!}\,e^{-\lambda}.$$
#' 
#' --------------------
#' 
#' ##### Exercise
#' 
#' Use **Rstudio**'s `manipulate` facility to plot the p.d.f. and c.d.f. of the Poisson distribution for several values of the parameter $\lambda$.
#' See [`poisson.R`](./poisson.R) for an example.
#' 
#' ------------------------
#' 
#' The expectation and variance of a Poisson r.v. are both equal to $\lambda$.
#' That is, if $X \sim \dist{Poisson}{\lambda}$, then $\expect{X}=\var{X}=\lambda$.
#' 
#' --------------------
#' 
#' ##### Exercise
#' 
#' Verify numerically that the Poisson distribution can be approximated by a binomial distribution:
#' $$\dist{Poisson}{\lambda} \approx \dist{Binomial}{n,\lambda/n}
#' $$
#' for sufficiently large $n$.
#' Convince yourself that this makes sense intuitively.
#' 
#' 
#' ------------------------
#' 
#' 
#' 
#' 
#' ### Geometric
#' 
#' 
#' If you perform a sequence of independent, identical $\dist{Bernoulli}{p}$ trials, the number of failures you have before the first success will be geometrically distributed.
#' Let $X$ be this r.v.
#' Then
#' $$\prob{X=x} = p\,(1-p)^x = \dist{Geometric}{p}.
#' $$
#' 
#' If $X\sim\dist{Geometric}{p}$, then $\expect{X}=\frac{1-p}{p}$ and $\var{X}=\frac{1-p}{p^2}=\expect{X}+\expect{X}^2$.
#' 
#' This distribution also arises in ecological models.
#' Specifically, in a constant-rate birth process, the number of offspring of one parent in any interval of time is geometrically distributed.
#' 
#' ### Negative binomial
#' 
#' The negative binomial arises naturally in many different contexts.
#' First, it is the sum of geometric r.v., i.e., in a sequence of independent $\dist{Bernoulli}{p}$ trials the number of failures that occur before $k$ successes is negative-binomially distributed.
#' If $X \sim \dist{Negbin}{k,p}$, then
#' $$\prob{X=x} = \binom{k+x-1}{k-1}\,p^k\,(1-p)^x.
#' $$
#' We refer to $k$ as the distribution's *size* parameter.
#' 
#' The negative binomial arises in ecological theory as the number of offspring that are born to $k$ mothers in a pure birth process over any interval of time.
#' As we'll see below, it arises also as a Poisson r.v., the rate parameter of which is itself Gamma distributed.
#' Finally, in modeling, the negative binomial is a useful inasmuch as it models count data with *overdispersion*.
#' 
#' If $X\sim\dist{Negbin}{k,p}$, then $\expect{X}=k\,\frac{1-p}{p}$ and $\var{X}=\expect{X}+\tfrac{1}{k}\,\expect{X}^2$.
#' Thus the negative binomial is overdispersed relative to the Poisson: it has a greater variance than the Poisson distribution.
#' The smaller the value of $k$, the greater the overdispersion.
#' 
#' It is often useful to use an alternative parameterization of the negative binomial.
#' We can parameterize the distribution by its mean, $\mu$, and size $k$ instead of by $p$ and $k$.
#' Then we have $p=k/(k+\mu)$, $\expect{X}=\mu$, and $\var{X}=\mu+\frac{\mu^2}{k}$.
#' **R** supports both of these parametrizations.
#' 
#' --------------------
#' 
#' ##### Exercise
#' 
#'   Use **Rstudio**'s `manipulate` facility to plot the p.d.f. and c.d.f. of the negative binomial distribution for several values of the parameters $k$ and $\mu$.
#' 
#' ------------------------
#' 
#' 
#' 
#' ## Continuous probability distributions
#' 
#' 
#' ### Continuous random variables
#' 
#' ### Uniform
#' 
#' 
#' The uniform distribution is a workhorse, underlying our ability to simulate all other distributions on a digital computer.
#' The `punif`, `dunif`, `qunif`, and `runif` commands in **R** give, respectively, its c.d.f., the p.d.f., quantile function, and simulator.
#' It is parameterized by its upper and lower limits.
#' In particular, if $X\sim\dist{Uniform}{a,b}$, then
#' $$  \prob{x < X < x+\dd{x}} = \begin{cases}
#'     \frac{\dd{x}}{b-a} & a \le x < b\\
#'     0 & \text{otherwise}.
#'   \end{cases}
#' $$
#' Thus the p.d.f. for a $\dist{Uniform}{a,b}$ distribution is just
#' $$  f(x) = \begin{cases}
#'   \frac{1} {b-a}, & a < x < b\\
#'   0, & \text{otherwise}.
#'   \end{cases}
#' $$
#' 
#' One place where the uniform distribution arises naturally is as the distribution of the c.d.f. of (almost) any continuous r.v.
#' In particular, let $X$ be any continuous random variable with a positive p.d.f. and let $F(x)=\prob{X<x}$ be its c.d.f.
#' Then $F(X)\sim\dist{Uniform}{0,1}$.
#' The following illustrates.
#' 
## ----echo=TRUE-----------------------------------------------------------
set.seed(483394413)
x <- rnorm(n=1e5,mean=23,sd=14)  ## x ~ Normal(23,14)
u <- pnorm(q=x,mean=23,sd=14)    ## u = F(x)
library(ggplot2)
ggplot(data=data.frame(x=x,u=u))+
    geom_histogram(aes(x=x,y=..density..),bins=30,color='black',fill=NA)+
    theme_bw()
ggplot(data=data.frame(x=x,u=u))+
    geom_histogram(aes(x=u,y=..density..),bins=30,boundary=0,color='black',fill=NA)+
    theme_bw()

#' 
#' -----------------------------
#' 
#' ##### Exercise
#' 
#' Suppose that $X_i\sim\dist{Uniform}{0,1}$ for $i=1,\dots,N$.
#' Let $Y$ be the number of $X_i$ lying within the interval $(a,b)$, where $0 < a < b < 1$.
#' Show by simulation that it is plausible that $Y\sim\dist{Poisson}{(b-a)\,N}$.
#' 
#' 
#' ------------------------
#' 
#' 
#' 
#' ### Normal
#' 
#' 
#' The normal distribution arises naturally whenever many random variables are added together.
#' The Central Limit Theorem is not actually one but several related theorems, all of which contain results about the distribution of sums of independent r.v.
#' Roughly speaking, whenever we have many independent r.v., provided they all have finite means and variances, then their mean will be approximately normally distributed.
#' 
#' Thus, in modeling, the normal distribution is a good description of what we typically mean by "noise".
#' 
#' The normal distribution is parameterized by its mean and variance, or, as we do here, by its mean and standard deviation (s.d.).
#' If $X\sim\dist{Normal}{\mu,\sigma}$, then
#' $$\prob{x < X < x+\dd{x}} = (2\pi\sigma^2)^{-1/2}\,\exp{\left(-\frac{(x-\mu)^2}{2\sigma^2}\right)}\,\dd{x}.$$
#' 
#' ### Exponential
#' 
#' 
#' The exponential distribution arises commonly in the context of *waiting times*:
#' the durations of intervals between the occurrence of random events.
#' For example, one might model the time until the next birth or death in a population, the next mutation, or the next spillover of a zoonotic virus into humans as an exponential random variable.
#' If $X\sim\dist{Exponential}{\mu}$ ($\mu$ is described as the *rate*), then
#' $$ \prob{x < X < x+\dd{x}} = \begin{cases}
#'     \mu\,e^{-\mu x}\,\dd{x} & x \ge 0\\
#'     0 & x < 0.
#'   \end{cases}$$
#' If $X\sim\dist{Exponential}{\mu}$, we also have
#' $$  \begin{gathered}
#'     \prob{X<x}=1-e^{-\mu\,x}, \qquad
#'     \expect{X} = \frac{1}{\mu}, \qquad
#'     \var{X} = \frac{1}{\mu^2}.
#'   \end{gathered}$$
#' 
#' --------------------
#' 
#' ##### Exercise
#' 
#'   Use **Rstudio**'s `manipulate` facility to plot the p.d.f. and c.d.f. of the exponential distribution for several values of the parameter $\mu$.
#'   Put in a vertical line to indicate the mean.
#' 
#' ------------------------
#' 
#' 
#' 
#' ### Gamma
#' 
#' 
#' The gamma distribution arises as a generalization of the exponential.
#' It, too, is frequently used to model waiting times.
#' Suppose $k$ mutations are required for a cell lineage to become cancerous, that each of the $k$ mutations occurs independently of the others, and that the waiting times are all i.i.d. $\dist{Exponential}{\mu}$.
#' Then the time to the occurrence of cancer is Gamma distributed with *rate* parameter $\mu$ and *shape* parameter $k$.
#' If $X\sim\dist{Gamma}{\mathrm{shape}=k,\mathrm{rate}=\mu}$, then $X\ge 0$ and
#' $$  \begin{gathered}
#'     \prob{x < X < x+\dd{x}} = \frac{1}{\Gamma(k)}\,(\mu x)^{k-1}\,e^{-\mu x}\,\mu\,\dd{x},\,\text{provided}\ x \ge 0\\
#'     \expect{X} = \frac{k}{\mu}, \qquad \var{X} = \frac{k}{\mu^2},
#'   \end{gathered}
#' $$
#' where $\Gamma$ is a mathematical function that generalizes the factorial (e.g., $\Gamma(k+1)=k\,\Gamma(k)$ for all $k>0$).
#' 
#' The example above shows that the sum of $k$ exponential r.v. is a gamma r.v.
#' The formulae above generalize, however, to all positive values of the shape parameter $k$.
#' 
#' A frequently used alternative parameterization is in terms of shape and *scale*;
#' the latter is just the reciprocal of the rate.
#' Both parameterizations are available in **R**.
#' 
#' --------------------
#' 
#' ##### Exercise
#' 
#'   Use **Rstudio**'s `manipulate` facility to plot the p.d.f. and c.d.f. of the gamma distribution for several values of the shape and scale (or rate) parameters.
#'   Put in a vertical line to indicate the mean.
#'   Be sure to explore both large and small values of the shape parameter.
#' 
#' ------------------------
#' 
#' 
#' ##### Exercise
#' 
#' Suppose that $X\sim\dist{Gamma}{2,3}$.
#' Verify by simulation that $F_X(X)$ is uniformly distributed.
#' 
#' -----------------------------
#' 
#' 
#' ### Beta
#' 
#' 
#' The beta distribution generalizes the uniform distribution and, like the uniform distribution, has support only on an interval.
#' It is useful as a model of probabilities, and can be transformed into a distribution on an arbitrary interval.
#' Two parameters determine the shape of the beta distribution.
#' If $X\sim\dist{Beta}{a,b}$, then we have $0 \le X \le 1$ and
#' $$  \begin{gathered}
#'     \prob{x<X<x+\dd{x}} = \frac{x^{a-1}\,(1-x)^{b-1}}{B(a,b)},\,\text{provided}\ 0 \le x < 1,\\
#'     \expect{X} = \frac{a}{a+b}, \qquad \var{X} = \frac{a\,b}{(a+b)^2\,(1+a+b)} = \frac{\expect{X}\,(1-\expect{X})}{1+a+b},
#'   \end{gathered}
#' $$
#' where $B(a,b)$ is the mathematical Beta function defined by $B(a,b)=\Gamma(a)\,\Gamma(b)/\Gamma(a+b)$.
#' 
#' --------------------
#' 
#' ##### Exercise
#' 
#'   Use **Rstudio**'s `manipulate` facility to plot the p.d.f. and c.d.f. of the beta distribution for several values of the first and second shape parameters.
#'   Put in a vertical line to indicate the mean.
#'   Be sure to explore the full ranges of the parameters.
#' 
#' ------------------------
#' 
#' 
#' 
#' ## Transforming random variables
#' 
#' 
#' We can make new r.v. by transforming given r.v.
#' To do so, we must be mindful of the rules by which probability distributions transform.
#' Discrete r.v. pose no problems, but the p.d.f. of continuous r.v. are densities and must be transformed as such.
#' The change-of-variables formula from integral calculus shows the way.
#' 
#' For example, the lognormal distribution is obtained from the normal distribution by exponentiation.
#' That is, if $X\sim\dist{Normal}{\mu,\sigma}$ and $Y=e^X$, then $Y\sim\dist{Lognormal}{\mu,\sigma}$.
#' Alternatively, if $Y\sim\dist{Lognormal}{\mu,\sigma}$, then $\log{Y}\sim\dist{Normal}{\mu,\sigma}$.
#' 
#' What does, $f_Y$, the p.d.f. of $Y$, look like?
#' Let's suppose $Y\sim\dist{Lognormal}{\mu,\sigma}$ and that $X=\log{Y}$.
#' Recall that the definition of the p.d.f. tells us that
#' $$  \prob{a < Y < b} = \int_a^b\!f_Y(y) \dd{y} = \int_{\log{a}}^{\log{b}}\!f_Y(e^x) \dd{e^x} = \int_{\log{a}}^{\log{b}}\!f_Y(e^x) e^x \dd{x}.
#' $$
#' On the other hand,
#' $$  \prob{a < Y < b} = \prob{\log{a} < X < \log{b}} = \int_{\log{a}}^{\log{b}}\!f_X(x)\,\dd{x}.
#' $$
#' Since $a$ and $b$ are arbitrary, we must have
#' $$  f_x(x) = f_Y(e^x)\,e^x \quad \text{and} \quad f_Y(y) = \frac{f_X(\log{y})}{y}.
#' $$
#' 
#' --------------------
#' 
#' ##### Exercise
#' 
#'   Plot the p.d.f. and c.d.f. of the lognormal distribution for various values of the parameters.
#'   Compare the lognormal distribution to the gamma.
#' 
#' ------------------------
#' 
#' 
#' 
#' More generally, when we define $Y=g(X)$, where $X$ has p.d.f. $f_X$, and $g$ is invertible, the transformation rule is
#' $$f_Y(g(x)) = \frac{f_X(x)}{|g'(x)|},$$
#' where $|\cdot|$ signifies the absolute value.
#' Equivalently, since $g$ is invertible, we can write
#' $$f_Y(y) = \frac{f_X(g^{-1}(y))}{|g'(g^{-1}(y))|}.$$
#' 
#' --------------------
#' 
#' ##### Exercise
#' 
#' Compute the p.d.f. of $Y=1/X$, where $X\sim\dist{Exponential}{\mu}$ using the change-of-variables formula.
#' Plot the p.d.f. and c.d.f. of $Y$.
#' As an extra challenge, determine $\expect{Y}$.
#' 
#' 
#' ------------------------
#' 
#' 
#' 
#' 
#' ## Mixing and compounding distributions
#' 
#' 
#' ### Finite mixtures
#' 
#' 
#' Consider the following scenario.
#' In a population of wild rabbits, the respiratory tracts of some animals are infected by a bacterium.
#' Additionally, all the rabbits are infected with a greater or lesser number of nematode parasites.
#' The intensity of nematode infection in host $i$, $H_i$, depends weakly on many factors, but strongly on whether the host is infected with the bacterium.
#' Let's model the bacterial infection status $B_i$ of host $i$ as a Bernoulli r.v., $B_i\sim\dist{Bernoulli}{p}$, where $p$ is the prevalence of bacterial infection among the rabbits.
#' Suppose the intensities of the worm infection in bacterium-free and -infected hosts, respectively are
#' $$  \begin{aligned}
#'     H_i|B_i\!=\!0\,&\sim\,\dist{Lognormal}{\mu_0,\sigma_0}\\
#'     H_i|B_i\!=\!1\,&\sim\,\dist{Lognormal}{\mu_1,\sigma_1}.\\
#'   \end{aligned}
#' $$
#' Then, unconditionally on $B_i$, host $i$ has a worm intensity that is said to be a *mixture* of two lognormal distributions.
#' 
#' The p.d.f. of this mixture distribution is
#' $$\prob{h<H<h+\dd{h}} = (1-p)\,\dist{Lognormal}{\mu_0,\sigma_0;h}\,\dd{h}+p\,\dist{Lognormal}{\mu_1,\sigma_1;h}\,\dd{h}.$$
#' 
#' --------------------
#' 
#' ##### Exercise
#' 
#' Use **Rstudio**'s `manipulate` facility to plot the p.d.f. and c.d.f. of this distribution for several values of $p$ and $\mu_1-\mu_0$.
#' Put in a vertical line to indicate the mean.
#' 
#' ------------------------
#' 
#' 
#' 
#' ### Zero-inflated distributions
#' 
#' 
#' An important special case of a finite mixture occurs when one process masks the expression of a process we wish to study, leading to an abundance of zeros in the data that do not represent the process under study.
#' For example, suppose that when our rabbits do shed bacteria, they shed a Poisson number of colony-forming units (CFU) each day.
#' However, even when severely infected, they may only shed on certain days.
#' We might model the event that bacteria are shed at all on any given day as a $\dist{Bernoulli}{p}$ process.
#' Thus, if $S_t$ represents the number of bacterial CFU shed on day $t$, and $Z_t$ represents the Bernoulli variable,
#' $$  \prob{S_t=0|Z_t\!=\!0} = 1 \quad \text{and} \quad S_t|Z_t\!=\!1\,\sim\,\dist{Poisson}{\lambda}.
#' $$
#' The following codes simulate one realization of this process, for $\lambda=10$ and $p=0.3$.
#' 
## ----zi-negin-sim--------------------------------------------------------
set.seed(296102642L)
t <- seq(0,90)
z <- rbinom(n=length(t),size=1,prob=0.4)
s <- ifelse(z==1,rpois(n=length(t),lambda=10),0)
ggplot(data=data.frame(t=t,z=z,s=s),mapping=aes(x=t,y=s))+
  stat_identity(geom='point')+
  theme_bw()+labs(x='day',y='CFU shed')
ggplot(data=data.frame(t=t,z=z,s=s),mapping=aes(x=s))+
  stat_bin(geom='bar',aes(y=..density..),binwidth=1)+
  theme_bw()+labs(y='probability density',x='CFU shed')

#' 
#' ### Compound distributions
#' 
#' 
#' In finite mixtures, each realization is drawn from one of several distributions, the choice following from the realization of a discrete random variable.
#' We can generalize this idea, by allowing the parameters of a distribution to themselves be r.v.
#' This is called *compounding* two distributions.
#' 
#' As an example, suppose that the rate at which s rabbit sheds bacteria is Poisson with a rate that is itself a gamma r.v.
#' To be precise, assume that $\Lambda_t$ is the rate of shedding and that
#' $$\begin{gathered}
#'     \Lambda_t \sim \dist{Gamma}{\text{shape}=k,\text{scale}=\frac{\lambda}{k}} \qquad S_t | \Lambda_t\!=\!\mu \sim \dist{Poisson}{\mu}.
#'   \end{gathered}$$
#' Under these assumptions, $\expect{\Lambda_t}=\lambda$ and $\var{\Lambda_t}=\lambda^2/k$.
#' What is the p.d.f. of $S_t$?
#' $$\begin{split}
#'     \prob{s<S_t<s+\dd{s}} &= \int_{0}^{\infty}\!\dist{Poisson}{\mu;s}\,\dist{Gamma}{k,\frac{\lambda}{k};\mu}\,\dd{\mu}\\
#'     &= \int_0^\infty\!\frac{\mu^s}{s!}\,\frac{1}{\Gamma(k)}\,\left(\frac{k \mu}{\lambda}\right)^k\,e^{-\mu(1+k/\lambda)}\frac{\dd{\mu}}{\mu}\\
#'     &= \frac{\Gamma(k+s)}{s!\,\Gamma(k)}\,\left(\frac{k}{k+\lambda}\right)^k\,\left(\frac{\lambda}{k+\lambda}\right)^s.
#'   \end{split}$$
#' The latter is the p.d.f. of a negative-binomial r.v. with shape $k$ and mean $\lambda$.
#' This gives another interpretation of the negative binomial distribution.
#' 
#' ### Beta-binomial distribution
#' 
#' 
#' Another useful distribution that arises through compounding is the so-called *beta-binomial*.
#' Let $P\sim\dist{Beta}{a,b}$ and $X|P\sim\dist{Binomial}{n,P}$.
#' Then $X$ is said to be beta-binomially distributed with size $n$ and shape parameters $a$ and $b$.
#' If $X\sim\dist{BetaBinomial}{n,a,b}$, then $X$ has p.d.f.
#' $$\prob{X=x} = \frac{B(a+x,b+n-x)}{B(a,b)}\,\binom{n}{x}.$$
#' 
#' The beta-binomial distribution is that it is an overdispersed version of the binomial distribution.
#' To see this, note that, if $X\sim\dist{Binomial}{n,p}$, then $\expect{X}=n\,p$, and $\var{X}=n\,p\,(1-p)$.
#' If $Y\sim\dist{BetaBinomial}{n,a,b}$, by contrast, then while we still have $\expect{Y}=n\,\expect{P}$, we have instead the larger
#' $$\var{Y}=n\,\expect{P}\,(1-\expect{P})\,\frac{a+b+n}{a+b+1}.$$
#' 
#' When using the beta-binomial to model an overdispersed binomial distribution, it is frequently useful to use an alternative parameterization in terms of mean, $p$, and dispersion parameter, $\theta$:
#' $$\begin{gathered}
#'       p = \frac{a}{a+b} \qquad \theta = a+b\\
#'       a = {p}\,{\theta} \qquad b = (1-p)\,{\theta}
#'   \end{gathered}$$
#' 
#' --------------------
#' 
#' ##### Exercise
#' 
#' Use **Rstudio**'s `manipulate` facility to plot the p.d.f. and c.d.f. of the beta-binomial distribution as $p$ and $\theta$ are changed.
#' Put in a vertical line to indicate the mean.
#' 
#' ------------------------
#' 
#' 
#' ### Compounding in general
#' 
#' 
#' More generally, suppose $\Theta\sim f$ is a continuous r.v. and $X|\Theta\sim g(\Theta)$.
#' We can obtain the p.d.f. of $X$ by computing
#' $$\expect{f(\Theta)}=\int\! f(\theta)\,g(\theta)\,\dd{\theta}.\tag{1}$$
#' For discrete $\Theta$, the formula is
#' $$\expect{f(\Theta)}=\sum\! f(\theta)\,g(\theta).\tag{2}$$
#' The integral and sum that appear in Eqs. 1--2 are each taken over the whole range of $\Theta$.
#' 
#' Three conditional probability identities that are of great use in this are the following:
#' $$\begin{gathered}
#'   \expect{X}=\expect{\expect{X|Y}}\\
#'   \var{X}=\expect{\var{X|Y}}+\var{\expect{X|Y}}\\
#'   \cov{X,Y}=\expect{\cov{X,Y|Z}}+\cov{\expect{X|Z},\expect{Y|Z}}.\\
#' \end{gathered}$$
#' 
#' 
#' ## The method of moments
#' 
#' The method of moments is a method of estimation that relies only on the law of large numbers.
#' It consists of fitting a model to data by matching moments of the model's distribution with the corresponding sample moments of the data.
#' Let's apply the method here to the estimation of seed-predation rates using the \citet{Duncan2000} data.
#' First, we'll fetch and preprocess the data:
#' 
## ----echo=TRUE-----------------------------------------------------------
library(magrittr)
library(plyr)
library(reshape2)
library(ggplot2)
options(stringsAsFactors=FALSE)

read.csv("http://kinglab.eeb.lsa.umich.edu/480/data/seedpred.csv",
         comment.char="#",
         colClasses=c(date="Date",station='factor',
                      dist='factor',species='factor')) %>%
  arrange(station,dist,species,date) -> seeds

seeds %>%
  ddply(~station+dist+species,summarize,
        avail=head(seeds,-1),
        taken=-diff(seeds),
        tint=diff(date)) %>%
  subset(avail>0) -> dat

summarize(seeds, taken=-diff(seeds))

read.csv("http://kinglab.eeb.lsa.umich.edu/480/data/seedpred.csv",
comment.char="#",
colClasses=c(date="Date",station='factor',
dist='factor',species='factor')) %>%
arrange(station,dist,species,date) -> seeds
seeds %>%
ddply(~station+dist+species,summarize,
avail=head(seeds,-1),
taken=-diff(seeds),
tint=diff(date)) %>%
subset(avail>0) -> dat


#' 
#' To begin with, let's focus only on those intervals that began with 5 seeds present.
#' 
## ----echo=TRUE-----------------------------------------------------------
fiveseeds <- subset(dat,avail==5)
pl <- ggplot(data=fiveseeds,mapping=aes(x=taken))+
  geom_histogram(binwidth=1,fill=NA,color='black',
                 mapping=aes(y=..density..))+
  theme_bw()
print(pl)

#' 
#' We'll try to fit a binomial distribution to these data by the method of moments.
#' We match the mean of the binomial distribution, $n\,p$, to the mean of the data, and solve for $n$ and $p$:
#' 
## ----echo=TRUE-----------------------------------------------------------
fiveseeds %>% summarize(n=unique(avail),p=mean(taken)/n) -> moms.binom
moms.binom

#' 
#' To test the goodness of fit, we can simply plot the estimated distribution on top of the empirical one.
#' 
## ----results='markup',echo=TRUE------------------------------------------
data.frame(x=seq(0,5)) %>%
    mutate(binom=dbinom(x=x,prob=moms.binom$p,size=moms.binom$n)) -> pdf

pl+geom_point(data=pdf,aes(x=x,y=binom),size=5)

#' 
#' The fit is not very good.
#' 
#' Now, let's try to fit a beta-binomial distribution to the data using the method of moments.
#' For $X\sim\dist{BetaBinomial}{n,p,\theta}$, we have
#' $$\begin{gathered}
#' \expect{X}=n\,p \qquad \var{X}=n\,p\,(1-p)\,\frac{\theta+n}{\theta+1}.
#' \end{gathered}$$
#' Solving for $p$ and $\theta$, we get
#' $$\begin{gathered}
#' p=\frac{m}{n} \qquad \theta=-n\,\frac{v-m(n-m)}{nv-m(n-m)},
#' \end{gathered}$$
#' where $m=\expect{X}$ and $v=\var{X}$.
#' Plugging in the empirical moments for these variables:
#' 
## ------------------------------------------------------------------------
library(emdbook) # this gives us the beta-binomial distribution

fiveseeds %>% summarize(n=unique(avail),m=mean(taken),v=var(taken),
                        p=m/n,theta=-n*(v-m*(n-m))/(n*v-m*(n-m))) -> moms.bb
moms.bb

pdf %<>% mutate(betabinom=dbetabinom(x=x,size=moms.bb$n,prob=moms.bb$p,theta=moms.bb$theta))

pl+geom_point(data=pdf,aes(x=x,y=betabinom),size=5)

#' 
#' --------------------
#' 
#' ## Assignment 7
#' 
#' Complete the analysis of the seed predation data by fitting binomial and beta-binomial distributions to the data for each distinct number of available seeds.
#' Construct a single plot to display the agreement between the data and the beta-binomial distribution.
#' For example, you might plot relative error, $\left\vert\text{observed}-\text{predicted}\right\vert/\text{predicted}$.
#' 
#' 
#' ----------------------------
#' 
#' ## [Back to course schedule](../schedule.html)
#' ## [**R** codes for this document](prob.R)
#' 
#' ---------------------------
#' 
#' ## References
#' 
