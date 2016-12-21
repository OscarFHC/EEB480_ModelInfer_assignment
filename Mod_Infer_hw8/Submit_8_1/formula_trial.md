# NRE538_Resampling
Feng-Hsun Chang  
2016.11.22  



\newcommand\expect[1]{\mathbb{E}\left[{#1}\right]}
\newcommand\var[1]{\mathrm{Var}\left[{#1}\right]}
\newcommand\loglik{\ell}
\newcommand\prob[1]{\mathbb{P}\left[{#1}\right]}
\newcommand{\dd}[1]{\mathrm{d}{#1}}

* What does the shape of the likelihood surface tell us about the identifiability of the parameters? What does it say about which aspects of the data are most informative?

The following is the likelihood contour of the original model with the `logit` function describing the hazard function and the `logit` function depends linearly on the carapath length ($L_i$), i.e. $\mathrm{logit}(p_i) = a + b\,L_i,$. 

This shape indicates that on a certain axis of parameter "a" and "b" combination, the likelihood of observing this given data are similar. Along this combination axis, the data are not informative to tell if the model is good enough to describe (or explain?) the data. 

Here I make the infection hazard a function of carapace length. I first try the hazard function in the Michaelis-Menten form. 

$$\lambda(L) = \frac{r_mL}{K+L}$$,  
where $r_m$ is the highest probability an individual could be infected, and $K$ describes the carapace length where the individual has half of the $r_m$. 

Accordingly, the CDF the determines the probability of an individual to be seropositive is 

$$\prob{X_i=\text{pos}} = 1 - \exp{\left[-\int_0^L\!\lambda(L)\dd{L}\right]} = 1 - \exp{\left [-r_m\,K (\frac{L}{K} - Log(1 + \frac{L}{K}) \right ]}$$.

$$Y_i\ \stackrel{\text{i.i.d.}}{\sim}\ f(g^{-1}((X\,\beta)_i),\dots)$$

$$f(g^{-1}((X\,\beta)_i),\dots)$$

- The $f$ is the Error distribution, BUT! it is actually the distribution of your dependent variable (i.e. the **data distribution**)... Theoretically, this can be ANY distribution.
- The $g^{-1}$ is the **link function**. 

To determine what data distribution to use requires you to understand how your data is being generated. For example, it would be logical to use [Bernoulli distribution](https://en.wikipedia.org/wiki/Bernoulli_distribution) as the data distribution if you are dealing with a yes-no data set. To determine what link function to use requires knowledge of the data, theoretical knowledge and especially the empirical fit to the data. Most of the time, there is a special link function that goes together with the data distribution, which is also called canonical link function. However, always keep in mind that __chosing data distribution and link function is just two pieces of the model__. It is just like figuring out what independent variables to include in your model.  
The following two posts are pretty clear in explaining link function and some conceptual theory of GLM. I encourage you to read them.  
[GLM](http://stats.stackexchange.com/questions/40876/difference-between-link-function-and-canonical-link-function-for-glm)  
[link function](http://stats.stackexchange.com/questions/20523/difference-between-logit-and-probit-models#30909)  

The data come from @johnson1973species [**faraway** package](http://people.bath.ac.uk/jjf23/ELM/), which describes the variables as follows:
