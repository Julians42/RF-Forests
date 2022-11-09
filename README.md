# RF-Forests

## Thesis title brainstorm
 * Random Forests in Small Area Estimation Models with Applications to Forestry
 * Forget the Lorax, I am a SMERF and I speak for the trees: Comparing Sae Mixed-Effects Random Forest (SMERF) models to other estimators in FIA data. 


# Update November 9
I went back and just modified the MixRF package to our case (literally just changing args and grabbing new output but the random effects didn’t really “converge” for their model either? It just ran until the loglikelihood difference between iterations was small enough (and it looked like they were just jumping around and getting lucky). So maybe my implementation that I thought was wrong was actually correct? Anyway it interesting that we don’t actually see nice convergence - I wonder if this is something we could look at and try to improve in the model?


## Update Nov 2
Links:
* https://www.fs.usda.gov/research/treesearch 

See the overleaf for a draft of RF and SMERF methods. The following questions arose when writing these methods:
* Do we want to ceiling estimates to zero at some point in the SMERF function since `BA` can't be negative? Or will this bias our results?
* Krennmair et al. say "For non-sampled areas, the proposed estimator for the area level means reduces to the fixed part for the RF" $\hat{\nu}_i = \frac{1}{N_i} \sum_{j \in U_i} \hat{f}(x_{ij})$. *Note* Now realizing this just means we ignore the means term for subsections where there are zero sampled observations. 
* When using `lmer` in [`merf_model.R`](/models/merf/merf_model.R), when we predict the area level fixed effects, are these equivalent to the fixed effects, $\nu_i$, as written in krennmair? (see overleaf for context):
$$     \hat{y}_{ij}^{SMERF} = \hat{f}(x_{ij})+\hat{\nu_i} = \hat{f}(x_{ij})+
    \left(\frac{\hat{\sigma^2_\nu}}{\hat{\sigma^2_\nu}+\hat{\sigma^2_\varepsilon}/\pi_{k,i}^n}\right)
    \left(\frac{1}{\pi_{k,i}^n} \sum_{j \in s_{k,i}^n}\left(y_{ij} - \hat{f}^{OOB}(x_{ij})\right)\right)$$

* I'm maybe still a little confused about how the fixed effects model is actually fit and how the formula translates to what is actually being done (theory recs to go read?): `resid ~ -1 +(1 | SUBSECTION)`. Specifically, why are we using -1 as opposed to 1 for our intercept? What does a negative intercept mean? 
* How does `logLik` calculate the residuals? I'm used to the straightforward definition: $\sum_{r_{ij} \in X} \log \mathbb{P}(r_{ij}|M, \theta)$ where the model $M$ is like a normal distribution or something more complicated that we can just plug in. In the above fixed effects case, I would guess that we are essentially plugging into the normal and centering each $r_{ij}$ by the subsection means? But not sure and wondering if you had a good resource for this.

## Update Oct 26
### Theory
 ** Continued in Overleaf **
Per Krennmair 2022, we note that: "For instance, the training/test-set paradigm is
central to machine learning and conceptually transfers to the methodology of unit-level SAE-models: the survey data serves as a training-set to construct a proper model, while supplementary data (usually census, register or administrative data) of auxiliary information is used to predict final indicators over sampled and non-sampled areas." Thus we conclude that to be consistent with SAE models, we must train our MERF model on only the sample data. 
#### Setup and Model assumptions
Let $U$ denote a finite population with $N$ elements where the elements of $U$ are broken into $i$ domains $i=1,2, \dotsb n$. We let the target response variable, $y_{ij}$, represent survey measurement $j$ in domain $i$. We assume we have population auxillary information $x_{ij}$ over which we hope to assess domain level means $\mu_{i}$. The general mixed effects regression model 

fit RF model and then use OOB predictions in lmer model to get sample fixed effects.


#### Concerns
I'm writing my own MERF model (thus far modifying `mixRF` to take a formula) with the eventual goal of adding an MSE estimation option. Perhaps it's useless to just reformulate the `mixRF` function but it's helping me learn the algorithm. Anyway, I'm somewhat concerned about the second phase where we predict the fixed effects from the residuals. I'm wondering what the correct formula should be to get these predictions? The simplest option is just to group residuals by SUBSECTION and then average. Alternatively, we could use `lmer` to predict without a fixed effect, but there are maybe issues see [here](https://stats.stackexchange.com/questions/19134/is-it-possible-to-specify-a-lmer-model-without-any-fixed-effects). E.g. `resid ~ -1 +(1 | SUBSECTION)`. What is the best way of doing this?

## Update Oct 20
### New Findings
This past week I build, simulated on the cluster, and compared the simple random forest model to the other study models (PS, area and unit EBLUP, and zero inflation model), again using `tcc16` and `evi` as our predictors across the 2000 simulation samples. We find rather variable results when it comes to relative bias: for subsections Aa Ab Ad, Ag, and Ah, the random forest model performs as well or better than the zero inflation model, while subsections Ac, Ae, and Ai the random forest performs poorly. The EMSE results are surprising, with the random forest performing shockingly consistently low compared with all other study models. 
![bias plot](models/explore/plots/bias_plot_w_rf.png) ![emse plot](models/explore/plots/emse_results_rf.png)

### Directions for Future Work
The next project goal is implementing a mixed effects random forest model (MERF). The algorithm for fitting a MERF is iterative and similar to the expectation maximization algorithm where it alternates fitting a random forest with known fixed effects, to predicting the fixed effects using the random forest. I considered using `LongituRF` however I couldn't figure out how exactly to get this working for non-longitudinal data (perhaps I was missing something) but maybe the package is designed only for that? I then looked into another package `MixRF` which seems to have what we want - although there seems to be limited ability to tune this model. One issue I wanted to discuss with this package is that it doesn't seem to be well applied to the small area estimation problem because it calculates the fixed effects (over our subsections) on the predictions from the sample only. I think we should be fitting the fixed effects on the population data - otherwise we'd get sample bias? 

On the other hand, I am concerned about the computation problem if we end up coding our own MERF model as the iterative approach would mean we'll need to predict the 3 million pixels potentially 100 times (or whatever max_iter is set as). 

The other package option is to contact on of the authors of the paper that implements a similar MERF model to see if i can get their code. 


## FASRC Resources
* Home: [fasrc homepage](https://www.rc.fas.harvard.edu)
* Examples: [user codes](https://github.com/fasrc/User_Codes)
* Submitting a lot of jobs: [multiple job submissions](https://docs.rc.fas.harvard.edu/kb/submitting-large-numbers-of-jobs/)
* Non embarassingly parallel jobs: [openmpi](https://docs.rc.fas.harvard.edu/kb/parallel-computing-on-odyssey/)

## Other Resources
* [FIA treesearch](https://www.fs.usda.gov/research/treesearch)
* 
