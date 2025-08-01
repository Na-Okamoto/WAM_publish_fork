# Model structure

The original behavioural model predicts the probability that a participant will estimate the rule as *random* on each trial.
For participant *j* and trial *t* the model uses:

- `distance_{jt}`: absolute distance from the stimulus centre.
- `is_good_{jt}`: binary score feedback (1 = good, 0 = bad).
- `error_{jt}`: difference between predicted feedback from a centre-based rule and the observed score.

Error is computed from a threshold on distance.  If the distance is below the threshold the predicted feedback is 1, otherwise 0.  In the hierarchical version this step is approximated with a smooth logistic function for differentiability.

The internal accumulator `z_{jt}` updates with learning rates that depend on the feedback sign:

```math
z_{1} = \begin{cases}
  \alpha_B\,error_{1} & \text{if }is\_good_{1}=0 \\
  \alpha_G\,error_{1} & \text{if }is\_good_{1}=1
\end{cases}
```

For later trials:

```math
z_{t} = \begin{cases}
  \alpha_B\,error_{t} + \gamma\,z_{t-1} & \text{if }is\_good_{t}=0 \\
  \alpha_G\,error_{t} + \gamma\,z_{t-1} & \text{if }is\_good_{t}=1
\end{cases}
```

A bias term also depends on the score sign (`\beta_G` or `\beta_B`).
The choice probability on each trial is

```math
p_{jt} = \operatorname{logit}^{-1}(z_{jt} + \text{bias}_{jt}).
```

The observed response `behaviour_{jt}` is Bernoulli distributed with parameter `p_{jt}`.

# Bayesian dependencies

For each participant `j` we model parameters
`(\alpha_G, \alpha_B, \beta_G, \beta_B, \gamma, \theta)` (the threshold) as random effects drawn from group-level (fixed effect) distributions.

```math
\alpha_{G,j} \sim \mathcal N(\mu_{\alpha_G},\sigma_{\alpha_G}), \\
\alpha_{B,j} \sim \mathcal N(\mu_{\alpha_B},\sigma_{\alpha_B}), \\
\beta_{G,j}  \sim \mathcal N(\mu_{\beta_G},\sigma_{\beta_G}), \\
\beta_{B,j}  \sim \mathcal N(\mu_{\beta_B},\sigma_{\beta_B}), \\
\gamma_j     \sim \text{logit}^{-1}(\mathcal N(\mu_{\gamma},\sigma_{\gamma})), \\
\theta_j     \sim \mathcal N(\mu_{\theta},\sigma_{\theta}).
```

Group means and standard deviations themselves are given weakly informative hyperpriors.
Each participant's sequence of choices is conditionally independent given their parameters.

The joint probability factorises as

```math
\begin{aligned}
&p(\text{data} \mid \{\phi_j\})\prod_j p(\phi_j \mid \mu,\sigma)\prod_k p(\mu_k,\sigma_k),\\
&\quad\phi_j = (\alpha_{G,j}, \alpha_{B,j}, \beta_{G,j}, \beta_{B,j}, \gamma_j, \theta_j).
\end{aligned}
```
where the likelihood term is the product over trials of Bernoulli probabilities defined above.
