import random

import numpy as np
import matplotlib.pyplot as plt
import pymc3 as pm

data_slope = 2.0
data_intercept = 0.0
X = np.linspace(0, 10, 15)
y = np.random.normal(data_slope * X + data_intercept, scale=2.0)

with pm.Model() as linear_model:
    slope = pm.Normal("slope", mu=0, sigma=1)
    intercept = pm.Normal("intercept", mu=0, sigma=1)
    noise = pm.Gamma("noise", alpha=2, beta=1)
    y_observed = pm.Normal(
        "y_observed",
        mu=slope * X + intercept,
        sigma=noise,
        observed=y,
    )
    posterior = pm.sample(return_inferencedata=False)

samples = list(zip(posterior.get_values("slope"),
                   posterior.get_values("intercept")))

fig, ax = plt.subplots()
ax.scatter(X, y)
for (slope, intercept) in random.sample(samples, 50):
    ax.plot(X, slope * X + intercept, alpha=0.1)
ax.set_xlabel("x")
ax.set_ylabel("y")
fig.tight_layout()
plt.show()
