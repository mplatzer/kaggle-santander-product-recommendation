# Kaggle Competition: Santander Product Recommendation

This repository reflects the source code of my submission to the `Santander Product Recommendation` Kaggle Competition, for which I ended up on [26th place](https://www.kaggle.com/c/santander-product-recommendation/leaderboard), out of 1809 total participants.

I used R (in particular [data.table](https://github.com/Rdatatable/data.table)) for data munging, and [LightGBM](https://github.com/Microsoft/LightGBM) for training a gradient boosting machine model. LightGBM turned out to be very fast and allowed for rapid iterations.

key ideas of this approach:
- use a single GBM model for binary classification (the product becomes yet another variable)
- use 2015-06 data for the two "seasonal" products `ind_reca_fin_ult1` and `ind_cco_fin_ult1`
- use last three months for all other products
- add 5-month lags of product usage (p1,..,p5)
- add flag whether each product was used at all in p2 to p5
- add count how many times each product was activated in p1 to p5
- add some customer covariate lags
- sanitize the customer covariates a bit
- leverage the information from the [leaderboard probes](https://www.kaggle.com/c/santander-product-recommendation/forums/t/25727/question-about-map-7?forumMessageId=146330#post146330)

many thanks to the Kaggle forum users and the Vienna Kaggle meetup for inspiring ideas!!
