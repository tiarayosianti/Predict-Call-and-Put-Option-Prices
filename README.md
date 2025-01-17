# Implementation of Various Methods to Predict Call and Put Option Prices in NASDAQ 100

## Objective  
This project aims to predict the fair prices of call and put options, assisting investors in making informed decisions when buying or selling options to manage risks effectively. The study evaluates the performance of several pricing models to assess their accuracy and reliability in predicting option prices.

## Data  
The research focuses on stocks within the NASDAQ 100, including:  
- Advanced Micro Devices, Inc. (AMD)  
- Amazon.com, Inc. (AMZN)  
- Intel Corporation (INTC)  
- Lucid Group, Inc. (LCID)  
- NVIDIA Corporation (NVDA)  

## Methods  
1. **Black-Scholes Method**: A model for option pricing based on normally distributed returns.  
2. **Black-Scholes with Gram-Charlier Expansion**: An extension of the Black-Scholes model that adjusts for skewness and kurtosis in normally distributed returns.  
3. **Variance Gamma Model**: A method designed for variance gamma-distributed returns.  
4. **Variance Gamma with Antithetic Variance Reduction (AVR)**: A variance gamma-based model incorporating AVR to enhance computational efficiency.  
5. **Bowman-Shelton Goodness-of-Fit Test**: A test used to verify the normality of the data.


### Results  
- **Stock Distribution Analysis**:  
  - Three stocks (AMD, LCID, NVDA) were found to be normally distributed.  
  - All five stocks (AMD, AMZN, INTC, LCID, NVDA) exhibited variance gamma distribution.  

- **Black-Scholes with Gram-Charlier Expansion**:  
  - The Black-Scholes method with Gram-Charlier Expansion outperformed the standard Black-Scholes method.  
  - This improvement is evident from the Mean Absolute Error (MAE) comparisons, where the Gram-Charlier Expansion provided results closer to actual market prices by accounting for skewness and kurtosis.  

- **Variance Gamma Model**:  
  - For variance gamma-distributed returns, the Variance Gamma model produced lower MAE compared to the Variance Gamma model with Antithetic Variance Reduction (AVR), indicating that the Variance Gamma model performs better without AVR.  

- **Efficiency Comparison**:  
  - Among all methods, the Black-Scholes method with Gram-Charlier Expansion proved to be the most efficient. It delivered stock option prices that were closer to market prices while incorporating adjustments for skewness and kurtosis, without requiring extensive simulations.  


## Impact  
This project provides valuable insights into option pricing methodologies, equipping investors and financial analysts with tools to assess fair option prices and manage financial risks effectively.
