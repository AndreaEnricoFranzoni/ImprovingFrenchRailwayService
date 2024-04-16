# Improving French railway system
Nonparametric statistics project @Politecnico di Milano, A.Y. 2023-2024
---------
A. E. Franzoni, N. Francescon, E. Garlanda <br />
Content of files: <br />
---------
Data preprocessing: aggregates data from 'Data_by_month.xlsx' into 'Data_by_year.xlsx' <br />
Exploratory analysis + Explaining delays: fits a permutational ANOVA for avg_delay_on_arrival and computes its 95% Bootstrap t-intervals. Moreover, a tentative linear model is fitted <br />
Cancellations and extreme delays: fits a bivariate robust analysis <br />
Dealing with strikes: computes reverse percentile intervals for the cancellation rate across the 47 months <br />
Data preprocessing - strikes removal: aggregates data from 'Data_by_month.xlsx' into 'Data_by_year_nostrikes.xlsx' <br />
Cancellations and extreme delays - no strikes:  fits again a bivariate robust analysis on the newly obtained dataset <br />
Cause analysis (multivariate + compositional): fits a bivariate robust analysis on the causes and deals with compositional data nature <br />
Final developments: performs functional analysis for different variables of interest to confirm the previous results <br />
report.pdf: statistical considerations and results' interpretation
