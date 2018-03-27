# MissingData
missing data project

correct the error in estimating daily average accelerometer data in the presensse of large amount of non-random missing data by:
1. Using weighted regression
2. Using imputed daily sum
3. Using K-nearest Neighbor

Paper: Statistical approaches to account for missing values in accelerometry data

File order:
1. data preparation and more
2. 100 simulations for method 1+2
3. 100 simulations for method 3
4. 100 simulations for adjustforwt method
5. 100 simulations for EMimputation
6. boxplot
7. mvpa true
8. mvpa simulations for method 1+2
9. sed true
10. sed simulations for method 1+2
11. mvpa+sed simulations with EM
12. true vs imputed mvpa&sed
