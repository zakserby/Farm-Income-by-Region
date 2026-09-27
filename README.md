# Farm Income by Region

R script for AEM 6850 (Empirical Methods, Cornell, Fall 2025) that plots US farm income from 1990 to 2020 by USDA climate hub region.

Script:
- farm_income_plot.R : reads the ERS data, adds United States totals, plots net cash income, cash receipts, other farm-related income, government payments and total cash expenses for the US and each region in one multi-panel figure, adds a map of the regions, and saves it as a png

Data:
- ers_farm_inc_climate_hubs.csv : USDA ERS farm income by climate hub region, 1990-2020

Output:
- farm_income_by_region.png : multi-panel figure
