# Data and Analysis Code for an Experiment on Potassium Fertilizer Sources and Application Methods in Cotton Cultivation on Cerrado Soils

This repository contains the data and R code for the statistical analysis and results visualization of a study aimed at evaluating the effects of different potassium fertilizer sources and application methods on cotton cultivation in Western Bahia.

The study is currently being evaluated for publication on a scientific journal. After eventual publishing this repository will be updated with the DOI and link to the paper.

## Methodological Aspects

The experiment was conducted at Novo Milênio Farm in Luís Eduardo Magalhães, Bahia, Brazil (12°19'10.1" S; 45°54'18.8" W), on a plot with flat relief. The region's climate is classified as Aw (tropical with a dry winter) according to Köppen, with a mean annual temperature of 24ºC and an average annual precipitation of 1,200 mm. The study focused on cotton on the 2018/19 season, following soybean as the previous crop during the 2017/18 growing season under a no-tillage system.

Six fertilization treatments were evaluated, applying K₂O at 160 kg ha⁻¹ through different methods: pre-planting application of muriate of potash (MOP), polyhalite (Poly4), or a 50/50 blend of MOP/Poly4 (Blend), as well as post-planting (top-dressing) application of MOP and Poly4. A control treatment without K application was also included. The MOP used contained 60% K₂O, while Poly4 provided 14% K₂O, 19% S, 17% Ca, and 6% Mg.

The experiment followed a completely randomized block design with four replications. Each plot consisted of five six-meter-long cotton rows (0.76 m spacing), totaling 22.8 m². The sampling area included the two central planting rows, using the central 3 m section of each row.

### Leaf Sampling and Analysis

On February 18, 2019, during the flowering stage, approximately 20 leaves with petioles from the fifth position from the apex were collected per plot. The samples were dried in an oven at 65°C for 72 hours, ground, and analyzed for N, P, K, Ca, Mg, S, B, Fe, Cu, Mn, and Zn levels.

### Harvest and Fiber Quality Analysis

Harvesting occurred from June 29 to July 1, 2019. Thirty cotton bolls were collected from the middle third of plants in each plot to assess fiber quality. The remaining bolls within the plot sampling area were also collected, combined with the initial sample, and used to estimate cotton yield. The harvested material was weighed and transported to the Bahia Cotton Producers Association (ABAPA) laboratory in Luís Eduardo Magalhães. There, fibers were separated from the seeds, and fiber quality was analyzed using a High-Volume Instrument (HVI) machine. This system measures fiber bundle strength and provides simultaneous testing for multiple fiber properties, including micronaire (Mic), fiber length (Len), short fiber index (SFI), uniformity (Uni), strength (Str), and yellowing grade (+b). Productivity was evaluated based on fiber and seed yield, measured within each plot and extrapolated to kg ha⁻¹.

### Statistical Analysis

Data were tested for homogeneity of variances using Bartlett’s test and for normality of residuals using the Shapiro-Wilk test (p < 0.05). If necessary, the Yeo-Johnson transformation was applied to meet ANOVA assumptions. Analysis of variance (ANOVA) was performed using the easyanova package (Arnhold, 2013) in R (R Core Team, 2024). When the F-test indicated significant differences between treatments (p < 0.10), the Scott-Knott test was used for multiple comparisons (p < 0.10). Some results were also visualized graphically using the ggplot2 package (Wickham, 2016) in R.


