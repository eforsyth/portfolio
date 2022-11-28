# Cycle crash clusters

This little project aims to explore the spatial distribution of cycle crashes across Auckland. In doing so, it touches upon some geographic data science based approaches to the task, and highlights how even a relatively simple sounding task can encompass a bunch of methodological issues and technical work. Specifically, we cover spatial autocorrelation, data mining, and point pattern measures to the task of finding clusters of cycle crashes. To briefly summarise each:

- The spatial autocorrelation approach uses Monte Carlo (i.e. randomised) simulations to identify clusters of unexpectedly high counts
- The data mining approach uses density based clustering to discover dense clusters of crashes, as defined by a density function
- The point pattern approach ... 

Note that raw crash counts/events are used here. Using crash rates would yield addional insight (and is overall just a better idea), however calculating the rate is too much effort as of the time of writing, but could be added at some point in the future.

## Results

...

Under the hood, the spatial autocorrelation approach uses random simulations to estimate how likely the observed pattern is the result of chance. This becomes a problem for this particular task because we have such a small number of events (n = 1532) across a large area (all of Auckland), meaning that nearly any crash incident is significant. Thus, we have to ask whether this is the most methodologically sound approach for this task. This issue of small numbers can potentially be tackled by honing in on dense clusters of crashes through the use of data mining techniques.

...

Whether the set hyperameters are optimal, or even appropriate, is unknown (but very, very unlikely as it was just the first values I thought of). However, it works for demonstrative purposes.

<!-- 

demo **(ADD LINK?)**. 

# spatial autocorrelation approach:
# https://carto.com/blog/predicting-traffic-accident-hotspots-with-spatial-data-science/

# dbscan approach:
# https://towardsdatascience.com/mapping-the-uks-traffic-accident-hotspots-632b1129057b
-->
