# RShiny_COVID-19


This R Shiny app was developed to visualize how the dynamics of COVID-19 state policies and various COVID-19 outcome measures are inter-related.
Specifically, COVID-19 outcome measures between 0 to 30 dayes from the onset of the COVID-19 state policies were compared.

COVID-19 outcome measures were referenced from IHME (Institute for Health Metrics and Evaluation of University of Washington), and COVID-19 state policies and their onset dates were referenced from healthdata.gov.


Although more thorough statistical analysis is required to support the claims herein, this exploratory visualization provides insights to potential hypotheses that may be worth investigating.

Such hypotheses include:

  (1) Earlier COVID-19 policies (implemented in March~April) were driven by increasing new cases. These policies were effective in bringinging the rate (daily difference in the new case) below 0, slowing down the infection rate. The difference between the peaks of the new confirmed case rate and the new death rate was 5 days (death peak slower). 
  
  (2) Nonessential business (start) policy and Shelter in place (start) policy are interesting that Shelter in place's confirmed case rate starts dropping off faster than Nonessential business confirmed case rate, but the death rates for both policies start dropping at the same time. Both Nonessential business and Shelter in place started later in comparisons to other earlier policies due to the greater population that was impacted. 
  
  (3) Childcare (start)'s confirmed case rate is similar to Entertainment, Food and Drink, and Gym (start)' confirmed case rates. However, Childcare policy's death rate starts dropping faster than other three.
  
  (4) All policy liftings (stop) were driven by low confirmed cases. However, the confirmed case rate started climbing (crossed 0) around 15 days after the issue dates, which is consistent with COVID-19's dormant period.
  
  (5) Mask mandate in public facing business is hard to interpret because of the wide dispersion. Majority of states implemented the around May, but some implemented only after 2~3 months later with 6 states that didn't implement the mask mandate (as of 10/15).
  
  (6) Mask mandates show quite different trends between public facing business (partial) and in all public space (full). It is difficult to make conclusions as of now without looking at how the date distributions differ. However, it can still be seen that both mask mandates were driven by high confirmed cases. It is also very interesting that the full mask mandate started with greater new confirmed cases but yields similar new deaths as partial mask mandate. Both mandates are associated with new confirmed cases and new deaths immediately decreasing, indicating that the policy started in response to greater cases and people already started taking individual measures to reduce the exposure. It is also worth noting that ICU overflow (=[total ICU capacity] - [average ICU bed usage]) for full mask mandate declines more rapidly than the partial public facing business mask mandate.
  
  (7) It seems that ICU overflow matches new deaths, which is frightening. 
  
  (8) New York and New Jersey accounted for 50% of the total confirmed cases in the U.S.
  
  (9) It would be helpful to see the correlations between the policy onset date and some of the count and rate parameters.
  


