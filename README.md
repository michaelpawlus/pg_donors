### Planned Giving Script

Includes some feature engineering (binning)

Also, some exploratory tables and plots

Lastly, modeling with regression, trees and ensembles

pg_donors: looks only at donors to see trends on age and giving when they made the planned gift as opposed to current age and giving

pg_prospects: includes current age and giving since it also includes those that have not made a planned gift

In the columns of the csv:

gillett = Gillett Socity which is our planned giving society (1 = planned gift and 0 = no planned gift)

lub_yrs = Lubbers Years which is our count of Consecutive Giving.  I was being a little lazy and use this metric but a better metric to use in this place is count of years or fiscal years of giving (not consecutive -- just total count)
