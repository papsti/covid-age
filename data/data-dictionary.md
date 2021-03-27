# Data dictionary

## figure-1.csv

* `date`: date of infection report (in YYYY-MM-DD format)
* `resolved_by_end_date`: was the infection marked as resolved by 16 Feb 2021? ("yes" or "no")
* `count`: count of known infections for a given date and resolved status

## figure-2.csv

* `age_group`: age group in two year increments up to age 100
* `value_type`: description of the value reported in the `value` column
* `value`: value for a given age group and value type

## figure-3.csv
* `age_group`: age group in two year increments up to age 100
* `outcome`: non-fatal outcomes are nested and tallied by the most intensive medical intervention used for each patient (ventilator use is the most intensive, followed by intubation, ICU admission, and hospitalization); deaths are split by whether or not the patient also had a record of hospitalization for COVID-19 treatment
* `value`: count of known infections for a given age group and outcome

## figure-4a.csv
* `lower_age`: lower age associated with the age group (*e.g.* 30 is the lower age associated with the 30-31 age group)
* `num`: count of known infections with a record of hospitalization for the associated age group  
* `tot`: total count of known infections for the associated age group
* `prop`: proportion of known infections with a record of hospitalization for the associated age group (`num`/`tot`)
*  `lwr`: lower 95% exact binomial confidence interval for `prop`
*  `upr`: upper 95% exact binomial confidence interval for `prop`   

## figure-4b.csv
* `lower_age`: lower age associated with the age group (*e.g.* 30 is the "lower age" associated with the 30-31 age group)
* `num`: count of fatal known infections with a record of hospitalization for the associated age group  
* `tot`: total count of known infections with a record of hospitalization for the associated age group
* `prop`: proportion of known infections with a record of hospitalization that were fatal for the associated age group (`num`/`tot`)
*  `lwr`: lower 95% exact binomial confidence interval for `prop`
*  `upr`: upper 95% exact binomial confidence interval for `prop`
