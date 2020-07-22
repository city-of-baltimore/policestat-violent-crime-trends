# PoliceStat Analysis

Routing plots and analysis used by CitiStat (Mayor's Office of Performance & Innovation, or OPI) to understand crime trends.

Uses publicly available data at [Open Baltimore](https://data.baltimorecity.gov). Programatically accesses data through an API using RSocrata package in R software.

Includes:  
- Rolling 7, 28, and 90 day totals by district and crime type (description)
- Deseasonalization using 2016-2019 averaged daily 90-day totals to remove seasonality from trends
