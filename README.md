# Statsbot

Statsbot is a simple application to collect data from multiple sources and publish them as a simple table.
There are many ways to solve this problem, whether using off-the-shelf tools like SqlBot.co, or an AWS lambda function.
Unfortunately, I wasn't able to find a solution that allowed pulling data from multiple sources and defining the queries outside of the source code.
Lambdas come close, but they're still a bit too complicated for non-engineers to tinker with (without writing more or less the same amount of code as this program).

So, Statsbot exists to solve a very specific job.
Given a list of metrics that need to be published somewhere, make it easy to flexibly define the queries for those metrics.
It should be easy for developers to define a new type of datasource, and non-devs should be able to add a metric using any previously-defined source without requiring a recompliation.

### Supported source types
Statsbot supports the bare minimum datasources required for our use, but its trivial to add new ones.

#### PostgreSQL

### Defining a new metric

