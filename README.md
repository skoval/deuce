deuce
=====

## Package Overview

`deuce` is an R package that provides easy access to a rich set of online data on professional tennis. By making tennis data more available to R users, `deuce` aims to be a useful tool for tennis analysts and a fun resource for teachers of statistics.


## Installation

To install in R, use the `devtools` package and the following:

`library(devtools)`

`install_github("skoval/deuce")`

There are 274 MB of data included with the package so the installation may take several minutes. 


## Learning about Contents

To find out about the datasets and functions included in `deuce`, you can use the following command to bring up the package index.

`help(package = "deuce")`

## Datasets

Any of the individual datasets can be loaded with the `data` command. For example, the following command brings the `atp_matches` data into the R environment and runs a summary on all of the columns.

`data(atp_matches)`

`summary(atp_matches)`


## Functions

There are some analytic functions and some functions for fetching additional tennis data from the Web. One example of an analytic function is the `elo_prediction` which computes the win chances for a player against a specific opponent given both player Elo ratings. Suppose, the player has an Elo rating of 2100. What is their implied win chance versus a player with a rating of 1950? We can compute that as follows:

`elo_prediction(2100, 1950)`


An example of one of the data-scraping functions is `fetch_activity`. When connected to the Internet, this can be used to retrieve the match results for an ATP player for a specific year of for their career. As an example, let's show how we would fetch the 2017 match results for Rafael Nadal.

`fetch_activity("Rafael Nadal", 2017)`




