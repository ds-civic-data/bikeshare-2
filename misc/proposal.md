---
title: "Project Proposal"
author: "Nathan Feldman, Canyon Foot, Zeki Kazan"
date: "3/29/2018"
output: github_document

## General Theme and Specific Questions

We would like to find out how people are using Portland's Bike Share Program. How are people getting access to the bikes? When and where do they ride? Who is using the program? Can we figure out some way to classify how the bike is being used (for enjoyment or for commuting). 

## Relevant Work

These two are from the NYC Citibike program, which also provides open data. 

http://toddwschneider.com/posts/a-tale-of-twenty-two-million-citi-bikes-analyzing-the-nyc-bike-share-system/

https://medium.com/@Urbica.co/city-bike-rebalanced-92ac61a867c7

This is from the Bay Area's bike share (despite the url)

https://nycdatascience.com/blog/student-works/r-visualization/graphic-look-bay-area-bike-share/

## Client and Stakeholders

We could try to contact someone at Biketown. Biketown's data is handled by Motivate (https://www.motivateco.com/use-our-data/), who we could also try and contact. 

## The Data

The data comes in .JSON files, which are updated every couple of minutes. They are accessed via their website, and have a github explanation here: https://github.com/NABSA/gbfs/blob/master/gbfs.md. At this time we aren't sure what the observational unit is, or what the data looks like, because we don't know how to use .json files. 

The one confidentiality concern is that other analyses of bike share data have highlighted the capability to detect "unique" riders. This won't be part of our project. However, this is open data, and we can't stop others from doing so.

## Deliverable

For a model, we could try to classify whether the rider is a commuter or a "leisure" rider. This could be affected by time of day, how they paid, whether they are a pass holder, etc. 

One idea is for us to build a simple shiny app. It would be a map of Portland, with the hubs highlighted, and a series of toggles. These toggles could be things like how the person paid, if they are a pass holder, or whether we have classified them as a commuter or as a single rider. There could be a slider to "advance" the time of the map. Which hubs are being used the most? Which are not used enough? These could be interactive elements that change with the changing of the time slider.