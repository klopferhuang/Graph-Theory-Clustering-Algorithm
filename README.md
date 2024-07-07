# Spatial Clustering Algorithm for Policy Effect Estimation

## Introduction

This repository contains a clustering algorithm designed for spatial Regression Discontinuity (RD) analysis. The algorithm clusters cities that are geographically similar into the same cluster to create a control group for estimating the policy effect. The clustering is based on a threshold distance that determines the connectivity between cities, forming a graph structure where clusters are defined.
For an annotated full code of the algorithm, please check the Graph-Theory-Clustering-Algorithm.md file.

## Clustering Algorithm Definition

The clustering algorithm operates based on the following rules:

1. **Threshold Distance**: All cities within a distance smaller than the given threshold are considered connected, forming a graph.
2. **Cluster Connectivity**: Only connected cities can be in the same cluster, ensuring clusters are complete networks or complete graphs.
3. **Maximal Clustering**: Each cluster will contain as many cities as possible based on the connectivity.
4. **Nearest Cluster Assignment**: If a city is eligible to be part of two or more clusters, it will be assigned to the nearest one, measured by the average distance from this city to other cities in the potential cluster.
5. **Isolated Cities**: Any isolated city (not connected to any other city within the threshold distance) forms its own cluster.

## Output

The final output of the algorithm is a table that shows the distribution of segment sizes and the average within-cluster distance for various threshold distances. The average within-cluster distance is measured in kilometers (km) and is provided in parentheses, similar to Table A.4 in the [paper](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4455034).
