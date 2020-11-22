## Scripts for analyzing quickly retaken temperatures in eICU datasets

This repository contains scripts that can be used to check and replicate the findings of

> Harding C, Pompei M, Burmistrov D, Pompei F. Overlooked bias with thermometer evaluations 
> using quickly retaken temperatures in EHR: axillary, oral, temporal artery, and 
> tympanic thermometry [Preprint]. medRxiv. 2020. doi: in process.

The article is in submission.


## Getting started

Analyses are of the the eICU Collaborative Research Database, version 2.0.

eICU dataset files `nurseCharting.csv`, `patient.csv`, `apachePatientResult.csv`, and 
`apachePredVar.csv` need to be placed in the `raw data` folder. 

When run, `main.R` performs the analyses.


## Resources

eICU datasets are available from PhysioNet:

> Pollard T, Johnson A, Raffa J, Celi L A, Badawi O, Mark R. eICU Collaborative Research Database (version 2.0). 
> PhysioNet. 2019. Available from: https://doi.org/10.13026/C2WM1R .

The eICU and PhysioNet programs are described here:

> Pollard TJ, Johnson AEW, Raffa JD, Celi LA, Mark RG and Badawi O. The eICU Collaborative Research Database, 
> a freely available multi-center database for critical care research. 
> Scientific Data. 2018. DOI: http://dx.doi.org/10.1038/sdata.2018.178 .

> Goldberger A, Amaral L, Glass L, Hausdorff J, Ivanov PC, Mark R, Mietus JE, Moody GB, Peng CK, Stanley HE. 
> PhysioBank, PhysioToolkit, and PhysioNet: Components of a new research resource for complex physiologic signals.
> Circulation. 2000:101;23:e215–e220.


## Conflicts of interest

The study was funded by Exergen, Corp., a manufacturer of temporal artery thermometers, and the authors
are affiliated with or consult for Exergen. 