# An algorithm to build synthetic temporal contact networks based on close-proximity interactions data
Audrey Duval $^{1}$ *, Quentin J Leclerc $^{1,2,3}$ *+, Didier Guillemot $^{1,2}$ , Laura Temime $^{2}$ **, Lulla Opatowski $^{1,3}$ **

$^{1}$ Epidemiology and Modelling of Antimicrobial Resistance, Institut Pasteur, France  
$^{2}$ Conservatoire National des Arts et Métiers, France  
$^{3}$ Université de Versailles Saint-Quentin-en-Yvelines/INSERM, France  

\* these authors contributed equally  
** these authors contributed equally  
\+ quentin.leclerc@pasteur.fr  

This repository contains the relevant contact networks and analysis code for the paper "An algorithm to build synthetic temporal contact networks based on close-proximity interactions data".

<br/>

## Analysis
Each script in this folder is named according to the figure it generates. The `helper_functions.R` script contains two utility functions used to analyse the contact networks.

## Figures
This folder contains all the figures and supplementary figures of the paper.

## Data
This folder contains the relevant observed and synthetic contact networks generated using our algorithm (note: although we generated 100 synthetic networks for each example we describe in the paper, here we only include 10 of each for data storage purposes).

### Observed
This folder contains the observed contact network, the patient admission and discharge data, and the staff schedule data. It also contains an Excel file used to aggregate staff categories into groups.

### Synthetic
This folder contains contact networks generated using our algorithm. This includes full reconstructed networks (SimulatedCtc), reconstructed networks with observation bias (testRecord), full random networks (RandomGraph), random networks with observation bias (RandomGraph2), and re-simulated full networks (ReSimulatedCtc).

### meetProb
This folder contains full networks generated for sensitivity with the recurring contact probability set to either 0.1, 0.5 or 0.9.

### Truncated
This folder contains full networks generated for sensitivity using either one or two weeks of data only.

