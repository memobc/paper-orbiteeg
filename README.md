# The Paper 

#### This repository includes scripts and data for the following paper (not public atm):
Ladyka-Wojcik, N., Schmidt, H., Cooper, R.A., & Ritchey, M. Neural signatures of recollection are sensitive to memory quality and specific event features. 

# Abstract
Episodic memories reflect a bound representation of multimodal features that can be recollected with varying levels of precision. Recent fMRI investigations have demonstrated that the precision and content of information retrieved from memory engage a network of posterior medial temporal and parietal regions co-activated with the hippocampus. Yet, comparatively little is known about how common neural signatures captured by electroencephalography (EEG) may be sensitive to the precise recollection of features bound in episodic memory. Here, we used a multi-feature paradigm previously reported in Cooper & Ritchey (2019) with continuous measures of memory, in conjunction with scalp EEG, to characterize the content and quality of information that drives ERP and oscillatory markers of episodic memory. A common signature of memory retrieval in left posterior regions, called the late positive component, was sensitive to overall memory quality and also to precision of recollection for spatial features. Analysis of oscillatory markers during recollection revealed that alpha/beta desynchronization was modulated by overall memory quality and also by individual features in memory. Importantly, we found evidence of a relationship between these two neural markers of memory retrieval, suggesting that they may represent complementary aspects of the recollection experience. These findings demonstrate how time-sensitive and dynamic processes identified with EEG correspond to overall episodic recollection, and also to the retrieval of precise features in memory. 

# Resources
R scripts are included in the `analysis` folder, with .csv files contains in `data` folder.
  `Orbit-EEG_Behavior.Rmd` contains analyses and visualization of behavioral data 
  `Orbit-EEG_FT-ERP.Rmd` contains analyses and visualization of feature-specific ERP data
  `Orbit-EEG_FT-TF.Rmd` contains analyses and visualization of feature-specific oscillatory data
  `Orbit-EEG_MQ-ERP.Rmd` contains analyses and visualization of memory quality ERP data
  `Orbit-EEG_MQ-TF.Rmd` contains analyses and visualization of memory quality oscillatory data
  `Orbit-EEG_MQ-Combined.Rmd` contains analyses and visualization of combined ERP and oscillatory activity for memory quality

The folder `myFunctions` contains mixture_model_functions.R which is necessary for running mixture models in R analysis scripts (https://github.com/eddjberry/precision-mixture-model)

*NB: .csv files in `data` folder may need to be unzipped before reading into R scripts*



