This folder contains the files needed to identify rhythmic time courses.

We used (1) WGCNA (Langfelder & Horvath, 2008, BMC bioinformatics) to generate co-expression modules, (2) a set of filters to separate co-expresseed time courses with positive correlation fromthe time courses with negative correlation and trim time courses that are included in the modules but show high levels of noise, (3) JTK_CYCLE (Hughes et al., 2010, J Biol Rhythms)to identify rhythmic modules from the median time course of a module.

There is a folder for each organ. The input file is a <organ>_expressed_timecourses.txt, which contains the Probe Name and the normalized expression level for each time point.

The WGCNA code was based on a tutorial from Langfelder & Horvath (https://horvath.genetics.ucla.edu/html/CoexpressionNetwork/Rpackages/WGCNA/Tutorials/).

