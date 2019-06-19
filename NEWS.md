# DramaAnalysis 3.0

This is a redesign and complete overhaul of the R package. Existing code that uses functions of the package cannot be expected to continue to run without modifications. The major changes (compared to 2.x) are:

- Classes: Most functions now produce a classed object. 
  This makes it easier to prevent an erroneous use of functions.
- Ids and names: All functions now rely only on character ids. If one needs character 
  names for plotting and visualisation, they can be inserted later using `characterNames()`, 
  with a controlled formatting.
- Data files now include stage directions and character mentions, and functions can process them
- Extensive documentation in the form of a tutorial: https://quadrama.github.io/DramaAnalysis/tutorial/3/