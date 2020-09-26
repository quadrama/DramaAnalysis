# DramaAnalysis 3.0.1

Bug fix release

- presence() function now also produces results if the play has only a single scene (#158)
- presence() function calculates correct active and passive presence (#161)
- fixes testing issue with respect floating point numbers ("noLD issue") (#162)
- loadDramaTEI() now only accepts a filename
- Documentation now uses proper -ize spelling

# DramaAnalysis 3.0.0

This is a redesign and complete overhaul of the R package. Existing code that uses functions of the package cannot be expected to continue to run without modifications. The major changes (compared to 2.x) are:

- Classes: Most functions now produce a classed object. 
  This makes it easier to prevent an erroneous use of functions.
- Ids and names: All functions now rely only on character ids. If one needs character 
  names for plotting and visualization, they can be inserted later using `characterNames()`, 
  with a controlled formatting.
- Data files now include stage directions and character mentions, and functions can process them
- Extensive documentation in the form of a tutorial: https://quadrama.github.io/DramaAnalysis/tutorial/3/
- The wording in documentation and functions has been updated, so that figures are now called characters.
- Some functions have been renamed:
  - `setup()` is now called `setDirectories()` to avoid name conflicts
  - `figureStatistics()` is now called `characterStatistics()`
- Some functions have been removed:
  - `rankFiguresByAppearance()`
  - `rankFiguresByDramatisPersonae()`
  - `regroup()`
  - `dictionaryStatisticsL()`
  - `limitFigures()`
  - `filterMentioned()`
  - `dramaHead()`
  - `dramaTail()`
  - `enrichDictionary()`