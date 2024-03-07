# Police Violence Project

## Structure

### Import Centralized
- **input** - original datasets. (READONLY)
- **src**
- **output** - working datasets.

### geocoding
- **import** Symlink to working datasets.
- **export-manual-geo**
- **transform-coords**
  - **import** manual-geo
  - **export** measuring quintiles
  

### Lpm
- **import** Symlink to working datasets.
- **geocode** Symlink to geocoded cords
  
- **Mortality-rates**
  - **import** - population data
  - **analysis** - Mortality rates analysis

-  **spatial-race-inc**
  - **import** - census data
  - **merge** - merging census with transformed-coords (upstream) 
  - **analysis** - Race and income concentration analysis

- **methods** Importing methodological appendix
- **plots** Data visualizations 

### economic_seg
- **import** Symlink to working datasets.
- **geocode** Symlink to geocoded cords
  
- **measure-quintiles**
  - **import** PUMS data
  - **measure-quintiles** measuring quintiles
    
- **ice**
  - **import** census data
  - **merge** - merging census with transformed-coords (upstream) 
  - **analysis** - Index of concentration at the extremes analysus

- **plots** Data visualizations 
