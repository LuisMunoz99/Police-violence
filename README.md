# Police Violence Project

## Project Structure

### Centralized
- **import**
  - Contains common utilities for importing data.
- **input**
  - Placeholder for input datasets.
- **src**
  - Shared source code for the entire project.
- **output**
  - Placeholder for final outputs, including the original "victimas fatales" data.

### Lpm
- **import**
  - Symlink to "victimas fatales" data.
- **geocode**
  - Symlink to "victimas fatales" data for geocoding.
- **Mortality-rates**
  - Source code for computing mortality rates.
- **concentration-ind**
  - **import**
    - Source code for importing census data.
  - **merge**
    - Source code and output for merging datasets.
  - **epa**
    - Source code for processing EPA data.
- **methods**
  - Source code and output for various project methods.
- **plots**
  - Source code and output for generating plots.

### economic_seg
- **import**
  - Symlink to relevant data.
- **geocode**
  - Symlink to geocoded data.
- **quintiles**
  - **import**
    - Source code and output for importing PUMS data.
  - **measure-quintiles**
    - Source code for measuring quintiles based on imported data.
- **ice**
  - **import**
    - Source code for importing census data.
  - **merge from sf**
    - Source code and output for merging datasets.
  - **epa**
    - Source code for processing EPA data.
- **plots**
  - Source code and output for generating plots.

### geocoding
- **import**
  - Symlink to "victimas fatales" data.
- **manual-geo**
  - Source code for manual geocoding.
- **sf**
  - Source code for using the `sf` package.

## How to Use
- **Centralized Data:**
  - Store shared datasets in the `input` directory.
- **Lpm:**
  - Execute scripts in the order of `import`, `geocode`, `Mortality-rates`, `concentration-ind`, `methods`, and `plots`.
- **economic_seg:**
  - Execute scripts in the order of `import`, `geocode`, `quintiles`, `measure-quintiles`, `ice`, and `plots`.
- **geocoding:**
  - Execute scripts in the order of `import`, `manual-geo`, and `sf`.
