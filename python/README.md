

# Python sample — Historical church directory → geocoded dataset

Author: Ziqian (Leah) Liu

`parse_geocode_church_directory.ipynb`

In this Jupyter Notebook, I turned the OCR'd *1942 Directory of Negro
Baptist Churches* (HathiTrust) into a clean, geocoded, SMSA-matched
dataset of \~7,300 churches.

Clean output dataset `black_churches_vol2_smsa.csv` is stored under `Ziqian_Liu_DDL_Application/code_samples/output` attached in the email. 

**What it shows:** text parsing from messy OCR, defensive cleaning
against OCR errors, dataset construction, geocoding with Census →
Nominatim fallbacks and a point-in-polygon state guardrail, and a
spatial overlay onto 1980 SMSA polygons. Final geocoding rate \~94%.

The notebook runs top to bottom with saved outputs, so it renders fully
on GitHub. Raw source text and large output CSVs are not included — see
the notebook's markdown cells for HathiTrust IDs and data structure.
Paths are relative; please point them at your own data directory to run.

Dependencies: `pandas`, `geopy`, `pyshp`, `pyproj`.
