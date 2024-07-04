# Places_autocomplete_dictionary

This module takes CSV files as input, creates place lists from it, and
writes them to binary files meant to be used by `gwd` API.

E.g. for a given language `LANG`, provide a `dico_place_LANG.csv` file
containing data as `town,are_code,county,region,country`, and it will
produce these files:

```
dico.town.LANG.bin~
dico.area_code.LANG.bin~
dico.county.LANG.bin~
dico.region.LANG.bin~
dico.country.LANG.bin~
```
