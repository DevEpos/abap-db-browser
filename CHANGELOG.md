# DBBrowser - Changelog

## v2.8.8   - 2019-04-20

### Features

- Updated Dependency Tree
  - Side by side tree and DDL Source code viewing
  - Control double click behaviour on node

### Fixes

- Fix in Variant managment
- Overall minor fixes

## v2.2.0   - 2019-02-26

### Features

- always use maxrows + 1 in query search to get better information if the 
  exact number was found or if more entries than maxrows was found
- add new method to create query from data
- add/adjust object browser search favorites

### Fixes

- minor fixes
- return cancelled flag from util method popup_get_value

## v2.0.0   - 2019-02-23

### Features

- command to execute dependency tree for CDS view
- New Custom queries
- Add navigation history to Object browser
- Overhaul of Central search
- Include query import/export into Object Browser

### Fixes

- minor fixes in formula editor

## v1.2.0   - 2019-01-18

### Features

- add show cds source to context menu
- update object browser context menu
- use new abap code viewer to display cds source
- mark target field green if domain and name matches (F4 Help)
- allow "me" value in owner-search parameter of Object Search

### Fixes

- minor bug during conversion of object browser search value
- update due to global data cache refactoring

## v1.1.0   - 2019-01-17

### Features

- add cds dependency tree feature
- read annotations for cds view
- add new icon constants for dependency analyzer
- adjust default settings for users
- add fieldname to cds view for annoations

### Fixes

- do not delete jump definitions if query is overridden
- minor spelling fixes

## v1.0.7   - 2019-01-15

### Fixes

- Conversion Fix for Settings dialog in Object Browser

### Features

- Add support to search for name-value pair for anno option in CDS View mode
- Allow case ignore search for all search options

## v1.0.6   - 2019-01-14

### Features

- Add support for distinct api options inside cds view mode of Object Browser
search

## v1.0.5   - 2019-01-13