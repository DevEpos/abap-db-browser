![](https://img.shields.io/badge/ABAP-v7.50+-orange)
# abap-db-browser

![DB Browser - Selection Screen](img/selection-screen.png)

## Overview
*ABAP DB Browser* is a SAP GUI Transaction which combines functionalities of well-known Transactions `SE16N` and `SE16H`. 

### Available Transactions
- `ZDBBR` - DB Browser (The main program)
- `ZDBBR_SEARCH` - DB Browser - Central Search (Transaction which is mainly focused on the search for DB Tables/Views, Queries or CDS Views)
- `ZDBBR_SQLCONSOLE` - SQL Console for testing SQL code or creating custom sql queries

### Feature Set
- Data selection from DB-Table, DB-View, CDS View, Custom Queries
  - Use Grouping and Aggregation functions
- Jump into data maintenance 
  > **Note**: This is only possible if transaction `SE16N` is installed on the system
- Create Favorites for your most used DB entities (Like SAP Easy Access Tree menu)
- History of your most recently used DB entities
- Create custom value helps for your selection fields
- Integrated advanced search capabilities for DB entities
  - see [abap-search-tools-ui](https://github.com/stockbal/abap-search-tools-ui)
- Advanced ALV Data Output
  - Live Filter => Filter is directly converted to selection criteria and triggers a new DB select 
    > Can be turned off in user settings
  - Perform Row Grouping - Like SQL Group By
  - Quick Filter via Keybinding `F9` - Currently selected Cells are converted into ALV Filter
  - Hide/Discard Rows which are not relevant 
  - Compare one or several rows - The rows should be sorted
  - Show additional text columns for columns whose data type has domain fixed values or an assigned text table
  - Show the cell content in a popup code editor
  - Easy search and navigation to columns 
  - Total calculation for arbitrary selected cells
  - Direct navigation to SQL Console defaulted with the current select statement 
- Advanced Features for CDS View
  - Support for Parameters
  - Follow Defined Associations (like ADT Data Output) up to 8 Levels deep
  - Navigate to Sub Entity (Association or Base Entity)
  - Show the dependency tree of the CDS View (like ADT feature)
  - Feature if CDS View has Annotation `@Analytics.query=true`:
    - Open in Analysis for Office (Excel)
    - Open in Transaction `RSRT` (i.e. Query Monitor)
    - Open the Query in the **SAP UI5 App** *Design Studio*
  - Calculation of virtual elements
- Create Custom Queries (Non-SQL)
  - Create Joins (Inner, Left/Right Outer) to DB-Table, DB-View or CDS View
  - Create custom selection mask for your Query
  - Customize output field order
  - Customize sorting
- Create Custom Queries (SQL)
  - Create custom query with SQL
  - Declare parameters to parameterize your SQL query
 

## Important Information
### Dependencies

- [abap-search-tools](https://github.com/stockbal/abap-search-tools)
- [abap-ui-toolbox](https://github.com/stockbal/abap-ui-toolbox)

### Installation Guide
Install this repository using [abapGit](https://github.com/abapGit/abapGit#).  
See the [Installation Guide](https://github.com/stockbal/abap-db-browser/wiki/Installation) on
the Wiki for further information.

#### SAP NetWeaver Compatibility
Due to some used classes which are only available starting from **v7.50** a downwards compatibility to **v7.40** in a single 
branch is currently not possible, without resorting to some dynamic programming.
