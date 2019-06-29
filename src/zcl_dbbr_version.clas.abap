"! <p class="shorttext synchronized" lang="en">Holds version information</p>
CLASS zcl_dbbr_version DEFINITION
  PUBLIC
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Displays the current version of DB Browser</p>
    "!
    CLASS-METHODS show_version.
  PROTECTED SECTION.
  PRIVATE SECTION.
    "! <p class="shorttext synchronized" lang="en">Current version of DB Browser</p>
    "! <strong>Changelog</strong> <br/><br/>
    "!
    "! ## v2.14.0    - 2019-06-27
    "!
    "! ### Features
    "!
    "! - add views for api state "Custom Fields" from table cfd_w_cds
    "! - enhance object search with new parameters
    "!   1) read all results
    "!   2) read API states (C0 contract does not count for this)
    "! - show endusertext-label for CDS view columns in CDS mode
    "!
    "! ### Fixes
    "!
    "! - add correct column optimization to Variant Pop-Up
    "! - fix some issues in the CDS factory
    "! - use global description language for domain fix values of text fields
    "! - cache original system language and use it for descriptions (see zcl_dbbr_system_helper)
    "!   - remove description language setting from UI
    "!
    "! ## v2.13.0    - 2019-06-03
    "!
    "! ### Features
    "!
    "! - new function in ALV output to hide all columns where no cell has any values
    "! - use UI-Toolbox ALV for detail output
    "! - Add some utility methods to where clause builder
    "!
    "! ### Fixes
    "!
    "! - Selection Screen history was not filled with initial entity at transaction start
    "! - Fix error in CDS Navigation
    "! - Read Automatic/Default variant with the current user name
    "! - Properly update the filtered entries count in selection output
    "! - Fix drag-n-drop error in Favorites tree
    "! - Remove unnecessary ALV refresh after some global ALV actions
    "! - Fix bug in CDS Selection Screen Util
    "! - Fix bug in recognizing Value Helps on CDS Parameters
    "! - Fix replace sy-tabix type with zdbbr_no_of_lines type in selection controller
    "!
    "! ## v2.12.0    - 2019-05-20
    "!
    "! ### Features
    "!
    "! - Create history entry if entity is started from AiE
    "! - Use new search dialog in selection screen Strg+F/Strg+G event
    "! - New mode in dependency analyzer of CDS View to display unique usages
    "!
    "! ### Fixes
    "!
    "! - Correct reading/display of custom F4 helps in Parameter popup
    "! - Fix some minor issues during the determination of the value helps for
    "!   a data type
    "!
    "! ## v2.11.0    - 2019-05-10
    "!
    "! ### Features
    "!
    "! - New User setting to control the location of the object navigator
    "! - New User setting for compact selection screen columns
    "! - Enhancements to Value help management
    "!   - New flag to control Alpha conversion during value transfer
    "!   - Assign existing custom value help to table/view field
    "!   - Refresh value helps of fields after F4 assignments were changed
    "!
    "! ## Fixes
    "!
    "! - Default Variant sort-/output fields are not loaded correctly
    "! - New selection dialog for ALV column jump
    "! - Fix for all selection during CDS View association navigation
    "! - Hide "Change parameter" UI function if no CDS view with parameter exists
    "!   in the selection
    "!
    "! ## v2.10.0   - 2019-04-29
    "!
    "! ### Features
    "!
    "! - Automatic selection criteria saving/loading setting
    "! - Allow clearing of navigation history in selection screen
    "! - New Value help dialog for choosing variants
    "!
    "! - Introduce AND logical operator for 1 to many DB object search parameters
    "!
    "! ### Fixes
    "!
    "! - Minor fix in Sort Field control
    "! - Minor fix in where clause builder -> use selopt structure with more characters in
    "!   low/high values
    "! - Proper support of Option negation for 1 to many parameters in object search
    "!
    "! ## v2.9.0   - 2019-04-25
    "!
    "! ### Features
    "!
    "! - CDS View Search now also searches DDL names with the given search string
    "! - ALV Output will be shown even if no Rows were found if the following criteria are met
    "!   - DB Browser was opened from ADT
    "!   - Entity is CDS View and has Parameters that are not System parameters
    "! - Reorganization of User settings dialog
    "!   - Renamed ALV list output to Data Ouput
    "!   - New settings tab called Data Selection
    "! - New options for CDS Views type Analytics.query = true
    "!   - Open in Analysis for Office - via custom options menu
    "!   - Open in Query Monitor - via custom options menu
    "!
    "! ### Fixes
    "!
    "! - Replace tabname with tabname_alias in field sorting control
    "! - Show Client field in custom SQL query
    "!
    "! ### Refactoring
    "!
    "! - Delete class ZIF_DBBR_C_SEARCH_FUNCTION
    "! - Make all methods in ZCL_DBBR_CUSTOM_F4_FACTORY static
    "!
    "! ## v2.8.8   - 2019-04-20
    "!
    "! ### Features
    "!
    "! - Updated Dependency Tree
    "!   - Side by side tree and DDL Source code viewing
    "!   - Control double click behaviour on node
    "!
    "! ### Fixes
    "!
    "! - Fix in Variant managment
    "! - Overall minor fixes
    "!
    "! ## v2.2.0   - 2019-02-26
    "!
    "! ### Features
    "!
    "! - always use maxrows + 1 in query search to get better information if the
    "!   exact number was found or if more entries than maxrows was found
    "! - add new method to create query from data
    "! - add/adjust object browser search favorites
    "!
    "! ### Fixes
    "!
    "! - minor fixes
    "! - return cancelled flag from util method popup_get_value
    "!
    "! ## v2.0.0   - 2019-02-23
    "!
    "! ### Features
    "!
    "! - command to execute dependency tree for CDS view
    "! - New Custom queries
    "! - Add navigation history to Object browser
    "! - Overhaul of Central search
    "! - Include query import/export into Object Browser
    "!
    "! ### Fixes
    "!
    "! - minor fixes in formula editor
    "!
    "! ## v1.2.0   - 2019-01-18
    "!
    "! ### Features
    "!
    "! - add show cds source to context menu
    "! - update object browser context menu
    "! - use new abap code viewer to display cds source
    "! - mark target field green if domain and name matches (F4 Help)
    "! - allow "me" value in owner-search parameter of Object Search
    "!
    "! ### Fixes
    "!
    "! - minor bug during conversion of object browser search value
    "! - update due to global data cache refactoring
    "!
    "! ## v1.1.0   - 2019-01-17
    "!
    "! ### Features
    "!
    "! - add cds dependency tree feature
    "! - read annotations for cds view
    "! - add new icon constants for dependency analyzer
    "! - adjust default settings for users
    "! - add fieldname to cds view for annoations
    "!
    "! ### Fixes
    "!
    "! - do not delete jump definitions if query is overridden
    "! - minor spelling fixes
    "!
    "! ## v1.0.7   - 2019-01-15
    "!
    "! ### Fixes
    "!
    "! - Conversion Fix for Settings dialog in Object Browser
    "!
    "! ### Features
    "!
    "! - Add support to search for name-value pair for anno option in CDS View mode
    "! - Allow case ignore search for all search options
    "!
    "! ## v1.0.6   - 2019-01-14
    "!
    "! ### Features
    "!
    "! - Add support for distinct api options inside cds view mode of Object Browser
    "! search
    "!
    "! ## v1.0.5   - 2019-01-13
    CLASS-DATA gv_version TYPE string VALUE '2.14.0'.
ENDCLASS.



CLASS zcl_dbbr_version IMPLEMENTATION.
  METHOD show_version.
    MESSAGE |DB Browser Version: { gv_version }| TYPE 'I'.
  ENDMETHOD.

ENDCLASS.
