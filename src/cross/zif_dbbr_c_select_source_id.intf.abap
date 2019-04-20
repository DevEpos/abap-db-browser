INTERFACE zif_dbbr_c_select_source_id
  PUBLIC .

  CONSTANTS:
    ddddlsrc                     TYPE string VALUE 'DDDDLSRC',
    ddldependency                TYPE string VALUE 'DDLDEPENDENCY',
    zdbbr_i_ddldependency        TYPE string VALUE 'ZDBBR_I_DDLDEPENDENCY',
    zdbbr_queryh                 TYPE string VALUE 'ZDBBR_QUERYH',
    zdbbr_queryt                 TYPE string VALUE 'ZDBBR_QUERYT',
    zdbbr_i_associatedincds      TYPE string VALUE 'ZDBBR_I_ASSOCIATEDINCDS',
    zdbbr_i_cdsfrompartentity    TYPE string VALUE 'ZDBBR_I_CDSFROMPARTENTITY',
    zdbbr_i_databasetable        TYPE string VALUE 'ZDBBR_I_DATABASETABLE',
    zdbbr_i_databaseentity       TYPE string VALUE 'ZDBBR_I_DATABASEENTITY',
    zdbbr_i_databaseentityaggr   TYPE string VALUE 'ZDBBR_I_DATABASEENTITYAGGR',
    zdbbr_p_cdsviewbase          TYPE string VALUE 'ZDBBR_P_CDSVIEWBASE',
    zdbbr_i_cdsextensionviews    TYPE string VALUE 'ZDBBR_I_CDSEXTENSIONVIEWS',
    zdbbr_i_cdsviewt             TYPE string VALUE 'ZDBBR_I_CDSVIEWT',
    zdbbr_i_databaseview         TYPE string VALUE 'ZDBBR_I_DATABASEVIEW',
    zdbbr_i_apistates            TYPE string VALUE 'ZDBBR_I_APISTATES',
    zdbbr_i_ddlusageinddl        TYPE string VALUE 'ZDBBR_I_DDLUSAGEINDDL',
    zdbbr_i_cdsviewfield         TYPE string VALUE 'ZDBBR_I_CDSVIEWFIELD',
    zdbbr_i_cdsviewwithparameter TYPE string VALUE 'ZDBBR_I_CDSVIEWWITHPARAMETER',
    zdbbr_i_cdsannotation        TYPE string VALUE 'ZDBBR_I_CDSANNOTATION',
    zdbbr_i_tablefield           TYPE string VALUE 'ZDBBR_I_TABLEFIELD'.

ENDINTERFACE.
