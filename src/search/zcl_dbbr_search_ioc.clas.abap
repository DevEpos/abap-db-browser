"! <p class="shorttext synchronized" lang="en">IoC for Db Browser specific search types</p>
CLASS zcl_dbbr_search_ioc DEFINITION
  PUBLIC
  INHERITING FROM zcl_sat_base_ioc
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_dbbr_c_object_browser.
    ALIASES:
      c_search_type FOR zif_dbbr_c_object_browser~c_search_type.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_contracts,
        query_validator TYPE classname VALUE 'ZIF_SAT_QUERY_VALIDATOR',
        query_converter TYPE classname VALUE 'ZIF_SAT_QUERY_CONVERTER',
        query_parser    TYPE classname VALUE 'ZIF_SAT_OBJECT_QUERY_PARSER',
        query_config    TYPE classname VALUE 'ZIF_SAT_OBJECT_SEARCH_CONFIG',
        search_provider TYPE classname VALUE 'ZIF_SAT_OBJECT_SEARCH_PROVIDER',
        search_engine   TYPE classname VALUE 'ZIF_SAT_SEARCH_ENGINE',
      END OF c_contracts,
      BEGIN OF c_implementer,
        query_parser TYPE classname VALUE 'ZCL_SAT_OBJECT_QUERY_PARSER',
      END OF c_implementer.
ENDCLASS.



CLASS zcl_dbbr_search_ioc IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    add_contract( iv_contract = c_contracts-query_config
    )->add_implementer( iv_filter = |{ c_search_type-query }|   iv_implementer = 'ZCL_DBBR_QUERY_CONFIG'
    )->add_implementer( iv_filter = |{ c_search_type-package }| iv_implementer = 'ZCL_DBBR_PACKAGE_QUERY_CONFIG' ).

    add_contract( iv_contract = c_contracts-search_provider
    )->add_implementer( iv_filter = |{ c_search_type-query }|   iv_implementer = 'ZCL_DBBR_OS_QUERY_PROVIDER' ).

    add_contract( iv_contract = c_contracts-query_parser
    )->add_implementer( iv_filter      = |{ c_search_type-query }|
                        iv_implementer = c_implementer-query_parser
                        it_dependencies = VALUE #(
                         ( parameter = 'IO_CONFIGURATION' contract = c_contracts-query_config    filter = c_search_type-query )
                         ( parameter = 'IO_VALIDATOR'     contract = c_contracts-query_validator filter = c_search_type-query )
                         ( parameter = 'IO_CONVERTER'     contract = c_contracts-query_converter ) )
    ).
    add_contract( iv_contract = c_contracts-query_parser
    )->add_implementer( iv_filter      = |{ c_search_type-package }|
                        iv_implementer = c_implementer-query_parser
                        it_dependencies = VALUE #(
                         ( parameter = 'IO_CONFIGURATION' contract = c_contracts-query_config    filter = c_search_type-package )
                         ( parameter = 'IO_VALIDATOR'     contract = c_contracts-query_validator )
                         ( parameter = 'IO_CONVERTER'     contract = c_contracts-query_converter ) )
    ).
  ENDMETHOD.

ENDCLASS.
