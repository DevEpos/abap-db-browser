CLASS zcl_dbbr_package DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA mv_package TYPE devclass READ-ONLY.
    DATA mv_package_description TYPE as4text READ-ONLY.
    METHODS constructor
      IMPORTING
        iv_package_name TYPE devclass
        iv_package_text TYPE as4text
        ir_package      TYPE REF TO if_package OPTIONAL.
    METHODS get_sub_package_list
      RETURNING
        VALUE(result) type ref to zif_uitb_list.
    METHODS get_elements.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mr_package_int TYPE REF TO if_package.
    DATA mf_subpackages_read TYPE abap_bool.
    DATA mr_subpackage_list TYPE REF TO zif_uitb_list.
    DATA mt_elements TYPE pakdevelemtab.

    METHODS read_package.
ENDCLASS.



CLASS zcl_dbbr_package IMPLEMENTATION.
  METHOD constructor.
    mv_package = iv_package_name.
    mv_package_description = iv_package_text.
    mr_package_int = ir_package.
  ENDMETHOD.

  METHOD get_sub_package_list.
    read_package( ).

    IF mf_subpackages_read = abap_false.
      mf_subpackages_read = abap_true.
      mr_subpackage_list = NEW zcl_uitb_object_list( ).

      mr_package_int->get_sub_packages(
       IMPORTING
         e_sub_packages   = DATA(lt_packages)
       EXCEPTIONS
         object_invalid   = 1
         leaf_package     = 2
         unexpected_error = 3
         OTHERS           = 4
     ).

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      LOOP AT lt_packages ASSIGNING FIELD-SYMBOL(<lr_package>).
        mr_subpackage_list->add(
          NEW zcl_dbbr_package( iv_package_name = <lr_package>->package_name
                                iv_package_text = <lr_package>->short_text
                                ir_package      = <lr_package> )
        ).

      ENDLOOP.
    ENDIF.

    result = mr_subpackage_list.

  ENDMETHOD.

  METHOD get_elements.
    read_package( ).

    mr_package_int->get_elements(
*      EXPORTING
*        i_check_existence = TPAK_C_FALSE    " Indicator
      IMPORTING
        e_elements        = mt_elements
    ).
  ENDMETHOD.

  METHOD read_package.
    CHECK mr_package_int IS INITIAL.

    cl_package=>load_package(
      EXPORTING
        i_package_name             = mv_package
*        i_force_reload             =     " Force System to Read from Database (X=Yes)
      IMPORTING
        e_package                  = mr_package_int
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        object_locked_and_modified = 4
        OTHERS                     = 5
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
