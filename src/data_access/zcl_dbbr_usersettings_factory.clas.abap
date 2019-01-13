class ZCL_DBBR_USERSETTINGS_FACTORY definition
  public
  final
  create public .

public section.

  class-methods GET_ENTITY_BROWSER_SETTINGS
    returning
      value(RS_SETTINGS) type ZDBBR_ENTBRWSUS .
  class-methods SET_ENTITY_BROWSER_SETTINGS
    importing
      !IS_SETTINGS type ZDBBR_ENTITY_BROWSER_STTNG_A .
  methods GET_SETTINGS
    returning
      value(RESULT) type ZDBBR_USER_SETTINGS_A .
  methods GET_LAST_USED_COUNT_SETTING
    returning
      value(RESULT) type SY-TABIX .
  methods SAVE_SETTINGS
    importing
      !VALUE type ZDBBR_USER_SETTINGS_A .
  methods UPDATE_START_SETTINGS
    importing
      !IV_ENTITY_ID type ZDBBR_ENTITY_ID
      !IV_ENTITY_TYPE type ZDBBR_ENTITY_TYPE .
  methods SHOULD_READ_DB_SIZE
    returning
      value(RESULT) type ABAP_BOOL .
  methods IS_EXPERIMENTAL_MODE_ACTIVE
    returning
      value(RESULT) type ABAP_BOOL .
  PROTECTED SECTION.
private section.

  methods GET_DEFAULT_SETTINGS
    returning
      value(RESULT) type ZDBBR_USRSETTNG .
ENDCLASS.



CLASS ZCL_DBBR_USERSETTINGS_FACTORY IMPLEMENTATION.


  METHOD get_default_settings.
    DATA: ls_settings TYPE zdbbr_usrsettng.

    SELECT * INTO TABLE @DATA(lt_user_values)
     FROM usr05
       WHERE bname = @sy-uname.

    IF sy-subrc = 0.

    ENDIF.
  ENDMETHOD.


  METHOD get_entity_browser_settings.
    SELECT SINGLE *
      FROM zdbbr_entbrwsus
      WHERE username = @sy-uname
      INTO CORRESPONDING FIELDS of @rs_settings.

      IF sy-subrc <> 0.
        rs_settings = value #(
          max_hits              = 500
          link_mode             = zif_dbbr_c_eb_link_mode=>open_in_db_browser_new_task
          entry_search_function = zif_dbbr_c_search_function=>find_tables
        ).
      ENDIF.
  ENDMETHOD.


  METHOD get_last_used_count_setting.
    DATA(ls_settings) = get_settings( ).

    result = ls_settings-last_used_count.
  ENDMETHOD.


  METHOD get_settings.

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF result
      FROM zdbbr_usrsettng
      WHERE username = sy-uname.

    IF sy-subrc <> 0.
      " try migration from memory id
      result = VALUE #(
          color_sort_columns    = abap_true
          zero_val_as_blank     = abap_true
          max_lines             = zif_dbbr_global_consts=>c_default_max_lines
          emphasize_text_fields = abap_true
          fav_user_mode         = zif_dbbr_global_consts=>gc_fav_user_modes-global
          last_used_count       = zif_dbbr_global_consts=>c_default_last_used_favs
          color_formula_fields  = abap_true
          description_language  = sy-langu
      ).
    ENDIF.
  ENDMETHOD.


  METHOD is_experimental_mode_active.
    CLEAR result.

    SELECT SINGLE experimental_mode INTO @result
      FROM zdbbr_usrsettng
      WHERE username = @sy-uname.
  ENDMETHOD.


  METHOD save_settings.
    DATA(ls_settings) = CORRESPONDING zdbbr_usrsettng( value ).
    ls_settings-username = sy-uname.

    MODIFY zdbbr_usrsettng FROM ls_settings.
    COMMIT WORK.
  ENDMETHOD.


  METHOD set_entity_browser_settings.
    DATA(ls_settings) = CORRESPONDING zdbbr_entbrwsus( is_settings ).
    ls_settings-username = sy-uname.

    MODIFY zdbbr_entbrwsus FROM ls_settings.
    commit work.
  ENDMETHOD.


  METHOD should_read_db_size.
    CLEAR result.

    SELECT SINGLE show_db_size_in_title INTO @result
      FROM zdbbr_usrsettng
      WHERE username = @sy-uname.
  ENDMETHOD.


  METHOD update_start_settings.
    DATA(ls_settings) = get_settings( ).

    ls_settings-last_entity_id = iv_entity_id.
    ls_settings-last_entity_type = iv_entity_type.

    save_settings( CORRESPONDING #( ls_settings ) ).
  ENDMETHOD.
ENDCLASS.
