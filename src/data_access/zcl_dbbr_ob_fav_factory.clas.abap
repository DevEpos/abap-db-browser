"! <p class="shorttext synchronized" lang="en">Factory for managin object browser favorites</p>
CLASS zcl_dbbr_ob_fav_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Get all favorites of user</p>
    CLASS-METHODS get_favorites
      RETURNING
        VALUE(rt_favorites) TYPE zdbbr_obj_browser_fav_t.
    "! <p class="shorttext synchronized" lang="en">Create new favorite</p>
    CLASS-METHODS create_favorite
      IMPORTING
        iv_query          TYPE string
        iv_type           TYPE zdbbr_obj_browser_mode
        iv_name           TYPE zdbbr_objbrs_favorite_name
      RETURNING
        VALUE(rf_success) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Changes the name of a single favorite</p>
    CLASS-METHODS change_favorite_name
      IMPORTING
        iv_id             TYPE zdbbr_objbrs_favorite_id
        iv_name           TYPE zdbbr_objbrs_favorite_name
      RETURNING
        VALUE(rf_success) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Delete favorite for the given id</p>
    "!
    CLASS-METHODS delete_favorite
      IMPORTING
        iv_id             TYPE zdbbr_objbrs_favorite_id
      RETURNING
        VALUE(rf_success) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dbbr_ob_fav_factory IMPLEMENTATION.

  METHOD create_favorite.
    DATA(ls_favorite) = VALUE zdbbr_objbrsfav(
        id            = zcl_dbbr_system_helper=>create_guid_22( )
        created_by    = sy-uname
        entity_type   = iv_type
        favorite_name = iv_name
        search_string = iv_query
    ).

    INSERT zdbbr_objbrsfav FROM ls_favorite.
    IF sy-subrc = 0.
      COMMIT WORK.
      rf_success = abap_true.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDMETHOD.

  METHOD get_favorites.
    SELECT *
      FROM zdbbr_objbrsfav
      WHERE created_by = @sy-uname
      order by entity_type ASCENDING
    INTO CORRESPONDING FIELDS OF TABLE @rt_favorites.
  ENDMETHOD.

  METHOD change_favorite_name.
    UPDATE zdbbr_objbrsfav SET favorite_name = iv_name WHERE id = iv_id.
    IF sy-subrc = 0.
      COMMIT WORK.
      rf_success = abap_true.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDMETHOD.

  METHOD delete_favorite.
    DELETE FROM zdbbr_objbrsfav WHERE id = iv_id.
    IF sy-subrc = 0.
      COMMIT WORK.
      rf_success = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
