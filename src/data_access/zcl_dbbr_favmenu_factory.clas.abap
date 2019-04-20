"! <p class="shorttext synchronized" lang="en">Factory for favorite menu</p>
CLASS zcl_dbbr_favmenu_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Clear most used list</p>
    METHODS clear_most_used_list .
    "! <p class="shorttext synchronized" lang="en">Delete favorite</p>
    METHODS delete_favorite
      IMPORTING
        !is_favmenu TYPE zdbbr_favmenu .
    "! <p class="shorttext synchronized" lang="en">Delete favorite entries for query name range</p>
    METHODS delete_favs_for_query_names
      IMPORTING
        !it_query_name_range TYPE zdbbr_selopt_itab .
    "! <p class="shorttext synchronized" lang="en">Delete global favorites</p>
    METHODS delete_global_favorites .
    "! <p class="shorttext synchronized" lang="en">Delete most used entry</p>
    METHODS delete_most_used
      IMPORTING
        !iv_entry TYPE zdbbr_favmenu-fav_entry
        !iv_type  TYPE zdbbr_favmenu-favtype .
    "! <p class="shorttext synchronized" lang="en">Delete multiple most used entries</p>
    "!
    "! @parameter it_most_used_k | <p class="shorttext synchronized" lang="en"></p>
    METHODS delete_most_used_multiple
      IMPORTING
        !it_most_used_k TYPE zif_dbbr_global_types=>tt_mostused_k .
    "! <p class="shorttext synchronized" lang="en">Delete user favorite</p>
    METHODS delete_user_favorites .
    "! <p class="shorttext synchronized" lang="en">Check if given favorite exists</p>
    METHODS favorite_exists
      IMPORTING
        !is_favmenu      TYPE zdbbr_favmenu
      RETURNING
        VALUE(rf_exists) TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Retrieve Favorites</p>
    METHODS get_favorites
      IMPORTING
        !if_global_favorites TYPE boolean
      EXPORTING
        !et_favmenu_entries  TYPE zdbbr_favorite_menu_itab .
    "! <p class="shorttext synchronized" lang="en">Get most used favorites</p>
    METHODS get_most_used_favorites
      IMPORTING
        !if_all             TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rt_most_used) TYPE zdbbr_mostused_data_itab .
    "! <p class="shorttext synchronized" lang="en">Get next object id</p>
    METHODS get_next_object_id
      IMPORTING
        !if_global               TYPE boolean
      RETURNING
        VALUE(rv_next_object_id) TYPE menu_num_5 .
    "! <p class="shorttext synchronized" lang="en">Checks if global favorite exists</p>
    METHODS global_favorites_exist
      RETURNING
        VALUE(rf_global_favs_exists) TYPE boolean .
    "! <p class="shorttext synchronized" lang="en">Refresh most used entries</p>
    METHODS refresh_most_used
      IMPORTING
        !iv_entry     TYPE tabname
        !iv_entry_raw TYPE zdbbr_entity_id_raw
        !iv_text      TYPE ddtext OPTIONAL
        !iv_type      TYPE zdbbr_favmenu_type .
    "! <p class="shorttext synchronized" lang="en">Repair favorites tree</p>
    METHODS repair_favorite_tree .
    "! <p class="shorttext synchronized" lang="en">Update description of favorite</p>
    METHODS update_favorite
      IMPORTING
        !is_favmenu TYPE zdbbr_favmenu .
    "! <p class="shorttext synchronized" lang="en">Update multiple favorites</p>
    METHODS update_favorites
      IMPORTING
        !it_favorites TYPE zdbbr_favmenu_itab .
    "! <p class="shorttext synchronized" lang="en">Checks if user favorites exist</p>
    METHODS user_favorites_exist
      RETURNING
        VALUE(rf_user_favs_exist) TYPE boolean .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_object_id_duplicate,
        uname                   TYPE sy-uname,
        object_id               TYPE menu_num_5,
        count                   TYPE sy-tabix,
        updated_object_id_count TYPE sy-tabix,
      END OF ty_object_id_duplicate .
    TYPES:
      tt_object_id_duplicate TYPE STANDARD TABLE OF ty_object_id_duplicate WITH DEFAULT KEY .

    "! <p class="shorttext synchronized" lang="en">Check if tree repair is needed</p>
    METHODS is_repair_neeeded
      EXPORTING
        !et_object_id           TYPE tt_object_id_duplicate
      RETURNING
        VALUE(rf_repair_needed) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Update parent id</p>
    METHODS update_parent_id_local
      IMPORTING
        !iv_new_parent_id   TYPE menu_num_5
        !iv_old_parent_id   TYPE menu_num_5
      CHANGING
        !ct_favmenu         TYPE zdbbr_favmenu_itab
        !ct_new_entries     TYPE zdbbr_favmenu_itab
        !ct_deleted_entries TYPE zdbbr_favmenu_itab .
ENDCLASS.



CLASS zcl_dbbr_favmenu_factory IMPLEMENTATION.


  METHOD clear_most_used_list.
    DELETE FROM zdbbr_mostused WHERE username = sy-uname.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD delete_favorite.
    DELETE FROM zdbbr_favmenu WHERE uname     = is_favmenu-uname
                                 AND favtype   = is_favmenu-favtype
                                 AND parent_id = is_favmenu-parent_id
                                 AND object_id = is_favmenu-object_id.

    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD delete_favs_for_query_names.
    CHECK it_query_name_range IS NOT INITIAL.

    DELETE FROM zdbbr_favmenu WHERE favtype   = zif_dbbr_c_favmenu_type=>query
                                AND fav_entry IN it_query_name_range.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.

    DELETE FROM zdbbr_mostused WHERE type = zif_dbbr_c_favmenu_type=>query
                                 AND most_used_entry IN it_query_name_range.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD delete_global_favorites.
    DELETE FROM zdbbr_favmenu WHERE uname = space.

    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD delete_most_used.
    DELETE FROM zdbbr_mostused WHERE most_used_entry = iv_entry
                                  AND type            = iv_type
                                  AND username        = sy-uname.

    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD delete_most_used_multiple.
    DELETE zdbbr_mostused FROM TABLE it_most_used_k.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD delete_user_favorites.
    DELETE FROM zdbbr_favmenu WHERE uname = sy-uname.

    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD favorite_exists.
    SELECT SINGLE object_id FROM zdbbr_favmenu INTO @DATA(lv_existing_id)
      WHERE uname     = @is_favmenu-uname
        AND favtype   = @is_favmenu-favtype
        AND fav_entry = @is_favmenu-fav_entry
        AND parent_id = @is_favmenu-parent_id
        AND object_id = @is_favmenu-object_id.

    rf_exists = xsdbool( lv_existing_id <> 0 ).

  ENDMETHOD.


  METHOD get_favorites.

    DATA(lv_username) = COND sysuname( WHEN if_global_favorites = abap_true THEN
                                         ''
                                       ELSE
                                         sy-uname ).

    SELECT * FROM zdbbr_favmenu INTO CORRESPONDING FIELDS OF TABLE et_favmenu_entries
      WHERE uname = lv_username.

    DATA(lr_variant_f) = NEW zcl_dbbr_variant_factory( ).

    LOOP AT et_favmenu_entries ASSIGNING FIELD-SYMBOL(<ls_fav_entry>) WHERE favtype <> zif_dbbr_c_favmenu_type=>folder.
      <ls_fav_entry>-has_variants = lr_variant_f->variant_exists_for_entity(
          iv_entity_id = <ls_fav_entry>-fav_entry
          iv_entity_type = <ls_fav_entry>-favtype
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_most_used_favorites.
    DATA(lv_last_used_count) = COND #(
      WHEN if_all = abap_true THEN 0
      ELSE
         zcl_dbbr_usersettings_factory=>get_last_used_count_setting( )
    ).

    SELECT *
      FROM zdbbr_mostused
      WHERE username = @sy-uname
      ORDER BY changed_date DESCENDING, changed_time DESCENDING
    INTO CORRESPONDING FIELDS OF TABLE @rt_most_used
      UP TO @lv_last_used_count ROWS.

    DATA(lr_variant_f) = NEW zcl_dbbr_variant_factory( ).

    LOOP AT rt_most_used ASSIGNING FIELD-SYMBOL(<ls_fav_entry>).
      <ls_fav_entry>-has_variants = lr_variant_f->variant_exists_for_entity(
          iv_entity_id   = <ls_fav_entry>-most_used_entry
          iv_entity_type = <ls_fav_entry>-type
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_next_object_id.

    IF if_global = abap_true.
      SELECT MAX( object_id ) FROM zdbbr_favmenu INTO @DATA(lv_max_object_id)
        WHERE uname = @space.
    ELSE.
      SELECT MAX( object_id ) FROM zdbbr_favmenu INTO @lv_max_object_id
        WHERE uname = @sy-uname.
    ENDIF.

    IF lv_max_object_id = 0.
      lv_max_object_id = 1.
    ENDIF.

    rv_next_object_id = lv_max_object_id + 1.
  ENDMETHOD.


  METHOD global_favorites_exist.
    SELECT COUNT( * ) FROM zdbbr_favmenu INTO @DATA(lv_count)
      WHERE uname = @space.

    rf_global_favs_exists = xsdbool( lv_count > 0 ).
  ENDMETHOD.


  METHOD is_repair_neeeded.
    " check if repair is needed
    SELECT uname, object_id, COUNT(*) AS count, 1 AS updated_object_id_count
    INTO CORRESPONDING FIELDS OF TABLE @et_object_id
    FROM zdbbr_favmenu
    GROUP BY uname, object_id
    HAVING COUNT(*) > 1.

    rf_repair_needed = xsdbool( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD refresh_most_used.
    zcl_dbbr_appl_util=>get_current_datetime(
      IMPORTING ev_time = DATA(lv_time)
                ev_date = DATA(lv_date)
    ).

    SELECT SINGLE *
    INTO @DATA(ls_most_used_entry)
    FROM zdbbr_mostused
    WHERE username = @sy-uname
      AND most_used_entry = @iv_entry
      AND type            = @iv_type.

    IF sy-subrc <> 0.
      ls_most_used_entry = VALUE #(
        most_used_entry     = iv_entry
        most_used_entry_raw = iv_entry_raw
        username            = sy-uname
        counter             = 1
        type                = iv_type
        text                = iv_text
      ).
    ELSE.
      ADD 1 TO ls_most_used_entry-counter.
    ENDIF.

    ls_most_used_entry-changed_date = lv_date.
    ls_most_used_entry-changed_time = lv_time.

    MODIFY zdbbr_mostused FROM ls_most_used_entry.
    COMMIT WORK.
  ENDMETHOD.


  METHOD repair_favorite_tree.
    TYPES: BEGIN OF lty_object_id_change,
             old_object_id TYPE menu_num_5,
             new_object_id TYPE menu_num_5,
           END OF lty_object_id_change.


    DATA: lt_folder_object_ids TYPE HASHED TABLE OF lty_object_id_change WITH UNIQUE KEY old_object_id,
          lt_object_id_counts  TYPE tt_object_id_duplicate,
          lt_favmenu_delete    TYPE STANDARD TABLE OF zdbbr_favmenu,
          lt_favmenu_new       TYPE STANDARD TABLE OF zdbbr_favmenu,
          lt_favmenu_all       TYPE STANDARD TABLE OF zdbbr_favmenu.

    DATA: lv_object_id TYPE menu_num_5 VALUE 00000.

    IF is_repair_neeeded( IMPORTING et_object_id = lt_object_id_counts ).
      " no repair is needed
      RETURN.
    ENDIF.

    " calculate current max object ids
    SELECT uname, MAX( object_id ) AS max_object_id
    INTO TABLE @DATA(lt_max_object_id)
    FROM zdbbr_favmenu
    GROUP BY uname.

    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE @lt_favmenu_all
    FROM zdbbr_favmenu
    ORDER BY uname, object_id.

    " create new object ids for every entry
    LOOP AT lt_favmenu_all ASSIGNING FIELD-SYMBOL(<ls_favmenu>)
    GROUP BY ( uname = <ls_favmenu>-uname )
    ASSIGNING FIELD-SYMBOL(<ls_favmenu_group>).

      DATA(lr_max_object_id) = REF #( lt_max_object_id[ uname = <ls_favmenu_group>-uname ] ).

      LOOP AT GROUP <ls_favmenu_group> ASSIGNING FIELD-SYMBOL(<ls_favmenu_group_entry>).

        TRY.
            DATA(ls_object_id_duplicate) = lt_object_id_counts[
                uname     = <ls_favmenu_group_entry>-uname
                object_id = <ls_favmenu_group_entry>-object_id
            ].

            DATA(ls_favmenu_new) = <ls_favmenu_group_entry>.
            ADD 1 TO lr_max_object_id->max_object_id.
            ls_favmenu_new-object_id = lr_max_object_id->max_object_id.
            lt_favmenu_new = VALUE #( BASE lt_favmenu_new ( ls_favmenu_new ) ).

            " delete the processed entry
            lt_favmenu_delete = VALUE #( BASE lt_favmenu_delete ( <ls_favmenu_group_entry> ) ).

            ADD 1 TO ls_object_id_duplicate-updated_object_id_count.

            " if the entry was a folder all the children have to be updated too
            IF <ls_favmenu_group_entry>-favtype = zif_dbbr_c_favmenu_type=>folder.
              INSERT VALUE #( old_object_id = <ls_favmenu_group_entry>-object_id
                              new_object_id = lr_max_object_id->max_object_id ) INTO TABLE lt_folder_object_ids.
            ENDIF.


            " duplicate entry was found
            IF ls_object_id_duplicate-updated_object_id_count = ls_object_id_duplicate-count.
              DELETE lt_object_id_counts WHERE uname = ls_object_id_duplicate-uname
                                           AND object_id = ls_object_id_duplicate-object_id.
            ENDIF.

          CATCH cx_sy_itab_line_not_found.
            CONTINUE.
        ENDTRY.

      ENDLOOP.

    ENDLOOP.

    " update childen
    LOOP AT lt_folder_object_ids ASSIGNING FIELD-SYMBOL(<ls_folder_id>).
      update_parent_id_local(
        EXPORTING
          iv_new_parent_id   = <ls_folder_id>-new_object_id
          iv_old_parent_id   = <ls_folder_id>-old_object_id
        CHANGING
          ct_favmenu         = lt_favmenu_all
          ct_new_entries     = lt_favmenu_new
          ct_deleted_entries = lt_favmenu_delete
      ).
    ENDLOOP.


    DATA(lf_delete_ok) = abap_true.

    " update the database
    IF lt_favmenu_delete IS NOT INITIAL.
      DELETE zdbbr_favmenu FROM TABLE lt_favmenu_delete.
      lf_delete_ok = xsdbool( sy-subrc = 0 ).
    ENDIF.

    DATA(lf_insert_ok) = abap_true.

    IF lt_favmenu_new IS NOT INITIAL.
      INSERT zdbbr_favmenu FROM TABLE lt_favmenu_new.
      lf_insert_ok = xsdbool( sy-subrc = 0 ).
    ENDIF.

    IF lf_insert_ok = abap_true AND lf_delete_ok = abap_true.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.


  METHOD update_favorite.
    MODIFY zdbbr_favmenu FROM is_favmenu.

    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD update_favorites.
    MODIFY zdbbr_favmenu FROM TABLE it_favorites.

    COMMIT WORK.

    repair_favorite_tree( ).
  ENDMETHOD.


  METHOD update_parent_id_local.
    LOOP AT ct_favmenu ASSIGNING FIELD-SYMBOL(<ls_favmenu>)
      WHERE parent_id = iv_old_parent_id.

      IF NOT line_exists( ct_deleted_entries[ object_id = <ls_favmenu>-object_id ] ).
        ct_deleted_entries = VALUE #( BASE ct_deleted_entries ( <ls_favmenu> ) ).
      ENDIF.

      DATA(lr_new_faventry) = REF #( ct_new_entries[ object_id = <ls_favmenu>-object_id ] OPTIONAL ).
      IF lr_new_faventry IS NOT INITIAL.
        lr_new_faventry->parent_id = iv_new_parent_id.
      ELSE.
        DATA(ls_new_favmenu) = <ls_favmenu>.

        ls_new_favmenu-parent_id = iv_new_parent_id.

        ct_new_entries = VALUE #( BASE ct_new_entries ( ls_new_favmenu ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD user_favorites_exist.
    SELECT COUNT( * ) FROM zdbbr_favmenu INTO @DATA(lv_count)
      WHERE uname = @sy-uname.

    rf_user_favs_exist = xsdbool( lv_count > 0 ).
  ENDMETHOD.
ENDCLASS.
