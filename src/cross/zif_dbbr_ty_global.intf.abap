"! <p class="shorttext synchronized" lang="en">Global Type definitions for DB Browser</p>
INTERFACE zif_dbbr_ty_global
  PUBLIC .
  TYPES:
    "! <p class="shorttext synchronized" lang="en">Text Field status</p>
    BEGIN OF ty_s_text_field,
      alv_fieldname TYPE fieldname,
      visible       TYPE abap_bool,
      new_field     TYPE abap_bool,
    END OF ty_s_text_field,
    ty_t_text_field TYPE STANDARD TABLE OF ty_s_text_field WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_s_sel_option,
      sign   TYPE ddsign,
      option TYPE ddoption,
      icon   TYPE c LENGTH 40,
      text   TYPE iconquick,
    END OF ty_s_sel_option,

    ty_t_sel_option TYPE STANDARD TABLE OF ty_s_sel_option WITH EMPTY KEY.

  TYPES:
    BEGIN OF ty_sel_ctrl_serialized,
      entity_id                TYPE zsat_entity_id,
      entity_id_raw            TYPE zsat_entity_id_raw,
      entity_type              TYPE zsat_entity_type,
      navigation_breadcrumbs   TYPE string_table,
      navigation_count         TYPE int1,
      selection_fields         TYPE zdbbr_selfield_itab,
      selection_fields_multi   TYPE zdbbr_selfield_itab,
      selection_fields_or      TYPE zdbbr_or_seltab_itab,
      technical_info           TYPE zdbbr_tech_info,
      tabfields_data           TYPE zdbbr_tabfield_list_data,
      tabfields_all_data       TYPE zdbbr_tabfield_list_data,
      table_to_alias_map       TYPE zsat_table_to_alias_map_itab,
      join_definition          TYPE zdbbr_join_data,
      join_def                 TYPE zdbbr_join_def,
      navigation_info          TYPE zsat_cds_association,
      source_entity_id         TYPE zsat_entity_id,
      source_entity_where_cond TYPE string_table,
      source_entity_params     TYPE string,
      entity_params            TYPE string,
    END OF ty_sel_ctrl_serialized.
ENDINTERFACE.
