*&---------------------------------------------------------------------*
*& Report  ZDBBR_rep_query_catalog
*&
*&---------------------------------------------------------------------*
*& ->Selection of querys, to edit, delete, execute them
*& ->Import querys from external files
*&---------------------------------------------------------------------*
REPORT ZDBBR_rep_query_catalog.

TABLES: dd03l.

DATA: gv_tabname TYPE tabname16,
      gv_created_by type zdbbr_created_by,
      gv_query_name type zdbbr_query_name,
      gv_description type ddtext.

SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE text-t01.
SELECT-OPTIONS so_scrna FOR gv_query_name NO INTERVALS.
SELECTION-SCREEN SKIP.

SELECT-OPTIONS so_crtby FOR gv_created_by NO INTERVALS.
SELECT-OPTIONS so_frstt FOR gv_tabname NO INTERVALS.
SELECT-OPTIONS so_shrtd FOR gv_description NO INTERVALS.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK tables WITH FRAME TITLE text-t02.

SELECT-OPTIONS so_anyt FOR gv_tabname NO INTERVALS.
SELECT-OPTIONS so_allt FOR gv_tabname NO INTERVALS.
SELECT-OPTIONS so_onlyt FOR gv_tabname NO INTERVALS.
SELECT-OPTIONS so_nonet FOR gv_tabname NO INTERVALS.

SELECTION-SCREEN END OF BLOCK tables.

SELECTION-SCREEN END OF BLOCK selection.

SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE text-t03.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETER p_rblist TYPE boolean RADIOBUTTON GROUP rb1 DEFAULT 'X' MODIF ID out.
SELECTION-SCREEN COMMENT 2(60) text-001 FOR FIELD p_rblist MODIF ID out.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETER p_rbimp TYPE boolean RADIOBUTTON GROUP rb1 MODIF ID imp.
SELECTION-SCREEN COMMENT 2(60) text-002 FOR FIELD p_rbimp MODIF ID imp.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK options.

START-OF-SELECTION.
  IF p_rblist = abap_true.
    NEW ZCL_DBBR_query_manager(
      it_query_name_so = so_scrna[]
      it_created_by_so = so_crtby[]
      it_first_table_so = so_frstt[]
      it_any_tables_so = so_anyt[]
      it_all_tables_so = so_allt[]
      it_only_tables_so = so_onlyt[]
      it_none_tables_so = so_nonet[]
      it_description_so = so_shrtd[]
    )->show( ).
  ELSE.
    " import querys from file
    NEW ZCL_DBBR_query_importer(
      it_created_by_so = so_crtby[]
      it_first_table_so = so_frstt[]
      it_any_tables_so = so_anyt[]
      it_all_tables_so = so_allt[]
      it_only_tables_so = so_onlyt[]
      it_none_tables_so = so_nonet[]
      it_description_so = so_shrtd[]
    )->import_data( ).
  ENDIF.
