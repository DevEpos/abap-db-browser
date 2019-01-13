CLASS ZCL_DBBR_fe_templates DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    CLASS-DATA sv_introduction_template TYPE string.
    CLASS-DATA sv_calc_difference_exmple TYPE string.
    CLASS-DATA sv_icon_example TYPE string.
    CLASS-DATA sv_icon_quicktip_example TYPE string.
    CLASS-DATA sv_form_field_tmplt TYPE string.
    class-data sv_form_field_rllnam_tmplt type string.
    CLASS-DATA sv_text_for_field_tmplt TYPE string.
    CLASS-DATA sv_icon_field_tmplt TYPE string.
    CLASS-DATA sv_icon_tt_field_tmplt TYPE string.
    class-data sv_set_icon_tmplt type string.
    class-data sv_set_row_color_template type string.
    class-data sv_set_cell_color_template type string.
    class-data st_valid_keywords_range type range of char20.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_DBBR_FE_TEMPLATES IMPLEMENTATION.


  METHOD class_constructor.
    sv_form_field_tmplt        = `$DEF      ffname   TYPE Table-Field`.
    sv_form_field_rllnam_tmplt = `$DEF      ffname   TYPE Data Element`.
    sv_text_for_field_tmplt    = `$TEXT     ffname   'Short Text'  'Long Text'.`.
    sv_icon_field_tmplt        = `$ICON     ffname.`.
    sv_icon_tt_field_tmplt     = `$ICON_TT  ffname.`.
    sv_set_icon_tmplt          = `$SET_ICON_TT   ffname   ICON_PLANT  'Long text (up to 30 Characters)'.`.
    sv_set_row_color_template  = `$SET_ROW_COLOR  'C500'.`.
    sv_set_cell_color_template = `$SET_CELL_COLOR fieldname 'C500'.`.

    sv_introduction_template =
        `* Eine Formel ist eine Serie von Anweisungen, die pro Zeile der` && cl_abap_char_utilities=>cr_lf &&
        `* Liste durchlaufen wird, um eines oder mehrere Formelfelder` && cl_abap_char_utilities=>cr_lf &&
        `* zu berechnen. Anschliessend werden die definierten Formelfelder` && cl_abap_char_utilities=>cr_lf &&
        `* ganz rechts in der Liste als neue Spalten ausgegeben, in der Form` && cl_abap_char_utilities=>cr_lf &&
        `* X~ffname (als eine Art von Pseudo-Join).` && cl_abap_char_utilities=>cr_lf &&
        `*` && cl_abap_char_utilities=>cr_lf &&
        `* Für die Definition der Formelfelder gibt es die Anweisungen` && cl_abap_char_utilities=>cr_lf &&
        `* $DEF und $TEXT (siehe die weiteren Beispiele).` && cl_abap_char_utilities=>cr_lf &&
        `*` && cl_abap_char_utilities=>cr_lf &&
        `* Ansonsten handelt es sich bei den Anweisungen um eine Untermenge` && cl_abap_char_utilities=>cr_lf &&
        `* von ABAP. Es können auf jeden Fall lokale Variablen definiert` && cl_abap_char_utilities=>cr_lf &&
        `* werden, und statische Variablen ebenfalls, falls man eine Formel` && cl_abap_char_utilities=>cr_lf &&
        `* mit globaler Betrachtung aufbauen möchte.` && cl_abap_char_utilities=>cr_lf &&
        `* Die statischen Variablen werden beim Durchlaufen der Formel für die` && cl_abap_char_utilities=>cr_lf &&
        `* erste Zeile der Liste initialisiert.` && cl_abap_char_utilities=>cr_lf &&
        `*` && cl_abap_char_utilities=>cr_lf &&
        `* Die Felder der aktuellen Listenzeile können mit ROW-feldname` && cl_abap_char_utilities=>cr_lf &&
        `* angesprochen werden, bzw. mit ROW-a-feldname, wo a der Alias ist.` && cl_abap_char_utilities=>cr_lf &&
        `*` && cl_abap_char_utilities=>cr_lf &&
        `* Die Anweisungen der Formel werden automatisch durch CATCH bzw. TRY` && cl_abap_char_utilities=>cr_lf &&
        `* abgesichert. Tritt eine Exception auf, so werden alle Formelfelder` && cl_abap_char_utilities=>cr_lf &&
        `* für die aktuelle Zeile auf Initialwert zurückgesetzt.` && cl_abap_char_utilities=>cr_lf &&
        `*` && cl_abap_char_utilities=>cr_lf &&
        `* Pro Listenebene kann zu einem Zeitpunkt nur eine Formel vorhanden` && cl_abap_char_utilities=>cr_lf &&
        `* sein. Die Formeln werden protokolliert und beim Sichern als query` && cl_abap_char_utilities=>cr_lf &&
        `* mitgesichert.` && cl_abap_char_utilities=>cr_lf.

    sv_calc_difference_exmple =
      `* In diesem Beispiel wird die Differenz zwischen Wunschmenge` && cl_abap_char_utilities=>cr_lf &&
      `* und bestätigter Menge errechnet.` && cl_abap_char_utilities=>cr_lf &&
      `* Es wird davon ausgegangen, dass die Liste Einträge von VBAP enthält.` && cl_abap_char_utilities=>cr_lf &&
      `*` && cl_abap_char_utilities=>cr_lf &&
      `* Zuerst wird das Differenzfeld definiert.` && cl_abap_char_utilities=>cr_lf &&
      `$DEF   DIFF    TYPE VBAP-KWMENG.` && cl_abap_char_utilities=>cr_lf &&
      `* Dann wird eine Mengeneinheit diesem Feld zugeordnet.` && cl_abap_char_utilities=>cr_lf &&
      `* Es wird dieselbe Einheit verwendet, wie für KWMENG und KBMENG.` && cl_abap_char_utilities=>cr_lf &&
      `* Diese Angabe ist optional. Die Mengeneinheit wird in ALV bei der` && cl_abap_char_utilities=>cr_lf &&
      `* Bildung von Summen berücksichtigt.` && cl_abap_char_utilities=>cr_lf &&
      `$UNIT  DIFF    ROW-VRKME.` && cl_abap_char_utilities=>cr_lf &&
      `* Optional kann eine Überschrift definiert werden.` && cl_abap_char_utilities=>cr_lf &&
      `$TEXT  DIFF  'unbest.'   'nicht bestätigte Menge'.` && cl_abap_char_utilities=>cr_lf &&
      `* Zum Schluß die Berechnung.` && cl_abap_char_utilities=>cr_lf &&
      `DIFF = ROW-KWMENG - ROW-KBMENG.` && cl_abap_char_utilities=>cr_lf.

    sv_icon_example =
      `* In diesem Beispiel wird der Liste eine Ikone hinzugefügt.` && cl_abap_char_utilities=>cr_lf &&
      `* Es wird davon ausgegangen, dass die Liste Einträge von VBAP enthält.` && cl_abap_char_utilities=>cr_lf &&
      `*` && cl_abap_char_utilities=>cr_lf &&
      `* Wenn es eine unbestätigte Menge gibt, d.h. wenn die bestätigte` && cl_abap_char_utilities=>cr_lf &&
      `* Menge KBMENG kleiner als die Wunschmenge KWMENG ist, dann soll` && cl_abap_char_utilities=>cr_lf &&
      `* in der entsprechenden Zeile eine kleine rote Fahne erscheinen.` && cl_abap_char_utilities=>cr_lf &&
      `*` && cl_abap_char_utilities=>cr_lf &&
      `* Zuerst wird das Ikonenfeld definiert. Dafür gibt es die Anweisungen` && cl_abap_char_utilities=>cr_lf &&
      `* $ICON (Nur Ikone) und $ICON_TT (Ikone mit Quicktip)` && cl_abap_char_utilities=>cr_lf &&
      `$ICON IKONE.` && cl_abap_char_utilities=>cr_lf &&
      `* Optional kann eine Überschrift definiert werden:` && cl_abap_char_utilities=>cr_lf &&
      `$TEXT IKONE   'unbest.' 'nicht bestätigte Menge vorhanden'.` && cl_abap_char_utilities=>cr_lf &&
      `* Jetzt kann die Ikone gefüllt werden.` && cl_abap_char_utilities=>cr_lf &&
      `* Welche Ikonen es gibt kann mit dem Programm SHOWICON ermittelt` && cl_abap_char_utilities=>cr_lf &&
      `* werden. Anstatt SPACE kann man auch ICON_SPACE verwenden.` && cl_abap_char_utilities=>cr_lf &&
      `IF ROW-KWMENG > ROW-KBMENG.` && cl_abap_char_utilities=>cr_lf &&
      `  IKONE = ICON_DEFECT.` && cl_abap_char_utilities=>cr_lf &&
      `ELSE.` && cl_abap_char_utilities=>cr_lf &&
      `  IKONE = SPACE.` && cl_abap_char_utilities=>cr_lf &&
      `ENDIF.` && cl_abap_char_utilities=>cr_lf.

    sv_icon_quicktip_example =
      `* In diesem Beispiel wird der Liste eine Ikone mit Quickinfo` && cl_abap_char_utilities=>cr_lf &&
      `* hinzugefügt (die Quickinfo erscheint, wenn der Mauszeiger über` && cl_abap_char_utilities=>cr_lf &&
      `* die Ikone gehalten wird).` && cl_abap_char_utilities=>cr_lf &&
      `* Es wird davon ausgegangen, dass die Liste Einträge von MARC enthält.` && cl_abap_char_utilities=>cr_lf &&
      `*` && cl_abap_char_utilities=>cr_lf &&
      `* Die Ikone wird verwendet, um die Beschaffungsart des Materials` && cl_abap_char_utilities=>cr_lf &&
      `* (MARC-BESKZ) zu verdeutlichen.` && cl_abap_char_utilities=>cr_lf &&
      `*` && cl_abap_char_utilities=>cr_lf &&
      `* Zuerst wird das Ikonenfeld definiert. Dies geschieht durch die` && cl_abap_char_utilities=>cr_lf &&
      `* Anweisung $ICON_TT iconName` && cl_abap_char_utilities=>cr_lf &&
      `$ICON_TT  IKONE.` && cl_abap_char_utilities=>cr_lf &&
      `* Optional kann eine Überschrift definiert werden:` && cl_abap_char_utilities=>cr_lf &&
      `$TEXT IKONE    'BesArt'  'Beschaffungsart'.` && cl_abap_char_utilities=>cr_lf &&
      `* Jetzt kann die Ikone gefüllt werden.` && cl_abap_char_utilities=>cr_lf &&
      `* Ikonen mit mit dem Befehl STRG+F6 (Ikonen suchen) eingefügt werden` && cl_abap_char_utilities=>cr_lf &&
      `* werden.` && cl_abap_char_utilities=>cr_lf &&
      `IF ROW-BESKZ = 'E'.` && cl_abap_char_utilities=>cr_lf &&
      `  $SET_ICON_TT  IKONE   ICON_PLANT        'Eigenfertigung'.` && cl_abap_char_utilities=>cr_lf &&
      `ELSEIF ROW-BESKZ = 'F'.` && cl_abap_char_utilities=>cr_lf &&
      `  $SET_ICON_TT  IKONE   ICON_ARROW_LEFT   'Fremdbeschaffung'.` && cl_abap_char_utilities=>cr_lf &&
      `ELSEIF ROW-BESKZ = 'X'.` && cl_abap_char_utilities=>cr_lf &&
      `  $SET_ICON_TT  IKONE   ICON_SYSTEMTYPE   'Beide Arten'.` && cl_abap_char_utilities=>cr_lf &&
      `ELSE.` && cl_abap_char_utilities=>cr_lf &&
      `  $SET_ICON_TT  IKONE   ICON_SPACE         space.` && cl_abap_char_utilities=>cr_lf &&
      `ENDIF.` && cl_abap_char_utilities=>cr_lf.

      data(lt_keywords) = VALUE char20_t(
        ( 'DATA' ) ( 'IF' ) ( 'ELSE' ) ( 'ELSEIF' ) ( 'ENDIF' ) ( 'ASSIGN' ) ( 'CASE' ) ( 'WHEN' ) ( 'ENDCASE' ) ( 'CHECK' ) ( 'CLEAR' ) ( 'COMPUTE' )
        ( 'WHILE' ) ( 'ENDWHILE' ) ( 'CONTINUE' ) ( 'EXIT' ) ( 'DO' ) ( 'ENDDO' )
        ( 'CONCATENATE' ) ( 'CONDENSE' ) ( 'CONSTANTS' )  ( 'CONVERT' )
        ( 'FIELD-SYMBOLS' ) ( 'MOVE' ) ( 'REPLACE' ) ( 'SHIFT' ) ( 'SPLIT' ) ( 'STATICS' ) ( 'TRANSLATE' ) ( 'TYPES' )
        ( 'WHEN' ) ( 'WRITE' )
      ).

      st_valid_keywords_range = value #(
        let i = 'I' eq = 'EQ' in
        for keyword in lt_keywords
        ( sign = i option = eq low = keyword )
      ).
  ENDMETHOD.
ENDCLASS.
