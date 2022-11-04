FUNCTION /odsmfe/fm_form_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*
  TYPE-POOLS: icon.

  DATA: lo_msg       TYPE REF TO cx_salv_msg,                     "ALV: Allg. Fehlerklasse mit Meldung
        lo_functions TYPE REF TO cl_salv_functions_list,          "Generische und selbstdef. Funktionen in listähnl. Tabellen
        lo_exref     TYPE REF TO cx_root,                         "#EC NEEDED
        lo_events    TYPE REF TO cl_salv_events_table.            "Ereignisse in einfachen, zweidimensionalen Tabellen

  DATA:  lv_text            TYPE string,
         lv_text1           TYPE string,
         lv_icon            TYPE string,
         lv_mesg            TYPE string,
         lv_createbutconfig TYPE char50. "++ES1K902363

  CONSTANTS: lc_true          TYPE sap_bool VALUE 'X',
             lc_display       TYPE string VALUE 'Display',
             lc_edit          TYPE string VALUE 'Edit',
             lc_i             TYPE string VALUE 'I',
             lc_modifconfig   TYPE string VALUE 'GUIMODIFCONFIG',  "++ES1K902363
             lc_formmasterset TYPE string VALUE 'FormMasterSet'."++ES1K902363

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

* Header object

  IF go_dock_container1 IS BOUND.
    FREE go_alv1.
    go_dock_container1->free( ).
  ENDIF.

  CREATE OBJECT go_dock_container1
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      side      = go_dock_container1->dock_at_right
      extension = 1560.

  TRY.
      cl_salv_table=>factory(
      EXPORTING
        r_container  = go_dock_container1
      IMPORTING
        r_salv_table = go_alv1
      CHANGING
        t_table      = go_control->gvib_model->gitib_final ).
    CATCH cx_salv_msg INTO lo_msg.
      lv_mesg = lo_msg->get_text( ).
      MESSAGE lv_mesg TYPE lc_i.
  ENDTRY.

  go_control->gvib_selections01 = go_alv1->get_selections( )."++ES1K902140
  go_control->gvib_selections01->set_selection_mode( if_salv_c_selection_mode=>row_column )."++ES1K902140

  lo_functions = go_alv1->get_functions( ).
  lo_events = go_alv1->get_event( ).

* Display button
  TRY.

      lv_icon = icon_display.
      lo_functions->add_function(
      name     = 'DISPLAY'
      icon     = lv_icon
      text     = lc_display
      tooltip  = lc_display
      position = if_salv_c_function_position=>left_of_salv_functions ).

    CATCH cx_salv_wrong_call.
      CLEAR: lv_mesg.
      lv_mesg = lo_exref->get_text( ).
      MESSAGE lv_mesg TYPE lc_i.
    CATCH cx_salv_existing.
      CLEAR: lv_mesg.
      lv_mesg = lo_exref->get_text( ).
      MESSAGE lv_mesg TYPE lc_i.
  ENDTRY.

*   SOC by ODS ES1K902363
  CLEAR lv_createbutconfig.

  SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_createbutconfig
         WHERE entitysetname = lc_formmasterset
         AND field = lc_modifconfig
         AND active = lc_true.
  IF sy-subrc EQ 0.
*   EOC by ODS ES1K902363
* Edit Button
    TRY.
        lv_icon  = icon_edit_file.
        lo_functions->add_function(
        name     = 'EDIT'
        icon     = lv_icon
        text     = lc_edit"lv_text1
        tooltip  = lc_edit"lv_text1
        position = if_salv_c_function_position=>left_of_salv_functions ).
*    CATCH cx_salv_wrong_call cx_salv_existing.
      CATCH cx_salv_wrong_call.
        CLEAR: lv_mesg.
        lv_mesg = lo_exref->get_text( ).
        MESSAGE lv_mesg TYPE lc_i.
      CATCH cx_salv_existing.
        CLEAR: lv_mesg.
        lv_mesg = lo_exref->get_text( ).
        MESSAGE lv_mesg TYPE lc_i.
    ENDTRY.
  ENDIF. "++ ES1K902363

** event handler
  SET HANDLER go_control->gmib_on_user_command FOR lo_events.

  go_control->gmib_set_columns( go_alv1 ).
* Displaying the ALV
  go_alv1->display( ).
ENDFUNCTION.
