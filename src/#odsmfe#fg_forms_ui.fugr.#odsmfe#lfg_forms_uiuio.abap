MODULE status_0300 OUTPUT.


***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    :
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Change Date            : 29/12/2020
* Transport No.          : ES1K902363
* Change Description     : Made configurable Create button
***********************************************************************

*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

  DATA: go_events              TYPE REF TO cl_salv_events_table,                    "Ereignisse in einfachen, zweidimensionalen Tabellen
        go_msg                 TYPE REF TO cx_salv_msg,                           "ALV: Allg. Fehlerklasse mit Meldung
        go_container           TYPE REF TO cl_gui_custom_container,
        go_docking_container_1 TYPE REF TO cl_gui_docking_container,   "Docking Control Container
        go_docking_container_2 TYPE REF TO cl_gui_docking_container,   "Docking Control Container
        go_functions           TYPE REF TO cl_salv_functions_list.     "Generische und selbstdef. Funktionen in listähnl. Tabellen

  DATA:  gv_msgtxt          TYPE string,
         gv_icon            TYPE string, "++ES1K902140
         gv_mesg            TYPE string, "++ES1K902140
         go_exref           TYPE REF TO cx_root, "++ES1K902140
         gv_createbutconfig TYPE char50. "++ES1K902363

* Constants
  CONSTANTS: gc_true          TYPE sap_bool VALUE 'X',
             gc_i             TYPE string VALUE 'I',
             gc_create        TYPE string VALUE 'Create', "++ES1K902140
             gc_modifconfig   TYPE string VALUE 'GUIMODIFCONFIG',  "++ES1K902363
             gc_formmasterset TYPE string VALUE 'FormMasterSet'. "++ES1K902363

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
  SET PF-STATUS 'S300'.
  SET TITLEBAR 'T300'.

  IF go_dock_container IS BOUND.
    FREE go_alv.
    go_dock_container->free( ).
  ENDIF.                                                               " IF GO_DOCK_CONTAINER IS BOUND

  CREATE OBJECT go_dock_container
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      side      = go_dock_container->dock_at_left
      extension = 650.                                                 "go_dock_container->ws_minimizebox.

  TRY.
      cl_salv_table=>factory(
      EXPORTING
        r_container  = go_dock_container
      IMPORTING
        r_salv_table = go_alv
      CHANGING
        t_table      = go_control->gvib_model->gitib_foass ).
    CATCH cx_salv_msg INTO go_msg.
      gv_msgtxt = go_msg->get_text( ).
      MESSAGE gv_msgtxt TYPE gc_i.
  ENDTRY.

*--sindhu "++ES1K902140
  go_control->gmib_set_columns( go_alv ).
  go_control->gmib_set_hotspot( go_alv ).

  go_control->gvib_selections = go_alv->get_selections( ).
  go_control->gvib_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  go_functions = go_alv->get_functions( ).

*   SOC by ODS ES1K902363
  CLEAR gv_createbutconfig.

  SELECT SINGLE low FROM /odsmfe/tb_filtr INTO gv_createbutconfig
  WHERE entitysetname = gc_formmasterset
  AND field = gc_modifconfig
  AND active = gc_true.

  IF sy-subrc EQ 0.
*   EOC by ODS ES1K902363
* Create button
    TRY.

        gv_icon = icon_create.
        go_functions->add_function(
        name     = 'CREATE'
        icon     = gv_icon
        text     = gc_create"lv_text
        tooltip  = gc_create"lv_text
        position = if_salv_c_function_position=>left_of_salv_functions ).

      CATCH cx_salv_wrong_call.
        CLEAR: gv_mesg.
        gv_mesg = go_exref->get_text( ).
        MESSAGE gv_mesg TYPE gc_i.
      CATCH cx_salv_existing.
        CLEAR: gv_mesg.
        gv_mesg = go_exref->get_text( ).
        MESSAGE gv_mesg TYPE gc_i.
    ENDTRY.

  ENDIF. "++ ES1K902363

* Displaying the ALV
  go_alv->display( ).

*--sindhu "++ES1K902140

* all events
  go_events = go_alv->get_event( ).

* event handler
  SET HANDLER go_control->gmib_on_link_click FOR go_events .

  SET HANDLER go_control->gmib_on_double_click FOR go_events .

*--sindhu "++ES1K902140
  SET HANDLER go_control->gmib_on_user_command FOR go_events.
*--sindhu "++ES1K902140
ENDMODULE.
