MODULE status_0300 OUTPUT.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    :
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

  DATA: go_events TYPE REF TO cl_salv_events_table.                    "Ereignisse in einfachen, zweidimensionalen Tabellen
  DATA: lo_msg    TYPE REF TO cx_salv_msg.                             "ALV: Allg. Fehlerklasse mit Meldung

  DATA: go_container       TYPE REF TO cl_gui_custom_container.        "Container fuer Custom Controls in der Dynpro Area
  DATA: go_docking_container_1 TYPE REF TO cl_gui_docking_container,   "Docking Control Container
        go_docking_container_2 TYPE REF TO cl_gui_docking_container,   "Docking Control Container
        go_functions           TYPE REF TO cl_salv_functions_list.     "Generische und selbstdef. Funktionen in listähnl. Tabellen

  CONSTANTS: gc_true TYPE sap_bool VALUE 'X'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
  SET PF-STATUS 'S300'.
  SET TITLEBAR 'T300'.

  IF go_dock_container IS BOUND.
    FREE go_alv.
    go_dock_container->free( ).
  ENDIF.                                                               " IF GO_DOCK_CONTAINER IS BOUND Line No. :41

  CREATE OBJECT go_dock_container
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      side      = go_dock_container->dock_at_left
      extension = 650.                                                 "go_dock_container->ws_minimizebox.

  TRY.
      cl_salv_table=>factory(
      EXPORTING
        r_container  = go_dock_container                               "go_container
      IMPORTING
        r_salv_table = go_alv
      CHANGING
        t_table      = go_control->gvib_model->gitib_foass ).
    CATCH cx_salv_msg INTO lo_msg.
  ENDTRY.

  DATA :lo_cols       TYPE REF TO cl_salv_columns,                     "Alle Spaltenobjekte
        lo_column     TYPE REF TO cl_salv_column.                      "Einzelnes Spaltenobjekt
  DATA: lo_func       TYPE REF TO cl_salv_functions,                   "Generische und selbstdefinierte Funktionen
        lo_functions  TYPE REF TO cl_salv_functions_list,              "Generische und selbstdef. Funktionen in listähnl. Tabellen
        lo_selections TYPE REF TO cl_salv_selections,                  "Markierungen in listähnlichen Ausgabetabellen
        lo_events     TYPE REF TO cl_salv_events_table.                "Ereignisse in einfachen, zweidimensionalen Tabellen
  DATA: lo_column1    TYPE REF TO cl_salv_column_table,                "Spaltenbeschreibung einfacher, zweidimensionaler Tabellen
        ls_color      TYPE lvc_s_colo.                                 "ALV control: Color coding

  go_control->gmib_set_columns( go_alv ).
  go_control->gmib_set_hotspot( go_alv ).

  lo_func  = go_alv->get_functions( ).
  lo_func->set_all( abap_true ).


* Get columns object
  lo_cols = go_alv->get_columns( ).
  lo_cols->set_optimize( abap_true ).

  lo_column1 ?= lo_cols->get_column( 'USER' ).

* Displaying the ALV
  go_alv->display( ).

* all events
  go_events = go_alv->get_event( ).

* event handler
  SET HANDLER go_control->gmib_on_link_click FOR go_events .

  SET HANDLER go_control->gmib_on_double_click FOR go_events .

  IF go_dock_container1 IS BOUND.
    FREE go_alv1.
    go_dock_container1->free( ).
  ENDIF.

  CREATE OBJECT go_dock_container1
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      side      = cl_gui_docking_container=>dock_at_right
      extension = 1560.

  DATA: lv_data      TYPE REF TO data,
        lv_add_count TYPE c.

  GET REFERENCE OF go_control->gvib_model->gitib_response_graph INTO lv_data .

  lv_add_count = abap_true.

  CALL METHOD go_control->gmib_show_graph
    EXPORTING
      im_data      = lv_data
      im_parent    = go_dock_container1
      im_add_count = ' '.
ENDMODULE.
