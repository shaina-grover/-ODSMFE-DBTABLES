*----------------------------------------------------------------------*
***INCLUDE /ODSMFE/INC_FORMS_STATUS_300.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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

  DATA: go_container TYPE REF TO cl_gui_custom_container.

  DATA: go_docking_container_1 TYPE REF TO cl_gui_docking_container,   "Docking Control Container
        go_docking_container_2 TYPE REF TO cl_gui_docking_container,   "Docking Control Container
        go_functions           TYPE REF TO cl_salv_functions_list.     "Generische und selbstdef. Funktionen in listähnl. Tabellen

  CONSTANTS: gc_true TYPE sap_bool VALUE 'X'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
  SET PF-STATUS 'S300'.
  SET TITLEBAR 'T300'.

  IF lr_parent IS BOUND.
    FREE lo_graph.
    lr_parent->free( ).
  ENDIF.

  CREATE OBJECT lr_parent
    EXPORTING
      repid          = sy-repid
      dynnr          = sy-dynnr
      container_name = 'CONT'.

  lt_outtab = <lt_form>.
  ls_columns = ''.

  CREATE OBJECT lo_graph
    EXPORTING
      i_parent               = lr_parent
      i_cu_guid              = lv_cu_guid
      it_fieldcatalog        = lt_fieldcatalog
      it_outtab              = lt_outtab
      it_columns             = lt_columns
      it_rows                = lt_rows
      is_variant             = ls_variant
      it_properties          = lt_properties
      i_web_mode             = lv_web_mode
    EXCEPTIONS
      error_create_dialogbox = 1
      error_create_dc        = 2
      error_init_dc          = 3
      error_init_gp          = 4
      error_fill_dc          = 5
      OTHERS                 = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.
