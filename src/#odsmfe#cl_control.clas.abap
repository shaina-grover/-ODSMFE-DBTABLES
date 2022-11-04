class /ODSMFE/CL_CONTROL definition
  public
  create public .

public section.
  type-pools ABAP .

  data GVIB_MODEL type ref to /ODSMFE/CL_MODEL .
  data GVIB_SELECTIONS type ref to CL_SALV_SELECTIONS .
  data GVIB_SELECTIONS01 type ref to CL_SALV_SELECTIONS .

  methods GMIB_GET_OBJECT
    importing
      !IM_NAME type CHAR30 .
  methods GMIB_SET_COLUMNS
    importing
      !IM_ALV type ref to CL_SALV_TABLE .
  methods GMIB_SET_HOTSPOT
    importing
      !IM_ALV type ref to CL_SALV_TABLE
    raising
      CX_SALV_NOT_FOUND .
  methods GMIB_ON_LINK_CLICK
    for event LINK_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW
      !COLUMN .
  methods GMIB_ON_USER_COMMAND
    for event ADDED_FUNCTION of CL_SALV_EVENTS
    importing
      !E_SALV_FUNCTION .
  methods GMIB_ON_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW
      !COLUMN .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_CONTROL IMPLEMENTATION.


  method GMIB_GET_OBJECT.

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

  DATA: lo_object TYPE REF TO object.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
* Generic object reference to importing class
  CREATE OBJECT lo_object TYPE (im_name).
  IF sy-subrc = 0.
*   Downcasting to assign generic object to O_MODEL
    gvib_model ?= lo_object.
  ENDIF.

  endmethod.


  METHOD GMIB_ON_DOUBLE_CLICK.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Method to trigger on double click
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
DATA: lst_foass TYPE /odsmfe/cl_model=>gtys_foass.

CONSTANTS:  lc_submitted   TYPE string VALUE 'SUBMITTED'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

      CLEAR: lst_foass.
      READ TABLE gvib_model->gitib_foass INTO lst_foass INDEX row.
      IF sy-subrc EQ 0.
        me->gvib_model->gmib_get_data( im_forms = lst_foass ).
      ENDIF.

  ENDMETHOD.


  METHOD gmib_on_link_click.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : method to trigger on link click
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
  DATA: lst_foass TYPE /odsmfe/cl_model=>gtys_foass.

  CONSTANTS:  lc_submitted   TYPE string VALUE 'SUBMITTED'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    IF column =  lc_submitted.

      CLEAR: lst_foass.
      READ TABLE gvib_model->gitib_foass INTO lst_foass INDEX row.
      IF sy-subrc EQ 0.
        me->gvib_model->gmib_get_data( im_forms = lst_foass ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD gmib_on_user_command.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Method to trigger on Button click
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Change Date            : 02/09/2020
* Transport No.          : ES1K902140
* Change Description     : Added Create Button navigation
***********************************************************************

*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

    DATA: lo_table   TYPE REF TO cl_salv_table,                        "Basisklasse für einfache Tabellen
          lo_columns TYPE REF TO cl_salv_columns_table.                "Spalten in einfachen, zweidimensionalen Tabellen

    DATA: lit_output TYPE STANDARD TABLE OF gvib_model->gtys_final,
          lst_rows   TYPE int4,                                        "Natural Number
          lst_output TYPE gvib_model->gtys_final,
          lit_rows   TYPE salv_t_row.
    DATA: lv_sub     TYPE c, "++ES1K902140
          lv_browser TYPE string.
* constants
    CONSTANTS: lc_forminstanceid TYPE string VALUE 'ForminstanceId',
               lc_formmasterset  TYPE string VALUE 'FormMasterSet',
               lc_browser        TYPE string VALUE 'BROWSER',
               lc_true           TYPE sap_bool VALUE 'X',
               lc_formmetadataid TYPE string VALUE 'FormMetaDataID'. "++ES1K902140

* Field symbols
    FIELD-SYMBOLS : <lfsst_final> TYPE gvib_model->gtys_final,
                    <lfsst_foass> TYPE gvib_model->gtys_foass. " Ysindhu ++ES1K902140

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    CLEAR lv_browser.

    SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_browser
           WHERE entitysetname = lc_formmasterset
           AND field = lc_browser
           AND active = lc_true.

    CASE e_salv_function.
* SOC -Ysindhu   ES1K902140
        " On Create button click
      WHEN 'CREATE'.
* Get the selected rows.
        REFRESH lit_rows.   "++ skammari ES1K902140
        lit_rows = gvib_selections->get_selected_rows( ).

* Display the selected rows.
        LOOP AT lit_rows INTO lst_rows.
          READ TABLE gvib_model->gitib_foass ASSIGNING <lfsst_foass> INDEX lst_rows.
          IF <lfsst_foass>-submitted CS 'No Forms'.
            CLEAR:<lfsst_foass>-submitted.
          ELSE.
            IF <lfsst_foass>-occur IS NOT INITIAL.
              IF <lfsst_foass>-submitted >= <lfsst_foass>-occur.
                MESSAGE i398(00) WITH text-007 <lfsst_foass>-formid.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.
          IF <lfsst_foass>-formid IS NOT INITIAL.

            CALL FUNCTION '/ODSMFE/FM_FORMS_CALL_URL'
              EXPORTING
                im_wo       = <lfsst_foass>-wo_num "++ skammari ES1K902140
                im_mode     = lc_formmetadataid
                im_formid   = <lfsst_foass>-formid
                im_version  = <lfsst_foass>-version
                im_browser  = lv_browser "text-001 "chrome.exe
                im_function = e_salv_function
                im_vornr    = <lfsst_foass>-operation.

            cl_gui_cfw=>dispatch( ).
            cl_gui_cfw=>flush( ).

            IF sy-subrc <> 0.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
*--EOC Ysindhu   ++ES1K902140

      WHEN 'DISPLAY'.
* Get the selected rows.
        REFRESH lit_rows."++ skammari ES1K902140

        lit_rows = gvib_selections01->get_selected_rows( ).
* Display the selected rows.
        LOOP AT lit_rows INTO lst_rows.
          READ TABLE gvib_model->gitib_final ASSIGNING <lfsst_final> INDEX lst_rows.
          IF sy-subrc = 0 AND <lfsst_final>-fname IS NOT INITIAL.
            CALL FUNCTION '/ODSMFE/FM_FORMS_CALL_URL'
              EXPORTING
                im_mode       = lc_forminstanceid
                im_formid     = <lfsst_final>-fname
                im_instanceid = <lfsst_final>-instanceid
                im_browser    = lv_browser "text-001 "chrome.exe
                im_function   = e_salv_function
                im_vornr      = <lfsst_final>-operation.

            cl_gui_cfw=>dispatch( ).
            cl_gui_cfw=>flush( ).

            IF sy-subrc <> 0.
              EXIT.
            ENDIF.                                                     " IF SY-SUBRC <> 0

          ENDIF.                                                       " IF SY-SUBRC = 0 AND <LFSST_FINAL>-FNAME IS NOT INITIAL
        ENDLOOP.

      WHEN 'EDIT'.
* Get the selected rows.
        REFRESH lit_rows. "++skammari ES1K902140

        lit_rows = gvib_selections01->get_selected_rows( )."++ skammari ES1K902140
* Display the selected rows.
        LOOP AT lit_rows INTO lst_rows.
          READ TABLE gvib_model->gitib_final ASSIGNING <lfsst_final> INDEX lst_rows.
          IF sy-subrc = 0 AND <lfsst_final>-draft IS NOT INITIAL.
            CALL FUNCTION '/ODSMFE/FM_FORMS_CALL_URL'
              EXPORTING
                im_mode       = lc_forminstanceid
                im_formid     = <lfsst_final>-fname
                im_instanceid = <lfsst_final>-instanceid
                im_browser    = lv_browser "text-001 "chrome.exe
                im_function   = e_salv_function
                im_vornr      = <lfsst_final>-operation.

            cl_gui_cfw=>dispatch( ).
            cl_gui_cfw=>flush( ).

            IF sy-subrc <> 0.
              EXIT.
            ENDIF.
          ELSE.
            MESSAGE i398(00) WITH text-006 <lfsst_final>-fname.
          ENDIF.
        ENDLOOP.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


  METHOD gmib_set_columns.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Method to set columns to display in ALV
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Change Date            : 08/09/2020
* Transport No.          : ES1K902140
* Change Description     : Added logic to hide work order column
***********************************************************************

*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

    DATA: lo_cols   TYPE REF TO cl_salv_columns_table,
          lo_column TYPE REF TO cl_salv_column_table.

    CONSTANTS: lc_draft           TYPE lvc_fname VALUE 'DRAFT',
               lc_occur           TYPE lvc_fname VALUE 'OCCUR',
               lc_submitted       TYPE lvc_fname VALUE 'SUBMITTED',
               lc_operation       TYPE lvc_fname VALUE 'OPERATION',
               lc_instanceid      TYPE lvc_fname VALUE 'INSTANCEID',
               lc_allowed_stext   TYPE scrtext_s VALUE 'Allowed',
               lc_allowed_mtext   TYPE scrtext_m VALUE 'Allowed',
               lc_allowed_ltext   TYPE scrtext_l VALUE 'Allowed',
               lc_operation_stext TYPE scrtext_s VALUE 'Oper',
               lc_operation_mtext TYPE scrtext_m VALUE 'Operation',
               lc_operation_ltext TYPE scrtext_l VALUE 'Operation',
               lc_submitted_stext TYPE scrtext_s VALUE 'Submitted',
               lc_submitted_mtext TYPE scrtext_m VALUE 'Submitted',
               lc_submitted_ltext TYPE scrtext_l VALUE 'Submitted',
               lc_draft_stext     TYPE scrtext_s VALUE 'Draft',
               lc_draft_mtext     TYPE scrtext_m VALUE 'Draft',
               lc_draft_ltext     TYPE scrtext_l VALUE 'Draft',
               lc_wonum           TYPE lvc_fname VALUE 'WO_NUM'. "++ ES1K902140

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------


    lo_cols = im_alv->get_columns( ).
    lo_cols->set_optimize( 'X' ).
    lo_cols->set_key_fixation( 'X' ).
    TRY.
        lo_column ?= lo_cols->get_column( lc_occur )."'OCCUR'
        lo_column->set_short_text( lc_allowed_stext ).
        lo_column->set_medium_text( lc_allowed_mtext ).
        lo_column->set_long_text( lc_allowed_ltext ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY .
    TRY.
        lo_column ?= lo_cols->get_column( lc_operation )."'OPERATION'
        lo_column->set_short_text( lc_operation_stext ).
        lo_column->set_medium_text( lc_operation_mtext ).
        lo_column->set_long_text( lc_operation_ltext ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY .
    TRY.
        lo_column ?= lo_cols->get_column( lc_submitted )."'SUBMITTED'
        lo_column->set_short_text( lc_submitted_stext ).
        lo_column->set_medium_text( lc_submitted_mtext ).
        lo_column->set_long_text( lc_submitted_ltext ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY .
    TRY.
        lo_column ?= lo_cols->get_column( lc_instanceid )."'INSTANCEID'
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY .
    TRY.
        lo_column ?= lo_cols->get_column( lc_draft )."'DRAFT'
        lo_column->set_short_text( lc_draft_stext ).
        lo_column->set_medium_text( lc_draft_mtext ).
        lo_column->set_long_text( lc_draft_ltext ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY .
    "SOC skammari ++ES1K902140
    TRY.
        lo_column ?= lo_cols->get_column( lc_wonum )."'WO_NUM'
        lo_column->set_visible( abap_false ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY .
    " EOC Skammari ++ES1K902140
  ENDMETHOD.


  METHOD gmib_set_hotspot.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  08/04/2020
* Transport No.          :  ES1K901528
* Program Description    :  Method to set Hotspot
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


    DATA: lo_cols_tab TYPE REF TO cl_salv_columns_table,               "Spalten in einfachen, zweidimensionalen Tabellen
          lo_col_tab  TYPE REF TO cl_salv_column_table,                "Spaltenbeschreibung einfacher, zweidimensionaler Tabellen
          lo_col      TYPE REF TO cl_salv_column.                      "Einzelnes Spaltenobjekt

    DATA: lv_value  TYPE sap_bool,                                   "mesg TYPE string. "
          lo_exref  TYPE REF TO cx_root,
          lv_msgtxt TYPE string,
          lv_mesg   TYPE string.

* constants
    CONSTANTS: lc_i               TYPE string VALUE 'I',
               lc_submitted       TYPE lvc_fname VALUE 'SUBMITTED',
               lc_submitted_mtext TYPE scrtext_m VALUE 'Submitted'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

*   get Columns object
    lo_cols_tab = im_alv->get_columns( ).

*   Get BUTTON column
    TRY.
        lo_col_tab ?= lo_cols_tab->get_column( lc_submitted )."'SUBMITTED'
        lo_col_tab->set_medium_text( lc_submitted_mtext )."'Submitted'

        lo_cols_tab->set_cell_type_column( 'CELL_TYPE' ).

      CATCH cx_salv_not_found INTO lo_exref.
        lv_mesg = lo_exref->get_text( ).
        MESSAGE lv_mesg TYPE lc_i.
      CATCH cx_salv_data_error INTO lo_exref.
        lv_msgtxt = lo_exref->get_text( ).
        MESSAGE lv_msgtxt TYPE lc_i.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
