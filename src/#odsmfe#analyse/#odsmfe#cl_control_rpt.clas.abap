class /ODSMFE/CL_CONTROL_RPT definition
  public
  create public .

public section.
  type-pools ABAP .

  data GVIB_MODEL type ref to /ODSMFE/CL_MODEL_RPT .
  data GVIB_SELECTIONS type ref to CL_SALV_SELECTIONS .

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
  methods GMIB_SHOW_GRAPH
    importing
      !IM_DATA type ref to DATA
      !IM_PARENT type ref to CL_GUI_CONTAINER
      !IM_ADD_COUNT type CHAR1 default ABAP_TRUE .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_CONTROL_RPT IMPLEMENTATION.


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
DATA: lst_foass  like line of /ODSMFE/CL_MODEL_RPT=>gitib_foass ."/odsmfe/cl_model=>gtys_foass.

CONSTANTS:  lc_submitted   TYPE string VALUE 'SUBMITTED'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

      CLEAR: lst_foass.

      READ TABLE gvib_model->gitib_foass INTO lst_foass INDEX row.

      IF sy-subrc EQ 0.
        me->gvib_model->gmib_get_data(
        im_forms = lst_foass
        ).
      ENDIF.

  ENDMETHOD.


  METHOD GMIB_ON_LINK_CLICK.
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
*  DATA: lst_foass TYPE /odsmfe/cl_model=>gtys_foass.

  CONSTANTS:  lc_submitted   TYPE string VALUE 'SUBMITTED'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    IF column =  lc_submitted.

*      CLEAR: lst_foass.
      READ TABLE gvib_model->gitib_foass INTO data(lst_foass) INDEX row.

      IF sy-subrc EQ 0.
        me->gvib_model->gmib_get_data( im_forms = lst_foass ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD GMIB_ON_USER_COMMAND.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Method to trigger on Button click
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

    DATA: lo_table   TYPE REF TO cl_salv_table,                        "Basisklasse für einfache Tabellen
          lo_columns TYPE REF TO cl_salv_columns_table.                "Spalten in einfachen, zweidimensionalen Tabellen

    DATA: lit_output TYPE STANDARD TABLE OF gvib_model->gtys_final,      "
          lst_rows   TYPE int4,                                        "Natural Number
          lst_output TYPE gvib_model->gtys_final,                        "
          lit_rows   TYPE salv_t_row.                                  "

* constants
    CONSTANTS: lc_forminstanceid TYPE string VALUE 'ForminstanceId',   "
               lc_browser        TYPE string VALUE 'chrome.exe'.       "

* Field symbols
    FIELD-SYMBOLS : <lfsst_final> TYPE gvib_model->gtys_final.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    CASE e_salv_function.

      WHEN 'DISPLAY'.
* Get the selected rows.
        lit_rows = gvib_selections->get_selected_rows( ).

* Display the selected rows.
        LOOP AT lit_rows INTO lst_rows.
          READ TABLE gvib_model->gitib_final ASSIGNING <lfsst_final> INDEX lst_rows.
          IF sy-subrc = 0 AND <lfsst_final>-fname IS NOT INITIAL.
            CALL FUNCTION '/ODSMFE/FM_FORMS_CALL_URL'
              EXPORTING
                im_mode       = lc_forminstanceid
                im_formid     = <lfsst_final>-fname
                im_instanceid = <lfsst_final>-instanceid
                im_browser    = lc_browser
                im_function   = e_salv_function.

            cl_gui_cfw=>dispatch( ).
            cl_gui_cfw=>flush( ).

            IF sy-subrc <> 0.
              EXIT.
            ENDIF.                                                     " IF SY-SUBRC <> 0

          ENDIF.                                                       " IF SY-SUBRC = 0 AND <LFSST_FINAL>-FNAME IS NOT INITIAL
        ENDLOOP.                                                       " LOOP AT LIT_ROWS


      WHEN 'EDIT'.
* Get the selected rows.
        lit_rows = gvib_selections->get_selected_rows( ).

* Display the selected rows.
        LOOP AT lit_rows INTO lst_rows.
          READ TABLE gvib_model->gitib_final ASSIGNING <lfsst_final> INDEX lst_rows.
          IF sy-subrc = 0 AND <lfsst_final>-draft IS NOT INITIAL.
            CALL FUNCTION '/ODSMFE/FM_FORMS_CALL_URL'
              EXPORTING
                im_mode       = lc_forminstanceid
                im_formid     = <lfsst_final>-fname
                im_instanceid = <lfsst_final>-instanceid
                im_browser    = lc_browser
                im_function   = e_salv_function.

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


  METHOD GMIB_SET_COLUMNS.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Method to set columns to display in ALV
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

    DATA: lo_cols   TYPE REF TO cl_salv_columns_table,
          lo_column TYPE REF TO cl_salv_column_table.

    CONSTANTS: lc_draft      TYPE lvc_fname VALUE 'DRAFT',
               lc_occur      TYPE lvc_fname VALUE 'OCCUR',
               lc_submitted  TYPE lvc_fname VALUE 'SUBMITTED',
               lc_operation  TYPE lvc_fname VALUE 'OPERATION',
               lc_instanceid TYPE lvc_fname VALUE 'INSTANCEID'.
    CONSTANTS: lc_allowed_stext   TYPE scrtext_s VALUE 'Allowed',
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
               lc_draft_ltext     TYPE scrtext_l VALUE 'Draft'.

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

  ENDMETHOD.


  METHOD GMIB_SET_HOTSPOT.

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
          lo_exref  TYPE REF TO cx_root,  "-EC NEEDED
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

      CATCH cx_salv_not_found.
        lv_mesg = lo_exref->get_text( ).
        MESSAGE lv_mesg TYPE lc_i.
      CATCH cx_salv_data_error.
        lv_msgtxt = lo_exref->get_text( ).
        MESSAGE lv_msgtxt TYPE lc_i.
    ENDTRY.

  ENDMETHOD.


  METHOD gmib_show_graph.
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*
    DATA: ref_final  TYPE REF TO data.
    DATA: ref_final1 TYPE REF TO data.
    DATA: wa_form    TYPE abap_componentdescr.

    DATA: lr_parent       TYPE REF TO cl_gui_custom_container,         "Abstracter Container fuer GUI Controls
          lo_graph        TYPE REF TO /odsmfe/cl_alv_graphics,         "ALV-Graphik (Anschluss an GFW)
          lv_cu_guid      TYPE guid_32,                                "GUID in 'CHAR' Format in Uppercase
          lt_fieldcatalog TYPE lvc_t_fcat,
          lt_sort         TYPE abap_sortorder_tab,
          ls_fieldcatalog LIKE LINE OF lt_fieldcatalog,
          lv_cell_type    TYPE char20 VALUE 'CELL_TYPE',
          lv_colpos       TYPE n,
          lt_columns      TYPE lvc_t_fnam,
          ls_columns      LIKE LINE OF lt_columns,
          lt_rows         TYPE lvc_t_roid,
          ls_rows         LIKE LINE OF lt_rows,
          ls_variant      TYPE disvariant,
          lt_properties   TYPE dtc_t_tc,
          ls_properties   LIKE LINE OF lt_properties,
          lv_web_mode     TYPE c.

    FIELD-SYMBOLS: <lt_form>       TYPE STANDARD TABLE,
                   <ls_form>       TYPE any,
                   <ls_form1>      TYPE any,
                   <ls_instanceid> TYPE any,
                   <lt_out>        TYPE STANDARD TABLE,
                   <lt_out1>       TYPE STANDARD TABLE,
                   <ls_field>      TYPE any.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*
* ----------------------------------------------------------------------*
* --- Addd data---------------------------------------------------------*
* ----------------------------------------------------------------------*

    ASSIGN im_data->* TO <lt_form>.
* ----------------------------------------------------------------------*
* --- get field cat-----------------------------------------------------*
* ----------------------------------------------------------------------*
    /odsmfe/cl_pm_util=>meth_dynamic_fcat( EXPORTING im_data = im_data CHANGING ch_i_fcat = lt_fieldcatalog ).
    /odsmfe/cl_pm_util=>meth_dynamic_fcat( EXPORTING im_fnam = 'SUBMITTED' im_ftext = 'Submitted' CHANGING ch_i_fcat = lt_fieldcatalog ).

* ----------------------------------------------------------------------*
* --- Add count---------------------------------------------------------*
* ----------------------------------------------------------------------*

    IF im_add_count IS NOT INITIAL.


      DATA: otable  TYPE REF TO cl_abap_tabledescr.                    "Run Time Type Services
      DATA: ostruct TYPE REF TO cl_abap_structdescr.                   "Run Time Type Services

      otable ?= cl_abap_tabledescr=>describe_by_data_ref(  im_data ).

      ostruct ?= otable->get_table_line_type( ).

      DATA(p_form) = ostruct->get_components( ).

      DELETE p_form WHERE ('NAME = LV_CELL_TYPE').
      wa_form-name = 'COL2'.
      wa_form-type = cl_abap_elemdescr=>get_i( ).                      "
      APPEND wa_form TO p_form.
      CLEAR wa_form.


      DATA(ostruct1) = cl_abap_structdescr=>create( p_form ).

      DATA(otable1) = cl_abap_tabledescr=>create( ostruct1 ).

      CREATE DATA ref_final TYPE HANDLE otable1.
      CREATE DATA ref_final1 TYPE HANDLE otable1.

      ASSIGN ref_final1->* TO <lt_out>.
      ASSIGN ref_final1->* TO <lt_out1>.

      LOOP AT p_form INTO wa_form.

        APPEND INITIAL LINE TO lt_sort ASSIGNING FIELD-SYMBOL(<ls_sort>).

        <ls_sort>-name  = wa_form-name.

      ENDLOOP.

      MOVE-CORRESPONDING <lt_form> TO <lt_out1>.

      SORT <lt_out1>[] BY (lt_sort) .

      LOOP AT <lt_out1> ASSIGNING FIELD-SYMBOL(<ls_out1>).

        ASSIGN COMPONENT 'COL2' OF STRUCTURE <ls_out1> TO <ls_field>.

        IF <ls_field> IS ASSIGNED.
          <ls_field> = 1.
        ENDIF.

        COLLECT <ls_out1>  INTO <lt_out>.

      ENDLOOP.

      CLEAR ls_rows.
      CLEAR lt_rows.

      ls_fieldcatalog-col_pos   = lines( p_form ) .
      ls_fieldcatalog-fieldname = 'COL2' .
      ls_fieldcatalog-seltext   = 'COL2' .
      ls_fieldcatalog-inttype   = 'I' .

      ls_columns                = 'COL2'.

      APPEND ls_fieldcatalog TO lt_fieldcatalog.

    ELSE.
      ASSIGN im_data->* TO <lt_out>.
    ENDIF.

* ----------------------------------------------------------------------*
* --- fill row table----------------------------------------------------*
* ----------------------------------------------------------------------*
    LOOP AT <lt_out> ASSIGNING FIELD-SYMBOL(<ls_out>).
      ls_rows-row_id = ls_rows-row_id + 1.

      APPEND ls_rows TO lt_rows.

    ENDLOOP.
* ----------------------------------------------------------------------*
* --- Display Graph-----------------------------------------------------*
* ----------------------------------------------------------------------*
    CREATE OBJECT lo_graph
      EXPORTING
        i_parent               = im_parent
        i_cu_guid              = lv_cu_guid
        it_fieldcatalog        = lt_fieldcatalog
        it_outtab              = <lt_out>
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
  ENDMETHOD.
ENDCLASS.
