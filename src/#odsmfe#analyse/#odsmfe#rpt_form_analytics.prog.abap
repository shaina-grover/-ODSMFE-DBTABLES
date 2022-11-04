*&---------------------------------------------------------------------*
*& Report  /ODSMFE/RPT_FORM_ANALYTICS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT /odsmfe/rpt_form_analytics.
*Type pools declaration for ALV
TYPES:BEGIN OF lty_out,
        col1 TYPE char255,                                            "contains 4 table elements of 250 Bytes
        col2 TYPE int4,
      END OF lty_out.

TYPE-POOLS: slis.                                                      " ALV Global Types
*data declaration for dynamic internal table and alv
DATA: l_structure  TYPE REF TO data,
      l_table      TYPE REF TO data,
      struc_desc   TYPE REF TO cl_abap_structdescr,                    "Run Time Type Services
      lt_layout    TYPE slis_layout_alv,
      ls_lvc_field TYPE lvc_s_fcat,                                    "ALV control: Field catalog
      lt_lvc_field TYPE lvc_t_fcat.

DATA: lr_parent       TYPE REF TO cl_gui_custom_container,             "Abstracter Container fuer GUI Controls
      lo_graph        TYPE REF TO cl_alv_graphics,                     "ALV-Graphik (Anschluss an GFW)
      lv_cu_guid      TYPE guid_32,                                    "GUID in 'CHAR' Format in Uppercase
      lt_fieldcatalog TYPE lvc_t_fcat,
      ls_fieldcatalog LIKE LINE OF lt_fieldcatalog,
      lt_outtab       TYPE STANDARD TABLE OF lty_out,
      ls_outtab       LIKE LINE OF lt_outtab,
      lt_columns      TYPE lvc_t_fnam,
      ls_columns      LIKE LINE OF lt_columns,
      lt_rows         TYPE lvc_t_roid,
      ls_rows         LIKE LINE OF lt_rows,
      ls_variant      TYPE disvariant,
      lt_properties   TYPE dtc_t_tc,
      ls_properties   LIKE LINE OF lt_properties,
      lv_web_mode     TYPE c.

DATA: lt_xfieldnames TYPE lvc_t_fnam,
      ls_xfieldnames LIKE LINE OF lt_xfieldnames,
      lt_yfieldnames TYPE lvc_t_fnam,
      ls_yfieldnames LIKE LINE OF lt_yfieldnames,
      lv_xtitle      TYPE string,
      lv_ytitle      TYPE string.

TYPES: BEGIN OF ty_form,
         instanceid   TYPE /odsmfe/de_instanceid,                        "ODS MFE InstanceId
         formid       TYPE /odsmfe/de_formid,                            "ODS Form ID
         version      TYPE /odsmfe/de_version,                           "ODS Version
         wo_num       TYPE aufnr,                                        "Order Number
         vornr        TYPE vornr,                                        "Operation/Activity Number
         equnr        TYPE equnr,                                        "Equipment Number
         tplnr        TYPE tplnr,                                        "Functional Location
         created_date TYPE dats,                                         "CreatedOn UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
         created_by   TYPE /odsmfe/de_createdby,                         "ODS Created By
       END OF ty_form.
DATA: lt_form TYPE STANDARD TABLE OF ty_form.

DATA: lt_formrsp    TYPE STANDARD TABLE OF /odsmfe/tb_forsp,           "Table to Capture Response
      lt_formmst    TYPE STANDARD TABLE OF /odsmfe/tb_fomst,           "FORM Master Table
      ls_formrsp    TYPE /odsmfe/tb_forsp,                             "Table to Capture Response
      ls_formmst    TYPE /odsmfe/tb_fomst,                             "FORM Master Table
      lt_filter_val TYPE /odsmfe/core_range_tab,
      ls_filter_val TYPE /odsmfe/core_range_str.                       "Generic all purpose Range Structure

DATA: lv_xml_string TYPE xstring,
      lv_icon       TYPE string,
      lt_data       TYPE swxmlcont,
      lt_retcode    TYPE sysubrc,                                      "Return Code
      lref_xml      TYPE REF TO cl_xml_document,                       "XML-Dokument für WF- WEB-Aktivität
      lv_subrc      TYPE sy-subrc,                                     "ABAP System Field: Return Code of ABAP Statements
      lv_size       TYPE sytabix,                                      "Row Index of Internal Tables
      lt_xml_data   TYPE TABLE OF smum_xmltb,                          "XML Table structure used for retreive and output XML doc
      ls_xml_data   TYPE smum_xmltb,                                   "XML Table structure used for retreive and output XML doc
      lt_return     TYPE STANDARD TABLE OF bapiret2,                   "Return Parameter
      ls_return     TYPE bapiret2,                                     "Return Parameter
      lv_type       TYPE bapi_mtype,                                   "Message type: S Success, E Error, W Warning, I Info, A Abort
      lv_message    TYPE bapi_msg,                                     "Message Text
      lv_tabix      TYPE sytabix.                                      "Row Index of Internal Tables
*field symbols declaration
FIELD-SYMBOLS: <lfs_formrsp> TYPE /odsmfe/tb_forsp,
               <it_table>    TYPE STANDARD TABLE,
               <is_table>    TYPE any,
               <dyn_str>     TYPE any,
               <str_comp>    TYPE abap_compdescr.

DATA: lo_func       TYPE REF TO cl_salv_functions,                     "Generische und selbstdefinierte Funktionen
      lo_functions  TYPE REF TO cl_salv_functions_list,                "Generische und selbstdef. Funktionen in listähnl. Tabellen
      lo_selections TYPE REF TO cl_salv_selections,                    "Markierungen in listähnlichen Ausgabetabellen
      lo_events     TYPE REF TO cl_salv_events_table.                  "Ereignisse in einfachen, zweidimensionalen Tabellen
DATA: lv_formid     TYPE char20 VALUE 'INSTANCEID'.

*selection screen declaration for table input
PARAMETERS :    p_forms TYPE /odsmfe/tb_forsp-formid." OBLIGATORY.
SELECT-OPTIONS: p_vrsn  FOR  ls_formrsp-version NO INTERVALS NO-EXTENSION.
PARAMETERS:     p_user  LIKE usr02-bname.
SELECT-OPTIONS: p_date FOR sy-datum NO-EXTENSION.

TYPES : BEGIN OF ty_version,
          formid  TYPE /odsmfe/tb_fomst-formid,                         "ODS Form ID
          version TYPE /odsmfe/de_version,                              "ODS Version
        END OF ty_version.

DATA :lt_version TYPE TABLE OF ty_version,
      ls_version TYPE ty_version.

DATA: lv_date          TYPE sy-datlo,                                  "ABAP System Field: Local Date of Current User
      lv_date1         TYPE sy-datlo,                                  "ABAP System Field: Local Date of Current User
      lv_data_exist(1) TYPE c VALUE ' '.

DATA: otable     TYPE REF TO cl_abap_tabledescr.                       "Run Time Type Services
DATA: ostruct    TYPE REF TO cl_abap_structdescr.                      "Run Time Type Services
DATA: lv_row     TYPE int4.
DATA: wa_form  TYPE abap_componentdescr,
      wa_table TYPE abap_compdescr.
DATA: ref_final  TYPE REF TO data.
DATA: ref_final1 TYPE REF TO data.

DATA :lo_cols   TYPE REF TO cl_salv_columns,
      lo_column TYPE REF TO cl_salv_column.

FIELD-SYMBOLS: <lt_form>       TYPE INDEX TABLE,
               <ls_form>       TYPE any,
               <ls_version>    TYPE any,
               <ls_wo>         TYPE any,
               <ls_formid>     TYPE any,
               <ls_form1>      TYPE any,
               <ls_instanceid> TYPE any,
               <lt_out>        TYPE STANDARD TABLE,
               <lt_out1>       TYPE STANDARD TABLE,
               <ls_field>      TYPE any.

CONSTANTS: lc_display TYPE string VALUE 'Display'.

CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.                                                              "lcl_handle_events DEFINITION

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
*class lcl_handle_events implementation.
    PERFORM handle_user_command USING e_salv_function.
  ENDMETHOD.
ENDCLASS.

DATA: gr_events TYPE REF TO lcl_handle_events.

*initialization event
INITIALIZATION.

*start of selection event
START-OF-SELECTION.

  "/P_user can be select option with no extentoin no range
  IF p_user IS NOT INITIAL.
    ls_filter_val-sign   = 'I'.
    ls_filter_val-option = 'EQ'.
    ls_filter_val-low    = p_user.
    APPEND ls_filter_val TO lt_filter_val.
  ENDIF.

  CLEAR lv_data_exist.
* Read data from the table selected.
  SELECT * FROM /odsmfe/tb_forsp
    INTO CORRESPONDING FIELDS OF TABLE lt_formrsp
    WHERE formid = p_forms
    AND version IN p_vrsn
    AND created_by IN lt_filter_val
    AND created_date IN p_date.

  CHECK sy-subrc IS INITIAL.

  SELECT  aufnr ,
          equnr,
          iloan,
          qmnum
     FROM afih
     INTO TABLE @DATA(lt_afih)
     FOR ALL ENTRIES IN @lt_formrsp
     WHERE aufnr = @lt_formrsp-wo_num.
  IF sy-subrc = 0.
    SELECT iloan,
           tplnr
       FROM iloa
       INTO TABLE @DATA(lt_iloa)
       FOR ALL ENTRIES IN @lt_afih
       WHERE iloan = @lt_afih-iloan.
  ENDIF.

  LOOP AT lt_formrsp INTO ls_formrsp.

* Converting timestamp into Date and Time
    CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
      EXPORTING
        iv_timestamp     = ls_formrsp-created_on
      IMPORTING
        o_date           = lv_date
      EXCEPTIONS
        conversion_error = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
*    implement suitable error handling here.
    ENDIF.

    IF lv_date IN p_date.

      lv_data_exist = 'X' .
* XML Transformation
      lv_xml_string = ls_formrsp-responsedata.
*  lv_xml_string = ls_formmst-formdata.
      CREATE OBJECT lref_xml.
* Convert XString to Binary
      TRY.
          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer     = lv_xml_string
            TABLES
              binary_tab = lt_data.
        CATCH cx_root INTO DATA(lo_oref1).                                                                   "##CATCH_ALL
          cl_demo_output=>display( lo_oref1->get_text( ) ).
      ENDTRY.
* Parse data
      TRY.
          CALL METHOD lref_xml->create_with_table
            EXPORTING
              table   = lt_data
*             SIZE    = 0
            RECEIVING
              retcode = lt_retcode.
        CATCH cx_root INTO lo_oref1.                                                                         "##CATCH_ALL
          cl_demo_output=>display( lo_oref1->get_text( ) ).
      ENDTRY.
* RENDER_2_XSTRING
      TRY.
          CALL METHOD lref_xml->render_2_xstring
            IMPORTING
              retcode = lv_subrc
              stream  = lv_xml_string
              size    = lv_size.
        CATCH cx_root INTO lo_oref1.                                                                         "##CATCH_ALL
          cl_demo_output=>display( lo_oref1->get_text( ) ).
      ENDTRY.
*
* Convert XML to internal table
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = lv_xml_string
        TABLES
          xml_table = lt_xml_data
          return    = lt_return.
*
*  "Delete the ignorables
      DELETE lt_xml_data WHERE cname+0(7) EQ '_IGNORE'.
* Delete Data from XML based on the below conditions
      DELETE lt_xml_data WHERE cname = 'instanceID'.
      DELETE lt_xml_data WHERE cname = 'start'(t05).
      DELETE lt_xml_data WHERE cname = 'end'.
      DELETE lt_xml_data WHERE cname = 'type'.
      DELETE lt_xml_data WHERE cname = 'meta'.
*******************************************************************

      IF <lt_form> IS NOT ASSIGNED.

* Build the field catalog and populate Cvalue from LT_XML_DATA
* Cname Represents Questions and cvalue represents Ans in LT_XML_DATA
        LOOP AT lt_xml_data INTO ls_xml_data WHERE type = 'V'.
*   Build ALV Fields
          ls_lvc_field-fieldname = ls_xml_data-cname.
          APPEND ls_lvc_field TO lt_lvc_field.
          CLEAR ls_lvc_field.
        ENDLOOP.

        otable ?= cl_abap_tabledescr=>describe_by_data( p_data = lt_form ).

        ostruct ?= otable->get_table_line_type( ).

        DATA(p_form) = ostruct->get_components( ).
        DATA(p_form1) = ostruct->get_components( ).
        CLEAR p_form1.
        LOOP AT lt_lvc_field INTO ls_lvc_field.
          wa_form-name = ls_lvc_field-fieldname.
          TRANSLATE wa_form-name TO LOWER CASE. " add by rakesh
          wa_form-type = cl_abap_elemdescr=>get_c( p_length = 255 ).
          APPEND wa_form TO p_form1.
          CLEAR wa_form.
        ENDLOOP.

        " add by rakesh
        SORT p_form1 BY name.
        DELETE ADJACENT DUPLICATES FROM p_form1 COMPARING name.
        APPEND LINES OF p_form1 TO p_form.
        " end by rakesh

        DATA(ostruct1) = cl_abap_structdescr=>create( p_form ).

        DATA(otable1) = cl_abap_tabledescr=>create( ostruct1 ).

        CREATE DATA ref_final TYPE HANDLE otable1.

        ASSIGN ref_final->* TO <lt_form>.

        LOOP AT lt_formrsp ASSIGNING <lfs_formrsp>.
* converting timestamp into date and time
          CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
            EXPORTING
              iv_timestamp     = <lfs_formrsp>-created_on
            IMPORTING
              o_date           = lv_date1
            EXCEPTIONS
              conversion_error = 1
              OTHERS           = 2.
          IF sy-subrc <> 0.
*    implement suitable error handling here.
          ENDIF.
          <lfs_formrsp>-created_date = lv_date1.
          READ TABLE lt_afih INTO DATA(ls_afih) WITH KEY aufnr = <lfs_formrsp>-wo_num.
          IF sy-subrc = 0.
            <lfs_formrsp>-equnr = ls_afih-equnr.
            READ TABLE lt_iloa INTO DATA(ls_iloa) WITH KEY iloan = ls_afih-iloan.
            IF sy-subrc = 0.
              <lfs_formrsp>-tplnr = ls_iloa-tplnr.
            ENDIF.
          ENDIF.
        ENDLOOP.

        MOVE-CORRESPONDING lt_formrsp TO <lt_form>.

      ENDIF.

      READ TABLE <lt_form> ASSIGNING <ls_form> WITH KEY (lv_formid) = ls_formrsp-instanceid.

      CHECK <ls_form> IS ASSIGNED.

      LOOP AT lt_lvc_field INTO ls_lvc_field.

        CLEAR ls_xml_data.

        READ TABLE lt_xml_data INTO ls_xml_data WITH KEY cname = ls_lvc_field-fieldname.

        CHECK  sy-subrc IS INITIAL.

        ASSIGN COMPONENT ls_lvc_field-fieldname OF STRUCTURE <ls_form> TO <ls_field>.

        CHECK <ls_field> IS ASSIGNED.

        <ls_field> = ls_xml_data-cvalue.

      ENDLOOP.

      CLEAR lt_xml_data.
    ENDIF.

  ENDLOOP.

  DATA  final_table        TYPE REF TO cl_salv_table.                "Basisklasse für einfache Tabellen
  DATA: lo_header          TYPE REF TO cl_salv_form_layout_grid.     "Grid-Element im Gestaltungsobjekt
  DATA: label_l            TYPE scrtext_l.                           "Long Field Label
  DATA: lo_cols_instanceid TYPE REF TO cl_salv_column_table,         "Spaltenbeschreibung einfacher, zweidimensionaler Tabellen
        lo_cols_formid     TYPE REF TO cl_salv_column_table,         "Spaltenbeschreibung einfacher, zweidimensionaler Tabellen
        lo_cols_version    TYPE REF TO cl_salv_column_table,         "Spaltenbeschreibung einfacher, zweidimensionaler Tabellen
        ls_color           TYPE lvc_s_colo.                          "ALV control: Color coding

  IF lv_data_exist IS NOT INITIAL.
*  Call Factory Method to generate ALV output
    TRY.
        cl_salv_table=>factory(
          IMPORTING
             r_salv_table   = final_table
          CHANGING
            t_table        = <lt_form> ).
      CATCH cx_salv_msg .
    ENDTRY.

* Set ALV functions
    lo_selections = final_table->get_selections( ).
    lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
    lo_func  = final_table->get_functions( ).
    lo_func->set_all( abap_true ).
    lo_events = final_table->get_event( ).
    CREATE OBJECT gr_events.
    SET HANDLER gr_events->on_user_command FOR lo_events.
* Get columns object
    lo_cols = final_table->get_columns( ).
    lo_cols->set_optimize( abap_true ).
    lo_cols_instanceid ?= lo_cols->get_column( 'INSTANCEID' ).
    lo_cols_formid ?= lo_cols->get_column( 'FORMID' ).
    lo_cols_version ?= lo_cols->get_column( 'VERSION' ).
    ls_color-col = 3.
    ls_color-int = 1.

    CALL METHOD lo_cols_instanceid->set_color
      EXPORTING
        value = ls_color.

    CALL METHOD lo_cols_formid->set_color
      EXPORTING
        value = ls_color.

    CALL METHOD lo_cols_version->set_color
      EXPORTING
        value = ls_color.

    LOOP AT ostruct1->components INTO wa_table.
      TRY.
          lo_column  = lo_cols->get_column( wa_table-name ).
          IF wa_table-name = 'INSTANCEID'.
            wa_table-name = 'Instance ID'.
          ENDIF.
          IF wa_table-name = 'FORMID '.
            wa_table-name = 'Form ID'.
          ENDIF.
          IF wa_table-name = 'VERSION'.
            wa_table-name = 'Version'.
          ENDIF.
          IF wa_table-name = 'WO_NUM'.
            wa_table-name = 'Work Order Num.'.
          ENDIF.
          IF wa_table-name = 'VORNR'.
            wa_table-name = 'OperationNum.'.
          ENDIF.
          IF wa_table-name = 'EQUNR'.
            wa_table-name = 'Equipment No.'.
          ENDIF.
          IF wa_table-name = 'TPLNR'.
            wa_table-name = 'FunctionalLoc.'.
          ENDIF.
          IF wa_table-name = 'CREATED_DATE'.
            wa_table-name = 'Created Date'.
          ENDIF.
          IF wa_table-name = 'CREATED_BY'.
            wa_table-name = 'Created By'.
          ENDIF.
          label_l = wa_table-name.
          lo_column->set_long_text( label_l ).
          CLEAR wa_table.
        CATCH cx_salv_not_found.                        "#EC NO_HANDLER
      ENDTRY .
    ENDLOOP.
    TRY.
        final_table->set_screen_status(
           pfstatus      =  'STANDARD_FULLSCREEN'
           report        =  sy-repid
           set_functions = final_table->c_functions_all ).
      CATCH cx_salv_msg.
    ENDTRY.

* Filters

    DATA: lo_filters TYPE REF TO cl_salv_filters.

    lo_filters = final_table->get_filters( ).

* Displaying the ALV
    final_table->display( ).
  ELSE.
    MESSAGE 'Data Does Not exist for selection' TYPE 'E'.
  ENDIF.

*****************************************************************************************
FORM handle_user_command USING i_ucomm TYPE salv_de_function.
  DATA: lit_rows        TYPE salv_t_row,
        lst_rows        TYPE int4,                                     "Natural Number
        e_salv_function TYPE salv_de_function.                         "ALV Function

* constants
  CONSTANTS: lc_forminstanceid TYPE string VALUE 'ForminstanceId',
             lc_browser        TYPE string VALUE 'chrome.exe',
             lc_instanceid     TYPE string VALUE 'INSTANCEID',
             lc_formid         TYPE string VALUE 'FORMID',
             lc_version        TYPE string VALUE 'VERSION',
             lc_wo_num         TYPE string VALUE 'WO_NUM',
             lc_vornr          TYPE string VALUE 'VORNR',
             lc_equnr          TYPE string VALUE 'EQUNR',
             lc_tplnr          TYPE string VALUE 'TPLNR',
             lc_created_date   TYPE string VALUE 'CREATED_DATE',
             lc_created_by     TYPE string VALUE 'CREATED_BY'.

  DATA: l_tabledescr_ref TYPE REF TO cl_abap_tabledescr,
        l_descr_ref      TYPE REF TO cl_abap_structdescr,
        wa_table         TYPE abap_compdescr,
        lt_return        TYPE bapiret2,
        lt_return2       TYPE TABLE OF bapiret2,
        lv_wo            TYPE aufnr,
        lt_char          TYPE /odsmfe/eq_char_tt,
        ls_char          TYPE /odsmfe/eq_char.
*
  TYPES:rng_typ TYPE RANGE OF sfield.
  DATA: lt_fields TYPE rng_typ,
        wa_fields TYPE LINE OF rng_typ.



  CASE i_ucomm.
    WHEN '&DISPLAY'.
* Get the selected rows.
      lit_rows = lo_selections->get_selected_rows( ).
      LOOP AT lit_rows INTO lst_rows.
        READ TABLE <lt_form> ASSIGNING <ls_form> INDEX lst_rows.
* Form ID Value
        ASSIGN COMPONENT 'FORMID' OF STRUCTURE <ls_form> TO <ls_form1>.
* Form Instance ID
        ASSIGN COMPONENT 'INSTANCEID' OF STRUCTURE <ls_form> TO <ls_instanceid>.
        IF sy-subrc = 0 AND <ls_form> IS NOT INITIAL.
          CALL FUNCTION '/ODSMFE/FM_FORMS_CALL_URL'
            EXPORTING
              im_mode       = lc_forminstanceid
              im_formid     = <ls_form1>
              im_instanceid = <ls_instanceid>
              im_browser    = lc_browser
              im_function   = 'DISPLAY'.

          cl_gui_cfw=>dispatch( ).
          cl_gui_cfw=>flush( ).
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.

    WHEN '&GRA'.

      lo_selections = final_table->get_selections( ).

      DATA(lt_column) = lo_selections->get_selected_columns( ).

      IF lines( lt_column ) > 1.
*    **********************************************************
        MESSAGE  'Support For More Than One Column Coming Soon, Please Select one Column For For Now.' TYPE 'I'.
*    **********************************************************
        EXIT.
      ENDIF.

      CLEAR: lt_fieldcatalog,
             lt_outtab,
             lt_columns,
             lt_rows.

      READ TABLE lt_column ASSIGNING FIELD-SYMBOL(<lfs_col>) INDEX 1.

      DATA: lv_fval   TYPE char255,                                    "Char255
            ls_out    TYPE lty_out,
            lv_colpos TYPE n,
            lt_sort   TYPE abap_sortorder_tab.

      CLEAR: lt_sort, lv_colpos.

      CLEAR p_form.

      LOOP AT lt_column ASSIGNING <lfs_col>.

        APPEND INITIAL LINE TO lt_sort ASSIGNING FIELD-SYMBOL(<ls_sort>).

        <ls_sort>-name  = <lfs_col>.

        wa_form-name = <lfs_col>.
        wa_form-type = cl_abap_elemdescr=>get_c( p_length = 255 ).
        APPEND wa_form TO p_form.
        CLEAR wa_form.

        ls_fieldcatalog-col_pos   = lv_colpos + 1 .
        ls_fieldcatalog-fieldname = <lfs_col> .
        ls_fieldcatalog-seltext   = <lfs_col> .
        ls_fieldcatalog-inttype   = 'C' .
        ls_fieldcatalog-key       = 'X' .
        ls_columns                = <lfs_col>.

        IF lv_xtitle IS INITIAL.

          lv_xtitle = <lfs_col>.

        ELSE.
          lv_xtitle = lv_xtitle &&'/' && <lfs_col>.

        ENDIF.
        APPEND ls_columns TO lt_columns.
        APPEND ls_fieldcatalog TO lt_fieldcatalog.

      ENDLOOP.

      SORT <lt_form>[] BY (lt_sort) .

      ls_fieldcatalog-col_pos   = lv_colpos + 1 .
      ls_fieldcatalog-fieldname = 'COL2' .
      ls_fieldcatalog-seltext   = 'COL2' .
      ls_fieldcatalog-inttype   = 'I' .

      ls_columns                = 'COL2'.

      APPEND ls_columns TO lt_columns.
      APPEND ls_fieldcatalog TO lt_fieldcatalog.


      wa_form-name = 'COL2'.
      wa_form-type = cl_abap_elemdescr=>get_i( ).                      "
      APPEND wa_form TO p_form.
      CLEAR wa_form.

      ostruct1 = cl_abap_structdescr=>create( p_form ).

      otable1 = cl_abap_tabledescr=>create( ostruct1 ).

      CREATE DATA ref_final TYPE HANDLE otable1.
      CREATE DATA ref_final1 TYPE HANDLE otable1.

      ASSIGN ref_final->* TO <lt_out>.
      ASSIGN ref_final1->* TO <lt_out1>.

* New Logic for Trial
      MOVE-CORRESPONDING <lt_form> TO <lt_out1>.

      SORT <lt_out1>[] BY (lt_sort) .

      LOOP AT <lt_out1> ASSIGNING FIELD-SYMBOL(<ls_out1>).

        ASSIGN COMPONENT 'COL2' OF STRUCTURE <ls_out1> TO <ls_field>.

        IF   <ls_field> IS ASSIGNED.

          <ls_field> = 1.
        ENDIF.

        COLLECT <ls_out1>  INTO <lt_out>.

      ENDLOOP.

      CLEAR ls_rows.
      CLEAR lt_rows.

      LOOP AT <lt_out> ASSIGNING FIELD-SYMBOL(<ls_out>).
        ls_rows-row_id = ls_rows-row_id + 1.

        APPEND ls_rows TO lt_rows.

      ENDLOOP.

      CREATE OBJECT lo_graph
        EXPORTING
*         i_parent               = lr_parent
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

    WHEN '&UPDCHAR'.
*
      l_tabledescr_ref ?= cl_abap_typedescr=>describe_by_data( <lt_form> ).
      l_descr_ref ?= l_tabledescr_ref->get_table_line_type( ).
* Get the selected rows.
      lit_rows = lo_selections->get_selected_rows( ).
      LOOP AT lit_rows INTO lst_rows.
        READ TABLE <lt_form> ASSIGNING <ls_form> INDEX lst_rows.
*
        ASSIGN COMPONENT lc_formid OF STRUCTURE <ls_form> TO <ls_formid>.
        ASSIGN COMPONENT lc_version OF STRUCTURE <ls_form> TO <ls_version>.
        ASSIGN COMPONENT lc_wo_num OF STRUCTURE <ls_form> TO <ls_wo>.
        lv_wo = <ls_wo>.

        SELECT * FROM /odsmfe/tb_focha INTO TABLE @DATA(lt_char1)
                 WHERE formid EQ @<ls_formid>
                 AND version EQ @<ls_version>.
*
        LOOP AT l_descr_ref->components INTO wa_table .
          IF wa_table-name EQ lc_instanceid OR wa_table-name EQ lc_formid OR wa_table-name EQ lc_version OR
             wa_table-name EQ lc_wo_num OR wa_table-name EQ lc_vornr OR wa_table-name EQ lc_equnr OR
             wa_table-name EQ lc_tplnr OR wa_table-name EQ lc_created_date OR wa_table-name EQ lc_created_by .

          ELSE.
* Form ID Value
            ASSIGN COMPONENT wa_table-name OF STRUCTURE <ls_form> TO <ls_form1>.

            LOOP AT lt_char1 INTO DATA(ls_char1) WHERE field = wa_table-name.
              MOVE wa_table-name TO ls_char-name.
              MOVE <ls_form1>  TO ls_char-value.
              MOVE-CORRESPONDING ls_char1 TO ls_char.
              APPEND ls_char TO lt_char.
              CLEAR : ls_char1.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
*
*--Method to update characterstics
        IF lt_char IS NOT INITIAL.
          DATA: lo_characteristics TYPE REF TO /odsmfe/cl_characteristics.
          CREATE OBJECT lo_characteristics.
          CALL METHOD lo_characteristics->equipment_char_update
            EXPORTING
              im_workorder = lv_wo
              im_char      = lt_char
            IMPORTING
              ex_return    = lt_return.

*
          MESSAGE  'Characteristics Data Updated' TYPE 'I'.
        ELSE.
*    **********************************************************
          MESSAGE  'No Characteristics Data Found' TYPE 'I'.
*    **********************************************************
        ENDIF.
        CLEAR: lt_return, lt_char , lv_wo.
*
      ENDLOOP.
  ENDCASE.
ENDFORM.
*********************************************************************

INCLUDE /odsmfe/inc_forms_status_300.
