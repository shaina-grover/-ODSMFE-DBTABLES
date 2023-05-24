**----------------------------------------------------------------------*
****INCLUDE /ODSMFE/RPT_ANALYTICS_EXCELF01.
**----------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**&      Form  /ODSMFE/FO_EXPORT_DATA
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM /odsmfe/fo_export_data .
*
*  SELECT * FROM /odsmfe/tb_forsp
*    INTO CORRESPONDING FIELDS OF TABLE git_formrsp
*    WHERE formid       IN s_forms
*      AND version      IN s_vrsn
*      AND created_by   IN s_user
*      AND created_date IN s_date.                       "#EC CI_NOFIELD
*
*  CHECK sy-subrc IS INITIAL.
*
*  SELECT  aufnr, equnr, iloan, qmnum
* FROM afih
* INTO TABLE @git_afih
* FOR ALL ENTRIES IN @git_formrsp
* WHERE aufnr = @git_formrsp-wo_num.
*
*
*  IF sy-subrc = 0.
*    SELECT iloan,tplnr
*     FROM iloa
*     INTO TABLE @git_iloa
*     FOR ALL ENTRIES IN @git_afih
*     WHERE iloan = @git_afih-iloan.
*  ENDIF.
*
*  CREATE OBJECT gv_a.
*  SELECT * FROM /odsmfe/tb_fomst
*    INTO TABLE git_fomst
*    WHERE formid  IN s_forms
*      AND version IN s_vrsn.
*
*  LOOP AT git_fomst INTO gst_fomst.
*    TRY.
*        gv_a->gmib_get_all_questions(
*      EXPORTING
*        im_formid    =  gst_fomst-formid   " ODS Form ID
*        im_version   =  gst_fomst-version  " ODS Version
*      IMPORTING
*        ex_form_data = git_question
*    ).
*      CATCH cx_sy_ref_is_initial.
*    ENDTRY.
*  ENDLOOP.
*
*  LOOP AT git_question INTO gst_question.
*    TRANSLATE gst_question-question_id TO LOWER CASE.
*    git_question_grp[] = git_question[].
*    DELETE git_question_grp WHERE group_id IS INITIAL.
*    READ TABLE git_question_grp INTO gst_question_grp WITH KEY group_id = gst_question-question_id.
*    IF sy-subrc = 0.
*      CONTINUE.
*    ELSE.
*      MOVE-CORRESPONDING gst_question TO gst_question1.
*    ENDIF.
*    APPEND gst_question1 TO git_question1.
*    CLEAR gst_question1.
*  ENDLOOP.
*
*  CLEAR : gst_fieldcat , git_fieldcat[].
*
*  gst_fieldcat-fieldname = 'QUESTION'.
*  gst_fieldcat-tabname   = 'git_question'.
*  gst_fieldcat-seltext_m = 'Question'.                ##NO_TEXT .
*  gst_fieldcat-outputlen = '50'.
*  APPEND gst_fieldcat TO git_fieldcat.
*  CLEAR gst_fieldcat.
*
*  gst_fieldcat-fieldname = 'GROUP_NAME'.
*  gst_fieldcat-tabname   = 'git_question'.
*  gst_fieldcat-seltext_m = 'Group'.                     ##NO_TEXT
*  gst_fieldcat-outputlen = '50'.
*  APPEND gst_fieldcat TO git_fieldcat.
*  CLEAR gst_fieldcat.
*
*  gst_fieldcat-fieldname = 'PRE_GROUP_NAME'.
*  gst_fieldcat-tabname   = 'git_question'.
*  gst_fieldcat-seltext_m = 'Parent Group'.                    ##NO_TEXT
*  gst_fieldcat-outputlen = '50'.
*  APPEND gst_fieldcat TO git_fieldcat.
*  CLEAR gst_fieldcat.
*
*  gv_program = sy-repid.
*
*  IF git_question IS NOT INITIAL.
*
*    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
*      EXPORTING
*        i_title              = 'Please Select Any' ##NO_TEXT
*        i_tabname            = 'git_question'
*        i_structure_name     = 'git_question'
*        i_checkbox_fieldname = 'CHECKBOX'
*        it_fieldcat          = git_fieldcat
*        i_callback_program   = gv_program
*      TABLES
*        t_outtab             = git_question1.
*
*    DELETE git_question1 WHERE checkbox NE 'X'.
*  ENDIF.
*
*  "/Get the questions count
*  DESCRIBE TABLE git_question1 LINES gv_no.
*  CONDENSE gv_no.
*  IF gv_no > 1000.
*    gv_msg1 = 'The maximum possible size of the structure is exceeded'.
*    gv_msg2 = '. Please select less than 1000 questions'.
*    CONCATENATE gv_msg1 ' (' gv_no ')' gv_msg2 INTO DATA(lv_message).
*    MESSAGE lv_message TYPE 'E'.
*    EXIT.
*  ELSE. "/ IF gv_no > 1000.
*    LOOP AT git_question1 INTO gst_question1.
*      gv_index1 = sy-tabix.
*      gst_lvc_field-fieldname = gst_question1-question_index.
*      APPEND gst_lvc_field TO git_lvc_field.
*      CLEAR : gst_lvc_field, gv_index1.
*    ENDLOOP.
*  ENDIF. "/ IF gv_no > 1000.
*
*  "/ Assign the Delimiter to field  symbol.
*  ASSIGN gv_deli TO <gfsst> TYPE 'X'.
*  gtys_hex-l_tab = gc_c09.
*  <gfsst> = gtys_hex-l_tab.
*
*  git_question2[] = git_question1[].
*
*  DELETE git_question2 WHERE repeat_group_name IS INITIAL AND group_name IS INITIAL.
*  SORT git_question2 BY group_name.
*  DELETE ADJACENT DUPLICATES FROM git_question2 COMPARING repeat_group_name." group_name.
*  DELETE ADJACENT DUPLICATES FROM git_question2 COMPARING group_name.
*  DESCRIBE TABLE git_question2 LINES gv_lines.
*  gv_lines = gv_lines + 1.
*
*  CREATE OBJECT gst_excel 'EXCEL.APPLICATION' .
*  SET PROPERTY OF gst_excel 'Visible' = 1 .
*  GET PROPERTY OF gst_excel 'Workbooks' = gst_wbooklist .
*  GET PROPERTY OF gst_wbooklist 'Application' = gst_application .
*  CALL METHOD OF gst_wbooklist 'Add' = gst_wbook .
*
*  IF git_question1 IS NOT INITIAL.
*    DO gv_lines TIMES .
*      IF sy-index = gv_lines.
*
*        LOOP AT git_formrsp ASSIGNING <gfsst_formrsp>.
*          "/ converting timestamp into date and time
*          CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'    "#EC CI_SUBRC.
*            EXPORTING
*              iv_timestamp     = <gfsst_formrsp>-created_on
*            IMPORTING
*              o_date           = gv_date1
*            EXCEPTIONS
*              conversion_error = 1
*              OTHERS           = 2.
*
*          <gfsst_formrsp>-created_date = gv_date1.
*          READ TABLE git_afih INTO gst_afih WITH KEY aufnr = <gfsst_formrsp>-wo_num.      ##NO_TEXT .
*          IF sy-subrc = 0.
*            <gfsst_formrsp>-equnr = gst_afih-equnr.
*            READ TABLE git_iloa INTO gst_iloa WITH KEY iloan = gst_afih-iloan.
*            IF sy-subrc = 0.
*              <gfsst_formrsp>-tplnr = gst_iloa-tplnr.
*            ENDIF.
*          ENDIF.
*        ENDLOOP. "/ LOOP AT git_formrsp ASSIGNING <gfsst_formrsp>.
*
*        LOOP AT git_formrsp INTO gst_formrsp.
*          "/ Converting timestamp into Date and Time                              ##NO_TEXT .
*          CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'              "#EC CI_SUBRC.
*            EXPORTING
*              iv_timestamp     = gst_formrsp-created_on
*            IMPORTING
*              o_date           = gv_date
*            EXCEPTIONS
*              conversion_error = 1
*              OTHERS           = 2.
*
*          IF gv_date IN s_date.
*            TRY.
*                gv_a->gmib_get_response_data(
*                  EXPORTING
*                    im_formid        = gst_formrsp-formid     " ODS Form ID
*                    im_version       = gst_formrsp-version    " ODS Version
*                    im_instanceid    = gst_formrsp-instanceid " ODS MFE InstanceId
*                  IMPORTING
*                    ex_response_data = git_responce
*                ).
*              CATCH cx_sy_ref_is_initial.
*            ENDTRY.
*
*            IF <gfsit_form> IS NOT ASSIGNED.
*              go_table ?= cl_abap_tabledescr=>describe_by_data( p_data = git_form ).
*              go_struct ?= go_table->get_table_line_type( ).
*
**              DATA p_form TYPE component_table.
*              DATA(p_form) = go_struct->get_components( ).
*
*              LOOP AT git_lvc_field INTO gst_lvc_field.  "#EC CI_SUBRC.
*                gst_componentdescr-name = gst_lvc_field-fieldname.
*                gst_componentdescr-type = cl_abap_elemdescr=>get_c( p_length = 255 ).
*                APPEND gst_componentdescr TO p_form.
*                CLEAR gst_componentdescr.
*              ENDLOOP. "/ LOOP AT git_lvc_field INTO gst_lvc_field.
*
*              go_struct1 = cl_abap_structdescr=>create(
*              p_components  = p_form
*              p_strict      = '*' ).
*
*              go_table1 = cl_abap_tabledescr=>create( go_struct1 ).
*              CREATE DATA go_ref_final TYPE HANDLE go_table1.
*              ASSIGN go_ref_final->* TO <gfsit_form>.
*
*              MOVE-CORRESPONDING git_formrsp TO <gfsit_form>.
*            ENDIF. "/ IF <gfsit_form> IS NOT ASSIGNED.
*
*            gv_created_on = gst_formrsp-created_on.
*
*            CONCATENATE gst_formrsp-instanceid gv_deli gst_formrsp-formid gv_deli gst_formrsp-version gv_deli   INTO gv_conc1.
*            CONCATENATE gv_conc1 gst_formrsp-wo_num gv_deli gst_formrsp-vornr gv_deli gst_formrsp-equnr gv_deli INTO gv_conc2.
*            CONCATENATE gv_conc2 gst_formrsp-tplnr gv_deli gv_created_on gv_deli gst_formrsp-created_by INTO gv_conc.
*
*            READ TABLE <gfsit_form> ASSIGNING <gfsst_form> WITH KEY (gv_formid) = gst_formrsp-instanceid.
*            CHECK <gfsst_form> IS ASSIGNED.
*            LOOP AT git_question1 INTO gst_question1.    "#EC CI_SUBRC.
*              CLEAR: gst_responce.
*              READ TABLE git_responce INTO gst_responce WITH KEY question_id = gst_question1-question_id.
*              CHECK  sy-subrc IS INITIAL.
*              ASSIGN COMPONENT gst_question1-question_index OF STRUCTURE <gfsst_form> TO <gfsst_field>.
*              CHECK <gfsst_field> IS ASSIGNED.
*              <gfsst_field> = gst_responce-responsedata.
*              IF gst_question1-group_id IS INITIAL.
*                CONCATENATE gv_conc gv_deli <gfsst_field> INTO gv_conc.
*              ENDIF. "/ IF gst_question1-group_id IS INITIAL.
*            ENDLOOP. "/ LOOP AT git_question1 INTO gst_question1.
*            gst_conc = gv_conc.
*            APPEND gst_conc TO git_conc.
*            CLEAR: gst_conc, gv_conc, git_responce.
*          ENDIF. "/ IF gv_date IN s_date.
*        ENDLOOP. "/ LOOP AT git_formrsp INTO gst_formrsp.
*
*        go_itab ?= cl_abap_typedescr=>describe_by_data( <gfsit_form> ).
*        go_line = go_itab->get_table_line_type( ).
*
*        "/ Appending column names to internal table.
*        IF go_line->kind = cl_abap_typedescr=>kind_struct.
*          go_struct ?= go_line.
*          LOOP AT go_struct->components[] ASSIGNING <gfsst_comp>.
*            gv_fieldname = VALUE gtys_types( colname = <gfsst_comp>-name ).
*            APPEND gv_fieldname TO git_colnames.
*            CLEAR gv_fieldname.
*          ENDLOOP. "/ LOOP AT go_struct->components[] ASSIGNING <gfsst_comp>.
*
*          CONCATENATE 'Instance ID' gv_deli 'Form ID' gv_deli 'Version' gv_deli 'Work Order' gv_deli 'Activity/Operation' gv_deli 'Equipment' gv_deli 'Functional Location' gv_deli 'Created Date' gv_deli 'Created By' INTO gv_con.    ##NO_TEXT .
*
**          FIELD-SYMBOLS : <lfsst_colnames> TYPE.
**           LOOP AT git_colnames ASSIGNING <lfsst_colnames>.
*          LOOP AT git_colnames ASSIGNING FIELD-SYMBOL(<lfsst_colnames>).
*            TRANSLATE <lfsst_colnames>-colname TO LOWER CASE.
*            READ TABLE git_question1 INTO gst_question1 WITH KEY question_index = <lfsst_colnames>-colname.
*            IF sy-subrc = 0.
*              IF gst_question1-group_id IS NOT INITIAL.
*                CONTINUE.
*              ELSE. "/ IF gst_question1-group_id IS NOT INITIAL.
*                CONCATENATE gv_con gv_deli gst_question1-question INTO gv_con.
*              ENDIF. "/ IF gst_question1-group_id IS NOT INITIAL.
*            ENDIF. "/ IF sy-subrc = 0.
*            CLEAR gst_question1.
*          ENDLOOP. "/ LOOP AT git_colnames ASSIGNING FIELD-SYMBOL(<lfsst_colnames>).
*
*          gst_concatenate = gv_con.
*          APPEND gst_concatenate TO git_concatenate.
*          CLEAR gst_concatenate.
*
*          LOOP AT git_conc INTO gst_conc.
*            APPEND gst_conc TO git_concatenate.
*            CLEAR gst_conc.
*          ENDLOOP. "/ LOOP AT git_conc INTO gst_conc.
*        ENDIF. "/ IF go_line->kind = cl_abap_typedescr=>kind_struct.
*
**--Forming sheet name
*        MOVE: 'Main' TO gv_sheet_name .                                           ##NO_TEXT .
*        CALL METHOD OF gst_sheets 'Add' = gst_newsheet.
*        SET PROPERTY OF gst_newsheet 'Name' = gv_sheet_name.
*
*        GET PROPERTY OF gst_application 'ActiveSheet' = gst_activesheet .
*        SET PROPERTY OF gst_activesheet 'Name' = gv_sheet_name .
*
*        gv_line_cntr = 1 . "line counter
*
*        "/ DATA download into excel first sheet
*        CALL METHOD cl_gui_frontend_services=>clipboard_export          "#EC CI_SUBRC.
*          IMPORTING
*            data         = git_concatenate[]
*          CHANGING
*            rc           = gv_rc
*          EXCEPTIONS
*            cntl_error   = 1
*            error_no_gui = 2
*            OTHERS       = 4.
*        CALL METHOD OF gst_activesheet 'Paste'.
*
*        CALL METHOD OF gst_excel 'Columns' = gst_column1.
*        CALL METHOD OF gst_column1 'Autofit'.
*        CALL METHOD OF gst_excel 'Rows' = gst_row1
*          EXPORTING #1 = 1.
*
*        GET PROPERTY OF gst_row1 'Font' = gst_font.
*        SET PROPERTY OF gst_font 'Bold' = 1.
*        SET PROPERTY OF gst_font 'ColorIndex' = 32.
*
*        CALL METHOD OF gst_row1 'Borders' = gst_border.
*        SET PROPERTY OF gst_border 'LineStyle' = 1.
*
*        CALL METHOD OF gst_row1 'Interior' = gst_interior.
*        SET PROPERTY OF gst_interior 'Color' = 12500670.
*        FREE OBJECT gst_column1.
*
*      ELSE. "/ IF git_question1 IS NOT INITIAL.-----------------------------Else part to get the sheets
*
*        IF sy-index = 1.
*          GET PROPERTY OF gst_application 'ActiveSheet' = gst_activesheet .
*        ELSE. "/ IF sy-index = 1.
*          GET PROPERTY OF gst_wbook 'Sheets' = gst_sheets .
*          CALL METHOD OF gst_sheets 'Add' = gst_newsheet.
*        ENDIF. "/ IF sy-index = 1.
*
*        CLEAR git_question2.
*        git_question2[] = git_question1[].
*        git_question3[] = git_question1[].
*
*        DELETE git_question2 WHERE repeat_group_name IS INITIAL AND group_name IS INITIAL.
*        DELETE git_question3 WHERE repeat_group_name IS INITIAL AND group_name IS INITIAL.
*        SORT git_question2 BY group_name.
*        DELETE ADJACENT DUPLICATES FROM git_question2 COMPARING repeat_group_name.
*        DELETE ADJACENT DUPLICATES FROM git_question2 COMPARING group_name.
*
*        IF gv_group_name IS NOT INITIAL.
*          grs_group_name-sign   = 'I'.
*          grs_group_name-option = 'EQ'.
*          grs_group_name-low    = gv_group_name.
*          APPEND grs_group_name TO grt_group_name.
*          CLEAR grs_group_name.
*          DELETE git_question2 WHERE group_name IN grt_group_name[].
*          DELETE git_question3 WHERE group_name IN grt_group_name[].
*        ENDIF. "/ IF gv_group_name IS NOT INITIAL.
*
*        UNASSIGN <gfsit_form>.
*
*        CLEAR grt_group_name1.
*        READ TABLE git_question2 INTO gst_question2 INDEX 1.
*        IF sy-subrc = 0.
*          grs_group_name-sign   = 'I'.
*          grs_group_name-option = 'EQ'.
*          grs_group_name-low    = gst_question2-group_name.
*          APPEND grs_group_name TO grt_group_name1.
*          CLEAR grs_group_name.
*          DELETE git_question3 WHERE group_name NOT IN grt_group_name1[].
*          gv_group_name = gst_question2-group_name.
*
*          LOOP AT git_formrsp ASSIGNING <gfsst_formrsp>.
*            "/ converting timestamp into date and time
*            CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'                      "#EC CI_SUBRC.
*              EXPORTING
*                iv_timestamp     = <gfsst_formrsp>-created_on
*              IMPORTING
*                o_date           = gv_date1
*              EXCEPTIONS
*                conversion_error = 1
*                OTHERS           = 2.
*
*            <gfsst_formrsp>-created_date = gv_date1.
*            READ TABLE git_afih INTO gst_afih WITH KEY aufnr = <gfsst_formrsp>-wo_num.
*            IF sy-subrc = 0.
*              <gfsst_formrsp>-equnr = gst_afih-equnr.
*              READ TABLE git_iloa INTO gst_iloa WITH KEY iloan = gst_afih-iloan.
*              IF sy-subrc = 0.
*                <gfsst_formrsp>-tplnr = gst_iloa-tplnr.
*              ENDIF. "/ IF sy-subrc = 0.
*            ENDIF. "/ IF sy-subrc = 0.
*          ENDLOOP. "/ LOOP AT git_formrsp ASSIGNING <gfsst_formrsp>.
*
*          LOOP AT git_formrsp INTO gst_formrsp.
*            "/ Converting timestamp into Date and Time
*            CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'                                  "#EC CI_SUBRC.
*              EXPORTING
*                iv_timestamp     = gst_formrsp-created_on
*              IMPORTING
*                o_date           = gv_date
*              EXCEPTIONS
*                conversion_error = 1
*                OTHERS           = 2.
*
*            IF gv_date IN s_date.
*              TRY.
*                  gv_a->gmib_get_response_data(
*                    EXPORTING
*                      im_formid        = gst_formrsp-formid     " ODS Form ID
*                      im_version       = gst_formrsp-version    " ODS Version
*                      im_instanceid    = gst_formrsp-instanceid " ODS MFE InstanceId
*                    IMPORTING
*                      ex_response_data = git_responce
*                  ).
*                CATCH cx_sy_ref_is_initial.
*              ENDTRY.
*
*              IF <gfsit_form> IS NOT ASSIGNED.
*
*                go_table ?= cl_abap_tabledescr=>describe_by_data( p_data = git_form ).
*                go_struct ?= go_table->get_table_line_type( ).
*
*                p_form = go_struct->get_components( ).
*
*                LOOP AT git_lvc_field INTO gst_lvc_field. "#EC CI_SUBRC.
*                  gst_componentdescr-name = gst_lvc_field-fieldname.
*                  gst_componentdescr-type = cl_abap_elemdescr=>get_c( p_length = 255 ).
*                  APPEND gst_componentdescr TO p_form.
*                  CLEAR gst_componentdescr.
*                ENDLOOP. "/ LOOP AT git_lvc_field INTO gst_lvc_field.
*
*                go_struct1 = cl_abap_structdescr=>create(
*                p_components  = p_form
*                p_strict      = '*' ).
*
*                go_table1 = cl_abap_tabledescr=>create( go_struct1 ).
*                CREATE DATA go_ref_final TYPE HANDLE go_table1.
*                ASSIGN go_ref_final->* TO <gfsit_form>.
*
*                MOVE-CORRESPONDING git_formrsp TO <gfsit_form>.
*              ENDIF.
*
*              gv_created_on = gst_formrsp-created_on.
*
*              CONCATENATE gst_formrsp-instanceid gv_deli gst_formrsp-formid gv_deli gst_formrsp-version gv_deli   INTO gv_conc1.
*              CONCATENATE gv_conc1 gst_formrsp-wo_num gv_deli gst_formrsp-vornr gv_deli gst_formrsp-equnr gv_deli INTO gv_conc2.
*              CONCATENATE gv_conc2 gst_formrsp-tplnr gv_deli gv_created_on gv_deli gst_formrsp-created_by INTO gv_conc.
*
*              git_responce1[] = git_responce[].
*
*              READ TABLE git_question3 INTO gst_question3 INDEX 1.
*              DELETE git_responce1 WHERE sub_group <> gst_question3-group_name.
*
*              CLEAR: gst_question3.
*              LOOP AT git_question3 INTO gst_question3.  "#EC CI_SUBRC.
*                grs_responce-sign   = 'I'.
*                grs_responce-option = 'EQ'.
*                grs_responce-low    = gst_question3-question_id.
*                APPEND grs_responce TO grt_responce.
*                CLEAR grs_responce.
*              ENDLOOP. "/ LOOP AT git_question3 INTO gst_question3.
*              DELETE git_responce1 WHERE question_id NOT IN grt_responce.
*
*              CLEAR: gst_responce1 , gv_index.
*
*              LOOP AT git_responce1 INTO gst_responce1.  "#EC CI_SUBRC.
*                gv_index = gv_index + 1.
*                IF gv_index = 1.
*                  gv_question_id = gst_responce1-question_id.
*                  CLEAR: gv_conc1, gv_conc2, gv_conc, gst_conc.
*                  CONCATENATE gst_formrsp-instanceid gv_deli gst_formrsp-formid gv_deli gst_formrsp-version gv_deli   INTO gv_conc1.
*                  CONCATENATE gv_conc1 gst_formrsp-wo_num gv_deli gst_formrsp-vornr gv_deli gst_formrsp-equnr gv_deli INTO gv_conc2.
*                  CONCATENATE gv_conc2 gst_formrsp-tplnr gv_deli gv_created_on gv_deli gst_formrsp-created_by INTO gv_conc.
*                ENDIF. "/ IF gv_index = 1.
*
*                IF gst_responce1-question_id = gv_question_id AND gv_index <> 1.
*                  gst_conc = gv_conc.
*                  APPEND gst_conc TO git_conc.
*                  CLEAR: gv_conc1, gv_conc2, gv_conc, gst_conc.
*                  CONCATENATE gst_formrsp-instanceid gv_deli gst_formrsp-formid gv_deli gst_formrsp-version gv_deli   INTO gv_conc1.
*                  CONCATENATE gv_conc1 gst_formrsp-wo_num gv_deli gst_formrsp-vornr gv_deli gst_formrsp-equnr gv_deli INTO gv_conc2.
*                  CONCATENATE gv_conc2 gst_formrsp-tplnr gv_deli gv_created_on gv_deli gst_formrsp-created_by INTO gv_conc.
*                ENDIF. "/ IF gst_responce1-question_id = gv_question_id AND gv_index <> 1.
*                READ TABLE git_question3 INTO gst_question3 WITH KEY question_id = gst_responce1-question_id.
*                IF sy-subrc = 0.
*                  CONCATENATE gv_conc gv_deli gst_responce1-responsedata INTO gv_conc.
*                ENDIF. "/ IF sy-subrc = 0.
*                CLEAR gst_responce1.
*              ENDLOOP. "/ LOOP AT git_responce1 INTO gst_responce1.
*              gst_conc = gv_conc.
*              APPEND gst_conc TO git_conc.
*            ENDIF. "/ IF gv_date IN s_date.
*            CLEAR: gst_conc, gv_conc, git_responce, gv_conc1, gv_conc2, gst_formrsp, gst_question3.
*          ENDLOOP. "/ LOOP AT git_formrsp INTO gst_formrsp.
*
*          go_itab ?= cl_abap_typedescr=>describe_by_data( <gfsit_form> ).
*          go_line = go_itab->get_table_line_type( ).
*
*          "/ Appending column names to internal table.
*          IF go_line->kind = cl_abap_typedescr=>kind_struct.
*            go_struct ?= go_line.
*            LOOP AT go_struct->components[] ASSIGNING <gfsst_comp>.
*              gv_fieldname = VALUE gtys_types( colname = <gfsst_comp>-name ).
*              APPEND gv_fieldname TO git_colnames.
*              CLEAR gv_fieldname.
*            ENDLOOP. "/ LOOP AT go_struct->components[] ASSIGNING <gfsst_comp>.
*
*            CONCATENATE 'Instance ID' gv_deli 'Form ID' gv_deli 'Version' gv_deli 'Work Order' gv_deli 'Activity/Operation' gv_deli 'Equipment' gv_deli 'Functional Location' gv_deli 'Created Date' gv_deli 'Created By' INTO gv_con.      ##NO_TEXT .
*
*            git_colnames1 = git_colnames.
*
*            LOOP AT git_colnames ASSIGNING <lfsst_colnames>.
*              TRANSLATE <lfsst_colnames>-colname TO LOWER CASE.
*              READ TABLE git_question3 INTO gst_question3 WITH KEY question_index = <lfsst_colnames>-colname.
*              IF sy-subrc = 0.
*                CONCATENATE gv_con gv_deli gst_question3-question INTO gv_con.
*              ENDIF.
*              CLEAR gst_question3.
*            ENDLOOP. "/ LOOP AT git_colnames ASSIGNING <lfsst_colnames>.
*
*            gst_concatenate = gv_con.
*            APPEND gst_concatenate TO git_concatenate.
*            CLEAR gst_concatenate.
*
*            LOOP AT git_conc INTO gst_conc.
*              APPEND gst_conc TO git_concatenate.
*              CLEAR gst_conc.
*            ENDLOOP. "/ LOOP AT git_conc INTO gst_conc.
*
*          ENDIF.
*
*          READ TABLE git_question2 INTO gst_question2 INDEX 1.
*
*          IF sy-index = 1.
*            SET PROPERTY OF gst_activesheet 'Name' = gst_question2-group_name .
*          ENDIF. "/ IF sy-index = 1.
*
*          GET PROPERTY OF gst_wbook 'Sheets' = gst_sheets .
*          SET PROPERTY OF gst_newsheet 'Name' = gst_question2-group_name .
*
*          CALL METHOD cl_gui_frontend_services=>clipboard_export              "#EC CI_SUBRC.
*            IMPORTING
*              data         = git_concatenate[]
*            CHANGING
*              rc           = gv_rc
*            EXCEPTIONS
*              cntl_error   = 1
*              error_no_gui = 2
*              OTHERS       = 4.
*
*          IF sy-index = 1.
*            CALL METHOD OF gst_activesheet 'Paste'.
*          ELSE. "/ IF sy-index = 1.
*            CALL METHOD OF gst_newsheet 'Paste'.
*          ENDIF. "/ IF sy-index = 1.
*
*          CALL METHOD OF gst_excel 'Columns' = gst_column1.
*          CALL METHOD OF gst_column1 'Autofit'.
*
*          CALL METHOD OF gst_excel 'Rows' = gst_row1
*          EXPORTING #1 = 1.
*
*          GET PROPERTY OF gst_row1 'Font' = gst_font.
*          SET PROPERTY OF gst_font 'Bold' = 1.
*          SET PROPERTY OF gst_font 'ColorIndex' = 32.
*
*          CALL METHOD OF gst_row1 'Borders' = gst_border.
*          SET PROPERTY OF gst_border 'LineStyle' = 1.
*
*          CALL METHOD OF gst_row1 'Interior' = gst_interior.
*          SET PROPERTY OF gst_interior 'Color' = 12500670.
*          FREE OBJECT gst_column1.
*
*        ENDIF. "/ IF sy-subrc = 0.
*      ENDIF. "/ IF sy-index = gv_lines.
*      CLEAR: gv_value , gv_val, gv_line_cntr, git_conc, git_colnames, gv_con, git_con, git_colnames1, git_concatenate, gst_column1.
*    ENDDO. "/ DO gv_lines TIMES .
*  ENDIF. "/ IF git_question1 IS NOT INITIAL.
*
*ENDFORM.
