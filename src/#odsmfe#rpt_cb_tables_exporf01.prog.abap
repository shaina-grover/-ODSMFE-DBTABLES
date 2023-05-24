*----------------------------------------------------------------------*
***INCLUDE /ODSMFE/RPT_CB_TABLES_EXPORF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  /ODSMFE/FO_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM /odsmfe/fo_download .

  IF p_table IS NOT INITIAL.
    CREATE DATA go_ref TYPE TABLE OF (p_table).
    ASSIGN go_ref->* TO <gfsst_table>.

    TRY.
        SELECT * INTO TABLE <gfsst_table> FROM (p_table). "#EC CI_DYNTAB.

        go_itab ?= cl_abap_typedescr=>describe_by_data( <gfsst_table> ).
        DATA(lo_line) = go_itab->get_table_line_type( ).

        "/ appending column names to internal table.
        IF lo_line->kind = cl_abap_typedescr=>kind_struct.
          go_struct ?= lo_line.
          LOOP AT go_struct->components[] ASSIGNING <gfsst_comp>.
            DATA(lv_fieldname) = VALUE gtys_types( colname = <gfsst_comp>-name ).
            APPEND lv_fieldname TO git_colnames.
          ENDLOOP."/ LOOP AT go_struct->components[] ASSIGNING <gfsst_comp>.
        ENDIF."/ IF lo_line->kind = cl_abap_typedescr=>kind_struct.
        IF <gfsst_table> IS NOT INITIAL.
          GET REFERENCE OF <gfsst_table> INTO go_excel_structure.
          DATA(lo_itab_services) = cl_salv_itab_services=>create_for_table_ref( go_excel_structure ).
          go_source_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( go_excel_structure  ).

          go_table_row_descriptor ?= go_source_table_descr->get_table_line_type( ).

          DATA(lo_tool_xls) = cl_salv_export_tool_ats_xls=>create_for_excel(
                                    EXPORTING r_data =  go_excel_structure  ) .

          DATA(lo_config) = lo_tool_xls->configuration( ).

          DATA lv_val TYPE string.
          IF git_colnames IS NOT INITIAL.
            LOOP AT git_colnames INTO gst_colnames.
              lv_val = gst_colnames-colname.
              lo_config->add_column(
                  EXPORTING
                    header_text          =  lv_val
                    field_name           =  lv_val
                    display_type         =   if_salv_bs_model_column=>uie_text_view ).
            ENDLOOP."/ LOOP AT git_colnames INTO gst_colnames.
          ENDIF."/ IF git_colnames IS NOT INITIAL.

          DATA lv_val1 TYPE string.

          TRY.
              lo_tool_xls->read_result(  IMPORTING content  = gv_content  ).
            CATCH cx_root.
          ENDTRY.

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer        = gv_content
            IMPORTING
              output_length = gv_length
            TABLES
              binary_tab    = git_binary_tab.

          CALL METHOD cl_gui_frontend_services=>file_save_dialog
            EXPORTING
*             window_title      = text-004
              window_title      = gc_window
              default_extension = 'XLSX'
              default_file_name = gv_filename1
            CHANGING
              filename          = gv_filename1
              path              = gv_path
              fullpath          = gv_fullpath.

          IF gv_fullpath IS NOT INITIAL.

            CALL FUNCTION 'GUI_DOWNLOAD'
              EXPORTING
                bin_filesize            = gv_length
                filename                = gv_fullpath
                filetype                = 'BIN'
              TABLES
                data_tab                = git_binary_tab
              EXCEPTIONS
                file_write_error        = 1
                no_batch                = 2
                gui_refuse_filetransfer = 3
                invalid_type            = 4
                no_authority            = 5
                unknown_error           = 6
                header_not_allowed      = 7
                separator_not_allowed   = 8
                filesize_not_allowed    = 9
                header_too_long         = 10
                dp_error_create         = 11
                dp_error_send           = 12
                dp_error_write          = 13
                unknown_dp_error        = 14
                access_denied           = 15
                dp_out_of_memory        = 16
                disk_full               = 17
                dp_timeout              = 18
                file_not_found          = 19
                dataprovider_exception  = 20
                control_flush_error     = 21
                OTHERS                  = 22.

            IF sy-subrc <> 0.
              MESSAGE text-005 TYPE 'S' DISPLAY LIKE 'E'.
            ELSE."/ IF sy-subrc <> 0.

              CALL METHOD cl_gui_frontend_services=>execute
                EXPORTING
                  document               = gv_fullpath
                EXCEPTIONS
                  cntl_error             = 1
                  error_no_gui           = 2
                  bad_parameter          = 3
                  file_not_found         = 4
                  path_not_found         = 5
                  file_extension_unknown = 6
                  error_execute_failed   = 7
                  synchronous_failed     = 8
                  not_supported_by_gui   = 9
                  OTHERS                 = 10.
              IF sy-subrc <> 0.
                MESSAGE 'NOT WORKING' TYPE 'I'.
              ELSE.
                MESSAGE text-006 TYPE 'S' DISPLAY LIKE 'S'.
              ENDIF."/ IF sy-subrc <> 0.
            ENDIF.
          ELSE.
            MESSAGE text-005 TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF."/ IF gv_fullpath IS NOT INITIAL.
        ELSE.
          MESSAGE text-007 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF."/ IF <gfsst_table> IS NOT INITIAL.

      CATCH cx_root INTO go_text.
        MESSAGE go_text->get_text( ) TYPE 'I'.
    ENDTRY.
  ELSE.
    MESSAGE text-008 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF."/ IF p_table IS NOT INITIAL.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  /ODSMFE/FO_DOWNLOAD_STRU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM /odsmfe/fo_download_stru .
  IF p_table2 IS NOT INITIAL.

    CREATE DATA go_ref TYPE TABLE OF (p_table2).
    ASSIGN go_ref->* TO <gfsst_table>.

    TRY.
        SELECT * INTO TABLE <gfsst_table> FROM (p_table2). "#EC CI_DYNTAB.

        go_itab ?= cl_abap_typedescr=>describe_by_data( <gfsst_table> ).
        DATA(lo_line) = go_itab->get_table_line_type( ).

        "appending column names to internal table.
        IF lo_line->kind = cl_abap_typedescr=>kind_struct.
          go_struct ?= lo_line.
          LOOP AT go_struct->components[] ASSIGNING <gfsst_comp>.
            DATA(lv_fieldname) = VALUE gtys_types( colname = <gfsst_comp>-name ).
            APPEND lv_fieldname TO git_colnames.
          ENDLOOP."/ LOOP AT go_struct->components[] ASSIGNING <gfsst_comp>.
        ENDIF."/ IF lo_line->kind = cl_abap_typedescr=>kind_struct.


        IF <gfsst_table> IS NOT INITIAL or <gfsst_table> is INITIAL.
          CLEAR <gfsst_table>.
          GET REFERENCE OF <gfsst_table> INTO go_excel_structure.
          DATA(lo_itab_services) = cl_salv_itab_services=>create_for_table_ref( go_excel_structure ).
          go_source_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( go_excel_structure  ).

          go_table_row_descriptor ?= go_source_table_descr->get_table_line_type( ).

          DATA(lo_tool_xls) = cl_salv_export_tool_ats_xls=>create_for_excel(
                                    EXPORTING r_data =  go_excel_structure  ) .

          DATA(lo_config) = lo_tool_xls->configuration( ).

          DATA lv_val TYPE string.
          IF git_colnames IS NOT INITIAL.
            LOOP AT git_colnames INTO gst_colnames.
              lv_val = gst_colnames-colname.
              lo_config->add_column(
                  EXPORTING
                    header_text          =  lv_val
                    field_name           =  lv_val
                    display_type         =   if_salv_bs_model_column=>uie_text_view ).
            ENDLOOP."/ LOOP AT git_colnames INTO gst_colnames.
          ENDIF."/ IF git_colnames IS NOT INITIAL.

          DATA lv_val1 TYPE string.

          TRY.
              lo_tool_xls->read_result(  IMPORTING content  = gv_content  ).
            CATCH cx_root.
          ENDTRY.

          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
            EXPORTING
              buffer        = gv_content
            IMPORTING
              output_length = gv_length
            TABLES
              binary_tab    = git_binary_tab.

          CALL METHOD cl_gui_frontend_services=>file_save_dialog
            EXPORTING
*             window_title      = 'text-004'
              window_title      = gc_window
              default_extension = 'XLSX'
              default_file_name = gv_filename1
            CHANGING
              filename          = gv_filename1
              path              = gv_path
              fullpath          = gv_fullpath.

          IF gv_fullpath IS NOT INITIAL.

            CALL FUNCTION 'GUI_DOWNLOAD'
              EXPORTING
                bin_filesize            = gv_length
                filename                = gv_fullpath
                filetype                = 'BIN'
              TABLES
                data_tab                = git_binary_tab
              EXCEPTIONS
                file_write_error        = 1
                no_batch                = 2
                gui_refuse_filetransfer = 3
                invalid_type            = 4
                no_authority            = 5
                unknown_error           = 6
                header_not_allowed      = 7
                separator_not_allowed   = 8
                filesize_not_allowed    = 9
                header_too_long         = 10
                dp_error_create         = 11
                dp_error_send           = 12
                dp_error_write          = 13
                unknown_dp_error        = 14
                access_denied           = 15
                dp_out_of_memory        = 16
                disk_full               = 17
                dp_timeout              = 18
                file_not_found          = 19
                dataprovider_exception  = 20
                control_flush_error     = 21
                OTHERS                  = 22.

            IF sy-subrc <> 0.
              MESSAGE text-005 TYPE 'S' DISPLAY LIKE 'E'.
            ELSE.
              CALL METHOD cl_gui_frontend_services=>execute
                EXPORTING
                  document               = gv_fullpath
                EXCEPTIONS
                  cntl_error             = 1
                  error_no_gui           = 2
                  bad_parameter          = 3
                  file_not_found         = 4
                  path_not_found         = 5
                  file_extension_unknown = 6
                  error_execute_failed   = 7
                  synchronous_failed     = 8
                  not_supported_by_gui   = 9
                  OTHERS                 = 10.
              IF sy-subrc <> 0.
                MESSAGE 'NOT WORKING' TYPE 'I'.
              ELSE.
                MESSAGE text-009 TYPE 'S' DISPLAY LIKE 'S'.
              ENDIF."/ IF sy-subrc <> 0.
            ENDIF."/ IF sy-subrc <> 0.
          ELSE.
            MESSAGE text-010 TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF."/ IF gv_fullpath IS NOT INITIAL.
        ELSE.
          MESSAGE text-011 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF."/ IF <gfsst_table> IS NOT INITIAL.

      CATCH cx_root INTO go_text."DATA(lo_text).
        MESSAGE go_text->get_text( ) TYPE 'I'.
    ENDTRY.
  ELSE.
    MESSAGE text-008 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF."/ IF p_table2 IS NOT INITIAL.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  /ODSMFE/FO_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
DATA: go_functions TYPE REF TO cl_salv_functions.
FORM /odsmfe/fo_upload .
  IF p_file IS NOT INITIAL.
    IF  p_str IS NOT INITIAL.
      "/ Generate table
      CREATE DATA go_dyn_tab TYPE TABLE OF (p_str).
      ASSIGN go_dyn_tab->* TO <gfsst_dyn_tab>.
      TRY.
          "/ Calling Function Module to upload excel data to DB Table
          CALL FUNCTION '/ODSMFE/FM_EXCEL_TO_ITAB'
            EXPORTING
              iv_filename         = p_file
              iv_structure        = p_str
            CHANGING
              ct_return_table     = <gfsst_dyn_tab>
            EXCEPTIONS
              structure_not_found = 1
              field_not_found     = 2
              OTHERS              = 3.
          IF sy-subrc <> 0.
            MESSAGE text-012 TYPE 'S' DISPLAY LIKE 'E'.
          ELSE.
            "/ Update the database table
            MODIFY (p_str) FROM TABLE <gfsst_dyn_tab>.
            MESSAGE text-013 TYPE 'S' DISPLAY LIKE 'S'.
          ENDIF."/ IF sy-subrc <> 0.
        CATCH cx_sy_conversion_no_number.
      ENDTRY.
      "/ ALV display of data
      TRY.
          cl_salv_table=>factory(
          IMPORTING
            r_salv_table = go_alv
            CHANGING
            t_table = <gfsst_dyn_tab> ).
        CATCH cx_salv_msg INTO go_msg.
      ENDTRY.
      "/ ALV functions
      go_functions = go_alv->get_functions( ).
      go_functions->set_all( abap_true ).
      "/ to set column length
      go_alv->get_columns( )->set_optimize( abap_true ).
      "/     to display data in striped pattern
      go_alv->get_display_settings( )->set_striped_pattern( abap_true ).
      "/ display ALV
      IF go_alv IS BOUND.
        go_alv->display( ).
      ENDIF."/ IF  p_str IS NOT INITIAL.

    ELSE.
      MESSAGE text-008 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF."/ IF  p_str IS NOT INITIAL.
  ELSE.
    MESSAGE text-014 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF."/ IF p_file IS NOT INITIAL.
ENDFORM.
