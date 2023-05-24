*&---------------------------------------------------------------------*
*&  Include           /ODSMFE/RPT_CB_TABLES_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD
*&---------------------------------------------------------------------*
*   download table data
*----------------------------------------------------------------------*
FORM download.

  IF p_table IS NOT INITIAL.
    CREATE DATA go_ref TYPE TABLE OF (p_table).
    ASSIGN go_ref->* TO <fs_table>.

    TRY.

        SELECT * FROM (p_table) INTO TABLE <fs_table>.

        go_itab ?= cl_abap_typedescr=>describe_by_data( <fs_table> ).
        DATA(lo_line) = go_itab->get_table_line_type( ).

        "appending column names to internal table.
        IF lo_line->kind = cl_abap_typedescr=>kind_struct.
          go_struct ?= lo_line.
          LOOP AT go_struct->components[] ASSIGNING <fs_comp>.
            DATA(lv_fieldname) = VALUE gtys_types( colname = <fs_comp>-name ).
            APPEND lv_fieldname TO git_colnames.
          ENDLOOP.
        ENDIF.
        IF <fs_table> IS NOT INITIAL.
          GET REFERENCE OF <fs_table> INTO go_excel_structure.
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
            ENDLOOP.
          ENDIF.

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
              window_title      = 'Enter File Name'
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
              MESSAGE 'Unable to download data' TYPE 'S' DISPLAY LIKE 'E'.
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
                MESSAGE 'Data downloaded successfully' TYPE 'S' DISPLAY LIKE 'S'.
              ENDIF.
            ENDIF.
          ELSE.
            MESSAGE 'Unable to download data' TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        ELSE.
          MESSAGE 'Table contains no records' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      CATCH cx_root INTO DATA(e_text).
        MESSAGE e_text->get_text( ) TYPE 'I'.
    ENDTRY.
  ELSE.
    MESSAGE 'Please select any table' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_STRU
*&---------------------------------------------------------------------*
*   download table structure
*----------------------------------------------------------------------*
FORM download_stru.
  IF p_table2 IS NOT INITIAL.

    CREATE DATA go_ref TYPE TABLE OF (p_table2).
    ASSIGN go_ref->* TO <fs_table>.

    TRY.
        SELECT * FROM (p_table2) INTO TABLE <fs_table>.

        go_itab ?= cl_abap_typedescr=>describe_by_data( <fs_table> ).
        DATA(lo_line) = go_itab->get_table_line_type( ).

        "appending column names to internal table.
        IF lo_line->kind = cl_abap_typedescr=>kind_struct.
          go_struct ?= lo_line.
          LOOP AT go_struct->components[] ASSIGNING <fs_comp>.
            DATA(lv_fieldname) = VALUE gtys_types( colname = <fs_comp>-name ).
            APPEND lv_fieldname TO git_colnames.
          ENDLOOP.
        ENDIF.


        IF <fs_table> IS NOT INITIAL.
          CLEAR <fs_table>.
          GET REFERENCE OF <fs_table> INTO go_excel_structure.
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
            ENDLOOP.
          ENDIF.

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
              window_title      = 'Enter File Name'
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
              MESSAGE 'Unable to download data' TYPE 'S' DISPLAY LIKE 'E'.
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
                MESSAGE 'Structure downloaded successfully' TYPE 'S' DISPLAY LIKE 'S'.
              ENDIF.
            ENDIF.
          ELSE.
            MESSAGE 'Unable to download structure' TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
        ELSE.
          MESSAGE 'Structure does not exist' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      CATCH cx_root INTO DATA(e_text).
        MESSAGE e_text->get_text( ) TYPE 'I'.
    ENDTRY.
  ELSE.
    MESSAGE 'Please select any table' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPLOAD
*&---------------------------------------------------------------------*
*   Upload data from excel to DB table
*----------------------------------------------------------------------*
FORM upload.

  IF p_file IS NOT INITIAL.
    "Generate table
    CREATE DATA go_dyn_tab TYPE TABLE OF (p_str).
    ASSIGN go_dyn_tab->* TO <lfs_dyn_tab>.
    TRY.
*        CALL FUNCTION 'ZFM_EXCEL_TO_ITAB'
        CALL FUNCTION '/ODSMFE/FM_EXCEL_TO_ITAB'
          EXPORTING
            iv_filename         = p_file
            iv_structure        = p_str
          CHANGING
            ct_return_table     = <lfs_dyn_tab>
          EXCEPTIONS
            structure_not_found = 1
            field_not_found     = 2
            OTHERS              = 3.
        IF sy-subrc <> 0.
          MESSAGE 'Unable to upload data' TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
*****        ">>>If needed the below line could be used to update the data to DB
*****        MODIFY (p_str) FROM TABLE <lfs_dyn_tab>.
          MESSAGE 'Data uploaded successfully' TYPE 'S' DISPLAY LIKE 'S'.
        ENDIF.
      CATCH cx_sy_conversion_no_number.
    ENDTRY.

    TRY.
        cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
          CHANGING
          t_table = <lfs_dyn_tab> ).
      CATCH cx_salv_msg INTO
    go_msg.
    ENDTRY.
    IF go_alv IS BOUND.
      go_alv->display( ).
    ENDIF.

  ELSE.
    MESSAGE 'Please Choose a File' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
