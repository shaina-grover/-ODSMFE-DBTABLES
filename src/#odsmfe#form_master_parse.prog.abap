*----------------------------------------------------------------------*
***INCLUDE /ODSMFE/FORM_MASTER_PARSE.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  /ODSMFE/FO_PARSE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM /odsmfe/fo_parse_data .
* Upload XML File
  CALL METHOD go_ref_xml->import_from_file
    EXPORTING
      filename = gv_localfilename
    RECEIVING
      retcode  = gv_subrc.

* method to render xstring
  CALL METHOD go_ref_xml->render_2_xstring
    IMPORTING
      retcode = gv_subrc  "##NEEDED 701-nshyamala
      stream  = gv_xml_string
      size    = gv_size.

* Convert XML to internal table
  CALL FUNCTION 'SMUM_XML_PARSE'
    EXPORTING
      xml_input = gv_xml_string
    TABLES
      xml_table = git_xml_data
      return    = git_return.


  IF gc_formmasterset IS NOT INITIAL.

    SELECT SINGLE low FROM /odsmfe/tb_filtr INTO gv_xslt_html
    WHERE entitysetname = gc_formmasterset "'FormMasterSet
    AND field = gc_xml_html     "'XML_HTML'.
    AND sign = gc_i "I
    AND options = gc_eq "EQ
    AND  active = gc_abap_on . " ##WARN_OK. 701-nshyamala

    IF sy-subrc <> 0.
      CLEAR gv_xslt_html.
    ENDIF.

    SELECT SINGLE low FROM /odsmfe/tb_filtr INTO gv_xslt_model
    WHERE entitysetname = gc_formmasterset "'FormMasterSet
    AND field = gc_xml_model  "'XML_MODEL'
    AND sign = gc_i "I
    AND options = gc_eq "EQ
    AND  active = gc_abap_on . "##WARN_OK 701-nshyamala.

    IF sy-subrc <> 0.
      CLEAR gv_xslt_model.
    ENDIF.
  ENDIF.

  SELECT SINGLE low high FROM /odsmfe/tb_filtr INTO (gv_xml_class,gv_xml_meth)
    WHERE entitysetname = 'FormMasterSet'
    AND field = 'XML_CLS_METH'
    AND active = 'X'.

  IF sy-subrc <> 0.
    CLEAR: gv_xml_class,gv_xml_meth.
  ENDIF.

  "   xslt tranformation to get HTML and Model
*  Create XSLT processor
  TRY.
      CREATE OBJECT go_ref_xsltp.

      go_ref_xsltp->set_source_xstring( sstring = gv_xml_string ).
      GET REFERENCE OF gv_doc_form_string INTO go_doc_form_string.
      go_ref_xsltp->set_result_string( rstring = go_doc_form_string ).

      TRY.
          CALL METHOD go_ref_xsltp->run
            EXPORTING
              progname = gv_xslt_html. "'/ODSMFE/XSLT_HTML5FORM'.
        CATCH cx_xslt_exception INTO gv_mesg.
          gv_mesg->get_text( RECEIVING result = gv_msg ).
          gv_mesg->get_longtext( RECEIVING result = gv_msg ).
          MESSAGE gv_msg TYPE 'I'.
      ENDTRY.

      TRY.

          CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
            EXPORTING
              text   = gv_doc_form_string
            IMPORTING
              buffer = gv_xml_form_string.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

          CLEAR: gv_mesg.
        CATCH cx_parameter_invalid_range " .
          cx_sy_codepage_converter_init
          cx_sy_conversion_codepage
          cx_parameter_invalid_type INTO gv_mesg.
          gv_mesg1 = gv_mesg->get_text( ).
          MESSAGE gv_mesg1 TYPE 'I'.

      ENDTRY.
      IF gv_xml_form_string IS NOT INITIAL.
        IF gv_xml_form_string(3) = cl_abap_char_utilities=>byte_order_mark_utf8.
          gv_xml_form_string = gv_xml_form_string+3.
        ENDIF.
      ENDIF.

      GET REFERENCE OF gv_doc_model_string INTO go_doc_model_string.
      go_ref_xsltp->set_result_string( rstring = go_doc_model_string ).
      go_ref_xsltp->run( progname = gv_xslt_model )."'/ODSMFE/XSLT_XMLMODEL'

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = gv_doc_model_string
        IMPORTING
          buffer = gv_xml_model_string
        EXCEPTIONS   " ##FM_SUBRC_OK -701-nshyamala
          failed = 1.
*         OTHERS = 2.

      IF sy-subrc EQ 0 AND gv_xml_model_string IS NOT INITIAL.
        IF gv_xml_model_string(3) = cl_abap_char_utilities=>byte_order_mark_utf8.
          gv_xml_model_string = gv_xml_model_string+3.
        ENDIF.
      ENDIF.

      gv_msg_count = go_ref_xsltp->num_messages( ).
      IF gv_msg_count > 0.
        CLEAR gv_msg_count.
      ENDIF.


  ENDTRY.

* Error handling
  CLEAR gst_return.
  LOOP AT git_return INTO gst_return WHERE type = gc_e OR type = gc_a.
    CLEAR gv_type.
    gv_type = gst_return-type.
    IF gv_type EQ gc_e OR gv_type EQ gc_a.
      gv_message = gst_return-message.
      MESSAGE gv_message TYPE gc_e.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.
