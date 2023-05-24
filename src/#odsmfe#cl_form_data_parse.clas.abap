class /ODSMFE/CL_FORM_DATA_PARSE definition
  public
  create public .

public section.

  types:
    BEGIN OF gtys_response_data,
        formid         TYPE /odsmfe/de_formid,
        version        TYPE /odsmfe/de_version,
        instanceid     TYPE /odsmfe/de_instanceid,
        wo_num         TYPE aufnr,
        vornr          TYPE vornr,
        equnr          TYPE equnr,
        tplnr          TYPE tplnr,
        created_date   TYPE dats,
        created_by     TYPE /odsmfe/de_createdby,
        group          TYPE string,
        repeat_group   TYPE string,
        sub_group      TYPE string,
        question_id    TYPE string,
        question_index TYPE string,
        questionname   TYPE string,
        responsedata   TYPE string,
      END OF gtys_response_data .
  types:
    gtyt_response_data TYPE STANDARD TABLE OF gtys_response_data .
  types:
    BEGIN OF gtys_form_data,
        lang              TYPE string,
        label             TYPE string,
        pre_group_id      TYPE string,
        group_id          TYPE string,
        repeat_group_id   TYPE string,
        question_id       TYPE string,
        question_index    TYPE string,
        pre_group_name    TYPE string,
        group_name        TYPE string,
        repeat_group_name TYPE string,
        question          TYPE string,
      END OF gtys_form_data .
  types:
    gtyt_form_data TYPE STANDARD TABLE OF gtys_form_data .

  methods GMIB_GET_ALL_QUESTIONS
    importing
      !IM_FORMID type /ODSMFE/DE_FORMID
      !IM_VERSION type /ODSMFE/DE_VERSION optional
    exporting
      !EX_FORM_DATA type GTYT_FORM_DATA .
  methods GMIB_GET_RESPONSE_DATA
    importing
      !IM_FORMID type /ODSMFE/DE_FORMID optional
      !IM_VERSION type /ODSMFE/DE_VERSION optional
      !IM_INSTANCEID type /ODSMFE/DE_INSTANCEID
    exporting
      !EX_RESPONSE_DATA type GTYT_RESPONSE_DATA
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_FORM_DATA_PARSE IMPLEMENTATION.


METHOD gmib_get_all_questions.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS-VSANAGALA
* Creation Date          :21.01.2023
* Transport No.          :ES1K903465
* Program Description    :Get the Form related data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                      *
* -----------------------------------------------------------------------*

  "/Types
  TYPES: BEGIN OF ltys_questions,
           lang         TYPE string,
           pre_group_id TYPE string,
           group_id     TYPE string,
           question_id  TYPE string,
           label        TYPE string,
           question     TYPE string,
         END OF ltys_questions,

         BEGIN OF ltys_ids,
           lang         TYPE string,
           pre_group_id TYPE string,
           group_id     TYPE string,
           question_id  TYPE string,
           data_text_id TYPE string,
         END OF ltys_ids,

         BEGIN OF ltys_form_data,
           group       TYPE string,
           sub_group   TYPE string,
           label       TYPE string,
           question_id TYPE string,
           question    TYPE string,
         END OF ltys_form_data,

         BEGIN OF ltys_label,
           label    TYPE string,
           question TYPE string,
         END OF ltys_label,

         BEGIN OF ltys_group,
           group_id   TYPE string,
           group_name TYPE string,
         END OF ltys_group,

         BEGIN OF ltys_pre_group,
           pre_group_id   TYPE string,
           pre_group_name TYPE string,
         END OF ltys_pre_group,

         BEGIN OF ltys_final,
           lang              TYPE string,
           label             TYPE string,
           pre_group_id      TYPE string,
           group_id          TYPE string,
           repeat_group_id   TYPE string,
           question_id       TYPE string,
           question_index    TYPE string,
           pre_group_name    TYPE string,
           group_name        TYPE string,
           repeat_group_name TYPE string,
           question          TYPE string,
         END OF ltys_final,

         BEGIN OF ltys_repeat_group,
           repeat_group_id TYPE string,
         END OF ltys_repeat_group.

  "/Tables and Structures
  DATA: lst_fomst        TYPE /odsmfe/tb_fomst,
        lit_html1        TYPE STANDARD TABLE OF smum_xmltb,
        lit_html         TYPE STANDARD TABLE OF smum_xmltb,
        lit_span         TYPE STANDARD TABLE OF smum_xmltb,
        lit_data_text_id TYPE STANDARD TABLE OF smum_xmltb,
        lit_lang         TYPE STANDARD TABLE OF smum_xmltb,
        lit_response     TYPE STANDARD TABLE OF smum_xmltb,
        lst_html         TYPE smum_xmltb,
        lst_html1        TYPE smum_xmltb,
        lst_span         TYPE smum_xmltb,
        lst_data_text_id TYPE smum_xmltb,
        lst_lang         TYPE smum_xmltb,
        lst_response     TYPE smum_xmltb,
        lit_return       TYPE STANDARD TABLE OF bapiret2,
        lst_question_id  TYPE ltys_form_data,
        lit_ids          TYPE STANDARD TABLE OF ltys_ids,
        lst_ids          TYPE ltys_ids,
        lst_forsp        TYPE /odsmfe/tb_forsp,
        lst_question     TYPE ltys_label,
        lit_question     TYPE TABLE OF ltys_label,
        lst_questions    TYPE ltys_questions,
        lst_questions1   TYPE ltys_questions,
        lit_questions1   TYPE STANDARD TABLE OF ltys_questions,
        lit_questions    TYPE STANDARD TABLE OF ltys_questions,
        lst_group        TYPE ltys_group,
        lst_pre_group    TYPE ltys_pre_group,
        lit_group        TYPE STANDARD TABLE OF ltys_group,
        lit_pre_group    TYPE STANDARD TABLE OF ltys_pre_group,
        lst_final        TYPE ltys_final,
        lit_final        TYPE STANDARD TABLE OF ltys_final,
        lst_repeat_group TYPE ltys_repeat_group,
        lit_repeat_group TYPE STANDARD TABLE OF ltys_repeat_group.

  "/Range Tables and Range structures
  DATA: lrs_version TYPE /odsmfe/st_core_range_str,
        lrt_version TYPE /odsmfe/tt_core_range_tab,
        lrs_final   TYPE /odsmfe/st_core_range_str,
        lrt_final   TYPE /odsmfe/tt_core_range_tab.

  "/Variables
  DATA: lv_xml_string   TYPE xstring,
        lv_num          TYPE i,
        lv_group        TYPE string,
        lv_group_id     TYPE string,
        lv_pre_group_id TYPE string,
        lv_index        TYPE string,
        lv_formid       TYPE /odsmfe/tb_fomst-formid.

  DATA :lst_label  TYPE string,
        lit_label  LIKE TABLE OF lst_label,
        lst_repeat TYPE string,
        lit_repeat LIKE TABLE OF lst_repeat.
  DATA lv_lines TYPE i.
  DATA lv_strlen TYPE i.

  DATA :  lt_data    TYPE swxmlcont,
          lref_xml   TYPE REF TO cl_xml_document,                       "XML-Dokument für WF- WEB-Aktivität
          lt_retcode TYPE sysubrc,                                      "Return Code
          lv_subrc   TYPE sy-subrc,                                     "ABAP System Field: Return Code of ABAP Statements
          lv_size    TYPE sytabix.                                      "Row Index of Internal Tables

  "/Constants
  CONSTANTS: lc_i  TYPE char1 VALUE 'I',
             lc_eq TYPE char2 VALUE 'EQ'.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*
  IF im_formid IS NOT INITIAL.
    lv_formid = im_formid.
  ENDIF.

  IF im_version IS NOT INITIAL.
    lrs_version-sign   = lc_i.
    lrs_version-option = lc_eq.
    lrs_version-low    = im_version.
    APPEND lrs_version TO lrt_version.
    CLEAR lrs_version.
  ENDIF.

  SELECT SINGLE *
    FROM /odsmfe/tb_fomst
    INTO lst_fomst
    WHERE formid  = lv_formid
      AND version IN lrt_version.

  lv_xml_string = lst_fomst-formhtml.

  CREATE OBJECT lref_xml.

* Convert XString to Binary
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = lv_xml_string
    TABLES
      binary_tab = lt_data.

* Parse data
  TRY.
      CALL METHOD lref_xml->create_with_table
        EXPORTING
          table   = lt_data
*         SIZE    = 0
        RECEIVING
          retcode = lt_retcode.
    CATCH cx_sy_ref_is_initial.
  ENDTRY.
* RENDER_2_XSTRING
  TRY.
      CALL METHOD lref_xml->render_2_xstring
        IMPORTING
          retcode = lv_subrc
          stream  = lv_xml_string
          size    = lv_size.
    CATCH cx_sy_ref_is_initial.
  ENDTRY.


  CALL FUNCTION 'SMUM_XML_PARSE'
    EXPORTING
      xml_input = lv_xml_string
    TABLES
      xml_table = lit_html
      return    = lit_return.

  lit_data_text_id[] = lit_html[].
  lit_lang[] = lit_html[].
  lit_html1[] = lit_html[].

  DELETE lit_data_text_id WHERE cname NE 'data-itext-id'.
  DELETE lit_lang WHERE cname NE 'lang'.
  DELETE lit_lang WHERE cvalue EQ space.

  LOOP AT lit_data_text_id INTO lst_data_text_id.
    lv_index = sy-tabix.
    lst_ids-data_text_id = lst_data_text_id-cvalue.
    SPLIT lst_data_text_id-cvalue AT '/' INTO TABLE lit_label.
    DESCRIBE TABLE lit_label LINES lv_lines.

    READ TABLE lit_label INTO lst_label INDEX lv_lines.
    lv_strlen = strlen( lst_label ).

    SPLIT lst_label AT '-' INTO DATA(lv_que) DATA(lv_check_label).

*    IF lv_check_label <> 'label'.
*      SPLIT lst_label AT '-' INTO TABLE DATA(lit_check_label)."DATA(lv_que) DATA(lv_check_label).
*      DESCRIBE TABLE lit_check_label LINES DATA(lv_check_label_lines).
*      READ TABLE lit_check_label INTO DATA(lst_check_label) INDEX lv_check_label_lines.
*      lv_check_label = lst_check_label.
*    ENDIF.

    IF lv_check_label = 'label'.

      IF lv_strlen > 6.
        lv_strlen = lv_strlen - 6.
        lst_label = lst_label(lv_strlen).
      ENDIF.
      lst_ids-question_id = lst_label.

      "----Group_id----"
      IF lv_lines > 1.
        lv_lines = lv_lines - 1.
        CLEAR: lst_label.
        READ TABLE lit_label INTO lst_label INDEX lv_lines.
        lst_ids-group_id = lst_label.
      ENDIF.

      "----Pre-Group_id----"
      IF lv_lines > 1.
        lv_lines = lv_lines - 1.
        CLEAR: lst_label.
        READ TABLE lit_label INTO lst_label INDEX lv_lines.
        lst_ids-pre_group_id = lst_label.
      ENDIF.

      "----Lang----"
      READ TABLE lit_lang INTO lst_lang INDEX lv_index.
      lst_ids-lang = lst_lang-cvalue.

      APPEND lst_ids TO lit_ids.
    ELSE. "/ IF lv_check_label = 'label'.
      CONTINUE.
    ENDIF. "/ IF lv_check_label = 'label'.
    CLEAR: lst_ids, lst_label, lit_label, lst_data_text_id, lv_lines, lv_index.
  ENDLOOP.

  LOOP AT lit_html INTO lst_html.
    lv_index = sy-tabix.
    IF lst_html-cname = 'data-itext-id'.
      lst_question-label = lst_html-cvalue.
      lv_index = lv_index - 2.
      READ TABLE lit_html1 INTO lst_html1 INDEX lv_index.
      IF lst_html1-cname = 'span'.
        lst_question-question = lst_html1-cvalue.
      ENDIF.
      APPEND lst_question TO lit_question.
    ENDIF.

    IF lst_html-cname = 'class' AND lst_html-cvalue = 'or-repeat'.
      lv_index = lv_index + 1.
      READ TABLE lit_html1 INTO lst_html1 INDEX lv_index.
      IF lst_html1-cname = 'name'.
        SPLIT lst_html1-cvalue AT '/' INTO TABLE lit_repeat.
        DESCRIBE TABLE lit_repeat LINES lv_lines.
        READ TABLE lit_repeat INTO lst_repeat INDEX lv_lines.
        lst_repeat_group-repeat_group_id = lst_repeat.
        APPEND lst_repeat_group TO lit_repeat_group.
      ENDIF.
    ENDIF.
    CLEAR: lst_question, lst_html1, lst_html, lv_index, lst_repeat_group.
  ENDLOOP.

  LOOP AT lit_ids INTO lst_ids.
    READ TABLE lit_question INTO lst_question WITH KEY label = lst_ids-data_text_id.
    lst_questions-lang         = lst_ids-lang.
    lst_questions-pre_group_id = lst_ids-pre_group_id.
    lst_questions-group_id     = lst_ids-group_id.
    lst_questions-question_id  = lst_ids-question_id.
    lst_questions-label        = lst_question-label.
    lst_questions-question     = lst_question-question.
    APPEND lst_questions TO lit_questions.
    CLEAR: lst_questions, lst_question, lst_ids.
  ENDLOOP.

  lit_questions1[] = lit_questions[].

  LOOP AT lit_questions INTO lst_questions.
    IF lst_questions-group_id IS NOT INITIAL.
      lv_group_id = lst_questions-group_id.
      READ TABLE lit_questions1 INTO lst_questions1 WITH KEY question_id = lv_group_id.
      lst_group-group_id   = lst_questions-group_id.
      lst_group-group_name = lst_questions1-question.
      APPEND lst_group TO lit_group.
    ENDIF.

    IF lst_questions-pre_group_id IS NOT INITIAL.
      lv_pre_group_id = lst_questions-pre_group_id.
      READ TABLE lit_questions1 INTO lst_questions1 WITH KEY question_id = lv_pre_group_id.
      lst_pre_group-pre_group_id = lst_questions-pre_group_id.
      lst_pre_group-pre_group_name = lst_questions1-question.
      APPEND lst_pre_group TO lit_pre_group.
    ENDIF.
    CLEAR: lst_group, lst_questions, lst_pre_group.
  ENDLOOP.

  CLEAR: lv_index.
  LOOP AT lit_questions INTO lst_questions.
    lst_final-lang        = lst_questions-lang.
    lst_final-label       = lst_questions-label.
    lst_final-question_id = lst_questions-question_id.
    lst_final-question    = lst_questions-question.
    lst_final-group_id    = lst_questions-group_id.

    READ TABLE lit_group INTO lst_group WITH KEY group_id = lst_questions-group_id.
    IF sy-subrc = 0.
      lst_final-group_name = lst_group-group_name.
    ENDIF.

    READ TABLE lit_pre_group INTO lst_pre_group WITH KEY pre_group_id = lst_questions-pre_group_id.
    IF sy-subrc = 0.
      lst_final-pre_group_id   = lst_questions-pre_group_id.
      lst_final-pre_group_name = lst_pre_group-pre_group_name.
    ENDIF.

    READ TABLE lit_repeat_group INTO lst_repeat_group WITH KEY repeat_group_id = lst_questions-group_id.
    IF sy-subrc = 0.
      lst_final-repeat_group_id   = lst_repeat_group-repeat_group_id.
      lst_final-repeat_group_name = lst_final-group_name.
    ENDIF.

    APPEND lst_final TO lit_final.
    CLEAR: lst_final, lst_group, lst_questions, lst_pre_group, lst_repeat_group.
  ENDLOOP.

*  IF lit_lang[] IS NOT INITIAL.
*    DELETE lit_final WHERE lang NE 'en'.
*  ENDIF. "/ IF lit_lang[] IS NOT INITIAL.

  DATA : lv_name_strlen TYPE i,
         lv_fieldname   TYPE string,
         lv_ques_index  TYPE string.

  LOOP AT lit_final ASSIGNING FIELD-SYMBOL(<lfsst_final>).
    lv_index = sy-tabix.
    CONDENSE lv_index.
    lv_name_strlen = strlen( <lfsst_final>-question_id ).
    IF lv_name_strlen > 24.
      lv_fieldname = <lfsst_final>-question_id(24).
    ELSE.
      lv_fieldname = <lfsst_final>-question_id.
    ENDIF.

    CONCATENATE lv_fieldname '_' lv_index INTO lv_ques_index.
    CONDENSE lv_ques_index.
    <lfsst_final>-question_index = lv_ques_index.
    CLEAR: lv_ques_index, lv_fieldname, lv_index, lv_name_strlen.
  ENDLOOP.

  ex_form_data[] = lit_final[].


ENDMETHOD.


  METHOD gmib_get_response_data.

*&----------------------------------------------------------------------*
* PROGRAM ID           :                                                *
* PROGRAM TITLE        :                                                *
* developer id :mrakesh                                      *
* SUPPLIER             :OnDevice Solutions                              *
* DATE                 :12.05.2023                                      *
* DEVELOPMENT ID       :$TMP\ HPQC                                      *
* CHANGE REQUEST (CTS) :DR0K######                                      *
*=======================================================================*
* COPIED FROM         : (CLONED PROGRAM)                                *
* TITLE               : (PROGRAM TITLE)                                 *
* OTHER RELATED OBJ   : (OBJECT NAMES)                                  *
*=======================================================================*
* CHANGE HISTORY LOG                                                    *
* CHANGE HISTORY LOG                                                    *
*-----------------------------------------------------------------------*
* CHANGE HISTORY LOG                                                    *
* MOD.NO.| DATE     | NAME           | CORRECTION NUMBER  |CHANGE       *
*                                                          REFERENCE    *
*-----------------------------------------------------------------------*
*        |          |                |                    |             *
*                                                                       *
* DESCRIPTION:                                                          *
*-----------------------------------------------------------------------*
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS-VSANAGALA
* Creation Date          :21.01.2023
* Transport No.          :ES1K903465
* Program Description    :Get the Form related data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                      *
* -----------------------------------------------------------------------*

                                                                               "/Types
types : begin of ltys_response,
         formid         type /odsmfe/de_formid,                                "ODS Form ID
         version        type /odsmfe/de_version,                               "ODS Version
         instanceid     type /odsmfe/de_instanceid,                            "ODS MFE InstanceId
         wo_num         type aufnr,                                            "Order Number
         vornr          type vornr,                                            "Operation/Activity Number
         equnr          type equnr,                                            "Equipment Number
         tplnr          type tplnr,                                            "Functional Location
         created_date   type dats,                                             "Field of type DATS
         created_by     type /odsmfe/de_createdby,                             "ODS Created By
         group          type string,                                           "
         repeat_group   type string,                                           "
         sub_group      type string,                                           "
         question_id    type string,                                           "
         question_index type string,                                           "
         questionname   type string,                                           "
         responsedata   type string,                                           "
        end of ltys_response.

                                                                               "/Variables
    DATA: lv_xml_string TYPE xstring,                                          "
          lit_fomrsp    TYPE TABLE OF smum_xmltb,                              "XML Table structure used for retreive and output XML doc
          lst_fomrsp    TYPE smum_xmltb,                                       "XML Table structure used for retreive and output XML doc
          lit_return    TYPE STANDARD TABLE OF bapiret2,                       "Return Parameter
          ls_return     TYPE bapiret2,                                         "Return Parameter
          lv_num        TYPE i,                                                "
          lv_instanceid TYPE /odsmfe/de_instanceid,                            "ODS MFE InstanceId
          lv_qid        TYPE string,                                           "
          lv_text_string TYPE string,                                            "
          lv_index      TYPE string.                                           "

                                                                               "/Range Tables and Range structures
    DATA: lrs_formid  TYPE /odsmfe/st_core_range_str,                          "ODS MFE: Filter Purpose Range Structure
          lrs_version TYPE /odsmfe/st_core_range_str,                          "ODS MFE: Filter Purpose Range Structure
          lrt_version TYPE /odsmfe/tt_core_range_tab,                          "
          lrt_formid  TYPE /odsmfe/tt_core_range_tab.                          "

    DATA: lt_forsp    TYPE STANDARD TABLE OF /odsmfe/tb_forsp,                 "Table to Capture Response
          lt_response TYPE STANDARD TABLE OF gtys_form_data,                   "
          ls_response TYPE gtys_form_data,                                     "
          ls_forsp    TYPE /odsmfe/tb_forsp,                                   "Table to Capture Response
          lit_final   TYPE TABLE OF ltys_response,                             "
          lst_final   TYPE ltys_response.                                      "

    DATA :lit_data    TYPE swxmlcont,                                           "
          lo_xml   TYPE REF TO cl_xml_document,                              "XML-Dokument für WF- WEB-Aktivität
          lit_retcode TYPE sysubrc,                                             "Return Code
          lv_subrc   TYPE sy-subrc,                                            "ABAP System Field: Return Code of ABAP Statements
          lv_size    TYPE sytabix.                                             "Row Index of Internal Tables

                                                                               "/Constants
    CONSTANTS: lc_i  TYPE char1 VALUE 'I',                                     "
               lc_eq TYPE char2 VALUE 'EQ'.                                    "

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

    IF im_instanceid IS NOT INITIAL.
      lv_instanceid = im_instanceid.
    ENDIF.                                                                     " IF IM_INSTANCEID IS NOT INITIAL Line No. :78

    IF im_formid IS NOT INITIAL.
      lrs_formid-sign   = lc_i.
      lrs_formid-option = lc_eq.
      lrs_formid-low    = im_formid.
      APPEND lrs_formid TO lrt_formid.
      CLEAR: lrs_formid.
    ENDIF.                                                                     " IF IM_FORMID IS NOT INITIAL Line No. :82

    IF im_version IS NOT INITIAL.
      lrs_version-sign   = lc_i.
      lrs_version-option = lc_eq.
      lrs_version-low    = im_version.
      APPEND lrs_version TO lrt_version.
      CLEAR lrs_version.
    ENDIF.                                                                     " IF IM_VERSION IS NOT INITIAL Line No. :90

                                                                               "/ Fetching the data from Form Response table
    SELECT SINGLE *
      FROM /odsmfe/tb_forsp
      INTO ls_forsp
      WHERE instanceid = lv_instanceid
        AND formid  IN lrt_formid[]
        AND version IN lrt_version[].

                                                                               "/ FM converts hexadecimal into internal table
    lv_xml_string = ls_forsp-responsedata.

    CREATE OBJECT lo_xml.
* Check Response data
    IF ls_forsp-responsedata IS NOT INITIAL.
      lv_xml_string = ls_forsp-responsedata.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid  = /iwbep/cx_mgw_busi_exception=>business_error
          message = 'Response data not found'.
    ENDIF.

* Convert XString to Binary
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = lv_xml_string
      TABLES
        binary_tab = lit_data.
    IF lit_data IS INITIAL.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid  = /iwbep/cx_mgw_busi_exception=>business_error
          message = 'Converting Xstring to Binary data Failed'.
    ENDIF.

* Parse data
    CALL METHOD lo_xml->create_with_table
      EXPORTING
        table   = lit_data
*       SIZE    = 0
      RECEIVING
        retcode = lit_retcode.
* render_2_xstring
    CALL METHOD lo_xml->render_2_xstring
      IMPORTING
        retcode = lv_subrc
        stream  = lv_xml_string
        size    = lv_size.
    IF lv_xml_string IS INITIAL.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid  = /iwbep/cx_mgw_busi_exception=>business_error
          message = 'Render to XString Failed'.
    ENDIF.

    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        xml_input = lv_xml_string
      TABLES
        xml_table = lit_fomrsp
        return    = lit_return.

                                                                               "/ Call method to get all the questions, groups and subgroups
    CALL METHOD gmib_get_all_questions
      EXPORTING
        im_formid    = ls_forsp-formid                                         " ODS Form ID
        im_version   = ls_forsp-version                                        " ODS Version
      IMPORTING
        ex_form_data = lt_response.

                                                                               "/ Getting all the response data for all questions
    LOOP AT lit_fomrsp INTO lst_fomrsp WHERE type = 'V'.
      lv_qid = lst_fomrsp-cname.
      READ TABLE lt_response INTO ls_response WITH KEY question_id = lv_qid.
      IF sy-subrc <> 0.
        READ TABLE lt_response INTO ls_response WITH KEY question = lv_qid.
      ENDIF.                                                                   " IF SY-SUBRC <> 0 Line No. :159
      IF ls_response IS NOT INITIAL.
        lst_final-instanceid     = ls_forsp-instanceid.
        lst_final-formid         = ls_forsp-formid.
        lst_final-version        = ls_forsp-version.
        lst_final-wo_num         = ls_forsp-wo_num.
        lst_final-vornr          = ls_forsp-vornr.
        lst_final-equnr          = ls_forsp-equnr.
        lst_final-tplnr          = ls_forsp-tplnr.
        lst_final-created_date   = ls_forsp-created_date.
        lst_final-created_by     = ls_forsp-created_by.
        lst_final-group          = ls_response-pre_group_name.
        lst_final-repeat_group   = ls_response-repeat_group_name.
        lst_final-sub_group      = ls_response-group_name.
        lst_final-question_id    = ls_response-question_id.
        lst_final-question_index = ls_response-question_index.
        lst_final-questionname   = ls_response-question.
        lst_final-responsedata   = lst_fomrsp-cvalue.
        APPEND lst_final TO ex_response_data.
        CLEAR: lst_final, ls_response, lst_fomrsp, ls_forsp.
      ENDIF.                                                                   " IF LS_RESPONSE IS NOT INITIAL Line No. :162
    ENDLOOP.                                                                   " LOOP AT LIT_FOMRSP Line No. :156

  ENDMETHOD.
ENDCLASS.
