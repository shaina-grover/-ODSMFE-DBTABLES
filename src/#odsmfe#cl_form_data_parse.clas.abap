CLASS /odsmfe/cl_form_data_parse DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gtys_response_data,
        formid         TYPE /odsmfe/de_formid,
        version        TYPE /odsmfe/de_version,
        instanceid     TYPE /odsmfe/de_instanceid,
        wo_num         TYPE aufnr,                    """""""""""""""""""""""""""""""""""""""""""""""""""
        vornr          TYPE vornr,                                        "Operation/Activity Number
        equnr          TYPE equnr,                                        "Equipment Number
        tplnr          TYPE tplnr,                                        "Functional Location
        created_date   TYPE dats,                                         "CreatedOn UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
        created_by     TYPE /odsmfe/de_createdby,
        group          TYPE string,
        sub_group      TYPE string,
        question_id    TYPE string,
        question_index TYPE string,
        questionname   TYPE string,
        responsedata   TYPE string,
      END OF gtys_response_data .
    TYPES:
      gtyt_response_data TYPE STANDARD TABLE OF gtys_response_data .
    TYPES:
      BEGIN OF gtys_form_data,
        lang           TYPE string,
        label          TYPE string,
        pre_group_id   TYPE string,
        group_id       TYPE string,
        question_id    TYPE string,
        question_index TYPE string,
        pre_group_name TYPE string,
        group_name     TYPE string,
        question       TYPE string,
      END OF gtys_form_data .
    TYPES:
      gtyt_form_data TYPE STANDARD TABLE OF gtys_form_data .

    METHODS gmib_get_all_questions
      IMPORTING
        !im_formid    TYPE /odsmfe/de_formid
        !im_version   TYPE /odsmfe/de_version OPTIONAL
      EXPORTING
        !ex_form_data TYPE gtyt_form_data .
    METHODS gmib_get_response_data
      IMPORTING
        !im_formid        TYPE /odsmfe/de_formid OPTIONAL
        !im_version       TYPE /odsmfe/de_version OPTIONAL
        !im_instanceid    TYPE /odsmfe/de_instanceid
      EXPORTING
        !ex_response_data TYPE gtyt_response_data .
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
           lang           TYPE string,
           label          TYPE string,
           pre_group_id   TYPE string,
           group_id       TYPE string,
           question_id    TYPE string,
           question_index TYPE string,
           pre_group_name TYPE string,
           group_name     TYPE string,
           question       TYPE string,
         END OF ltys_final.

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
        lit_final        TYPE STANDARD TABLE OF ltys_final.

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
    SPLIT lst_data_text_id-cvalue AT '/' INTO TABLE DATA(lit_label).
    DESCRIBE TABLE lit_label LINES DATA(lv_lines).

    READ TABLE lit_label INTO DATA(lst_label) INDEX lv_lines.
    DATA(lv_strlen) = strlen( lst_label ).

    SPLIT lst_label AT '-' INTO DATA(lv_que) DATA(lv_check_label).

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
    CLEAR: lst_question, lst_html1, lst_html.
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

    APPEND lst_final TO lit_final.
    CLEAR: lst_final, lst_group, lst_questions, lst_pre_group.
  ENDLOOP.

  IF lit_lang[] IS NOT INITIAL.
    DELETE lit_final WHERE lang NE 'en'.
  ENDIF. "/ IF lit_lang[] IS NOT INITIAL.

  LOOP AT lit_final ASSIGNING FIELD-SYMBOL(<lfsst_final>).
    lv_index = sy-tabix.
    CONDENSE lv_index.
    DATA(lv_name_strlen) = strlen( <lfsst_final>-question_id ).
    IF lv_name_strlen > 24.
      DATA(lv_fieldname) = <lfsst_final>-question_id(24).
    ELSE.
      lv_fieldname = <lfsst_final>-question_id.
    ENDIF.

    CONCATENATE lv_fieldname '_' lv_index INTO DATA(lv_ques_index).
    CONDENSE lv_ques_index.
    <lfsst_final>-question_index = lv_ques_index.
    CLEAR: lv_ques_index, lv_fieldname, lv_index, lv_name_strlen.
  ENDLOOP.

  ex_form_data[] = lit_final[].


ENDMETHOD.


  METHOD gmib_get_response_data.
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
    TYPES : BEGIN OF ltys_response,
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
              sub_group      TYPE string,
              question_id    TYPE string,
              question_index TYPE string,
              questionname   TYPE string,
              responsedata   TYPE string,
            END OF ltys_response.

    "/Variables
    DATA: lv_xml_string TYPE xstring,
          lit_fomrsp    TYPE TABLE OF smum_xmltb,
          lst_fomrsp    TYPE smum_xmltb,
          lit_return    TYPE STANDARD TABLE OF bapiret2,
          ls_return     TYPE bapiret2,
          lv_num        TYPE i,
          lv_instanceid TYPE /odsmfe/de_instanceid,
          lv_qid        TYPE string,
          lv_index      TYPE string.

    "/Range Tables and Range structures
    DATA: lrs_formid  TYPE /odsmfe/st_core_range_str,
          lrs_version TYPE /odsmfe/st_core_range_str,
          lrt_version TYPE /odsmfe/tt_core_range_tab,
          lrt_formid  TYPE /odsmfe/tt_core_range_tab.

    DATA: lt_forsp    TYPE STANDARD TABLE OF /odsmfe/tb_forsp,
          lt_response TYPE STANDARD TABLE OF gtys_form_data,
          ls_response TYPE  gtys_form_data,
          ls_forsp    TYPE /odsmfe/tb_forsp,
          lit_final   TYPE TABLE OF ltys_response,
          lst_final   TYPE ltys_response.

    "/Constants
    CONSTANTS: lc_i  TYPE char1 VALUE 'I',
               lc_eq TYPE char2 VALUE 'EQ'.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

    IF im_instanceid IS NOT INITIAL.
      lv_instanceid = im_instanceid.
    ENDIF.

    IF im_formid IS NOT INITIAL.
      lrs_formid-sign   = lc_i.
      lrs_formid-option = lc_eq.
      lrs_formid-low    = im_formid.
      APPEND lrs_formid TO lrt_formid.
      CLEAR: lrs_formid.
    ENDIF.

    IF im_version IS NOT INITIAL.
      lrs_version-sign   = lc_i.
      lrs_version-option = lc_eq.
      lrs_version-low    = im_version.
      APPEND lrs_version TO lrt_version.
      CLEAR lrs_version.
    ENDIF.

    "/ Fetching the data from Form Response table
    SELECT SINGLE *
      FROM /odsmfe/tb_forsp
      INTO ls_forsp
      WHERE instanceid = lv_instanceid
        AND formid  IN lrt_formid[]
        AND version IN lrt_version[].

    "/ FM converts hexadecimal into internal table
    lv_xml_string = ls_forsp-responsedata.
    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        xml_input = lv_xml_string
      TABLES
        xml_table = lit_fomrsp
        return    = lit_return.

    "/ Call method to get all the questions, groups and subgroups
    CALL METHOD gmib_get_all_questions
      EXPORTING
        im_formid    = ls_forsp-formid  " ODS Form ID
        im_version   = ls_forsp-version " ODS Version
      IMPORTING
        ex_form_data = lt_response.

    "/ Getting all the response data for all questions
    LOOP AT lit_fomrsp INTO lst_fomrsp WHERE type = 'V'.
      lv_qid = lst_fomrsp-cname.
      READ TABLE lt_response INTO ls_response WITH KEY question_id = lv_qid.
      IF sy-subrc <> 0.
        READ TABLE lt_response INTO ls_response WITH KEY question = lv_qid.
      ENDIF.
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
        lst_final-sub_group      = ls_response-group_name.
        lst_final-question_id    = ls_response-question_id.
        lst_final-question_index = ls_response-question_index.
        lst_final-questionname   = ls_response-question.
        lst_final-responsedata   = lst_fomrsp-cvalue.
        APPEND lst_final TO ex_response_data.
        CLEAR: lst_final, ls_response, lst_fomrsp, ls_forsp.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
