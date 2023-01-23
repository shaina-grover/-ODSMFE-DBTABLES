CLASS /odsmfe/cl_form_data_parse DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gtys_form_data,
        group       TYPE string,
        sub_group   TYPE string,
        label       TYPE string,
        question_id TYPE string,
        question    TYPE string,
      END OF gtys_form_data,

      gtyt_form_data TYPE STANDARD TABLE OF gtys_form_data.

    METHODS gmib_get_all_questions
      IMPORTING
        !im_formid    TYPE /odsmfe/de_formid
        !im_version   TYPE /odsmfe/de_version OPTIONAL
      EXPORTING
        !ex_form_data TYPE gtyt_form_data.
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_FORM_DATA_PARSE IMPLEMENTATION.


METHOD gmib_get_all_questions.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
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
  TYPES: BEGIN OF ltys_field,
           group       TYPE string,
           label       TYPE string,
           question_id TYPE string,
         END OF ltys_field.

  "/Tables and Structures
  DATA: lst_fomst       TYPE /odsmfe/tb_fomst,
        lit_html        TYPE STANDARD TABLE OF smum_xmltb,
        lst_html        TYPE smum_xmltb,
        lit_return      TYPE STANDARD TABLE OF bapiret2,
        lit_field       TYPE STANDARD TABLE OF ltys_field,
        lst_field       TYPE ltys_field,
        lit_model       TYPE STANDARD TABLE OF smum_xmltb,
        lst_model       TYPE smum_xmltb,
        lst_question_id TYPE gtys_form_data.

  "/Range Tables and Range structures
  DATA: lrs_version TYPE /odsmfe/st_core_range_str,
        lrt_version TYPE /odsmfe/tt_core_range_tab.

  "/Variables
  DATA: lv_xml_string TYPE xstring,
        lv_num        TYPE i,
        lv_group      TYPE string,
        lv_index      TYPE syst_tabix,
        lv_formid     TYPE /odsmfe/tb_fomst-formid.

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

  CLEAR lv_xml_string.
  lv_xml_string = lst_fomst-formmodel.

  CALL FUNCTION 'SMUM_XML_PARSE'
    EXPORTING
      xml_input = lv_xml_string
    TABLES
      xml_table = lit_model
      return    = lit_return.

  CLEAR: lv_xml_string, lit_field.

  LOOP AT lit_html INTO lst_html.
    lv_index = sy-tabix.
    IF lst_html-cvalue CS 'or-group' AND lst_html-type = 'A'.
      lv_num = sy-tabix + 1.
      READ TABLE lit_html INTO lst_html INDEX  lv_num.
      IF lst_html-cname = 'data-relevant'.
        lv_num  = lv_num + 3.
        DATA(lv_num1) = lv_num + 2.
      ELSEIF lst_html-cname = 'name'.
        lv_num  = lv_num + 2.
        lv_num1 = lv_num + 2.
      ENDIF.
      READ TABLE lit_html INTO lst_html INDEX lv_num.
      IF lst_html-cvalue CS 'or-repeat'.
        CONTINUE.
      ELSE.
        lst_field-group = lst_html-cvalue.
        READ TABLE lit_html INTO lst_html INDEX lv_num1.
        lst_field-label = lst_html-cvalue.
        SPLIT lst_html-cvalue AT '/' INTO TABLE DATA(lit_label).
        DESCRIBE TABLE lit_label LINES DATA(lv_lines).
        READ TABLE lit_label INTO DATA(lst_label) INDEX lv_lines.
        DATA(lv_strlen) = strlen( lst_label ).
        lv_strlen = lv_strlen - 6.
        lst_label = lst_label(lv_strlen).
        lst_field-question_id = lst_label.
        APPEND lst_field TO lit_field.
        CLEAR: lst_field, lst_html, lst_label, lv_strlen, lit_label, lv_lines .
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT lit_html INTO lst_html.
    lv_index = sy-tabix.
    IF lst_html-cname = 'data-itext-id'.
      DATA(lv_label) = lst_html-cvalue.
      lv_index = lv_index - 4.
      READ TABLE lit_html INTO lst_html INDEX lv_index.
      IF lst_html-cname = 'type'.
        CONTINUE.
      ELSE.
        lv_index = lv_index + 2.
        lst_question_id-label = lv_label.
        SPLIT lv_label AT '/' INTO TABLE lit_label.
        DESCRIBE TABLE lit_label LINES lv_lines.

        READ TABLE lit_label INTO lst_label INDEX lv_lines.
        lv_strlen = strlen( lst_label ).
        lv_strlen = lv_strlen - 6.
        lst_label = lst_label(lv_strlen).
        lst_question_id-question_id = lst_label.

        READ TABLE lit_field INTO lst_field WITH KEY question_id = lst_label.
        IF sy-subrc = 0.
          lv_lines = lv_lines - 1.
        ELSE.
          lv_lines = lv_lines - 2.
        ENDIF.

        READ TABLE lit_label INTO lst_label INDEX lv_lines.
        IF sy-subrc = 0.
          READ TABLE lit_field INTO lst_field WITH KEY question_id = lst_label.
          CLEAR lv_group.

          lst_question_id-group = lst_field-group.
          lv_group = lst_field-group.
        ELSE.
          lst_question_id-group = lv_group.
        ENDIF.

        READ TABLE lit_html INTO lst_html INDEX lv_index.
        lst_question_id-question = lst_html-cvalue.

        CLEAR lst_field.
        READ TABLE lit_field INTO lst_field WITH KEY label = lst_question_id-label.
        IF sy-subrc = 0.
          lst_question_id-sub_group = lst_field-group.
        ELSE.
          lv_lines = lv_lines + 1.
          READ TABLE lit_label INTO lst_label INDEX lv_lines.
          IF sy-subrc = 0.
            IF lst_label CS '-label'.
              lv_strlen = strlen( lst_label ).
              lv_strlen = lv_strlen - 6.
              lst_label = lst_label(lv_strlen).
            ENDIF.
            READ TABLE lit_field INTO lst_field WITH KEY question_id = lst_label.
            lst_question_id-sub_group = lst_field-group.
          ENDIF.
        ENDIF.

        APPEND lst_question_id TO ex_form_data.
        CLEAR: lst_question_id.
      ENDIF.
    ENDIF.
    CLEAR: lst_html, lv_index, lst_html, lst_field.
  ENDLOOP.

ENDMETHOD.
ENDCLASS.
