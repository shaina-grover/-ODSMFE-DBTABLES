*&---------------------------------------------------------------------*
*&  Include  /ODSMFE/ANALYTICS_EXCEL_TOP
*&---------------------------------------------------------------------*
INCLUDE ole2incl .
"/ Type-pools
TYPE-POOLS : abap, slis.

"/ Types
TYPES:
  BEGIN OF gtys_form,
    instanceid   TYPE /odsmfe/de_instanceid,                        "ODS MFE InstanceId
    formid       TYPE /odsmfe/de_formid,                            "ODS Form ID
    version      TYPE /odsmfe/de_version,                           "ODS Version
    wo_num       TYPE aufnr,                                        "Order Number
    vornr        TYPE vornr,                                        "Operation/Activity Number
    equnr        TYPE equnr,                                        "Equipment Number
    tplnr        TYPE tplnr,                                        "Functional Location
    created_date TYPE dats,                                         "CreatedOn UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
    created_by   TYPE /odsmfe/de_createdby,                         "ODS Created By
  END OF gtys_form,

  BEGIN OF gtys_form_data,
    lang              TYPE string,                                  "language
    label             TYPE string,                                  "label
    pre_group_id      TYPE string,                                  "pre group id
    group_id          TYPE string,                                  "group id
    repeat_group_id   TYPE string,                                  "repeat group id
    question_id       TYPE string,                                  "question id
    question_index    TYPE string,                                  "question index
    pre_group_name    TYPE string,                                  "pre group name
    group_name        TYPE string,                                  "group name
    repeat_group_name TYPE string,                                  "repeat group name
    question          TYPE string,                                  "question
  END OF gtys_form_data,

  BEGIN OF gtys_response_data,
    formid         TYPE /odsmfe/de_formid,                            "ODS Form ID
    version        TYPE /odsmfe/de_version,                           "ODS Version
    instanceid     TYPE /odsmfe/de_instanceid,                        "ODS MFE InstanceId
    wo_num         TYPE aufnr,                                        "Order Number
    vornr          TYPE vornr,                                        "Operation/Activity Number
    equnr          TYPE equnr,                                        "Equipment Number
    tplnr          TYPE tplnr,                                        "Functional Location
    created_date   TYPE dats,                                         "CreatedOn UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
    created_by     TYPE /odsmfe/de_createdby,                         "ODS Created By
    group          TYPE string,                                       "group
    repeat_group   TYPE string,                                       "repeat group
    sub_group      TYPE string,                                       "sub group
    question_id    TYPE string,                                       "question id
    question_index TYPE string,                                       "question index
    questionname   TYPE string,                                       "question name
    responsedata   TYPE string,                                       "response data
  END OF gtys_response_data,

  BEGIN OF gtys_ques,
    checkbox          TYPE char1,                                     "checkbox
    formid            TYPE /odsmfe/de_formid,                         "ODS Form ID
    version           TYPE /odsmfe/de_version,                        "ODS Version
    pre_group_id      TYPE string,                                    "pre group id
    group_id          TYPE string,                                    "group id
    repeat_group_id   TYPE string,                                    "repeat group id
    question_id       TYPE string,                                    "question id
    question_index    TYPE string,                                    "question index
    pre_group_name    TYPE string,                                    "pre group name
    group_name        TYPE string,                                    "group name
    repeat_group_name TYPE string,                                    "repeat group name
    question          TYPE string,                                    "question
  END OF gtys_ques.

TYPES : BEGIN OF gtys_ques2,
          checkbox          TYPE char1,                             "checkbox
          formid            TYPE /odsmfe/de_formid,                 "ODS Form ID
          version           TYPE /odsmfe/de_version,                "ODS Version
          pre_group_id      TYPE string,                            "pre group id
          group_id          TYPE string,                            "group id
          repeat_group_id   TYPE string,                            "repeat group id
          question_id       TYPE string,                            "question id
          question_index    TYPE string,                            "question index
          pre_group_name    TYPE string,                            "pre group name
          group_name        TYPE string,                            "group name
          repeat_group_name TYPE string,                            "repeat group name
          question          TYPE string,                            "question
        END OF gtys_ques2.

TYPES : BEGIN OF gtys_afih,
          aufnr TYPE afih-aufnr,
          equnr TYPE afih-equnr,
          iloan TYPE afih-iloan,
          qmnum TYPE afih-qmnum,
        END OF gtys_afih.

TYPES : BEGIN OF gtys_iloa,
          iloan TYPE iloa-iloan,
          tplnr TYPE iloa-tplnr,
        END OF gtys_iloa.

TYPES: gtys_data(5000) TYPE c,
       gtys_ty         TYPE TABLE OF gtys_data.

TYPES: BEGIN OF gtys_types,
         colname(255) TYPE c,
       END OF gtys_types.

TYPES: gtys_conc TYPE string,
       gtyt_conc TYPE TABLE OF gtys_conc.

TYPES: gtys_con TYPE string,
       gtyt_con TYPE TABLE OF gtys_con.


TYPES : BEGIN OF gtys_fomst,
          formid      TYPE /odsmfe/tb_fomst-formid,
          version     TYPE /odsmfe/tb_fomst-version,
          description TYPE /odsmfe/tb_fomst-description,
          formhtml    TYPE /odsmfe/tb_fomst-formhtml,
          formmodel   TYPE /odsmfe/tb_fomst-formmodel,
          created_on  TYPE /odsmfe/tb_fomst-created_on,
          created_by  TYPE /odsmfe/tb_fomst-created_by,
          modified_on TYPE /odsmfe/tb_fomst-modified_on,
          modified_by TYPE /odsmfe/tb_fomst-modified_by,
          plant       TYPE /odsmfe/tb_fomst-plant,
          funareaid   TYPE /odsmfe/tb_fomst-funareaid,
          subareaid   TYPE /odsmfe/tb_fomst-subareaid,
        END OF gtys_fomst.
DATA : git_fomst TYPE TABLE OF gtys_fomst,                        "internal table for /odsmfe/tb_fomst
       gst_fomst TYPE gtys_fomst.                                 "Structure type for /odsmfe/tb_fomst

DATA: gst_excel       TYPE ole2_object,                                       "oleobject
      gst_wbooklist   TYPE ole2_object,                                       "oleobject
      gst_application TYPE ole2_object,                                       "oleobject
      gst_wbook       TYPE ole2_object,                                       "oleobject
      gst_activesheet TYPE ole2_object,                                       "oleobject
      gst_sheets      TYPE ole2_object,                                       "oleobject
      gst_newsheet    TYPE ole2_object,                                       "oleobject
      gst_column1     TYPE ole2_object,                                       "oleobject
      gst_border      TYPE ole2_object,                                       "oleobject
      gst_row1        TYPE ole2_object,                                       "oleobject
      gst_font        TYPE ole2_object,                                       "oleobject
      gst_interior    TYPE ole2_object.                                       "oleobject

DATA : gv_sheet_name(20) TYPE c,                                                "sheet_name
       gv_outer_index    LIKE sy-index,                                         "inder_num
       gv_intex(2)       TYPE c,                                                "char
       gv_line_cntr      TYPE i ,                                               "line counter
       gv_linno          TYPE i ,                                               "line number
       gv_colno          TYPE i ,                                               "column number
       gv_value          TYPE i ,                                               "data
       gv_rc             TYPE i.                                                "int

DATA : git_question2 TYPE TABLE OF gtys_ques2,                                  "internal table for gtys_ques2
       gst_question2 TYPE gtys_ques2,                                           "Structure type for gtys_ques2
       git_question3 TYPE TABLE OF gtys_ques2,                                  "internal table for gtys_ques2
       gst_question3 TYPE gtys_ques2.                                           "Structure type for gtys_ques2

"/ Tables and Structures
DATA: git_lvc_field      TYPE lvc_t_fcat,                                       "ALV control: Field catalog
      gst_lvc_field      TYPE lvc_s_fcat,                                       "ALV control: Field catalog
      git_form           TYPE STANDARD TABLE OF gtys_form,                      "internal table for gtys_form
      git_formrsp        TYPE STANDARD TABLE OF /odsmfe/tb_forsp,               "Table to Capture Response
      gst_formrsp        TYPE /odsmfe/tb_forsp,                                 "FORM Master Table
      gst_componentdescr TYPE abap_componentdescr,                              "Structure table for abap_componentdescr
      git_question       TYPE STANDARD TABLE OF gtys_form_data,                 "internal table for gtys_form_data
      git_question_grp   TYPE STANDARD TABLE OF gtys_form_data,                 "internal table for gtys_form_data
      gst_question_grp   TYPE gtys_form_data,                                   "Structure type for gtys_form_data
      gst_question       TYPE gtys_form_data,                                   "Structure type for gtys_form_data
      git_responce       TYPE STANDARD TABLE OF gtys_response_data,             "internal table for gtys_response_data
      git_responce1      TYPE STANDARD TABLE OF gtys_response_data,             "internal table for gtys_response_data
      gst_responce       TYPE gtys_response_data,                               "Structure type for gtys_response_data
      gst_responce1      TYPE gtys_response_data,                               "Structure type for gtys_response_data
      git_question1      TYPE TABLE OF gtys_ques,                               "internal table for gtys_ques
      gst_question1      TYPE gtys_ques,                                        "Structure type for gtys_ques
      gst_fieldcat       TYPE slis_fieldcat_alv,                                "Structure type for slis_fieldcat_alv
      git_fieldcat       TYPE STANDARD TABLE OF slis_fieldcat_alv.              "internal table for slis_fieldcat_alv
*      git_fomst          TYPE TABLE OF /odsmfe/tb_fomst,                        "internal table for /odsmfe/tb_fomst
*      gst_fomst          TYPE /odsmfe/tb_fomst.                                 "Structure type for /odsmfe/tb_fomst

"/ Variables
DATA: gv_user        TYPE user02,                                               "User Data for Logon
      gv_formid1     TYPE /odsmfe/tb_forsp,                                     "ODSMFE: formid
      gv_formid      TYPE char20 VALUE 'INSTANCEID',                            "char value
      gv_date        TYPE sy-datlo,                                             "ABAP System Field: Local Date of Current User
      gv_date1       TYPE sy-datlo,                                             "ABAP System Field: Local Date of Current User
      gv_question_id TYPE char30,                                               "char value
      gv_index       TYPE string,                                               "string
      gv_a           TYPE REF TO /odsmfe/cl_form_data_parse,                    "Form Data Parsing
      gv_val         TYPE string,                                               "string
      gv_no          TYPE char10.                                               "char value

"/ Reference Variables
DATA: go_table     TYPE REF TO cl_abap_tabledescr,                              "Run Time Type Services
      go_struct    TYPE REF TO cl_abap_structdescr,                             "Run Time Type Services
      go_ref_final TYPE REF TO data.                                            "data class

DATA : go_itab      TYPE REF TO cl_abap_tabledescr,                             "Class reference for cl_abap_tabledescr
       git_colnames TYPE STANDARD TABLE OF gtys_types.                          "internal table for gtys_types

DATA : git_colnames1 TYPE STANDARD TABLE OF gtys_types.                         "internal table for gtys_types

DATA: gv_conc  TYPE string,                                                     "string
      gv_conc1 TYPE string,                                                     "string
      gv_conc2 TYPE string.                                                     "string

DATA: gst_conc      TYPE gtys_conc,                                              "Structure type for gtys_conc
      git_conc      TYPE TABLE OF gtys_conc,                                     "internal table for gtys_conc
      gv_created_on TYPE string.                                                 "string

DATA : gv_program TYPE sy-repid.                                                 "Report name

TYPES : BEGIN OF gtys_hex,
          l_tab TYPE x,
        END OF gtys_hex.

DATA gst_hex TYPE gtys_hex.
*DATA: BEGIN OF gtys_hex,
*        l_tab TYPE x,
*      END OF gtys_hex.

DATA: gv_deli(1) TYPE c.                                                          "char value

DATA: gv_con TYPE string.                                                         "string

DATA: git_con TYPE gtyt_con.                                                      "internal table for gtyt_con


DATA: gst_concatenate TYPE gtys_data,                                             "Structure type for gtys_data
      git_concatenate TYPE gtys_ty.                                               "internal table for gtys_ty

DATA: gv_group_name   TYPE string,                                                "string
      grs_group_name  TYPE /odsmfe/st_core_range_str,                             "ODS MFE: Filter Purpose Range Structure
      grs_responce    TYPE /odsmfe/st_core_range_str,                             "ODS MFE: Filter Purpose Range Structure
      grt_group_name  TYPE TABLE OF /odsmfe/st_core_range_str,                    "ODS MFE: Filter Purpose Range Structure
      grt_responce    TYPE TABLE OF /odsmfe/st_core_range_str,                    "ODS MFE: Filter Purpose Range Structure
      grt_group_name1 TYPE TABLE OF /odsmfe/st_core_range_str.                    "ODS MFE: Filter Purpose Range Structure

DATA : git_afih TYPE TABLE OF gtys_afih,                                          "internal table for gtys_afih
       gst_afih TYPE gtys_afih.                                                   "Structure type for gtys_afih

DATA : git_iloa TYPE TABLE OF gtys_iloa,                                          "internal table for gtys_iloa
       gst_iloa TYPE gtys_iloa.                                                   "Structure type for gtys_iloa

DATA gv_fieldname TYPE string.                                                    "string
DATA go_line TYPE REF TO cl_abap_typedescr.                                       "type ref to cl_abap_typedescr

DATA : go_table1  TYPE REF TO cl_abap_tabledescr,                                 "type ref to cl_abap_tabledescr
       go_struct1 TYPE REF TO cl_abap_structdescr.                                "type ref to cl_abap_tabledescr

DATA : gv_lines  TYPE i,                                                          "int value
       gv_index1 TYPE i,                                                          "int value
       gv_msg1   TYPE string,                                                     "string
       gv_msg2   TYPE string.                                                     "string

DATA : gst_form TYPE abap_component_tab.

CONSTANTS gc_c09(2) TYPE n VALUE 09.                                              "constants

"/ Field symbols
FIELD-SYMBOLS: <gfsst_formrsp> TYPE /odsmfe/tb_forsp,                             "dynamic internal table for Table to Capture Response
               <gfsit_form>    TYPE STANDARD TABLE,                               "dynamic internal
               <gfsst_form>    TYPE any,                                          "dynamic structure
               <gfsst_field>   TYPE any.                                          "dynamic structure

FIELD-SYMBOLS : <gfsst_table> TYPE STANDARD TABLE,                                "dynamic internal
                <gfsst_comp>  LIKE LINE OF go_struct->components.                 "dynamic structure for cl_abap_structdescr

FIELD-SYMBOLS: <gfsst_x> TYPE any.                                                          "dynamic structure

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*
* -----------------------------------------------------------------------*
*                       M A I N    S E C T I O N                         *
* -----------------------------------------------------------------------*
