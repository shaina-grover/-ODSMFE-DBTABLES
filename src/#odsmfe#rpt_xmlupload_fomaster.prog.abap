REPORT /odsmfe/rpt_xmlupload_fomaster NO STANDARD PAGE HEADING MESSAGE-ID /odsmfe/myjobcard LINE-COUNT 15(5) LINE-SIZE 256.
********************** CREATED HISTORY **********************
* Program Author (SID)   : Srinivas Kammari
* Creation Date          : 13/2/2020
* Transport No.          : ES1K901528
* Program Description    : This Program is used for Dynamic Form Engine
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Change Date            : 15/12/2020
* Transport No.          : ES1K902363
* Change Description     : Added Task list assignment type.
***********************************************************************
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
TYPE-POOLS:abap,vrm,trwbo.
TABLES sscrfields.

INCLUDE /odsmfe/form_master_top.

INCLUDE /odsmfe/form_selection_screen.

INITIALIZATION.
  but1 = text-025. "Assignment.
  gst_list-key = '2'.
  gst_list-text = gc_workorder.
  APPEND gst_list TO git_list.
  gst_list-key = '3'.
  gst_list-text = gc_operation.
  APPEND gst_list TO git_list.
**  soc
  gst_list-key = '4'.
  gst_list-text = gc_equip.
  APPEND gst_list TO git_list.
  gst_list-key = '5'.
  gst_list-text = gc_floc.
  APPEND gst_list TO git_list.
**  eoc
* SOC by ODS ES1K902363
  gst_list-key = '6'.
  gst_list-text = gc_tasklist.
  APPEND gst_list TO git_list.
* EOC by ODS ES1K902363.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = gc_p_asinmt
      values          = git_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

START-OF-SELECTION .

  IF p_file IS INITIAL.
    MESSAGE text-021 TYPE gc_s DISPLAY LIKE gc_e.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF p_des IS INITIAL.
    MESSAGE text-022 TYPE gc_s DISPLAY LIKE gc_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

* SOC Sravan kumar "++ ES1K902842
* When Form Category is empty shows message
*  IF p_fmcat IS INITIAL.
*    MESSAGE text-027 TYPE gc_s DISPLAY LIKE gc_e.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
** When Functional Area ID is empty shows message
*  IF p_funare IS INITIAL.
*    MESSAGE text-028 TYPE gc_s DISPLAY LIKE gc_e.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
** When Sub Area ID is empty shows message
*  IF p_subare IS INITIAL.
*    MESSAGE text-029 TYPE gc_s DISPLAY LIKE gc_e.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
* EOC Sravan kumar "++ ES1K902842

  CREATE OBJECT go_ref_xml.
  CLEAR gv_filename .
  MOVE : p_file TO gv_filename .
  PERFORM /odsmfe/fo_load_file USING gv_filename .
  PERFORM /odsmfe/fo_save_file_contents .

END-OF-SELECTION.

  INCLUDE /odsmfe/form_master_f01.

  INCLUDE /odsmfe/form_master_parse.

  INCLUDE /odsmfe/form_master_codegrp.

  INCLUDE /odsmfe/form_master_tr.
