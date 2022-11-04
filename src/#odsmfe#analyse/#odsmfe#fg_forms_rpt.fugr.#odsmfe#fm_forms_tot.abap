FUNCTION /odsmfe/fm_forms_tot.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_AUFNR) TYPE  AUFNR OPTIONAL
*"     REFERENCE(IM_AUART) TYPE  AUFART OPTIONAL
*"     REFERENCE(IM_UNAME) TYPE  /ODSMFE/DE_CREATEDBY OPTIONAL
*"     REFERENCE(IM_FORMID) TYPE  /ODSMFE/DE_FORMID OPTIONAL
*"     REFERENCE(IM_VERSION) TYPE  /ODSMFE/DE_VERSION
*"     REFERENCE(IM_DATE) TYPE  /ODSMFE/CORE_RANGE_TAB
*"----------------------------------------------------------------------

* Iniiate controller
  CREATE OBJECT go_control.
*
* Get the object from Control
  CALL METHOD go_control->gmib_get_object
    EXPORTING
      im_name = '/ODSMFE/CL_MODEL_RPT'.

  gv_formid  = im_formid.
  gv_uname   = im_uname.
  gv_version = im_version.
  gt_date    = im_date.


  CALL METHOD go_control->gvib_model->gmib_get_data
    EXPORTING
      im_aufnr   = im_aufnr
      im_auart   = im_auart
      im_uname   = im_uname
      im_version = im_version
      im_formid  = im_formid
      im_date    = im_date.                                            "'Customer_Feedback' ."im_formid.


  IF go_control->gvib_model->gitib_foass IS INITIAL.
    MESSAGE 'No Data Found!' TYPE 'I'.
    EXIT.
  ENDIF.
  CALL SCREEN 300.                                                     " ENDING   AT 160  20.
ENDFUNCTION.
