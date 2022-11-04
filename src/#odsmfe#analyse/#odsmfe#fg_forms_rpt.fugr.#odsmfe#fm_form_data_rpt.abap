FUNCTION /odsmfe/fm_form_data_rpt.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_FORMID) TYPE  /ODSMFE/DE_FORMID OPTIONAL
*"     REFERENCE(IM_UNAME) TYPE  /ODSMFE/DE_CREATEDBY OPTIONAL
*"     REFERENCE(IM_VERSION) TYPE  /ODSMFE/DE_VERSION OPTIONAL
*"     REFERENCE(IM_DATE) TYPE  /ODSMFE/CORE_RANGE_TAB
*"----------------------------------------------------------------------
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*

  DATA:
    lt_filter_val TYPE /ODSMFE/CORE_RANGE_TAB,
    ls_filter_val TYPE /ODSMFE/CORE_RANGE_STR.                        "Generic all purpose Range Structure

  IF im_version IS NOT INITIAL.
    ls_filter_val-sign   = 'I'.
    ls_filter_val-option = 'EQ'.
    ls_filter_val-low    = im_version.
    APPEND ls_filter_val TO lt_filter_val.
  ENDIF.

  SUBMIT /odsmfe/rpt_form_analytics WITH p_forms EQ im_formid
                      WITH p_user EQ im_uname
                      WITH p_date IN gt_date
                      WITH p_version IN lt_filter_val AND RETURN.

ENDFUNCTION.
