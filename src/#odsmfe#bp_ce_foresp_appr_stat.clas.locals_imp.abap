CLASS lhc_FormRespApprover DEFINITION INHERITING FROM cl_abap_behavior_handler.


  PUBLIC SECTION.
    "/Tables and Structures
    DATA: lst_formresponse TYPE /odsmfe/tb_finst,
          lst_form         TYPE /odsmfe/tb_finst,
          lit_form         TYPE TABLE OF /odsmfe/tb_finst.

    DATA: lv_aufnr          TYPE aufnr.
    DATA:
*     lv_formid     TYPE char50,
      lv_formid     TYPE /odsmfe/de_formid,
      lv_version    TYPE char3,
*     lv_forminstid TYPE char50,
      lv_formistid  TYPE /odsmfe/de_instanceid,
      lv_forminstid TYPE /odsmfe/de_instanceid,
      lv_approverid TYPE char40,
      lv_formsubby  TYPE char35,
      lv_counter    TYPE char12.

  PRIVATE SECTION.

    METHODS modify FOR BEHAVIOR IMPORTING
                                  roots_to_create FOR CREATE FormRespApprover
                                  roots_to_update FOR UPDATE FormRespApprover
                                  roots_to_delete FOR DELETE  FormRespApprover.


*        METHODS read FOR READ
*        IMPORTING lit_formapprover FOR READ FormRespApprover RESULT ex_formapprover.


    METHODS read FOR READ
      IMPORTING keys FOR READ FormRespApprover RESULT result.

ENDCLASS.

CLASS lhc_FormRespApprover IMPLEMENTATION.

  METHOD modify.

    "/ ---------------Handle Create Method-----------------------/"
    IF roots_to_create IS NOT INITIAL.
      LOOP AT roots_to_create INTO DATA(lst_formresponse).
        lst_form-formid            = lst_formresponse-formid.
        lst_form-version           = lst_formresponse-version.
        lst_form-forminstanceid    = lst_formresponse-forminstanceid.
        lst_form-approverid        = lst_formresponse-approverid.
        lst_form-formsubmittedby   = lst_formresponse-formsubmittedby.
        lst_form-counter           = lst_formresponse-counter.
        lst_form-formcontentstatus = lst_formresponse-formcontentstatus.
        lst_form-remarks           = lst_formresponse-remarks.
        lst_form-createddate       = lst_formresponse-createddate.
        lst_form-createdtime       = lst_formresponse-createdtime.
        lst_form-formname          = lst_formresponse-formname.
        lst_form-iterationrequired = lst_formresponse-iterationrequired.
      ENDLOOP.

      IF lst_formresponse-formid IS NOT INITIAL.
* Insert data to Table /ODSMFE/TB_FINST
        INSERT /odsmfe/tb_finst FROM @lst_form.
        IF sy-subrc <> 0.
          CLEAR lst_form.
        ENDIF.
      ENDIF.

* Iteration Required - Set the form response status to draft
      IF lst_formresponse-iterationrequired = abap_true.
        UPDATE /odsmfe/tb_forsp SET isdraft = @abap_true
                    WHERE instanceid = @lst_formresponse-forminstanceid.
      ENDIF.

    ENDIF.

* Update Delta table for WO - Exchange Mechanism

    SELECT SINGLE wo_num
           FROM /odsmfe/tb_forsp
           WHERE instanceid = @lst_formresponse-forminstanceid INTO @lv_aufnr.

    IF lv_aufnr IS NOT INITIAL.
*--Start of changes SKOTRA - ES1K902967
      DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ).
      IF lr_exchtab IS BOUND.
        lr_exchtab->exch_table_update( lv_aufnr ).
      ENDIF.

    ENDIF.

    "/ --------------Handle Update Method-----------------------/"
    IF roots_to_update IS NOT INITIAL.
      LOOP AT roots_to_update INTO DATA(lst_formresponse_update).
        lst_form-formid            = lst_formresponse_update-formid.
        lst_form-version           = lst_formresponse_update-version.
        lst_form-forminstanceid    = lst_formresponse_update-forminstanceid.
        lst_form-approverid        = lst_formresponse_update-approverid.
        lst_form-formsubmittedby   = lst_formresponse_update-formsubmittedby.
        lst_form-counter           = lst_formresponse_update-counter.
        lst_form-formcontentstatus = lst_formresponse_update-formcontentstatus.
        lst_form-remarks           = lst_formresponse_update-remarks.
        lst_form-createddate       = lst_formresponse_update-createddate.
        lst_form-createdtime       = lst_formresponse_update-createdtime.
        lst_form-formname          = lst_formresponse_update-formname.
        lst_form-iterationrequired = lst_formresponse_update-iterationrequired.
      ENDLOOP.
* Fetching data from table
      SELECT SINGLE formid, version, forminstanceid, approverid, formsubmittedby, counter
      FROM /odsmfe/tb_finst
      WHERE   formid          = @lst_formresponse_update-formid
      AND     version         = @lst_formresponse_update-version
      AND     forminstanceid  = @lst_formresponse_update-forminstanceid
      AND     approverid      = @lst_formresponse_update-approverid
      AND     formsubmittedby = @lst_formresponse_update-formsubmittedby
      AND     counter         = @lst_formresponse_update-counter INTO (@lv_formid,@lv_version,@lv_forminstid,@lv_approverid,@lv_formsubby, @lv_counter).

      IF lv_forminstid IS NOT INITIAL.
* Update data to Table /ODSMFE/TB_FINST
        MODIFY /odsmfe/tb_finst FROM @lst_form.
        IF sy-subrc <> 0.
          CLEAR lst_form.
        ENDIF.
      ENDIF.
* Iteration Required - Set the form response status to draft
      IF lst_formresponse-iterationrequired = abap_true.
        UPDATE /odsmfe/tb_forsp SET isdraft = @abap_true
                    WHERE instanceid = @lst_formresponse-forminstanceid.
      ENDIF.
    ENDIF.


* Update Delta table for WO - Exchange Mechanism

    SELECT SINGLE wo_num
           FROM /odsmfe/tb_forsp
*           INTO @lv_aufnr
           WHERE instanceid = @lst_formresponse-forminstanceid INTO @lv_aufnr.

    IF lv_aufnr IS NOT INITIAL.
*--Start of changes SKOTRA - ES1K902967
      DATA(lr_exchtab1) = NEW /odsmfe/cl_exchmechwo( ).
      IF lr_exchtab IS BOUND.
        lr_exchtab->exch_table_update( lv_aufnr ).
      ENDIF.
    ENDIF.


    IF roots_to_delete IS NOT INITIAL.
      LOOP AT roots_to_delete INTO DATA(lst_formresponse_delete).
        lst_form-formid            = lst_formresponse_delete-FormID.
        lst_form-version           = lst_formresponse_delete-Version.
        lst_form-forminstanceid    = lst_formresponse_delete-FormInstanceID.
        lst_form-approverid        = lst_formresponse_delete-ApproverID.
        lst_form-formsubmittedby   = lst_formresponse_delete-FormSubmittedBy.
        lst_form-counter           = lst_formresponse_delete-Counter.
      ENDLOOP.
 "Fetching data from ODS MFE: FormInstance Status Table------
      SELECT SINGLE * FROM /odsmfe/tb_finst
   WHERE   formid          = @lst_form-formid
   AND     version         = @lst_form-version
   AND     forminstanceid  = @lst_form-forminstanceid
   AND     approverid      = @lst_form-approverid
   AND     formsubmittedby = @lst_form-formsubmittedby
   AND     counter        = @lst_form-counter INTO @lst_form.

      IF sy-subrc EQ 0.
* Delete data to Table /ODSMFE/TB_FINST
        DELETE /odsmfe/tb_finst FROM  @lst_form..

      ENDIF. "/if roots_to_delete IS NOT INITIAL.
    ENDIF.
  ENDMETHOD.
  METHOD read.
  ENDMETHOD.


ENDCLASS.
