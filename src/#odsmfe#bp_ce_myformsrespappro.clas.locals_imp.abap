
*******************Create method************************

CLASS lhc_myformsrespapr DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PUBLIC SECTION.
    """/ Internal tables & Structures
    DATA:
      lst_form   TYPE /odsmfe/tb_finst,                                       "Structure for ODS MFE: FormInstance Status Table
      lst_myform TYPE /odsmfe/tb_finst,
      lst_form1  TYPE /odsmfe/tb_finst,                                       "Structure for Table to Capture Response
      lit_final  TYPE STANDARD TABLE OF /odsmfe/tb_finst.

    """/ Variables
    DATA : lv_instanceid TYPE /odsmfe/de_instanceid,
           lv_formid     TYPE c LENGTH 50,                                                    "ODS Form ID
           lv_version    TYPE c LENGTH 3,                                                     "ODS Version
           lv_forminstid TYPE c LENGTH 50,                                                    "ODS MFE InstanceId
           lv_approverid TYPE c LENGTH 40,                                                    "ODS MFE: Form Approver ID
           lv_formsubby  TYPE c LENGTH 35,                                                    "ODS MFE: Form Submitted BY
           lv_counter    TYPE c LENGTH 12,                                                    "ODS: Counter
           lv_date       TYPE  timestamp.                                    "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)


  PRIVATE SECTION.

    METHODS modify FOR BEHAVIOR IMPORTING
                                  roots_to_create FOR CREATE myformsrespapr
                                  roots_to_update FOR UPDATE myformsrespapr
                                  roots_to_delete FOR DELETE  myformsrespapr .


    METHODS read FOR READ
      IMPORTING lit_formapprover FOR READ myformsrespapr RESULT ex_myformsrespapr.

ENDCLASS.

CLASS lhc_myformsrespapr IMPLEMENTATION.
  METHOD modify.


*****logic for Create method******

    IF roots_to_create IS NOT INITIAL.
      LOOP AT roots_to_create INTO DATA(lst_formresponse).

        IF lst_formresponse IS NOT INITIAL.
          lst_form-formid            = lst_formresponse-FormID.
          lst_form-version           = lst_formresponse-Version.
          lst_form-forminstanceid    = lst_formresponse-FormInstanceID.
          lst_form-approverid        = lst_formresponse-ApproverID.
          lst_form-formsubmittedby   = lst_formresponse-FormSubmittedBy.
          lst_form-counter           = lst_formresponse-Counter.
          lst_form-formcontentstatus = lst_formresponse-FormContentStatus.
          lst_form-remarks           = lst_formresponse-Remarks.
          lst_form-createddate       = lst_formresponse-CreatedDate.
          lst_form-createdtime       = lst_formresponse-CreatedTime.
          lst_form-formname          = lst_formresponse-FormName.
          lst_form-iterationrequired = lst_formresponse-IterationRequired.
        ENDIF.
      ENDLOOP.


      "/ Insert requested data to table ODS MFE: FormInstance Status Table
      IF lst_form IS NOT INITIAL.
        INSERT /odsmfe/tb_finst FROM @lst_form.
        IF sy-subrc <> 0.
          CLEAR lst_form.
        ENDIF.
      ENDIF."/ IF sy-subrc = 0.
    ENDIF.


***************Update method***************

    IF roots_to_update IS NOT INITIAL.

      LOOP AT roots_to_update INTO DATA(lst_formresponse1).
        IF lst_formresponse1 IS NOT INITIAL.
          lst_form1-formid            = lst_formresponse1-FormID.
          lst_form1-version           = lst_formresponse1-Version.
          lst_form1-forminstanceid    = lst_formresponse1-FormInstanceID.
          lst_form1-approverid        = lst_formresponse1-ApproverID.
          lst_form1-formsubmittedby   = lst_formresponse1-FormSubmittedBy.
          lst_form1-counter           = lst_formresponse1-Counter.
          lst_form-formcontentstatus = lst_formresponse1-FormContentStatus.
          lst_form1-remarks           = lst_formresponse1-Remarks.
          lst_form1-createddate       = lst_formresponse1-CreatedDate.
          lst_form1-createdtime       = lst_formresponse1-CreatedTime.
          lst_form1-formname          = lst_formresponse1-FormName.
          lst_form1-iterationrequired = lst_formresponse1-IterationRequired.
        ENDIF.
      ENDLOOP.

      "/ Fetching data from table ODS MFE: FormInstance Status Table
      SELECT SINGLE formid ,version ,forminstanceid ,approverid, formsubmittedby ,counter
      FROM /odsmfe/tb_finst
      WHERE   formid          = @lst_formresponse1-FormID
      AND     version         = @lst_formresponse1-Version
      AND     forminstanceid  = @lst_formresponse1-FormInstanceID
      AND     approverid      = @lst_formresponse1-ApproverID
      AND     formsubmittedby = @lst_formresponse1-FormSubmittedBy
      AND     counter         = @lst_formresponse1-Counter INTO (@lv_formid,@lv_version,@lv_forminstid,@lv_approverid,@lv_formsubby, @lv_counter).

      IF lv_forminstid IS NOT INITIAL.
        "/ Update requested data into Form Approval table
        MODIFY /odsmfe/tb_finst FROM @lst_form.
        IF sy-subrc <> 0.
          CLEAR lst_form.
        ENDIF."/ IF sy-subrc <> 0.
      ENDIF."/ IF lv_forminstid IS NOT INITIAL.

      "/ Set the Form status to draft in Response Capture Table when the Form Instance is rejected
      IF lst_formresponse-iterationrequired = abap_true.
        GET TIME STAMP FIELD lv_date.
        IF sy-subrc = 0.

          UPDATE /odsmfe/tb_forsp SET isdraft = @abap_true,
                                      modified_on = @lv_date,
                                      modified_by = @sy-uname,
                                      modified_date = @sy-datum,
                                      modified_time = @sy-timlo
          WHERE instanceid = @lst_formresponse1-FormInstanceID.
        ENDIF."/ IF sy-subrc = 0.
        IF sy-subrc = 0.
          CLEAR lst_form.
        ENDIF.
      ENDIF."/ IF lst_formresponse-iterationrequired = abap_true.
    ENDIF."/ IF lst_formresponse IS NOT INITIAL.


************Delete method logic ***********************

    IF roots_to_delete IS NOT INITIAL.

      LOOP AT roots_to_delete INTO DATA(lst_myforms).
        lst_myform-formid            = lst_myforms-FormID.
        lst_myform-version           = lst_myforms-Version.
        lst_myform-forminstanceid    = lst_myforms-FormInstanceID.
        lst_myform-approverid        = lst_myforms-ApproverID.
        lst_myform-formsubmittedby   = lst_myforms-FormSubmittedBy.
        lst_myform-counter           = lst_myforms-Counter.
      ENDLOOP.

      SELECT SINGLE * FROM /odsmfe/tb_finst
                WHERE   formid  EQ @lst_myforms-FormID
             AND     version         EQ @lst_myforms-Version
             AND     forminstanceid  EQ @lst_myforms-FormInstanceID
             AND     approverid     EQ @lst_myforms-ApproverID
             AND     formsubmittedby EQ @lst_myforms-FormSubmittedBy
             AND     counter         EQ @lst_myforms-Counter INTO @lst_myform..

      IF sy-subrc EQ 0.
        "/ Delete requested data from Table ODS MFE: FormInstance Status Table
        DELETE /odsmfe/tb_finst FROM @lst_myform.
        IF sy-subrc = 0.
          CLEAR lst_myform.
        ENDIF.
      ENDIF."/ IF sy-subrc EQ 0.

    ENDIF.
  ENDMETHOD.
  METHOD read.
  ENDMETHOD.
ENDCLASS.
