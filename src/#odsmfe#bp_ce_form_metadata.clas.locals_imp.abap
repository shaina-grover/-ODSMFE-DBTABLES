CLASS lhc_FormMetadata DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PUBLIC SECTION.

    "/Tables and Structures
    DATA: lst_response TYPE /odsmfe/tb_forsp,
          "lst_formmetadata TYPE ltys_formmetadata,
          lit_response TYPE TABLE OF  /odsmfe/tb_forsp.

    " Variables
    DATA: lv_cdate TYPE sy-datlo,
          lv_ctime TYPE sy-timlo.



  PRIVATE SECTION.

    METHODS modify FOR BEHAVIOR IMPORTING
                                  roots_to_create FOR CREATE FormMetadata.
    METHODS read FOR READ
      IMPORTING lit_formmetadata FOR READ FormMetadata RESULT ex_formmetadata.
ENDCLASS.

CLASS lhc_FormMetadata IMPLEMENTATION.

  METHOD modify.

    "/ ---------------Handle Create Method-----------------------/"
    IF roots_to_create IS NOT INITIAL.
      LOOP AT roots_to_create INTO DATA(lst_formmetadata).
        lst_response-instanceid = lst_formmetadata-InstanceId.
        lst_response-formid     = lst_formmetadata-FormID.
        lst_response-version    = lst_formmetadata-Version.
        lst_response-responsedata = lst_formmetadata-ResponseData.
        lst_response-wo_num       = lst_formmetadata-WoNum.
        lst_response-multiple_sub      = lst_formmetadata-MultipleSub.
        lst_response-created_on  = lst_formmetadata-CreatedOn.
        lst_response-created_by =  lst_formmetadata-CreatedBy.
        lst_response-formhtml =  lst_formmetadata-FormHtml.
        lst_response-formmodel =  lst_formmetadata-FormModel.
        lst_response-isdraft  = lst_formmetadata-IsDraft.
        APPEND lst_response TO lit_response.
        CLEAR: lst_response.
      ENDLOOP.

      IF lst_formmetadata-CreatedOn IS NOT INITIAL.

        cl_abap_tstmp=>systemtstmp_utc2syst(
          EXPORTING
            utc_tstmp = lst_formmetadata-CreatedOn
          IMPORTING
            syst_date = lv_cdate
            syst_time = lv_ctime
        ).
        IF sy-subrc = 0.

          lst_response-created_date  = lv_cdate.
          lst_response-created_time  = lv_ctime.

        ENDIF.
*
      ENDIF.
      IF lit_response[] IS NOT INITIAL.
        MODIFY /odsmfe/tb_forsp FROM TABLE @lit_response[].
      ENDIF.

    ENDIF. "/if roots_to_create IS NOT INITIAL.

  ENDMETHOD.
  METHOD read.
  ENDMETHOD.

ENDCLASS.
