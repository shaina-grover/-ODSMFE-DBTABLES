CLASS lhc_myformsresponse DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.
        "/Tables and Structures
        DATA: lst_response   TYPE /odsmfe/tb_forsp.

        "/Variables
        DATA:    lv_cdate             TYPE sy-datlo,
                     lv_mdate            TYPE sy-datlo,
                    lv_ctime              TYPE sy-timlo,
                     lv_mtime            TYPE sy-timlo,
                     lv_deleted           TYPE c LENGTH 1,
                    lv_instanceid      TYPE /odsmfe/de_instanceid,
                    lv_modified_by   TYPE /odsmfe/de_modifiedby.

  PRIVATE SECTION.

        METHODS modify FOR behavior IMPORTING
        roots_to_create     FOR create myformsresponse
        roots_to_update    FOR update myformsresponse
        roots_to_delete     FOR delete  myformsresponse .

        METHODS read FOR READ
        IMPORTING lit_myformresp FOR READ myformsresponse RESULT ex_myformresp.

ENDCLASS.

CLASS lhc_myformsresponse IMPLEMENTATION.

  METHOD modify.

        "/**********************Handle Create Method**************************/"
        IF roots_to_create IS NOT INITIAL.

            LOOP AT roots_to_create INTO DATA(lst_to_create).

                IF lst_to_create IS NOT INITIAL.
                    IF lst_to_create-CreatedBy IS NOT INITIAL.
                        lst_response-created_by      = lst_to_create-CreatedBy.
                    ELSE.
                         lst_to_create-CreatedBy      = sy-uname.
                    ENDIF. "/IF lst_to_create-CreatedBy IS NOT INITIAL.
                    lst_response-instanceid               = lst_to_create-instanceid.
                    lst_response-formid                     = lst_to_create-formid.
                    lst_response-version                    = lst_to_create-version.
                    TRANSLATE lst_to_create-responsedata TO UPPER CASE.
                     lst_response-responsedata         = lst_to_create-responsedata.
                     lst_response-wo_num                 = lst_to_create-WoNum.
                     lst_response-vornr                      = lst_to_create-OperationNum.
                     lst_response-plnty                      = lst_to_create-TaskListType.
                     lst_response-plnnr                       = lst_to_create-Groups.
                     lst_response-plnal                       = lst_to_create-GroupCounter.
                     lst_response-zaehl                       = lst_to_create-InternalCounter.
                     lst_response-equnr                      = lst_to_create-Equipment.
                     lst_response-tplnr                        = lst_to_create-FunctionLocation.
                    lst_response-created_on             = lst_to_create-CreatedOn.
                    lst_response-modified_on            = lst_to_create-ModifiedOn.
                    TRANSLATE lst_to_create-ModifiedBy TO UPPER CASE.
                    lst_response-modified_by            = lst_to_create-ModifiedBy.
                    lst_response-nonobjtype             = lst_to_create-nonobjtype.
                   lst_response-counter                   = lst_to_create-counter.
                   lst_response-isdraft                    = lst_to_create-isdraft.      "For saving form in Draft
                    lst_response-remarks                 = lst_to_create-remarks.

                ENDIF. "/IF lst_to_create IS NOT INITIAL.

                IF lst_response-created_on IS NOT INITIAL.
                    cl_abap_tstmp=>systemtstmp_utc2syst(
                      EXPORTING
                        utc_tstmp = lst_response-created_on
                      IMPORTING
                        syst_date = lv_cdate
                        syst_time = lv_ctime ).

                    lst_response-created_date = lv_cdate.
                    lst_response-created_time = lv_ctime.
               ENDIF. "/ IF lst_response-created_on IS NOT INITIAL.
               CLEAR: lv_cdate, lv_ctime.

               IF lst_response-modified_on IS NOT INITIAL.
                    cl_abap_tstmp=>systemtstmp_utc2syst(
                    EXPORTING
                        utc_tstmp = lst_response-modified_on
                    IMPORTING
                        syst_date = lv_mdate ).

                  lst_response-modified_date = lv_mdate.
                  lst_response-modified_time = lv_mtime.
             ENDIF. "/if lst_response-modified_on IS NOT INITIAL.
             clear: lv_mdate, lv_mtime.

            ENDLOOP. "/LOOP AT roots_to_create INTO DATA(lst_to_create).

        ENDIF. "/IF roots_to_create IS NOT INITIAL.

        IF lst_response-instanceid IS NOT INITIAL.
            SELECT SINGLE instanceid
                FROM /odsmfe/tb_forsp
                WHERE instanceid = @lst_response-instanceid
                INTO @lv_instanceid.

            IF sy-subrc NE 0 AND lv_instanceid IS INITIAL.
                INSERT /odsmfe/tb_forsp FROM @lst_response.
                IF sy-subrc NE 0.
                    CLEAR: lst_response.
                ENDIF. "/IF sy-subrc NE 0.
            ELSE.
                UPDATE /odsmfe/tb_forsp FROM @lst_response.
                IF sy-subrc NE 0.
                    CLEAR: lst_response.
                ENDIF. "/IF sy-subrc NE 0.

            ENDIF. "/IF sy-subrc NE 0 AND lv_instanceid IS INITIAL.

        ENDIF. "/IF lst_response-instanceid IS NOT INITIAL.

        "/***********************Handle Modify Method***************************/"
        IF roots_to_update IS NOT INITIAL.

            LOOP AT roots_to_update INTO DATA(lst_to_update).

                IF lst_to_update IS NOT INITIAL.
                    IF lst_to_update-CreatedBy IS NOT INITIAL.
                        lst_response-created_by      = lst_to_update-CreatedBy.
                    ELSE.
                         lst_to_update-CreatedBy      = sy-uname.
                    ENDIF. "/IF lst_to_create-CreatedBy IS NOT INITIAL.
                    lst_response-instanceid               = lst_to_update-instanceid.
                    lst_response-formid                     = lst_to_update-formid.
                    lst_response-version                    = lst_to_update-version.
                    TRANSLATE lst_to_update-responsedata TO UPPER CASE.
                     lst_response-responsedata         = lst_to_update-responsedata.
                     lst_response-wo_num                 = lst_to_update-WoNum.
                     lst_response-vornr                      = lst_to_update-OperationNum.
                     lst_response-plnty                      = lst_to_update-TaskListType.
                     lst_response-plnnr                       = lst_to_update-Groups.
                     lst_response-plnal                       = lst_to_update-GroupCounter.
                     lst_response-zaehl                       = lst_to_update-InternalCounter.
                     lst_response-equnr                      = lst_to_update-Equipment.
                     lst_response-tplnr                        = lst_to_update-FunctionLocation.
                    lst_response-created_on             = lst_to_update-CreatedOn.
                    lst_response-modified_on            = lst_to_update-ModifiedOn.
                    TRANSLATE lst_to_update-ModifiedBy TO UPPER CASE.
                    lst_response-modified_by            = lst_to_update-ModifiedBy.
                    lst_response-nonobjtype             = lst_to_update-nonobjtype.
                   lst_response-counter                   = lst_to_update-counter.
                   lst_response-isdraft                    = lst_to_update-isdraft.      "For saving form in Draft
                    lst_response-remarks                 = lst_to_update-remarks.

                ENDIF. "/IF lst_to_update IS NOT INITIAL.

                IF lst_response-created_on IS NOT INITIAL.
                    cl_abap_tstmp=>systemtstmp_utc2syst(
                      EXPORTING
                        utc_tstmp = lst_response-created_on
                      IMPORTING
                        syst_date = lv_cdate
                        syst_time = lv_ctime ).

                    lst_response-created_date = lv_cdate.
                    lst_response-created_time = lv_ctime.
               ENDIF. "/ IF lst_response-created_on IS NOT INITIAL.
               CLEAR: lv_cdate, lv_ctime.

               IF lst_response-modified_on IS NOT INITIAL.
                    cl_abap_tstmp=>systemtstmp_utc2syst(
                    EXPORTING
                        utc_tstmp = lst_response-modified_on
                    IMPORTING
                        syst_date = lv_mdate ).

                  lst_response-modified_date = lv_mdate.
                  lst_response-modified_time = lv_mtime.
             ENDIF. "/if lst_response-modified_on IS NOT INITIAL.
             clear: lv_mdate, lv_mtime.

            ENDLOOP. "/LOOP AT roots_to_create INTO DATA(lst_to_create).

        ENDIF. "/IF roots_to_create IS NOT INITIAL.

        IF lst_response-instanceid IS NOT INITIAL.
            SELECT SINGLE instanceid
                FROM /odsmfe/tb_forsp
                WHERE instanceid = @lst_response-instanceid
                INTO @lv_instanceid.

            IF sy-subrc EQ 0 AND lv_instanceid IS NOT INITIAL.
                UPDATE /odsmfe/tb_forsp FROM @lst_response.
                IF sy-subrc NE 0.
                    CLEAR: lst_response.
                ENDIF. "/IF sy-subrc NE 0.

            ENDIF. "/IF sy-subrc NE 0 AND lv_instanceid IS NOT INITIAL.
            CLEAR: lv_instanceid.

        ENDIF. "/IF lst_response-instanceid IS NOT INITIAL.

        "/***********************Handle Delete Method*****************************/"
        IF roots_to_delete IS NOT INITIAL.

            LOOP AT roots_to_delete INTO DATA(lst_to_delete).
                IF lst_to_delete-InstanceID IS NOT INITIAL.
                    SELECT SINGLE instanceid
                        FROM /odsmfe/tb_forsp
                        WHERE instanceid = @lst_to_delete-InstanceID
                        INTO @lv_instanceid.

                    IF sy-subrc EQ 0 AND lv_instanceid IS NOT INITIAL.
                        lv_deleted = abap_true.
                        UPDATE /odsmfe/tb_forsp SET deleted = @lv_deleted
                        WHERE instanceid = @lv_instanceid.

                    ENDIF. "/IF sy-subrc EQ 0 AND lv_instanceid IS NOT INITIAL.

                ENDIF. "/IF lst_to_delete IS NOT INITIAL.

            ENDLOOP. "/LOOP AT roots_to_delete INTO DATA(lst_to_delete).

        ENDIF. "/ IF roots_to_delete IS NOT INITIAL.

  ENDMETHOD.


  METHOD read.
  ENDMETHOD.


ENDCLASS.
