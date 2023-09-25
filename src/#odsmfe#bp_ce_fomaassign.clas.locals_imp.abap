CLASS lhc_CE_FOMAASSIGN DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.

*------------------------Declaration for create method-----------------------*

        TYPES: BEGIN OF ltys_final,
             formid             TYPE  /odsmfe/de_formid,
             version            TYPE  /odsmfe/de_version,
             formassignmenttype TYPE  /odsmfe/de_objcat,
             workordernum       TYPE  aufnr,
             notification       TYPE  c LENGTH 12,
             oprnum             TYPE  c LENGTH 4,
             notificationitem   TYPE  c LENGTH 4,
             notificationtask   TYPE  c LENGTH 4,
             equipment          TYPE c LENGTH 18,
             functionallocation TYPE c LENGTH 30,
           END OF ltys_final.


         TYPES:  BEGIN OF ltys_roles,
             low  TYPE /odsmfe/de_mfe_low100,
             high TYPE /odsmfe/de_mfe_high,
           END OF ltys_roles .

    DATA: lst_form           TYPE /odsmfe/tb_fmass,                            "ODS : Form Manual Assignment Table
          lit_response       TYPE STANDARD TABLE OF /odsmfe/tb_fmass,          "ODS : Form Manual Assignment Table
          lit_final                 TYPE TABLE OF ltys_final,
          "lst_formassignment TYPE ltys_formassignment,                         "Structure for ltys_formassignment
          lv_roleid          TYPE /odsmfe/de_roleid.                           "Role ID
    DATA lst_usrroletab TYPE ltys_roles.

     CONSTANTS: lc_pfcg_role TYPE string VALUE 'PFCG_ROLE',                     "Filter Options: PFCG_ROLE
               lc_meth      TYPE string VALUE 'ROLE_ASSIGNMENT',               "Filter Options: ROLE_ASSIGNMENT
               lc_x         TYPE char1 VALUE 'X',                              "Single-Character Indicator
               lc_true      TYPE string VALUE 'TRUE'.                          "Filter Options: TRUE

    DATA :lv_pfcg_role TYPE /odsmfe/de_mfe_value,                              "ODS MFE: Parameter Value
          lv_class     TYPE /odsmfe/de_mfe_value,                              "ODS MFE: Parameter Value
          lo_auth      TYPE REF TO object,                                     "Authorization
          lv_user      TYPE uname,                                             "User Name
          lv_role      TYPE c length 40."agval.                                             "Agrfield


*------------------------End of declaration for create method-----------------------*


*------------------------Declaration for update method-----------------------*
    DATA: lv_formid             TYPE c LENGTH 50,                                   "Comment
          lv_version            TYPE char3,                                    "3-Byte field
          lv_formassignmenttype TYPE char40,                                   "Character field of length 40
          lv_workordernum       TYPE aufnr,                                    "Order Number
          lv_oprnum             TYPE c LENGTH 4,                                  "Operation/Activity Number
          lv_notification       TYPE c LENGTH 12,                                    "Notification No
          lv_notificationitem   TYPE n LENGTH 4,                                   "Item Number in Item Record
          lv_notificationtask   TYPE n LENGTH 4,                                    "Sequential Task Number
          lv_equipment          TYPE c LENGTH 18,                                     "Equipment Number
          lv_functionallocation TYPE c LENGTH 30.                                     "Functional Location


  PRIVATE SECTION.

    METHODS modify FOR BEHAVIOR IMPORTING
                                  roots_to_create FOR CREATE formassign
                                  roots_to_update FOR UPDATE formassign
                                  roots_to_delete FOR DELETE  formassign .

    METHODS read FOR READ
      IMPORTING lit_formassign FOR READ formassign  RESULT ex_formassign .

ENDCLASS.

CLASS lhc_CE_FOMAASSIGN IMPLEMENTATION.

  METHOD modify.

   IF roots_to_create IS NOT INITIAL.
      LOOP AT roots_to_create INTO DATA(lst_formassignment).
      lst_form-formid             = lst_formassignment-FormID.
      lst_form-version            = lst_formassignment-Version.
      lst_form-formassignmenttype = lst_formassignment-FormAssignmentType.
      lst_form-workordernum       = lst_formassignment-WorkOrderNum.
      lst_form-oprnum             = lst_formassignment-OprNum.
      lst_form-notification       = lst_formassignment-Notification.
      lst_form-notificationitem   = lst_formassignment-NotificationItem.
      lst_form-notificationtask   = lst_formassignment-NotificationTask.
      lst_form-equipment          = lst_formassignment-Equipment.
      lst_form-functionallocation = lst_formassignment-FunctionalLocation.
      lst_form-mandatory          = lst_formassignment-Mandatory.
      lst_form-multiplesub        = lst_formassignment-MultipleSub.
      lst_form-occur              = lst_formassignment-Occur.
      lst_form-category      = lst_formassignment-FormCategory.
      lst_form-postnotification   = lst_formassignment-PostNotification.
      lst_form-theme              = lst_formassignment-Theme.
      lst_form-stylesheet         = lst_formassignment-Stylesheet.
      lst_form-assigneddate       = lst_formassignment-AssignedDate.
      lst_form-assignedtime       = lst_formassignment-AssignedTime.
      lst_form-assignedby         = lst_formassignment-AssignedBy.
      lst_form-jobtype            = lst_formassignment-JobType.
      lst_form-flowsequence       = lst_formassignment-FlowSequence.
      lst_form-formname           = lst_formassignment-FormName.
      lst_form-active             = abap_true.
      ENDLOOP.

        DATA: lo_exchtab TYPE REF TO /odsmfe/cl_exchmechwo.
      CREATE OBJECT lo_exchtab.

      IF lo_exchtab IS BOUND.
* ----------------------------------------------------------------------*
* --- E: MOD-001 |11/03/2022| SKOTRA |   ES1K902967    |/ODSMFE/MAIN----*       " MOD  Start
* ----------------------------------------------------------------------*

* ----------------------------------------------------------------------*
* --- S: MOD-002 |27/01/2023| PPRIYANKA |   ES1K903522    |/ODSMFE/MAIN-*
* ----------------------------------------------------------------------*
        SELECT SINGLE param_value                                         ##WARN_OK  "#EC CI_NOFIELD
           FROM /odsmfe/tb_apcon
           WHERE param_name = @lc_pfcg_role
           AND active = @lc_x INTO @lv_pfcg_role.

        IF sy-subrc = 0.
          IF lv_pfcg_role EQ lc_true.
            lv_user = sy-uname .
            SELECT SINGLE param_value                                      ##WARN_OK   "#EC CI_NOFIELD
                    FROM /odsmfe/tb_apcon
                    WHERE param_name = @lc_meth
                    AND active = @lc_x  INTO @lv_class.
            IF sy-subrc EQ 0.
              CREATE OBJECT lo_auth TYPE (lv_class).
            ENDIF.                                                             " IF SY-SUBRC EQ 0
            TRY.
                CALL METHOD lo_auth->(lc_meth)                                 " Get PFCG Role ID
                  EXPORTING
                    iv_uname = lv_user
                  IMPORTING
                    ev_field = lv_role.
                IF lv_role IS NOT INITIAL.
                  lst_form-roleid = lv_role.
                ENDIF.                                                         " IF LV_ROLE IS NOT INITIAL
              "CATCH /iwbep/cx_mgw_busi_exception ##NO_HANDLER.
            ENDTRY.
* ----------------------------------------------------------------------*
* --- E: MOD-002 |27/01/2023| PPRIYANKA |   ES1K903522    |/ODSMFE/MAIN-*      " MOD  Start
* ----------------------------------------------------------------------*
          ELSE.                                                                " IF LV_PFCG_ROLE EQ LC_TRUE
            SELECT SINGLE roleid FROM (lst_usrroletab-low)       "#EC CI_DYNTAB
            WHERE userid = @sy-uname
            AND startdate LE @sy-datum
            AND enddate GE @sy-datum INTO @lv_roleid.
            IF sy-subrc = 0.
              lst_form-roleid = lv_roleid.
            ENDIF.
            ENDIF.                                                            " IF SY-SUBRC = 0
          ENDIF.                                                               " IF LV_PFCG_ROLE EQ LC_TRUE
          IF lst_formassignment-formid IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- Insert requested data to ODS : Form Manual Assignment Table-------*
* ----------------------------------------------------------------------*
            INSERT /odsmfe/tb_fmass FROM @lst_form.                             "#EC CI_TABLES
            IF sy-subrc <> 0.
              CLEAR lst_form.
            ENDIF.                                                             " IF SY-SUBRC <> 0
          ENDIF.                                                               " IF LST_FORMASSIGNMENT-FORMID IS NOT INITIAL
        ENDIF.                                                                 " IF SY-SUBRC = 0
                                                                         " IF lo_exchtab IS BOUND

        IF lst_formassignment-workordernum IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- S: MOD-001 |11/03/2022| SKOTRA |   ES1K902967    |/ODSMFE/MAIN----*
* ----------------------------------------------------------------------*
      IF lo_exchtab IS BOUND.
        lo_exchtab->exch_table_update( lst_formassignment-workordernum ).
      ENDIF.                                                                   " IF lo_exchtab IS BOUND
* ----------------------------------------------------------------------*
* --- E: MOD-001 |11/03/2022| SKOTRA |   ES1K902967    |/ODSMFE/MAIN----*      " MOD  Start
* ----------------------------------------------------------------------*
    ENDIF.
    ENDIF.
    CLEAR: lst_form.
*-------------------Logic for Update method-----------------------*

 IF roots_to_update IS NOT INITIAL.
      LOOP AT roots_to_update INTO DATA(lst_formassign_upadte).
      lst_form-formid             = lst_formassign_upadte-FormID.
      lst_form-version            = lst_formassign_upadte-Version.
      lst_form-formassignmenttype = lst_formassign_upadte-FormAssignmentType.
      lst_form-workordernum       = lst_formassign_upadte-WorkOrderNum.
      lst_form-notification       = lst_formassign_upadte-Notification.
      lst_form-oprnum             = lst_formassign_upadte-OprNum.
      lst_form-notificationitem   = lst_formassign_upadte-NotificationItem.
      lst_form-notificationtask   = lst_formassign_upadte-NotificationTask.
      lst_form-equipment          = lst_formassign_upadte-Equipment.
      lst_form-functionallocation = lst_formassign_upadte-FunctionalLocation.
      lst_form-mandatory          = lst_formassign_upadte-Mandatory.
      lst_form-multiplesub        = lst_formassign_upadte-MultipleSub.
      lst_form-occur              = lst_formassign_upadte-Occur.
      lst_form-category       = lst_formassign_upadte-FormCategory.
      lst_form-postnotification   = lst_formassign_upadte-PostNotification.
      lst_form-theme              = lst_formassign_upadte-Theme.
      lst_form-stylesheet         = lst_formassign_upadte-Stylesheet.
      lst_form-assigneddate       = lst_formassign_upadte-AssignedDate.
      lst_form-assignedtime       = lst_formassign_upadte-AssignedTime.
      lst_form-assignedby         = lst_formassign_upadte-AssignedBy.
      lst_form-jobtype            = lst_formassign_upadte-JobType.
      lst_form-flowsequence       = lst_formassign_upadte-FlowSequence.
      lst_form-formname           = lst_formassign_upadte-FormName.
      lst_form-active             = abap_true.

      ENDLOOP.

      CREATE OBJECT lo_exchtab.

      IF lo_exchtab IS BOUND.

* ----------------------------------------------------------------------*
* --- S: MOD-002 |27/01/2023| PPRIYANKA |   ES1K903522    |/ODSMFE/MAIN-*
* ----------------------------------------------------------------------*
        SELECT SINGLE param_value                                        ##WARN_OK    "#EC CI_NOFIELD
           FROM /odsmfe/tb_apcon
           WHERE param_name = @lc_pfcg_role
           AND active = @lc_x INTO @lv_pfcg_role.

        IF sy-subrc = 0.
          IF lv_pfcg_role EQ lc_true.
            lv_user = sy-uname .
            SELECT SINGLE param_value                                     ##WARN_OK    "#EC CI_NOFIELD
                FROM /odsmfe/tb_apcon
                WHERE param_name = @lc_meth
                AND active = @lc_x INTO @lv_class.

            IF sy-subrc EQ 0.
              CREATE OBJECT lo_auth TYPE (lv_class).
            ENDIF.                                                             " IF SY-SUBRC EQ 0
            TRY.
                CALL METHOD lo_auth->(lc_meth)                                 " Get PFCG Role ID
                  EXPORTING
                    iv_uname = lv_user
                  IMPORTING
                    ev_field = lv_role.
                IF lv_role IS NOT INITIAL.
                  lst_form-roleid = lv_role.
                ENDIF.                                                         " IF LV_ROLE IS NOT INITIAL
*              CATCH /iwbep/cx_mgw_busi_exception ##NO_HANDLER.
            ENDTRY.
* ----------------------------------------------------------------------*
* --- E: MOD-002 |27/01/2023| PPRIYANKA |   ES1K903522    |/ODSMFE/MAIN-*
* ----------------------------------------------------------------------*
          ELSE.                                                                " IF LV_PFCG_ROLE EQ LC_TRUE
            SELECT SINGLE roleid FROM (lst_usrroletab-low)     "#EC CI_DYNTAB
            WHERE userid = @sy-uname
            AND startdate LE @sy-datum
            AND enddate GE @sy-datum INTO @lv_roleid  .
            IF sy-subrc = 0.
              lst_form-roleid = lv_roleid.
            ENDIF.                                                             " IF SY-SUBRC = 0
          ENDIF.                                                               " IF LV_PFCG_ROLE EQ LC_TRUE
        ENDIF.                                                                 " IF SY-SUBRC = 0
      ENDIF.                                                                   " IF lo_exchtab IS BOUND

* ----------------------------------------------------------------------*
* --- Fetching data from ODS : Form Manual Assignment Table-------------*
* ----------------------------------------------------------------------*
      SELECT SINGLE formid,version,formassignmenttype,workordernum,notification,
                    oprnum,notificationitem,notificationtask,equipment,functionallocation
      FROM /odsmfe/tb_fmass
      WHERE formid              = @lst_form-formid
      AND   version             = @lst_form-version
      AND   formassignmenttype  = @lst_form-formassignmenttype
      AND   workordernum        = @lst_form-workordernum
      AND   notification        = @lst_form-notification
      AND   oprnum              = @lst_form-oprnum
      AND   notificationitem    = @lst_form-notificationitem
      AND   notificationtask    = @lst_form-notificationtask
      AND   equipment           = @lst_form-equipment
      AND   functionallocation  = @lst_form-functionallocation INTO (@lv_formid,@lv_version,@lv_formassignmenttype,@lv_workordernum,@lv_notification,@lv_oprnum,@lv_notificationitem,
      @lv_notificationtask,@lv_equipment,@lv_functionallocation).

      IF sy-subrc EQ 0 AND lst_formassign_upadte-FormID EQ lv_formid.
* ----------------------------------------------------------------------*
* --- Insert requested data to ODS : Form Manual Assignment Table-------*
* ----------------------------------------------------------------------*
        MODIFY /odsmfe/tb_fmass FROM @lst_form.                                 "#EC CI_TABLES
        IF sy-subrc <> 0.
          CLEAR lst_form.
        ENDIF.                                                                 " IF SY-SUBRC <> 0
      ENDIF.
      IF lst_formassign_upadte-WorkOrderNum IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- S: MOD-001 |11/03/2022|  SKOTRA  |    ES1K902967    |/ODSMFE/MAIN-*
* ----------------------------------------------------------------------*
      IF lo_exchtab IS BOUND.
        lo_exchtab->exch_table_update( lst_formassign_upadte-WorkOrderNum ).
      ENDIF.                                                                   " IF lo_exchtab IS BOUND
* ----------------------------------------------------------------------*
* --- E: MOD-001 |11/03/2022|  SKOTRA  |    ES1K902967    |/ODSMFE/MAIN-*
* ----------------------------------------------------------------------*
    ENDIF.
                                                                        " IF SY-SUBRC EQ 0 AND LST_FORMASSIGNMENT-FORMID EQ LV_FORMID
    ENDIF.

    CLEAR: lst_form.

"/**************************Handle Delete Method***************************/"

  IF roots_to_delete IS NOT INITIAL.

        LOOP AT roots_to_delete INTO DATA(lst_to_delete).
            lst_form-formid      = lst_to_delete-FormID.
            lst_form-version     = lst_to_delete-Version.
            lst_form-formassignmenttype = lst_to_delete-FormAssignmentType.
            lst_form-workordernum = lst_to_delete-WorkOrderNum.
            lst_form-oprnum    = lst_to_delete-OprNum.
            lst_form-notification = lst_to_delete-Notification.
            lst_form-notificationitem = lst_to_delete-NotificationItem.
            lst_form-notificationtask = lst_to_delete-NotificationTask.
            lst_form-equipment = lst_to_delete-Equipment.
            lst_form-functionallocation = lst_to_delete-FunctionalLocation.
        ENDLOOP. "/LOOP AT roots_to_delete INTO DATA(lst_to_delete).

        IF lst_form IS NOT INITIAL.
            SELECT formid,
                         version,
                         formassignmenttype,
                         workordernum,
                         notification,
                         oprnum,
                         notificationitem,
                         notificationtask,
                         equipment,
                         functionallocation
                  FROM /odsmfe/tb_fmass
                   WHERE formid = @lst_form-formid
                   AND      version = @lst_form-version
                   AND      formassignmenttype = @lst_form-formassignmenttype
                   AND      workordernum = @lst_form-workordernum
                   AND      notification   = @lst_form-notification
                   AND      oprnum         = @lst_form-oprnum
                   AND      notificationitem = @lst_form-notificationitem
                   AND      notificationtask = @lst_form-notificationtask
                   AND      equipment   = @lst_form-equipment
                   AND      functionallocation = @lst_form-functionallocation
                      INTO TABLE @lit_final.

                 IF sy-subrc EQ 0.
                    UPDATE /odsmfe/tb_fmass SET deleted = @abap_true
                    WHERE formid = @lst_form-formid
                   AND      version = @lst_form-version
                   AND      formassignmenttype = @lst_form-formassignmenttype
                   AND      workordernum = @lst_form-workordernum
                   AND      notification   = @lst_form-notification
                   AND      oprnum         = @lst_form-oprnum
                   AND      notificationitem = @lst_form-notificationitem
                   AND      notificationtask = @lst_form-notificationtask
                   AND      equipment   = @lst_form-equipment.

                   IF sy-subrc <> 0.
                        CLEAR: lst_form.
                   ENDIF.

                 ENDIF.





        ENDIF.



  ENDIF. "/IF roots_to_delete IS NOT INITIAL.





  ENDMETHOD.
  METHOD read.
  ENDMETHOD.
ENDCLASS.
