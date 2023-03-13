class /ODSMFE/CL_FORMMANUALASSIGNMNT definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  type-pools ABAP .

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_FORMMANUALASSIGNMENT .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_FORMMANUALASSIGNMENT .
  data GVIB_USER type USNAM .
  data GSTIB_DEL_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_FORMMANUALASSIGNMENT .
  data GITIB_DEL_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_FORMMANUALASSIGNMENT .

  methods READ_DELTA_TABLE
    importing
      !DELTA_TOKEN type TIMESTAMP
    exporting
      !EX_AUFNR_DATA type /ODSMFE/TT_EX_AUFNR .
  methods MODIFY_DELTA_TABLE
    importing
      !ORDER_NUMBER type AUFNR optional
      !TIME_TOKEN type STRING optional
      !NOTIFICATION type QMNUM optional .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_CREATE_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_DELETE_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_MODIFY_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_FORMMANUALASSIGNMNT IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_create_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 27/05/2021
* Transport No.          : ES1K902703
* Program Description    : Method used to create Forms
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   : SKOTRA
* Change Date            : 11/03/2022
* Transport No.          : ES1K902967
* Change Description     : Refaactoring SP07
***********************************************************************
********************** CHANGE HISTORY **********************************
* Program Author (SID)   :PPRIYANKA
* Change Date            :27/01/2023
* Transport No.          :ES1K903522
* Change Description     :Logic to fetch userrole via PFCG
************************************************************************
*----------------------------------------------------------------------
*  Data declaration
*----------------------------------------------------------------------
* Internal table & Structures
    TYPES: BEGIN OF ltys_formassignment ,
             formid             TYPE /odsmfe/tb_fmass-formid,
             version            TYPE c LENGTH 3,
             formassignmenttype TYPE c LENGTH 40,
             workordernum       TYPE aufnr,
             notification       TYPE qmnum,
             oprnum             TYPE vornr,
             notificationitem   TYPE felfd,
             notificationtask   TYPE manum,
             equipment          TYPE equnr,
             functionallocation TYPE tplnr,
             mandatory          TYPE c LENGTH 1,
             multiplesub        TYPE c LENGTH 1,
             occur              TYPE c LENGTH 3,
             formcategory       TYPE c LENGTH 30,
             postnotification   TYPE c LENGTH 1,
             theme              TYPE c LENGTH 20,
             stylesheet         TYPE c LENGTH 20,
             assigneddate       TYPE dats,
             assignedtime       TYPE tims,
             assignedby         TYPE c LENGTH 50,
             jobtype            TYPE c LENGTH 4,
             flowsequence       TYPE c LENGTH 3,
             formname           TYPE c LENGTH 50,
             active             TYPE /odsmfe/de_active,
           END OF ltys_formassignment .

    DATA: lst_form           TYPE /odsmfe/tb_fmass,
          lit_response       TYPE STANDARD TABLE OF /odsmfe/tb_fmass,
          lst_formassignment TYPE ltys_formassignment,
          lv_roleid          TYPE /odsmfe/de_roleid.
**********Data and Constant declartion - To fetch userrole via PFCG - ES1K903522
*    Check if table is active or not in table odsmfe/tb_apcon

    CONSTANTS:lc_pfcg_role TYPE string VALUE 'PFCG_ROLE',
              lc_meth TYPE string VALUE 'ROLE_ASSIGNMENT',
              lc_x         TYPE char1 VALUE 'X',
              lc_true      TYPE string VALUE 'TRUE'.

    DATA :  lv_pfcg_role TYPE  /odsmfe/de_mfe_value,
            lv_class TYPE  /odsmfe/de_mfe_value,
            lo_auth TYPE REF TO object,
            lv_user TYPE uname,
            lv_role TYPE agval.       "agrfield

**********End of Data and Constant declartion - To fetch userrole via PFCG - ES1K903522
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    im_data_provider->read_entry_data( IMPORTING es_data = lst_formassignment ).

    IF lst_formassignment IS NOT INITIAL.
      lst_form-formid             = lst_formassignment-formid.
      lst_form-version            = lst_formassignment-version.
      lst_form-formassignmenttype = lst_formassignment-formassignmenttype.
      lst_form-workordernum       = lst_formassignment-workordernum.
      lst_form-oprnum             = lst_formassignment-oprnum.
      lst_form-notification       = lst_formassignment-notification.
      lst_form-notificationitem   = lst_formassignment-notificationitem.
      lst_form-notificationtask   = lst_formassignment-notificationtask.
      lst_form-equipment          = lst_formassignment-equipment.
      lst_form-functionallocation = lst_formassignment-functionallocation.
      lst_form-mandatory          = lst_formassignment-mandatory.
      lst_form-multiplesub        = lst_formassignment-multiplesub.
      lst_form-occur              = lst_formassignment-occur.
      lst_form-formcategory       = lst_formassignment-formcategory.
      lst_form-postnotification   = lst_formassignment-postnotification.
      lst_form-theme              = lst_formassignment-theme.
      lst_form-stylesheet         = lst_formassignment-stylesheet.
      lst_form-assigneddate       = lst_formassignment-assigneddate.
      lst_form-assignedtime       = lst_formassignment-assignedtime.
      lst_form-assignedby         = lst_formassignment-assignedby.
      lst_form-jobtype            = lst_formassignment-jobtype.
      lst_form-flowsequence       = lst_formassignment-flowsequence.
      lst_form-formname           = lst_formassignment-formname.
      lst_form-active             = abap_true.

*--Start of changes SKOTRA - ES1K902967
*--Get reference for fetching value of user role table
      DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ).
      IF lr_exchtab IS BOUND.
        DATA(ls_usrroletab) = lr_exchtab->get_userrole_tab( ).
*--End of changes SKOTRA - ES1K902967
**********Start of Modificaion- TO fetch userrole via PFCG - ES1K903522*************
        SELECT SINGLE param_value
           FROM /odsmfe/tb_apcon
           INTO lv_pfcg_role
           WHERE param_name = lc_pfcg_role
           AND active = lc_x.

        IF sy-subrc = 0.
          IF lv_pfcg_role EQ lc_true.
            lv_user = sy-uname .
            SELECT SINGLE param_value
                    FROM /odsmfe/tb_apcon
                    INTO lv_class
                    WHERE param_name = lc_meth
                    AND active = lc_x.
              IF sy-subrc EQ 0.
                  CREATE OBJECT lo_auth TYPE (lv_class).
              ENDIF.
           TRY.
             CALL METHOD lo_auth->(lc_meth)    "Get PFCG Role ID
                  EXPORTING
                    iv_uname = lv_user
                  IMPORTING
                    ev_field = lv_role.
                IF lv_role IS NOT INITIAL.
                  lst_form-roleid = lv_role.
                ENDIF.
              CATCH /iwbep/cx_mgw_busi_exception.
            ENDTRY.
**********End of Modificaion- TO fetch userrole via PFCG - ES1K903522*****************
          ELSE.
            SELECT SINGLE roleid FROM (ls_usrroletab-low) INTO lv_roleid " SKOTRA - ES1K902967
            WHERE userid = sy-uname
            AND startdate LE sy-datum
            AND enddate GE sy-datum.
            IF sy-subrc = 0.
              lst_form-roleid = lv_roleid.
            ENDIF.
          ENDIF.
          IF lst_formassignment-formid IS NOT INITIAL.
* Insert data to Table /odsmfe/tb_fmass
            INSERT /odsmfe/tb_fmass FROM lst_form.
            IF sy-subrc <> 0.
              CLEAR lst_form.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
* Send the Form ID to Front end application
    gstib_entity-formid             = lst_formassignment-formid.
    gstib_entity-version            = lst_formassignment-version.
    gstib_entity-formassignmenttype = lst_formassignment-formassignmenttype.
    gstib_entity-workordernum       = lst_formassignment-workordernum.
    gstib_entity-oprnum             = lst_formassignment-oprnum.
    gstib_entity-notification       = lst_formassignment-notification.
    gstib_entity-notificationitem   = lst_formassignment-notificationitem.
    gstib_entity-notificationtask   = lst_formassignment-notificationtask.
    gstib_entity-equipment          = lst_formassignment-equipment.
    gstib_entity-functionallocation = lst_formassignment-functionallocation.
    gstib_entity-mandatory          = lst_formassignment-mandatory.
    gstib_entity-multiplesub        = lst_formassignment-multiplesub.
    gstib_entity-occur              = lst_formassignment-occur.
    gstib_entity-formcategory       = lst_formassignment-formcategory.
    gstib_entity-postnotification   = lst_formassignment-postnotification.
    gstib_entity-theme              = lst_formassignment-theme.
    gstib_entity-stylesheet         = lst_formassignment-stylesheet.
    gstib_entity-assigneddate       = lst_formassignment-assigneddate.
    gstib_entity-assignedtime       = lst_formassignment-assignedtime.
    gstib_entity-assignedby         = lst_formassignment-assignedby.
    gstib_entity-jobtype            = lst_formassignment-jobtype.
    gstib_entity-flowsequence       = lst_formassignment-flowsequence.
    gstib_entity-formname           = lst_formassignment-formname.
    gstib_entity-active             = abap_true.
    gstib_entity-roleid             = lv_roleid.
    GET REFERENCE OF gstib_entity INTO ex_entity.

    " updating work order exchange table
    IF lst_formassignment-workordernum IS NOT INITIAL.
*--Start of changes SKOTRA - ES1K902967
      IF lr_exchtab IS BOUND.
        lr_exchtab->exch_table_update( lst_formassignment-workordernum ).
      ENDIF.
*--End of changes SKOTRA - ES1K902967
    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_delete_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 27/05/2021
* Transport No.          : ES1K902703
* Program Description    : This Method used to Delete a Forms
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
*----------------------------------------------------------------------
*  Data declaration
*----------------------------------------------------------------------
* Internal table & Structures
    DATA: lrt_formid                TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_version               TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_formassignmenttype    TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_workordernum          TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_oprnum                TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_notif                 TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_notifitem             TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_notiftask             TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_equip                 TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_funlocation           TYPE TABLE OF /odsmfe/st_core_range_str,
          lst_key_tab               TYPE /iwbep/s_mgw_name_value_pair,
          lrs_it_formid             TYPE /odsmfe/st_core_range_str,
          lrs_it_version            TYPE /odsmfe/st_core_range_str,
          lrs_it_formassignmenttype TYPE /odsmfe/st_core_range_str,
          lrs_workordernum          TYPE /odsmfe/st_core_range_str,
          lrs_oprnum                TYPE /odsmfe/st_core_range_str,
          lrs_notif                 TYPE /odsmfe/st_core_range_str,
          lrs_notifitem             TYPE /odsmfe/st_core_range_str,
          lrs_notiftask             TYPE /odsmfe/st_core_range_str,
          lrs_equip                 TYPE /odsmfe/st_core_range_str,
          lrs_funlocation           TYPE /odsmfe/st_core_range_str,
          lst_formassignment        TYPE /odsmfe/tb_fmass,
          lit_final                 TYPE STANDARD TABLE OF /odsmfe/tb_fmass.

* Constants
    CONSTANTS: lc_e                  TYPE string VALUE 'E',
               lc_i                  TYPE string VALUE 'I',
               lc_eq                 TYPE string VALUE 'EQ',
               lc_formid             TYPE string VALUE 'FormID',
               lc_version            TYPE string VALUE 'Version',
               lc_workordernum       TYPE string VALUE 'WorkOrderNum',
               lc_oprnum             TYPE string VALUE 'OprNum',
               lc_formassignmenttype TYPE string VALUE 'FormAssignmentType',
               lc_notif              TYPE string VALUE 'Notification',
               lc_notifitem          TYPE string VALUE 'NotificationItem',
               lc_notiftask          TYPE string VALUE 'NotificationTask',
               lc_equip              TYPE string VALUE 'Equipment',
               lc_funlocation        TYPE string VALUE 'FunctionalLocation'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

*    im_data_provider->read_entry_data( IMPORTING es_data = lst_formassignment ).

    IF im_key_tab IS NOT INITIAL.

      LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
        CASE lst_key_tab-name.
          WHEN lc_formid.
            lrs_it_formid-sign = lc_i.
            lrs_it_formid-option = lc_eq.
            lrs_it_formid-low = lst_key_tab-value.
            lst_formassignment-formid = lst_key_tab-value.
            APPEND lrs_it_formid TO lrt_formid.
            CLEAR lrs_it_formid.

          WHEN lc_version.
            lrs_it_version-sign   = lc_i.
            lrs_it_version-option = lc_eq.
            lrs_it_version-low    = lst_key_tab-value.
            lst_formassignment-version = lst_key_tab-value.
            APPEND lrs_it_version TO lrt_version.
            CLEAR lrs_it_version.

          WHEN lc_workordernum.
            lrs_workordernum-sign   = lc_i.
            lrs_workordernum-option = lc_eq.
            lrs_workordernum-low    = lst_key_tab-value.
            lst_formassignment-workordernum = lst_key_tab-value.
            CALL FUNCTION 'CONVERSION_EXIT_AUFNR_INPUT'
              EXPORTING
                input  = lst_formassignment-workordernum
              IMPORTING
                output = lst_formassignment-workordernum.
            APPEND lrs_workordernum TO lrt_workordernum.
            CLEAR lrs_workordernum.

          WHEN lc_oprnum.
            lrs_oprnum-sign   = lc_i.
            lrs_oprnum-option = lc_eq.
            lrs_oprnum-low    = lst_key_tab-value.
            lst_formassignment-oprnum    = lst_key_tab-value.
            APPEND lrs_oprnum TO lrt_oprnum.
            CLEAR lrs_oprnum.

          WHEN lc_formassignmenttype.
            lrs_it_formassignmenttype-sign   = lc_i.
            lrs_it_formassignmenttype-option = lc_eq.
            lrs_it_formassignmenttype-low    = lst_key_tab-value.
            lst_formassignment-formassignmenttype = lst_key_tab-value.
            APPEND lrs_it_formassignmenttype TO lrt_formassignmenttype.
            CLEAR lrs_it_formassignmenttype.

          WHEN lc_notif.
            lrs_notif-sign   = lc_i.
            lrs_notif-option = lc_eq.
            lrs_notif-low    = lst_key_tab-value.
            lst_formassignment-notification = lst_key_tab-value.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = lst_formassignment-notification
              IMPORTING
                output = lst_formassignment-notification.
            APPEND lrs_notif TO lrt_notif.
            CLEAR lrs_notif.

          WHEN lc_notifitem.
            lrs_notifitem-sign   = lc_i.
            lrs_notifitem-option = lc_eq.
            lrs_notifitem-low    = lst_key_tab-value.
            lst_formassignment-notificationitem    = lst_key_tab-value.
            APPEND lrs_notifitem TO lrt_notifitem.
            CLEAR lrs_notifitem.

          WHEN lc_notiftask.
            lrs_notiftask-sign   = lc_i.
            lrs_notiftask-option = lc_eq.
            lrs_notiftask-low    = lst_key_tab-value.
            lst_formassignment-notificationtask = lst_key_tab-value.
            APPEND lrs_notiftask TO lrt_notiftask.
            CLEAR lrs_notiftask.

          WHEN lc_equip.
            lrs_equip-sign   = lc_i.
            lrs_equip-option = lc_eq.
            lrs_equip-low    = lst_key_tab-value.
            lst_formassignment-equipment = lst_key_tab-value.
            APPEND lrs_equip TO lrt_equip.
            CLEAR lrs_equip.

          WHEN lc_funlocation.
            lrs_funlocation-sign   = lc_i.
            lrs_funlocation-option = lc_eq.
            lrs_funlocation-low    = lst_key_tab-value.
            lst_formassignment-functionallocation =  lst_key_tab-value.
            APPEND lrs_funlocation TO lrt_funlocation.
            CLEAR lrs_funlocation.

        ENDCASE.
      ENDLOOP.
    ENDIF.

* Fetching data from table /odsmfe/tb_fmass
    SELECT * FROM /odsmfe/tb_fmass
             INTO TABLE lit_final
             WHERE formid             IN lrt_formid
             AND   version            IN lrt_version
             AND   formassignmenttype IN lrt_formassignmenttype
             AND   workordernum       EQ lst_formassignment-workordernum
             AND   notification       EQ lst_formassignment-notification
             AND   oprnum             IN lrt_oprnum
             AND   notificationitem   IN lrt_notifitem
             AND   notificationtask   IN lrt_notiftask
             AND   equipment          IN lrt_equip
             AND   functionallocation IN lrt_funlocation.

    IF sy-subrc EQ 0. .
* Delete data to Table /odsmfe/tb_fmass
      lst_formassignment-deleted = abap_true. " Mark the line item deleted
      UPDATE /odsmfe/tb_fmass SET deleted = lst_formassignment-deleted
      WHERE formid = lst_formassignment-formid
      AND   version = lst_formassignment-version
      AND   workordernum       EQ lst_formassignment-workordernum
      AND   notification       EQ lst_formassignment-notification
      AND   oprnum             EQ lst_formassignment-oprnum
      AND   notificationitem   EQ lst_formassignment-notificationitem
      AND   notificationtask   EQ lst_formassignment-notificationtask
      AND   equipment          EQ lst_formassignment-equipment
      AND   functionallocation EQ lst_formassignment-functionallocation.

* Send the instance ID to Front end application
      gstib_entity-formid             = lst_formassignment-formid.
      gstib_entity-version            = lst_formassignment-version.
      gstib_entity-formassignmenttype = lst_formassignment-formassignmenttype.
      gstib_entity-workordernum       = lst_formassignment-workordernum.
      gstib_entity-oprnum             = lst_formassignment-oprnum.
      gstib_entity-notification       = lst_formassignment-notification.
      gstib_entity-notificationitem   = lst_formassignment-notificationitem.
      gstib_entity-notificationtask   = lst_formassignment-notificationtask.
      gstib_entity-equipment          = lst_formassignment-equipment.
      gstib_entity-functionallocation = lst_formassignment-functionallocation.
      gstib_entity-mandatory          = lst_formassignment-mandatory.
      gstib_entity-multiplesub        = lst_formassignment-multiplesub.
      gstib_entity-occur              = lst_formassignment-occur.
      gstib_entity-formcategory       = lst_formassignment-formcategory.
      gstib_entity-postnotification   = lst_formassignment-postnotification.
      gstib_entity-theme              = lst_formassignment-theme.
      gstib_entity-stylesheet         = lst_formassignment-stylesheet.
      gstib_entity-assigneddate       = lst_formassignment-assigneddate.
      gstib_entity-assignedtime       = lst_formassignment-assignedtime.
      gstib_entity-assignedby         = lst_formassignment-assignedby.
      gstib_entity-jobtype            = lst_formassignment-jobtype.
      gstib_entity-flowsequence       = lst_formassignment-flowsequence.
      gstib_entity-formname           = lst_formassignment-formname.
      gstib_entity-deleted            = lst_formassignment-deleted.
      gstib_entity-active             = abap_true.

      GET REFERENCE OF gstib_entity INTO ex_entity.

    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_modify_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 27/05/2021
* Transport No.          : ES1K902703
* Program Description    : Method used to Update the Forms
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   : SKOTRA
* Change Date            : 11/03/2022
* Transport No.          : ES1K902967
* Change Description     : Refaactoring SP07
***********************************************************************
********************** CHANGE HISTORY **********************************
* Program Author (SID)   :PPRIYANKA
* Change Date            :27/01/2023
* Transport No.          :ES1K903522
* Change Description     :Logic to fetch userrole via PFCG
************************************************************************
*----------------------------------------------------------------------
*  Data declaration
*----------------------------------------------------------------------
* Internal table & Structures
    TYPES: BEGIN OF ltys_formassignment ,
             formid             TYPE /odsmfe/tb_fmass-formid,
             version            TYPE c LENGTH 3,
             formassignmenttype TYPE c LENGTH 40,
             workordernum       TYPE aufnr,
             notification       TYPE qmnum,
             oprnum             TYPE vornr,
             notificationitem   TYPE felfd,
             notificationtask   TYPE manum,
             equipment          TYPE equnr,
             functionallocation TYPE tplnr,
             mandatory          TYPE c LENGTH 1,
             multiplesub        TYPE c LENGTH 1,
             occur              TYPE c LENGTH 3,
             formcategory       TYPE c LENGTH 30,
             postnotification   TYPE c LENGTH 1,
             theme              TYPE c LENGTH 20,
             stylesheet         TYPE c LENGTH 20,
             assigneddate       TYPE dats,
             assignedtime       TYPE tims,
             assignedby         TYPE c LENGTH 50,
             jobtype            TYPE c LENGTH 4,
             flowsequence       TYPE c LENGTH 3,
             formname           TYPE c LENGTH 50,
             active             TYPE /odsmfe/de_active,
           END OF ltys_formassignment .

    DATA: lst_form           TYPE /odsmfe/tb_fmass,
          lit_response       TYPE STANDARD TABLE OF /odsmfe/tb_fmass,
          lst_formassignment TYPE ltys_formassignment.

* Variables
    DATA: lv_formid             TYPE char50,
          lv_version            TYPE char3,
          lv_formassignmenttype TYPE char40,
          lv_workordernum       TYPE aufnr,
          lv_oprnum             TYPE vornr,
          lv_notification       TYPE qmnum,
          lv_notificationitem   TYPE felfd,
          lv_notificationtask   TYPE manum,
          lv_equipment          TYPE equnr,
          lv_functionallocation TYPE tplnr,
          lv_roleid             TYPE /odsmfe/de_roleid.

**********Data and Constant declartion - To fetch userrole via PFCG - ES1K903522
*    Check if table is active or not in table odsmfe/tb_apcon

    CONSTANTS:lc_pfcg_role TYPE string VALUE 'PFCG_ROLE',
              lc_x         TYPE char1 VALUE 'X',
              lc_meth      TYPE string VALUE 'ROLE_ASSIGNMENT',
              lc_true      TYPE string VALUE 'TRUE'.

    DATA :  lv_pfcg_role TYPE  /odsmfe/de_mfe_value,
            lv_class     TYPE  /odsmfe/de_mfe_value,
            lo_auth      TYPE REF TO object,
            lv_user      TYPE uname,
            lv_role      TYPE agval.       "agrfield

**********End of Data and Constant declartion - To fetch userrole via PFCG - ES1K903522
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    im_data_provider->read_entry_data( IMPORTING es_data = lst_formassignment ).

    IF lst_formassignment IS NOT INITIAL.
      lst_form-formid             = lst_formassignment-formid.
      lst_form-version            = lst_formassignment-version.
      lst_form-formassignmenttype = lst_formassignment-formassignmenttype.
      lst_form-workordernum       = lst_formassignment-workordernum.
      lst_form-notification       = lst_formassignment-notification.
      lst_form-oprnum             = lst_formassignment-oprnum.
      lst_form-notificationitem   = lst_formassignment-notificationitem.
      lst_form-notificationtask   = lst_formassignment-notificationtask.
      lst_form-equipment          = lst_formassignment-equipment.
      lst_form-functionallocation = lst_formassignment-functionallocation.
      lst_form-mandatory          = lst_formassignment-mandatory.
      lst_form-multiplesub        = lst_formassignment-multiplesub.
      lst_form-occur              = lst_formassignment-occur.
      lst_form-formcategory       = lst_formassignment-formcategory.
      lst_form-postnotification   = lst_formassignment-postnotification.
      lst_form-theme              = lst_formassignment-theme.
      lst_form-stylesheet         = lst_formassignment-stylesheet.
      lst_form-assigneddate       = lst_formassignment-assigneddate.
      lst_form-assignedtime       = lst_formassignment-assignedtime.
      lst_form-assignedby         = lst_formassignment-assignedby.
      lst_form-jobtype            = lst_formassignment-jobtype.
      lst_form-flowsequence       = lst_formassignment-flowsequence.
      lst_form-formname           = lst_formassignment-formname.
      lst_form-active             = abap_true.


* Role ID
*--Start of changes - SKOTRA ES1K902967
*--Get reference for fetching value of user role table
      DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ).
      IF lr_exchtab IS BOUND.
        DATA(ls_usrroletab) = lr_exchtab->get_userrole_tab( ).

**********Start of Modificaion- TO fetch userrole via PFCG - ES1K903522**************
        SELECT SINGLE param_value
           FROM /odsmfe/tb_apcon
           INTO lv_pfcg_role
           WHERE param_name = lc_pfcg_role
           AND active = lc_x.

        IF sy-subrc = 0.
          IF lv_pfcg_role EQ lc_true.
            lv_user = sy-uname .
            SELECT SINGLE param_value
                FROM /odsmfe/tb_apcon
                INTO lv_class
                WHERE param_name = lc_meth
                AND active = lc_x.

            IF sy-subrc EQ 0.
              CREATE OBJECT lo_auth TYPE (lv_class).
            ENDIF.
            TRY.
                CALL METHOD lo_auth->(lc_meth)    "Get PFCG Role ID
                  EXPORTING
                    iv_uname = lv_user
                  IMPORTING
                    ev_field = lv_role.
                IF lv_role IS NOT INITIAL.
                  lst_form-roleid = lv_role.
                ENDIF.
              CATCH /iwbep/cx_mgw_busi_exception.
            ENDTRY.
**********End of Modificaion- TO fetch userrole via PFCG - ES1K903522*****************
          ELSE.
            SELECT SINGLE roleid FROM (ls_usrroletab-low) INTO lv_roleid " SKOTRA ES1K902967
            WHERE userid = sy-uname
            AND startdate LE sy-datum
            AND enddate GE sy-datum.
            IF sy-subrc = 0.
              lst_form-roleid = lv_roleid.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

* Fetching data from table /odsmfe/tb_fmass
      SELECT SINGLE formid  version formassignmenttype workordernum notification oprnum notificationitem notificationtask  equipment functionallocation
      FROM /odsmfe/tb_fmass
      INTO (lv_formid,lv_version,lv_formassignmenttype,lv_workordernum,lv_notification,lv_oprnum,lv_notificationitem,
      lv_notificationtask,lv_equipment,lv_functionallocation)
      WHERE formid              = lst_form-formid
      AND   version             = lst_form-version
      AND   formassignmenttype  = lst_form-formassignmenttype
      AND   workordernum        = lst_form-workordernum
      AND   notification        = lst_form-notification
      AND   oprnum              = lst_form-oprnum
      AND   notificationitem    = lst_form-notificationitem
      AND   notificationtask    = lst_form-notificationtask
      AND   equipment           = lst_form-equipment
      AND   functionallocation  = lst_form-functionallocation.

      IF sy-subrc EQ 0 AND lst_formassignment-formid EQ lv_formid.
* Insert data to Table /odsmfe/tb_fmass
        MODIFY /odsmfe/tb_fmass FROM lst_form.
        IF sy-subrc <> 0.
          CLEAR lst_form.
        ENDIF.
      ENDIF.
    ENDIF.

* Send the instance ID to Front end application
    gstib_entity-formid             = lst_formassignment-formid.
    gstib_entity-version            = lst_formassignment-version.
    gstib_entity-formassignmenttype = lst_formassignment-formassignmenttype.
    gstib_entity-workordernum       = lst_formassignment-workordernum.
    gstib_entity-oprnum             = lst_formassignment-oprnum.
    gstib_entity-notification       = lst_formassignment-notification.
    gstib_entity-notificationitem   = lst_formassignment-notificationitem.
    gstib_entity-notificationtask   = lst_formassignment-notificationtask.
    gstib_entity-equipment          = lst_formassignment-equipment.
    gstib_entity-functionallocation = lst_formassignment-functionallocation.
    gstib_entity-mandatory          = lst_formassignment-mandatory.
    gstib_entity-multiplesub        = lst_formassignment-multiplesub.
    gstib_entity-occur              = lst_formassignment-occur.
    gstib_entity-formcategory       = lst_formassignment-formcategory.
    gstib_entity-postnotification   = lst_formassignment-postnotification.
    gstib_entity-theme              = lst_formassignment-theme.
    gstib_entity-stylesheet         = lst_formassignment-stylesheet.
    gstib_entity-assigneddate       = lst_formassignment-assigneddate.
    gstib_entity-assignedtime       = lst_formassignment-assignedtime.
    gstib_entity-assignedby         = lst_formassignment-assignedby.
    gstib_entity-jobtype            = lst_formassignment-jobtype.
    gstib_entity-flowsequence       = lst_formassignment-flowsequence.
    gstib_entity-formname           = lst_formassignment-formname.
    gstib_entity-active             = abap_true.
    gstib_entity-roleid             = lv_roleid.
    GET REFERENCE OF gstib_entity INTO ex_entity.

    " updating work order exchange table
    IF lst_formassignment-workordernum IS NOT INITIAL.
*--Start of changes SKOTRA - ES1K902967
      IF lr_exchtab IS BOUND.
        lr_exchtab->exch_table_update( lst_formassignment-workordernum ).
      ENDIF.
*--End of changes SKOTRA - ES1K902967
    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 27/05/2021
* Transport No.          : ES1K902703
* Program Description    : Method used to Fetch the Forms
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   : ODS
* Change Date            : 11.03.2022
* Transport No.          : ES1K902967
* Change Description     : ODSMFE Refactoring SP07
***********************************************************************
*----------------------------------------------------------------------
*  Data declaration
*----------------------------------------------------------------------
* object reference
    DATA: lo_filter        TYPE REF TO /iwbep/if_mgw_req_filter,
          lst_filter       TYPE /iwbep/s_mgw_select_option,
          lst_filter_range TYPE /iwbep/s_cod_select_option,
          lv_aufnr         TYPE aufnr,
          lv_delta_token   TYPE timestamp,
          lv_mobileuser    TYPE string.
    DATA: lit_entity_temp TYPE TABLE OF /odsmfe/tb_fmass,
          lst_entity_temp TYPE /odsmfe/tb_fmass,
          lit_valid_wo    TYPE STANDARD TABLE OF /odsmfe/pm_valid_aufnr_str.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
    IF im_tech_request_context IS SUPPLIED.
      lo_filter = im_tech_request_context->get_filter( ).
    ENDIF.

    IF im_filter_select_options IS NOT INITIAL.
      LOOP AT im_filter_select_options INTO lst_filter.
        TRANSLATE lst_filter-property TO UPPER CASE.
        CASE lst_filter-property.
          WHEN text-002."WorkOrderNumber
            READ TABLE lst_filter-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL .
              lv_aufnr = lst_filter_range-low.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = lv_aufnr
                IMPORTING
                  output = lv_aufnr.
            ENDIF.
          WHEN text-003."EnteredBy
            READ TABLE lst_filter-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL .
              lv_mobileuser = lst_filter_range-low.
              TRANSLATE lv_mobileuser TO UPPER CASE.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    IF lv_mobileuser IS INITIAL.
      lv_mobileuser = sy-uname.
    ENDIF.
*--Start of changes - ES1K902967

    DATA(lr_getworkorder) = NEW /odsmfe/cl_get_workorder_data( ).

    IF lr_getworkorder IS BOUND.
      lr_getworkorder->gmib_get_workorder_data(
      EXPORTING
        im_mobileuser = lv_mobileuser
        im_tech_request_context = im_tech_request_context
        im_filter_select_options = im_filter_select_options
        im_entity_name =  im_entity_name
      IMPORTING
        lit_valid_wo = lit_valid_wo ).
    ENDIF.
*--End of changes - ES1K902967

    SORT lit_valid_wo.
    DELETE ADJACENT DUPLICATES FROM lit_valid_wo COMPARING ALL FIELDS.

    IF lit_valid_wo IS NOT INITIAL.
      SELECT * FROM /odsmfe/tb_fmass                            " FETCHING ALL THE FIELDS FROM RESPONSE CAPTURE TABLE
      INTO CORRESPONDING FIELDS OF TABLE lit_entity_temp
      FOR ALL ENTRIES IN lit_valid_wo
      WHERE formid NE space
      AND workordernum = lit_valid_wo-aufnr.

      IF sy-subrc IS INITIAL.
        SORT lit_entity_temp BY formid.
      ENDIF.
    ENDIF.
    IF lit_entity_temp IS NOT INITIAL .
      LOOP AT lit_entity_temp INTO lst_entity_temp.
        MOVE-CORRESPONDING lst_entity_temp TO gstib_entity.
        IF gstib_entity-assignedtime EQ space.
          CLEAR gstib_entity-assignedtime.
        ENDIF.
        APPEND gstib_entity TO gitib_entity.
        CLEAR: lst_entity_temp,gstib_entity.
      ENDLOOP.

      LOOP AT lit_entity_temp INTO lst_entity_temp WHERE deleted IS NOT INITIAL.
        MOVE-CORRESPONDING lst_entity_temp TO gstib_del_entity.
        IF gstib_del_entity-assignedtime EQ space.
          CLEAR gstib_del_entity-assignedtime.
        ENDIF.
        APPEND gstib_del_entity TO gitib_del_entity.
        CLEAR: lst_entity_temp,gstib_del_entity.
      ENDLOOP.

    ENDIF.
* Delete deleted entry from inital entity
    DELETE gitib_entity WHERE deleted IS NOT INITIAL.

* Delete Duplicate entries
    DELETE ADJACENT DUPLICATES FROM gitib_entity COMPARING ALL FIELDS.
    IF im_tech_request_context_entity IS SUPPLIED AND  gitib_entity IS NOT INITIAL.
      READ TABLE gitib_entity INTO gstib_entity INDEX 1.
      IF sy-subrc EQ 0.
        GET REFERENCE OF gstib_entity INTO ex_entity.
      ENDIF.
    ELSE.
      GET REFERENCE OF gitib_entity INTO ex_entityset.
      GET REFERENCE OF gitib_del_entity INTO ex_deleted_entityset.
    ENDIF.
* TimeStamp Field for supplying Delta token
    GET TIME STAMP FIELD lv_delta_token.

* Export the delta token
    ex_response_context-deltatoken = lv_delta_token.

  ENDMETHOD.


  METHOD modify_delta_table.

    DATA : lst_delta             TYPE /odsmfe/tb_wo_ex,
           lv_count              TYPE i,
           lv_exchange_table(20) TYPE c.

    CONSTANTS : lc_mobile_app_mfe    TYPE string VALUE '/ODSMFE/SAP_WM_MOBILE_APP'.

    " Insert data
    lst_delta-mandt      = sy-mandt.
    lst_delta-mobile_app = lc_mobile_app_mfe.
    lst_delta-changed_ts = time_token.
    lst_delta-changed_by = sy-uname.

    IF order_number IS NOT INITIAL.

      SELECT COUNT(*) FROM /odsmfe/tb_wo_ex INTO lv_count
            WHERE objkey = order_number.
      IF lv_count = 0.
        lst_delta-action = 'I'.
      ELSE.
        lst_delta-action = 'U'.
      ENDIF.

      lst_delta-objkey     = order_number.

      SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_exchange_table
                          WHERE entitysetname = 'FormAttachmentSet'
                            AND tabname = 'WORKORDER'
                            AND field = 'EXCHANGE_TABLE'.

    ELSEIF notification IS NOT INITIAL.

      SELECT COUNT(*) FROM /odsmfe/tb_no_ex INTO lv_count
          WHERE objkey = notification.

      IF lv_count = 0.
        lst_delta-action = 'I'.
      ELSE.
        lst_delta-action = 'U'.
      ENDIF.

      lst_delta-objkey     = notification.

      SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_exchange_table
                          WHERE entitysetname = 'FormAttachmentSet'
                            AND tabname = 'NOTIFICATION'
                            AND field = 'EXCHANGE_TABLE'.

    ENDIF.

    IF lv_exchange_table IS NOT INITIAL.

      MODIFY (lv_exchange_table) FROM lst_delta.
      IF sy-subrc = 0 .
        COMMIT WORK .
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD read_delta_table.
*Variables
    DATA: lv_exchobj        TYPE /syclo/core_exchobj_dte,
          lv_mobile_app     TYPE /syclo/core_mobile_app_dte,
          lv_bor_objtyp     TYPE oj_name,
          lv_sys_tzone      TYPE tznzonesys,
          lv_ts             TYPE timestamp,
          lv_ts_str         TYPE string,
          lv_date           TYPE datum,
          lv_time           TYPE uzeit,
          lv_sys_time_token TYPE string.
    DATA: lv_filter TYPE string.

*Tables & Structures
    DATA: BEGIN OF ls_date_time,
            date TYPE datum,
            time TYPE uzeit,
          END OF ls_date_time.

    DATA : lit_ex_aufnr          TYPE TABLE OF /odsmfe/ex_aufnr,
           lv_exchange_table(20) TYPE c.

    CONSTANTS :  lc_tzone     TYPE tznzonesys VALUE 'UTC',
                 lc_tzone_utc TYPE tznzonesys VALUE 'UTC'.

    lv_ts_str = delta_token.
    ls_date_time = lv_ts_str.
    lv_mobile_app = '/ODSMFE/SAP_WM_MOBILE_APP'.
    /syclo/cl_core_bapi_tools=>get_system_time(
      IMPORTING ev_sys_tzone = lv_sys_tzone ).
    IF lv_sys_tzone = lc_tzone.
      lv_sys_time_token = delta_token.
    ELSEIF lc_tzone = lc_tzone_utc.
      CONVERT TIME STAMP delta_token TIME ZONE lv_sys_tzone
        INTO DATE lv_date
             TIME lv_time.
      CONCATENATE lv_date lv_time INTO lv_sys_time_token.
    ELSE.
      lv_date = ls_date_time-date.
      lv_time = ls_date_time-time.
      CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_ts TIME ZONE lc_tzone.
      CONVERT TIME STAMP delta_token TIME ZONE lv_sys_tzone
       INTO DATE lv_date
            TIME lv_time.
      CONCATENATE lv_date lv_time INTO lv_sys_time_token.
    ENDIF.

    CONCATENATE 'CHANGED_TS > ' '''' lv_sys_time_token '''' INTO lv_filter SEPARATED BY space.
    CONCATENATE lv_filter ' AND MOBILE_APP = ' '''' lv_mobile_app ''''
      INTO lv_filter.

    SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_exchange_table
                    WHERE entitysetname = 'FormAttachmentSet'
                      AND tabname = 'WORKORDER'
                      AND field = 'EXCHANGE_TABLE'.

    IF lv_exchange_table IS NOT INITIAL.

      SELECT * FROM (lv_exchange_table) INTO TABLE lit_ex_aufnr
                                     WHERE (lv_filter).
      IF lit_ex_aufnr IS NOT INITIAL.
        ex_aufnr_data[] = lit_ex_aufnr[].
      ENDIF.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
