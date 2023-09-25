CLASS /odsmfe/cl_formmanualassignmnt DEFINITION
  PUBLIC
  INHERITING FROM /odsmfe/cl_get_ent_super_bapi
  CREATE PUBLIC .

  PUBLIC SECTION.
* type-pools ABAP .

    "data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_FORMMANUALASSIGNMENT .
    "data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_FORMMANUALASSIGNMENT .
    DATA gvib_user TYPE usnam .
    "data GSTIB_DEL_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_FORMMANUALASSIGNMENT .
    "data GITIB_DEL_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_FORMMANUALASSIGNMENT .

    METHODS read_delta_table
      IMPORTING
        !delta_token   TYPE timestamp
      EXPORTING
        !ex_aufnr_data TYPE /odsmfe/tt_ex_aufnr .
    METHODS modify_delta_table
      IMPORTING
        !order_number TYPE aufnr OPTIONAL
        !time_token   TYPE string OPTIONAL
        !notification TYPE /odsmfe/tb_fmass-notification OPTIONAL .  " Changed by Priynaka QMNUM to /odsmfe/tb_fmass-notification

    METHODS /odsmfe/if_get_entityset_bapi~gmib_create_entityset
        REDEFINITION .
    METHODS /odsmfe/if_get_entityset_bapi~gmib_delete_entityset
        REDEFINITION .
    METHODS /odsmfe/if_get_entityset_bapi~gmib_modify_entityset
        REDEFINITION .
    METHODS /odsmfe/if_get_entityset_bapi~gmib_read_entityset
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA im_tech_request_context TYPE REF TO if_rap_query_request.
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
*----------------------------------------------------------------------
*  Data declaration
*----------------------------------------------------------------------
* Internal table & Structures
    TYPES: BEGIN OF ltys_formassignment ,
             formid             TYPE /odsmfe/tb_fmass-formid,
             version            TYPE  /odsmfe/tb_fmass-version, "c LENGTH 3,
             formassignmenttype TYPE /odsmfe/tb_fmass-formassignmenttype, "c LENGTH 40,
             workordernum       TYPE aufnr,
             notification       TYPE /odsmfe/tb_fmass-notification,
             oprnum             TYPE /odsmfe/tb_fmass-oprnum, "vornr,
             notificationitem   TYPE /odsmfe/tb_fmass-notificationitem, "felfd
             notificationtask   TYPE /odsmfe/tb_fmass-notificationtask , "manum
             equipment          TYPE /odsmfe/tb_fmass-equipment, "
             functionallocation TYPE /odsmfe/tb_fmass-functionallocation, "tplnr
             mandatory          TYPE  /odsmfe/tb_fmass-mandatory          , "c LENGTH 1,
             multiplesub        TYPE  /odsmfe/tb_fmass-multiplesub, "c LENGTH 1,
             occur              TYPE  /odsmfe/tb_fmass-occur, "c LENGTH 3,
             formcategory       TYPE  /odsmfe/tb_fmass-category    , "c LENGTH 30,
             postnotification   TYPE /odsmfe/tb_fmass-postnotification, "c LENGTH 1,
             theme              TYPE /odsmfe/tb_fmass-theme, "c LENGTH 20,
             stylesheet         TYPE /odsmfe/tb_fmass-stylesheet, " c LENGTH 20,
             assigneddate       TYPE /odsmfe/tb_fmass-assigneddate,  "dats,
             assignedtime       TYPE /odsmfe/tb_fmass-assignedtime,   "tims,
             assignedby         TYPE /odsmfe/tb_fmass-assignedby, " LENGTH 50,
             jobtype            TYPE  /odsmfe/tb_fmass-jobtype, " c LENGTH 4,
             flowsequence       TYPE  /odsmfe/tb_fmass-flowsequence, " c LENGTH 3,
             formname           TYPE /odsmfe/tb_fmass-formname, "c LENGTH 50,
             active             TYPE /odsmfe/de_active,
           END OF ltys_formassignment .

    DATA: lst_form           TYPE /odsmfe/tb_fmass,
          lit_response       TYPE STANDARD TABLE OF /odsmfe/tb_fmass,
          lst_formassignment TYPE ltys_formassignment,
          lv_roleid          TYPE /odsmfe/de_roleid.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

*    im_data_provider->read_entry_data( IMPORTING es_data = lst_formassignment ).
*
*    IF lst_formassignment IS NOT INITIAL.
*      lst_form-formid             = lst_formassignment-formid.
*      lst_form-version            = lst_formassignment-version.
*      lst_form-formassignmenttype = lst_formassignment-formassignmenttype.
*      lst_form-workordernum       = lst_formassignment-workordernum.
*      lst_form-oprnum             = lst_formassignment-oprnum.
*      lst_form-notification       = lst_formassignment-notification.
*      lst_form-notificationitem   = lst_formassignment-notificationitem.
*      lst_form-notificationtask   = lst_formassignment-notificationtask.
*      lst_form-equipment          = lst_formassignment-equipment.
*      lst_form-functionallocation = lst_formassignment-functionallocation.
*      lst_form-mandatory          = lst_formassignment-mandatory.
*      lst_form-multiplesub        = lst_formassignment-multiplesub.
*      lst_form-occur              = lst_formassignment-occur.
*      lst_form-formcategory       = lst_formassignment-formcategory.
*      lst_form-postnotification   = lst_formassignment-postnotification.
*      lst_form-theme              = lst_formassignment-theme.
*      lst_form-stylesheet         = lst_formassignment-stylesheet.
*      lst_form-assigneddate       = lst_formassignment-assigneddate.
*      lst_form-assignedtime       = lst_formassignment-assignedtime.
*      lst_form-assignedby         = lst_formassignment-assignedby.
*      lst_form-jobtype            = lst_formassignment-jobtype.
*      lst_form-flowsequence       = lst_formassignment-flowsequence.
*      lst_form-formname           = lst_formassignment-formname.
*      lst_form-active             = abap_true.

*--Start of changes SKOTRA - ES1K902967
*--Get reference for fetching value of user role table

    " DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ). commented by Priyanka
    DATA: lr_exchtab TYPE REF TO /odsmfe/cl_exchmechwo.
    IF lr_exchtab IS BOUND.
      DATA(ls_usrroletab) = lr_exchtab->get_userrole_tab( ).
*--End of changes SKOTRA - ES1K902967
*--Get Role ID
      SELECT SINGLE roleid FROM (ls_usrroletab-low)  " SKOTRA - ES1K902967
      WHERE userid = @sy-uname
      AND startdate LE @sy-datum
      AND enddate GE @sy-datum INTO @lv_roleid.
      IF sy-subrc = 0.
        lst_form-roleid = lv_roleid.
      ENDIF.
    ENDIF.
    IF lst_formassignment-formid IS NOT INITIAL.
* Insert data to Table /odsmfe/tb_fmass
      INSERT /odsmfe/tb_fmass FROM @lst_form.
      IF sy-subrc <> 0.
        CLEAR lst_form.
      ENDIF.
    ENDIF.


* Send the Form ID to Front end application
*    gstib_entity-formid             = lst_formassignment-formid.
*    gstib_entity-version            = lst_formassignment-version.
*    gstib_entity-formassignmenttype = lst_formassignment-formassignmenttype.
*    gstib_entity-workordernum       = lst_formassignment-workordernum.
*    gstib_entity-oprnum             = lst_formassignment-oprnum.
*    gstib_entity-notification       = lst_formassignment-notification.
*    gstib_entity-notificationitem   = lst_formassignment-notificationitem.
*    gstib_entity-notificationtask   = lst_formassignment-notificationtask.
*    gstib_entity-equipment          = lst_formassignment-equipment.
*    gstib_entity-functionallocation = lst_formassignment-functionallocation.
*    gstib_entity-mandatory          = lst_formassignment-mandatory.
*    gstib_entity-multiplesub        = lst_formassignment-multiplesub.
*    gstib_entity-occur              = lst_formassignment-occur.
*    gstib_entity-formcategory       = lst_formassignment-formcategory.
*    gstib_entity-postnotification   = lst_formassignment-postnotification.
*    gstib_entity-theme              = lst_formassignment-theme.
*    gstib_entity-stylesheet         = lst_formassignment-stylesheet.
*    gstib_entity-assigneddate       = lst_formassignment-assigneddate.
*    gstib_entity-assignedtime       = lst_formassignment-assignedtime.
*    gstib_entity-assignedby         = lst_formassignment-assignedby.
*    gstib_entity-jobtype            = lst_formassignment-jobtype.
*    gstib_entity-flowsequence       = lst_formassignment-flowsequence.
*    gstib_entity-formname           = lst_formassignment-formname.
*    gstib_entity-active             = abap_true.
*    gstib_entity-roleid             = lv_roleid.
*    GET REFERENCE OF gstib_entity INTO ex_entity.
    """SOC BY Priyanka
    " ex_response = lst_formassignment.
    """EOC BY Priyanka

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
          " lst_key_tab               TYPE /iwbep/s_mgw_name_value_pair,
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
          lit_formassignment        TYPE STANDARD TABLE OF /odsmfe/tb_fmass,
          lit_final                 TYPE STANDARD TABLE OF /odsmfe/tb_fmass.

* Constants
    CONSTANTS: lc_e                  TYPE string VALUE 'E',
               lc_i                  TYPE string VALUE 'I',
               lc_eq                 TYPE string VALUE 'EQ',
               lc_formid             TYPE string VALUE 'FORMID',
               lc_version            TYPE string VALUE 'VERSION',
               lc_workordernum       TYPE string VALUE 'WORKORDERNUM',
               lc_oprnum             TYPE string VALUE 'OPRNUM',
               lc_formassignmenttype TYPE string VALUE 'FORASSIGNMENTTYPE', "FormAssignmentType',
               lc_notif              TYPE string VALUE 'NOTIFICATION', "Notification',
               lc_notifitem          TYPE string VALUE 'NOTIFICATIONITEM', "NotificationItem',
               lc_notiftask          TYPE string VALUE 'NOTIFICATIONTASK', "NotificationTask',
               lc_equip              TYPE string VALUE 'EQUIPMENT', "'Equipment',
               lc_funlocation        TYPE string VALUE 'FUNCTIONLOCATION'. "'FunctionalLocation'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

*    im_data_provider->read_entry_data( IMPORTING es_data = lst_formassignment ).

*    IF im_key_tab IS NOT INITIAL.
*
*      LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
*        CASE lst_key_tab-name.
*          WHEN lc_formid.
*            lrs_it_formid-sign = lc_i.
*            lrs_it_formid-option = lc_eq.
*            lrs_it_formid-low = lst_key_tab-value.
*            lst_formassignment-formid = lst_key_tab-value.
*            APPEND lrs_it_formid TO lrt_formid.
*            CLEAR lrs_it_formid.
*
*          WHEN lc_version.
*            lrs_it_version-sign   = lc_i.
*            lrs_it_version-option = lc_eq.
*            lrs_it_version-low    = lst_key_tab-value.
*            lst_formassignment-version = lst_key_tab-value.
*            APPEND lrs_it_version TO lrt_version.
*            CLEAR lrs_it_version.
*
*          WHEN lc_workordernum.
*            lrs_workordernum-sign   = lc_i.
*            lrs_workordernum-option = lc_eq.
*            lrs_workordernum-low    = lst_key_tab-value.
*            lst_formassignment-workordernum = lst_key_tab-value.
*            CALL FUNCTION 'CONVERSION_EXIT_AUFNR_INPUT'
*              EXPORTING
*                input  = lst_formassignment-workordernum
*              IMPORTING
*                output = lst_formassignment-workordernum.
*            APPEND lrs_workordernum TO lrt_workordernum.
*            CLEAR lrs_workordernum.
*
*          WHEN lc_oprnum.
*            lrs_oprnum-sign   = lc_i.
*            lrs_oprnum-option = lc_eq.
*            lrs_oprnum-low    = lst_key_tab-value.
*            lst_formassignment-oprnum    = lst_key_tab-value.
*            APPEND lrs_oprnum TO lrt_oprnum.
*            CLEAR lrs_oprnum.
*
*          WHEN lc_formassignmenttype.
*            lrs_it_formassignmenttype-sign   = lc_i.
*            lrs_it_formassignmenttype-option = lc_eq.
*            lrs_it_formassignmenttype-low    = lst_key_tab-value.
*            lst_formassignment-formassignmenttype = lst_key_tab-value.
*            APPEND lrs_it_formassignmenttype TO lrt_formassignmenttype.
*            CLEAR lrs_it_formassignmenttype.
*
*          WHEN lc_notif.
*            lrs_notif-sign   = lc_i.
*            lrs_notif-option = lc_eq.
*            lrs_notif-low    = lst_key_tab-value.
*            lst_formassignment-notification = lst_key_tab-value.

*            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*              EXPORTING
*                input  = lst_formassignment-notification
*              IMPORTING
*                output = lst_formassignment-notification.
*            APPEND lrs_notif TO lrt_notif.
*            CLEAR lrs_notif.
*
*          WHEN lc_notifitem.
*            lrs_notifitem-sign   = lc_i.
*            lrs_notifitem-option = lc_eq.
*            lrs_notifitem-low    = lst_key_tab-value.
*            lst_formassignment-notificationitem    = lst_key_tab-value.
*            APPEND lrs_notifitem TO lrt_notifitem.
*            CLEAR lrs_notifitem.
*
*          WHEN lc_notiftask.
*            lrs_notiftask-sign   = lc_i.
*            lrs_notiftask-option = lc_eq.
*            lrs_notiftask-low    = lst_key_tab-value.
*            lst_formassignment-notificationtask = lst_key_tab-value.
*            APPEND lrs_notiftask TO lrt_notiftask.
*            CLEAR lrs_notiftask.
*
*          WHEN lc_equip.
*            lrs_equip-sign   = lc_i.
*            lrs_equip-option = lc_eq.
*            lrs_equip-low    = lst_key_tab-value.
*            lst_formassignment-equipment = lst_key_tab-value.
*            APPEND lrs_equip TO lrt_equip.
*            CLEAR lrs_equip.
*
*          WHEN lc_funlocation.
*            lrs_funlocation-sign   = lc_i.
*            lrs_funlocation-option = lc_eq.
*            lrs_funlocation-low    = lst_key_tab-value.
*            lst_formassignment-functionallocation =  lst_key_tab-value.
*            APPEND lrs_funlocation TO lrt_funlocation.
*            CLEAR lrs_funlocation.
*
*        ENDCASE.
*      ENDLOOP.
*    ENDIF.

    CALL METHOD me->get_cloud_dest
      IMPORTING
        ex_dest = DATA(lv_rfc).

    LOOP AT im_filter_select_options INTO DATA(ls_filter_select_options).
      CASE ls_filter_select_options-name.
        WHEN lc_formid.
          lrt_formid = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_formid WHERE low IS INITIAL.
          READ TABLE lrt_formid INTO lrs_it_formid INDEX 1.
          IF sy-subrc = 0.
            lst_formassignment-formid = lrs_it_formid-low.
          ENDIF.

        WHEN lc_version.
          lrt_version = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_version WHERE low IS INITIAL.
          READ TABLE lrt_version INTO lrs_it_version INDEX 1.
          IF sy-subrc = 0.
            lst_formassignment-version = lrs_it_version-low.
          ENDIF.


        WHEN lc_workordernum.
          lrt_workordernum = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_workordernum WHERE low IS INITIAL.
          READ TABLE lrt_workordernum INTO lrs_workordernum INDEX 1.
          IF sy-subrc = 0.
            lst_formassignment-workordernum = lrs_workordernum-low.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_AUFNR_INPUT'
            DESTINATION lv_rfc
            EXPORTING
              input  = lst_formassignment-workordernum
            IMPORTING
              output = lst_formassignment-workordernum.

        WHEN lc_oprnum.
          lrt_oprnum = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_oprnum WHERE low IS INITIAL.
          READ TABLE lrt_oprnum INTO lrs_oprnum INDEX 1.
          IF sy-subrc = 0.
            lst_formassignment-oprnum = lrs_oprnum-low.
          ENDIF.


        WHEN lc_formassignmenttype.
          lrt_formassignmenttype = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_formassignmenttype WHERE low IS INITIAL.
          READ TABLE lrt_formassignmenttype INTO lrs_it_formassignmenttype INDEX 1.
          IF sy-subrc = 0.
            lst_formassignment-formassignmenttype = lrs_it_formassignmenttype-low.
          ENDIF.

        WHEN lc_notif.
          lrt_notif = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_notif WHERE low IS INITIAL.
          READ TABLE lrt_notif INTO lrs_notif INDEX 1.
          IF sy-subrc = 0.
            lst_formassignment-notification = lrs_notif-low.
          ENDIF.

        WHEN lc_notifitem.
          lrt_notifitem = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_notifitem WHERE low IS INITIAL.

        WHEN lc_notiftask.
          lrt_notiftask = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_notiftask WHERE low IS INITIAL.

        WHEN lc_equip.
          lrt_equip = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_equip WHERE low IS INITIAL.

        WHEN lc_funlocation.
          lrt_funlocation = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_funlocation WHERE low IS INITIAL.

      ENDCASE.
    ENDLOOP.

* Fetching data from table /odsmfe/tb_fmass
    SELECT * FROM /odsmfe/tb_fmass
             WHERE formid             IN @lrt_formid
             AND   version            IN @lrt_version
             AND   formassignmenttype IN @lrt_formassignmenttype
             AND   workordernum       EQ @lst_formassignment-workordernum
             AND   notification       EQ @lst_formassignment-notification
             AND   oprnum             IN @lrt_oprnum
             AND   notificationitem   IN @lrt_notifitem
             AND   notificationtask   IN @lrt_notiftask
             AND   equipment          IN @lrt_equip
             AND   functionallocation IN @lrt_funlocation INTO TABLE @lit_final.

    IF sy-subrc EQ 0. .
* Delete data to Table /odsmfe/tb_fmass
      lst_formassignment-deleted = abap_true. " Mark the line item deleted
      UPDATE /odsmfe/tb_fmass SET deleted = @lst_formassignment-deleted
      WHERE formid = @lst_formassignment-formid
      AND   version = @lst_formassignment-version
      AND   workordernum       EQ @lst_formassignment-workordernum
      AND   notification       EQ @lst_formassignment-notification
      AND   oprnum             EQ @lst_formassignment-oprnum
      AND   notificationitem   EQ @lst_formassignment-notificationitem
      AND   notificationtask   EQ @lst_formassignment-notificationtask
      AND   equipment          EQ @lst_formassignment-equipment
      AND   functionallocation EQ @lst_formassignment-functionallocation.

* Send the instance ID to Front end application
*      gstib_entity-formid             = lst_formassignment-formid.
*      gstib_entity-version            = lst_formassignment-version.
*      gstib_entity-formassignmenttype = lst_formassignment-formassignmenttype.
*      gstib_entity-workordernum       = lst_formassignment-workordernum.
*      gstib_entity-oprnum             = lst_formassignment-oprnum.
*      gstib_entity-notification       = lst_formassignment-notification.
*      gstib_entity-notificationitem   = lst_formassignment-notificationitem.
*      gstib_entity-notificationtask   = lst_formassignment-notificationtask.
*      gstib_entity-equipment          = lst_formassignment-equipment.
*      gstib_entity-functionallocation = lst_formassignment-functionallocation.
*      gstib_entity-mandatory          = lst_formassignment-mandatory.
*      gstib_entity-multiplesub        = lst_formassignment-multiplesub.
*      gstib_entity-occur              = lst_formassignment-occur.
*      gstib_entity-formcategory       = lst_formassignment-formcategory.
*      gstib_entity-postnotification   = lst_formassignment-postnotification.
*      gstib_entity-theme              = lst_formassignment-theme.
*      gstib_entity-stylesheet         = lst_formassignment-stylesheet.
*      gstib_entity-assigneddate       = lst_formassignment-assigneddate.
*      gstib_entity-assignedtime       = lst_formassignment-assignedtime.
*      gstib_entity-assignedby         = lst_formassignment-assignedby.
*      gstib_entity-jobtype            = lst_formassignment-jobtype.
*      gstib_entity-flowsequence       = lst_formassignment-flowsequence.
*      gstib_entity-formname           = lst_formassignment-formname.
*      gstib_entity-deleted            = lst_formassignment-deleted.
*      gstib_entity-active             = abap_true.

*      GET REFERENCE OF gstib_entity INTO ex_entity.
*   MOVE-CORRESPONDING lst_formassignment to ex_entity.


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
*----------------------------------------------------------------------
*  Data declaration
*----------------------------------------------------------------------
* Internal table & Structures
    TYPES: BEGIN OF ltys_formassignment ,
             formid             TYPE /odsmfe/tb_fmass-formid,
             version            TYPE /odsmfe/tb_fmass-version, "c LENGTH 3,
             formassignmenttype TYPE /odsmfe/tb_fmass-formassignmenttype, "c LENGTH 40,
             workordernum       TYPE aufnr,
             notification       TYPE /odsmfe/tb_fmass-notification, "c length 12,"qmnum,
             oprnum             TYPE /odsmfe/tb_fmass-oprnum, "vornr,
             notificationitem   TYPE /odsmfe/tb_fmass-notificationitem, "felfd,
             notificationtask   TYPE /odsmfe/tb_fmass-notificationtask, "manum,
             equipment          TYPE /odsmfe/tb_fmass-equipment, "equnr,
             functionallocation TYPE /odsmfe/tb_fmass-functionallocation, "tplnr,
             mandatory          TYPE  /odsmfe/tb_fmass-mandatory, "c LENGTH 1,
             multiplesub        TYPE /odsmfe/tb_fmass-multiplesub, "c LENGTH 1,
             occur              TYPE /odsmfe/tb_fmass-occur, "c LENGTH 3,
             formcategory       TYPE /odsmfe/tb_fmass-category, "c LENGTH 30,
             postnotification   TYPE /odsmfe/tb_fmass-postnotification, "c LENGTH 1,
             theme              TYPE /odsmfe/tb_fmass-theme, "c LENGTH 20,
             stylesheet         TYPE /odsmfe/tb_fmass-stylesheet, "c LENGTH 20,
             assigneddate       TYPE /odsmfe/tb_fmass-assigneddate, "dats,
             assignedtime       TYPE /odsmfe/tb_fmass-assignedtime, "tims,
             assignedby         TYPE /odsmfe/tb_fmass-assignedby, "c LENGTH 50,
             jobtype            TYPE /odsmfe/tb_fmass-jobtype, "c LENGTH 4,
             flowsequence       TYPE /odsmfe/tb_fmass-flowsequence, "c LENGTH 3,
             formname           TYPE /odsmfe/tb_fmass-formname, " LENGTH 50,
             active             TYPE /odsmfe/de_active,
           END OF ltys_formassignment .

    DATA: lst_form           TYPE /odsmfe/tb_fmass,
          lit_response       TYPE STANDARD TABLE OF /odsmfe/tb_fmass,
          lst_formassignment TYPE ltys_formassignment.

* Variables
    DATA: lv_formid             TYPE /odsmfe/tb_fmass-formid,
          lv_version            TYPE /odsmfe/tb_fmass-version,
          lv_formassignmenttype TYPE /odsmfe/tb_fmass-formassignmenttype,
          lv_workordernum       TYPE aufnr,
          lv_oprnum             TYPE /odsmfe/tb_fmass-oprnum,
          lv_notification       TYPE /odsmfe/tb_fmass-notification, "qmnum,
          lv_notificationitem   TYPE /odsmfe/tb_fmass-notificationitem,
          lv_notificationtask   TYPE /odsmfe/tb_fmass-notificationtask, "manum,
          lv_equipment          TYPE /odsmfe/tb_fmass-equipment, "equnr,
          lv_functionallocation TYPE /odsmfe/tb_fmass-functionallocation, "tplnr,
          lv_roleid             TYPE /odsmfe/de_roleid.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

*    im_data_provider->read_entry_data( IMPORTING es_data = lst_formassignment ).

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
      lst_form-category       = lst_formassignment-formcategory.
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
*--End of changes - SKOTRA ES1K902967
*--Get Role ID
        SELECT SINGLE roleid FROM (ls_usrroletab-low)  " SKOTRA ES1K902967
        WHERE userid = @sy-uname
        AND startdate LE @sy-datum
        AND enddate GE @sy-datum INTO @lv_roleid.
        IF sy-subrc = 0.
          lst_form-roleid = lv_roleid.
        ENDIF.
      ENDIF.

* Fetching data from table /odsmfe/tb_fmass
      SELECT SINGLE formid ,version, formassignmenttype, workordernum, notification, oprnum ,notificationitem ,notificationtask , equipment, functionallocation
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
      AND   functionallocation  = @lst_form-functionallocation
      INTO (@lv_formid,@lv_version,@lv_formassignmenttype,@lv_workordernum,@lv_notification,@lv_oprnum,@lv_notificationitem,
      @lv_notificationtask,@lv_equipment,@lv_functionallocation).

      IF sy-subrc EQ 0 AND lst_formassignment-formid EQ lv_formid.
* Insert data to Table /odsmfe/tb_fmass
        MODIFY /odsmfe/tb_fmass FROM @lst_form.
        IF sy-subrc <> 0.
          CLEAR lst_form.
        ENDIF.
      ENDIF.
    ENDIF.

* Send the instance ID to Front end application
*    gstib_entity-formid             = lst_formassignment-formid.
*    gstib_entity-version            = lst_formassignment-version.
*    gstib_entity-formassignmenttype = lst_formassignment-formassignmenttype.
*    gstib_entity-workordernum       = lst_formassignment-workordernum.
*    gstib_entity-oprnum             = lst_formassignment-oprnum.
*    gstib_entity-notification       = lst_formassignment-notification.
*    gstib_entity-notificationitem   = lst_formassignment-notificationitem.
*    gstib_entity-notificationtask   = lst_formassignment-notificationtask.
*    gstib_entity-equipment          = lst_formassignment-equipment.
*    gstib_entity-functionallocation = lst_formassignment-functionallocation.
*    gstib_entity-mandatory          = lst_formassignment-mandatory.
*    gstib_entity-multiplesub        = lst_formassignment-multiplesub.
*    gstib_entity-occur              = lst_formassignment-occur.
*    gstib_entity-formcategory       = lst_formassignment-formcategory.
*    gstib_entity-postnotification   = lst_formassignment-postnotification.
*    gstib_entity-theme              = lst_formassignment-theme.
*    gstib_entity-stylesheet         = lst_formassignment-stylesheet.
*    gstib_entity-assigneddate       = lst_formassignment-assigneddate.
*    gstib_entity-assignedtime       = lst_formassignment-assignedtime.
*    gstib_entity-assignedby         = lst_formassignment-assignedby.
*    gstib_entity-jobtype            = lst_formassignment-jobtype.
*    gstib_entity-flowsequence       = lst_formassignment-flowsequence.
*    gstib_entity-formname           = lst_formassignment-formname.
*    gstib_entity-active             = abap_true.
*    gstib_entity-roleid             = lv_roleid.
*   GET REFERENCE OF gstib_entity INTO ex_entity.
*     ex_response = lst_formassignment.

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
*----------------------------------------------------------------------
* object reference
    DATA: "lo_filter        TYPE REF TO /iwbep/if_mgw_req_filter,
      lst_filter       TYPE TABLE OF /odsmfe/st_core_range_str,
      lst_filter_range TYPE /odsmfe/st_core_range_str,
      lrt_workordernum TYPE TABLE OF /odsmfe/st_core_range_str,
      lrs_workordernum TYPE /odsmfe/st_core_range_str,
      lrt_enterby      TYPE TABLE OF /odsmfe/st_core_range_str,
      lrs_enterby      TYPE /odsmfe/st_core_range_str,
      lv_aufnr         TYPE aufnr,
      lv_delta_token   TYPE timestamp,
      lv_mobileuser    TYPE string.
    DATA: lit_entity_temp TYPE TABLE OF /odsmfe/tb_fmass,
          lst_entity_temp TYPE /odsmfe/tb_fmass,
          lit_valid_wo    TYPE STANDARD TABLE OF /odsmfe/pm_valid_aufnr_str.
    CONSTANTS: lc_workordernum TYPE string VALUE 'WORKORDERNUM',
               lc_enterby      TYPE string VALUE 'CREATEDBY'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
*    IF im_tech_request_context IS SUPPLIED.
*      lo_filter = im_tech_request_context->get_filter( ).
*    ENDIF.
    CALL METHOD me->get_cloud_dest
      IMPORTING
        ex_dest = DATA(lv_rfc).

    LOOP AT im_filter_select_options INTO DATA(ls_filter_select_options).
      CASE ls_filter_select_options-name.
        WHEN lc_workordernum."WorkOrderNumber
          lrt_workordernum = CORRESPONDING #(  ls_filter_select_options-range ).
          DELETE lrt_workordernum WHERE low IS INITIAL.
          READ TABLE lrt_workordernum INTO lrs_workordernum INDEX 1.
          IF sy-subrc = 0.
            " lv_aufnr = lrs_workordernum-low.
            lv_aufnr = |{ lrs_workordernum-low ALPHA = OUT }|.


*              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*              DESTINATION lv_rfc
*                EXPORTING
*                  input  = lv_aufnr
*                IMPORTING
*                  output = lv_aufnr.
          ENDIF.
        WHEN lc_enterby."EnteredBy
          lrt_enterby = CORRESPONDING #(  ls_filter_select_options-range ).
          READ TABLE lrt_enterby  INTO lrs_enterby INDEX 1.
          IF sy-subrc EQ 0 AND lrs_enterby-low IS NOT INITIAL .
            lv_mobileuser = lrs_enterby-low.
            TRANSLATE lv_mobileuser TO UPPER CASE.
          ENDIF.
      ENDCASE.
    ENDLOOP.


    IF lv_mobileuser IS INITIAL.
      lv_mobileuser = sy-uname.
    ENDIF.
*--Start of changes - ES1K902967

    DATA:lr_getworkorder TYPE REF TO /odsmfe/cl_get_workorder_data.
    CREATE OBJECT lr_getworkorder
      EXPORTING
        im_entity_name = im_entity_name.

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
      FOR ALL ENTRIES IN @lit_valid_wo
      WHERE formid NE @space
      AND workordernum = @lit_valid_wo-aufnr  INTO CORRESPONDING FIELDS OF TABLE @lit_entity_temp.

      IF sy-subrc IS INITIAL.
        SORT lit_entity_temp BY formid.
      ENDIF.
    ENDIF.
    IF lit_entity_temp IS NOT INITIAL .
*      LOOP AT lit_entity_temp INTO lst_entity_temp.
*        MOVE-CORRESPONDING lst_entity_temp TO gstib_entity.
*        APPEND gstib_entity TO gitib_entity.
*        CLEAR: lst_entity_temp,gstib_entity.
*      ENDLOOP.
      MOVE-CORRESPONDING lit_entity_temp TO ex_response_data.

*      LOOP AT lit_entity_temp INTO lst_entity_temp WHERE deleted IS NOT INITIAL.
*        MOVE-CORRESPONDING lst_entity_temp TO gstib_del_entity.
*        APPEND gstib_del_entity TO gitib_del_entity.
*        CLEAR: lst_entity_temp,gstib_del_entity.
*      ENDLOOP.

    ENDIF.
* Delete deleted entry from inital entity
*    DELETE gitib_entity WHERE deleted IS NOT INITIAL.
*
** Delete Duplicate entries
*    DELETE ADJACENT DUPLICATES FROM gitib_entity COMPARING ALL FIELDS.
*    IF im_tech_request_context_entity IS SUPPLIED AND  gitib_entity IS NOT INITIAL.
*      READ TABLE gitib_entity INTO gstib_entity INDEX 1.
*      IF sy-subrc EQ 0.
*        GET REFERENCE OF gstib_entity INTO ex_entity.
*      ENDIF.
*    ELSE.
*      GET REFERENCE OF gitib_entity INTO ex_entityset.
*      GET REFERENCE OF gitib_del_entity INTO ex_deleted_entityset.
*    ENDIF.
** TimeStamp Field for supplying Delta token
*    GET TIME STAMP FIELD lv_delta_token.
*
** Export the delta token
*    ex_response_context-deltatoken = lv_delta_token.

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

      SELECT COUNT(*) FROM /odsmfe/tb_wo_ex
            WHERE objkey = @order_number INTO @lv_count.
      IF lv_count = 0.
        lst_delta-action = 'I'.
      ELSE.
        lst_delta-action = 'U'.
      ENDIF.

      lst_delta-objkey     = order_number.

      SELECT SINGLE low FROM /odsmfe/tb_filtr
                          WHERE entitysetname = 'FormAttachmentSet'
                            AND tabname = 'WORKORDER'
                            AND field = 'EXCHANGE_TABLE' INTO @lv_exchange_table.

    ELSEIF notification IS NOT INITIAL.

      SELECT COUNT(*) FROM /odsmfe/tb_no_ex
          WHERE objkey = @notification INTO @lv_count.

      IF lv_count = 0.
        lst_delta-action = 'I'.
      ELSE.
        lst_delta-action = 'U'.
      ENDIF.

      lst_delta-objkey     = notification.

      SELECT SINGLE low FROM /odsmfe/tb_filtr
                          WHERE entitysetname = 'FormAttachmentSet'
                            AND tabname = 'NOTIFICATION'
                            AND field = 'EXCHANGE_TABLE' INTO @lv_exchange_table.

    ENDIF.

    IF lv_exchange_table IS NOT INITIAL.

      MODIFY (lv_exchange_table) FROM @lst_delta.
      IF sy-subrc = 0 .
        COMMIT WORK .
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD read_delta_table.
*Variables
    DATA: "lv_exchobj        TYPE /syclo/core_exchobj_dte,
      lv_mobile_app     TYPE string, "/syclo/core_mobile_app_dte,
      lv_bor_objtyp     TYPE c LENGTH 10, " oj_name,
      lv_sys_tzone      TYPE tznzone, "tznzonesys,
      lv_ts             TYPE timestamp,
      lv_ts_str         TYPE string,
      lv_date           TYPE datn,
      lv_time           TYPE timn,
      lv_sys_time_token TYPE string.
    DATA: lv_filter TYPE string.

*Tables & Structures
    DATA: BEGIN OF ls_date_time,
            date TYPE datn, "datum
            time TYPE timn, "uzeit,
          END OF ls_date_time.

    DATA : lit_ex_aufnr          TYPE TABLE OF /odsmfe/ex_aufnr,
           lv_exchange_table(20) TYPE c.

    CONSTANTS : lc_tzone     TYPE tznzone VALUE 'UTC', "tznzonesys
                lc_tzone_utc TYPE tznzone VALUE 'UTC'. "tznzonesys

    lv_ts_str = delta_token.
    ls_date_time = lv_ts_str.
    lv_mobile_app = '/ODSMFE/SAP_WM_MOBILE_APP'.
*    /syclo/cl_core_bapi_tools=>get_system_time(
*      IMPORTING ev_sys_tzone = lv_sys_tzone ).
*    SOC BY Priyanka
    TYPES:
      BEGIN OF ty_data,
        wa(512) TYPE c,
      END OF ty_data.

    TYPES: BEGIN OF ty_options,
             text(72) TYPE c,
           END OF ty_options.

    TYPES: BEGIN OF ty_fields,
             fieldname(30) TYPE c,
             offset(6)     TYPE n,
             length(6)     TYPE n,
             type(1)       TYPE c,
             fieldtext(60) TYPE c,
           END OF ty_fields.


    DATA:lt_fields TYPE TABLE OF ty_fields,
         lt_data   TYPE TABLE OF ty_data,
         ls_data   TYPE ty_data.

    CALL METHOD me->get_cloud_dest
      IMPORTING
        ex_dest = DATA(lv_rfc).

    lt_fields  = VALUE #( ( fieldname = 'TZONESYS' ) ).


    CALL FUNCTION 'RFC_READ_TABLE'
      DESTINATION lv_rfc
      EXPORTING
        query_table = 'TTZCU'
      TABLES
        fields      = lt_fields
        data        = lt_data.

    lv_sys_tzone = ls_Data+0(6).

*       SOC BY Priyanka

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

    SELECT SINGLE low FROM /odsmfe/tb_filtr
                    WHERE entitysetname = 'FormAttachmentSet'
                      AND tabname = 'WORKORDER'
                      AND field = 'EXCHANGE_TABLE' INTO @lv_exchange_table.

    IF lv_exchange_table IS NOT INITIAL.

      SELECT * FROM (lv_exchange_table)
                                     WHERE (lv_filter) INTO TABLE @lit_ex_aufnr.
      IF lit_ex_aufnr IS NOT INITIAL.
        ex_aufnr_data[] = lit_ex_aufnr[].
      ENDIF.
    ENDIF.


  ENDMETHOD.
ENDCLASS.
