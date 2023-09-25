class /ODSMFE/CL_FORMAPPROVER definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public.

public section.

*  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_FORMAPPROVER .
*  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_FORMAPPROVER .

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



CLASS /ODSMFE/CL_FORMAPPROVER IMPLEMENTATION.


    METHOD /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_CREATE_ENTITYSET .
*
************************************************************************
*********************** CREATED HISTORY ********************************
** Program Author (SID)   : Sravan Kumar
** Creation Date          : 28/05/2021
** Transport No.          : ES1K902703
** Program Description    : Method used to Create the Approver
************************************************************************
*********************** CHANGE HISTORY *********************************
** Program Author (SID)   :
** Change Date            :
** Transport No.          :
** Change Description     :
************************************************************************
**----------------------------------------------------------------------
**  Data declaration
**----------------------------------------------------------------------
** Internal table & Structures
*    TYPES: BEGIN OF ltys_formaid ,
*             formid             TYPE /odsmfe/tb_fapvr-formid,
*             version            TYPE c LENGTH 3,
*             workordernum       TYPE aufnr,
*             oprnum             TYPE c LENGTH 4,                  "vornr,
*             approverid         TYPE c LENGTH 20,
*             notification       TYPE c length 12,
*             notificationitem   TYPE N length 4, "felfd,
*             notificationtask   TYPE N length 4, "manum,
*             equipment          TYPE c LENGTH 18, "equnr,
*             functionallocation TYPE c LENGTH 30, "tplnr,
*             formname           TYPE c LENGTH 50,
*             formstatus         TYPE c LENGTH 15,
*             assigneddate       TYPE c LENGTH 8,                     "dats,
*             assignedtime       TYPE c LENGTH 6,             "tims,
*             assignedby         TYPE c LENGTH 50,
*           END OF ltys_formaid.
*
*    DATA: lst_formid TYPE ltys_formaid,
*          lst_form   TYPE /odsmfe/tb_fapvr.
**-------------------------------------------------------------
** Main Section
**-------------------------------------------------------------
*SOC by OBUL
*    im_data_provider->read_entry_data( IMPORTING es_data = lst_formid ). *
*EOC by OBUL
*    IF lst_formid IS NOT INITIAL.
*      lst_form-formid              = lst_formid-formid.
*      lst_form-version             = lst_formid-version.
*      lst_form-workordernum        = lst_formid-workordernum.
*      lst_form-oprnum              = lst_formid-oprnum.
*      lst_form-approverid          = lst_formid-approverid.
*      lst_form-notification        = lst_formid-notification .
*      lst_form-notificationitem    = lst_formid-notificationitem.
*      lst_form-notificationtask    = lst_formid-notificationtask.
*      lst_form-equipment           = lst_formid-equipment.
*      lst_form-functionallocation  = lst_formid-functionallocation.
*      lst_form-formname            = lst_formid-formname.
*      lst_form-formstatus          = lst_formid-formstatus.
*      lst_form-assigneddate        = lst_formid-assigneddate.
*      lst_form-assignedtime        = lst_formid-assignedtime.
*      lst_form-assignedby          = lst_formid-assignedby.
*
*
*      IF lst_formid-formid IS NOT INITIAL.
* Insert data to Table /odsmfe/tb_fapvr
*        INSERT /odsmfe/tb_fapvr FROM @lst_form.
*        IF sy-subrc <> 0.
*          CLEAR lst_form.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
* Send the Form ID to Front end application
*    gstib_entity-formid              = lst_formid-formid.
*    gstib_entity-version             = lst_formid-version.
*    gstib_entity-workordernum        = lst_formid-workordernum.
*    gstib_entity-oprnum              = lst_formid-oprnum.
*    gstib_entity-approverid          = lst_formid-approverid.
*    gstib_entity-notification        = lst_formid-notification .
*    gstib_entity-notificationitem    = lst_formid-notificationitem.
*    gstib_entity-notificationtask    = lst_formid-notificationtask.
*    gstib_entity-equipment           = lst_formid-equipment.
*    gstib_entity-functionallocation  = lst_formid-functionallocation.
*    gstib_entity-formname            = lst_formid-formname.
*   gstib_entity-formstatus          = lst_formid-formstatus.
*    gstib_entity-assigneddate        = lst_formid-assigneddate.
*    gstib_entity-assignedtime        = lst_formid-assignedtime.
*    gstib_entity-assignedby          = lst_formid-assignedby.
*
*    GET REFERENCE OF gstib_entity INTO ex_entity.

    ENDMETHOD.


 METHOD /odsmfe/if_get_entityset_bapi~gmib_delete_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 28/05/2021
* Transport No.          : ES1K902703
* Program Description    : This Method Used to Delete the FormApprover
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
*    DATA: lrt_formid        TYPE TABLE OF /odsmfe/st_core_range_str,
*          lrt_version       TYPE TABLE OF /odsmfe/st_core_range_str,
*          lrt_approverid    TYPE TABLE OF /odsmfe/st_core_range_str,
*          lrt_workordernum  TYPE TABLE OF /odsmfe/st_core_range_str,
*          lrt_oprnum        TYPE TABLE OF /odsmfe/st_core_range_str,
*          lrt_notif         TYPE TABLE OF /odsmfe/st_core_range_str,
*          lrt_notifitem     TYPE TABLE OF /odsmfe/st_core_range_str,
*          lrt_notiftask     TYPE TABLE OF /odsmfe/st_core_range_str,
*          lrt_equip         TYPE TABLE OF /odsmfe/st_core_range_str,
*          lrt_funlocation   TYPE TABLE OF /odsmfe/st_core_range_str,
*          "lst_key_tab       TYPE /iwbep/s_mgw_name_value_pair,
*          lrs_it_formid     TYPE /odsmfe/st_core_range_str,
*          lrs_it_version    TYPE /odsmfe/st_core_range_str,
*          lrs_it_approverid TYPE /odsmfe/st_core_range_str,
*          lrs_workordernum  TYPE /odsmfe/st_core_range_str,
*          lrs_oprnum        TYPE /odsmfe/st_core_range_str,
*          lrs_notif         TYPE /odsmfe/st_core_range_str,
*          lrs_notifitem     TYPE /odsmfe/st_core_range_str,
*          lrs_notiftask     TYPE /odsmfe/st_core_range_str,
*          lrs_equip         TYPE /odsmfe/st_core_range_str,
*          lrs_funlocation   TYPE /odsmfe/st_core_range_str,
*          lst_formapprover  TYPE /odsmfe/tb_fapvr,
*          lit_final         TYPE STANDARD TABLE OF /odsmfe/tb_fapvr.
*          "lit_key_tab       TYPE /iwbep/t_mgw_name_value_pair.
*
** Constants
*    CONSTANTS: lc_e            TYPE string VALUE 'E',
*               lc_i            TYPE string VALUE 'I',
*               lc_eq           TYPE string VALUE 'EQ',
*               lc_formid       TYPE string VALUE 'FormID',
*               lc_version      TYPE string VALUE 'Version',
*               lc_workordernum TYPE string VALUE 'WorkOrderNum',
*               lc_oprnum       TYPE string VALUE 'OprNum',
*               lc_approverid   TYPE string VALUE 'ApproverID',
*               lc_notif        TYPE string VALUE 'Notification',
*               lc_notifitem    TYPE string VALUE 'NotificationItem',
*               lc_notiftask    TYPE string VALUE 'NotificationTask',
*               lc_equip        TYPE string VALUE 'Equipment',
*               lc_funlocation  TYPE string VALUE 'FunctionalLocation'.
**-------------------------------------------------------------
** Main Section
**-------------------------------------------------------------
*
**    im_data_provider->read_entry_data( IMPORTING es_data = lst_formapprover ).
** SOC BY OBUL
**    IF im_key_tab IS NOT INITIAL.
*
**      LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
**        CASE lst_key_tab-name.
**          WHEN lc_formid.
**            lrs_it_formid-sign = lc_i.
**            lrs_it_formid-option = lc_eq.
**            lrs_it_formid-low = lst_key_tab-value.
**            lst_formapprover-formid = lst_key_tab-value.
**            APPEND lrs_it_formid TO lrt_formid.
**            CLEAR lrs_it_formid.
**
**          WHEN lc_version.
**            lrs_it_version-sign   = lc_i.
**            lrs_it_version-option = lc_eq.
**            lrs_it_version-low    = lst_key_tab-value.
**            lst_formapprover-version    = lst_key_tab-value.*
**            APPEND lrs_it_version TO lrt_version.
**            CLEAR lrs_it_version.
**
**          WHEN lc_workordernum.
**            lrs_workordernum-sign   = lc_i.
**            lrs_workordernum-option = lc_eq.
**            lrs_workordernum-low    = lst_key_tab-value.
**            lst_formapprover-workordernum = lst_key_tab-value.
**            CALL FUNCTION 'CONVERSION_EXIT_AUFNR_INPUT'
**              EXPORTING
**                input  = lst_formapprover-workordernum
**              IMPORTING
**                output = lst_formapprover-workordernum.
**            APPEND lrs_workordernum TO lrt_workordernum.
**            CLEAR lrs_workordernum.
**
**          WHEN lc_oprnum.
**            lrs_oprnum-sign   = lc_i.
**            lrs_oprnum-option = lc_eq.
**            lrs_oprnum-low    = lst_key_tab-value.
**            lst_formapprover-oprnum    = lst_key_tab-value.
**            APPEND lrs_oprnum TO lrt_oprnum.
**            CLEAR lrs_oprnum.
**
**          WHEN lc_approverid.
**            lrs_it_approverid-sign   = lc_i.
**            lrs_it_approverid-option = lc_eq.
**            lrs_it_approverid-low    = lst_key_tab-value.
**            lst_formapprover-approverid    = lst_key_tab-value.
**            APPEND lrs_it_approverid TO lrt_approverid.
**            CLEAR lrs_it_approverid.
*
**          WHEN lc_notif.
**            lrs_notif-sign   = lc_i.
**            lrs_notif-option = lc_eq.
**            lrs_notif-low    = lst_key_tab-value.
**            lst_formapprover-notification = lst_key_tab-value.
**
**            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
**              EXPORTING
**                input  = lst_formapprover-notification
**              IMPORTING
**                output = lst_formapprover-notification.
**            APPEND lrs_notif TO lrt_notif.
**            CLEAR lrs_notif.
**
**          WHEN lc_notifitem.
**            lrs_notifitem-sign   = lc_i.
**            lrs_notifitem-option = lc_eq.
**            lrs_notifitem-low    = lst_key_tab-value.
**            lst_formapprover-notificationitem    = lst_key_tab-value.
**            APPEND lrs_notifitem TO lrt_notifitem.
**            CLEAR lrs_notifitem.
**
**          WHEN lc_notiftask.
**            lrs_notiftask-sign   = lc_i.
**            lrs_notiftask-option = lc_eq.
**            lrs_notiftask-low    = lst_key_tab-value.
**            lst_formapprover-notificationtask = lst_key_tab-value.
**            APPEND lrs_notiftask TO lrt_notiftask.
**            CLEAR lrs_notiftask.
**
**          WHEN lc_equip.
**            lrs_equip-sign   = lc_i.
**            lrs_equip-option = lc_eq.
**            lrs_equip-low    = lst_key_tab-value.
**            lst_formapprover-equipment = lst_key_tab-value.
**            APPEND lrs_equip TO lrt_equip.
**            CLEAR lrs_equip.
**
**          WHEN lc_funlocation.
**            lrs_funlocation-sign   = lc_i.
**            lrs_funlocation-option = lc_eq.
**            lrs_funlocation-low    = lst_key_tab-value.
**            lst_formapprover-functionallocation =  lst_key_tab-value.
**            APPEND lrs_funlocation TO lrt_funlocation.
**            CLEAR lrs_funlocation.
**
**        ENDCASE.
**      ENDLOOP.
**    ENDIF.
**EOC BY OBUL
*
** Fetching data from table
*    SELECT * FROM /odsmfe/tb_fapvr
**             INTO TABLE @lit_final
*             WHERE   formid             IN @lrt_formid
*             AND     version            IN @lrt_version
**             AND     workordernum       IN lrt_workordernum
*             AND     workordernum       EQ @lst_formapprover-workordernum
*             AND     oprnum             IN @lrt_oprnum
*             AND     approverid         IN @lrt_approverid
**             AND     notification       IN lrt_notif
*             AND     notification       EQ @lst_formapprover-notification
*             AND     notificationitem   IN @lrt_notifitem
*             AND     notificationtask   IN @lrt_notiftask
*             AND     equipment          IN @lrt_equip
*             AND     functionallocation IN @lrt_funlocation
*             INTO TABLE @lit_final.
*
*
*    IF sy-subrc EQ 0.
** Delete data to Table /odsmfe/tb_fapvr
*      DELETE /odsmfe/tb_fapvr FROM TABLE @lit_final.
*
** Send the Form ID to Front end application
**      gstib_entity-formid              = lst_formapprover-formid.
**      gstib_entity-version             = lst_formapprover-version.
**      gstib_entity-workordernum        = lst_formapprover-workordernum.
**      gstib_entity-oprnum              = lst_formapprover-oprnum.
**      gstib_entity-approverid          = lst_formapprover-approverid.
**      gstib_entity-notification        = lst_formapprover-notification .
**      gstib_entity-notificationitem    = lst_formapprover-notificationitem.
**      gstib_entity-notificationtask    = lst_formapprover-notificationtask.
**      gstib_entity-equipment           = lst_formapprover-equipment.
**      gstib_entity-functionallocation  = lst_formapprover-functionallocation.
**      gstib_entity-formname            = lst_formapprover-formname.
**      gstib_entity-formstatus          = lst_formapprover-formstatus.
**      gstib_entity-assigneddate        = lst_formapprover-assigneddate.
**      gstib_entity-assignedtime        = lst_formapprover-assignedtime.
**      gstib_entity-assignedby          = lst_formapprover-assignedby.
*
**      GET REFERENCE OF gstib_entity INTO ex_entity.
*   ENDIF.

 ENDMETHOD.


 METHOD /odsmfe/if_get_entityset_bapi~gmib_modify_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 28/05/2021
* Transport No.          : ES1K902703
* Program Description    : Method used to Update the Approver
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
*    TYPES: BEGIN OF ltys_formaid ,
*             formid             TYPE /odsmfe/tb_fapvr-formid,
*             version            TYPE /ODSMFE/CE_FORMAPPROVER-Version,
*             workordernum       TYPE /ODSMFE/CE_FORMAPPROVER-WorkOrderNum,             "aufnr,
*             oprnum             TYPE /ODSMFE/CE_FORMAPPROVER-OprNum,                   "vornr,
*             approverid         TYPE /ODSMFE/CE_FORMAPPROVER-ApproverID,
*             notification       TYPE /ODSMFE/CE_FORMAPPROVER-Notification,             "qmnum,
*             notificationitem   TYPE /ODSMFE/CE_FORMAPPROVER-NotificationItem,         "felfd,
*             notificationtask   TYPE /ODSMFE/CE_FORMAPPROVER-NotificationTask,         "manum,
*             equipment          TYPE /ODSMFE/CE_FORMAPPROVER-Equipment,                "equnr,
*             functionallocation TYPE /ODSMFE/CE_FORMAPPROVER-FunctionalLocation,       "tplnr
*             formname           TYPE /ODSMFE/CE_FORMAPPROVER-FormName,
*             formstatus         TYPE /ODSMFE/CE_FORMAPPROVER-FormStatus,
*             assigneddate       TYPE /ODSMFE/CE_FORMAPPROVER-AssignedDate,
*             assignedtime       TYPE /ODSMFE/CE_FORMAPPROVER-AssignedTime,
*             assignedby         TYPE /ODSMFE/CE_FORMAPPROVER-AssignedBy,
*           END OF ltys_formaid.
*
*    DATA: lst_formid TYPE ltys_formaid,
*          lst_form   TYPE /odsmfe/tb_fapvr.
*
** Variables
*    DATA: lv_formid       TYPE c LENGTH 50,
*          lv_formversion  TYPE c LENGTH 3,
*          lv_workordernum TYPE aufnr,
*          lv_oprnum       TYPE c LENGTH 4,              "vornr,
*          lv_approverid   TYPE c LENGTH 40,
*          lv_notif        TYPE /ODSMFE/CE_FORMAPPROVER-Notification,        "qmnum,
*          lv_notifitem    TYPE /ODSMFE/CE_FORMAPPROVER-NotificationItem,    "fenum,
*          lv_notiftask    TYPE /ODSMFE/CE_FORMAPPROVER-NotificationTask,     "manum,
*          lv_equip        TYPE /ODSMFE/CE_FORMAPPROVER-Equipment,             "equnr,
*          lv_funlocation  TYPE /ODSMFE/CE_FORMAPPROVER-FunctionalLocation.    " tplnr.
**-------------------------------------------------------------
** Main Section
**-------------------------------------------------------------
*
*   " im_data_provider->read_entry_data( IMPORTING es_data = lst_formid ).
*
*    IF lst_formid IS NOT INITIAL.
*      lst_form-formid              = lst_formid-formid.
*      lst_form-version             = lst_formid-version.
*      lst_form-workordernum        = lst_formid-workordernum.
*      lst_form-oprnum              = lst_formid-oprnum.
*      lst_form-approverid          = lst_formid-approverid.
*      lst_form-notification        = lst_formid-notification .
*      lst_form-notificationitem    = lst_formid-notificationitem.
*      lst_form-notificationtask    = lst_formid-notificationtask.
*      lst_form-equipment           = lst_formid-equipment.
*      lst_form-functionallocation  = lst_formid-functionallocation.
*      lst_form-formname            = lst_formid-formname.
*      lst_form-formstatus          = lst_formid-formstatus.
*      lst_form-assigneddate        = lst_formid-assigneddate.
*      lst_form-assignedtime        = lst_formid-assignedtime.
*      lst_form-assignedby          = lst_formid-assignedby.
*
*      IF lst_form IS NOT INITIAL.
** Fetching data from table
*        SELECT SINGLE formid, version, workordernum, oprnum, approverid, notification, notificationitem,
*        notificationtask, equipment, functionallocation
*        FROM /odsmfe/tb_fapvr
*        WHERE   formid             = @lst_formid-formid
*        AND     version            = @lst_formid-version
*        AND     workordernum       = @lst_formid-workordernum
*        AND     oprnum             = @lst_formid-oprnum
*        AND     approverid         = @lst_formid-approverid
*        AND     notification       = @lst_formid-notification
*        AND     notificationitem   = @lst_formid-notificationitem
*        AND     notificationtask   = @lst_formid-notificationtask
*        AND     equipment          = @lst_formid-equipment
*        AND     functionallocation = @lst_formid-functionallocation
*        INTO (@lv_formid, @lv_formversion, @lv_workordernum, @lv_oprnum, @lv_approverid, @lv_notif, @lv_notifitem, @lv_notiftask, @lv_equip, @lv_funlocation).
*
*      ENDIF.
*
*      IF sy-subrc EQ 0 AND lst_formid-formid EQ lv_formid.
** Insert data to Table /odsmfe/tb_fapvr
*        MODIFY /odsmfe/tb_fapvr FROM @lst_form.
*        IF sy-subrc <> 0.
*          CLEAR lst_form.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
** Send the Form ID to Front end application
**    gstib_entity-formid              = lst_formid-formid.
**    gstib_entity-version             = lst_formid-version.
**    gstib_entity-workordernum        = lst_formid-workordernum.
**    gstib_entity-oprnum              = lst_formid-oprnum.
**    gstib_entity-approverid          = lst_formid-approverid.
**    gstib_entity-notification        = lst_formid-notification .
**    gstib_entity-notificationitem    = lst_formid-notificationitem.
**    gstib_entity-notificationtask    = lst_formid-notificationtask.
**    gstib_entity-equipment           = lst_formid-equipment.
**    gstib_entity-functionallocation  = lst_formid-functionallocation.
**    gstib_entity-formname            = lst_formid-formname.
**    gstib_entity-formstatus          = lst_formid-formstatus.
**    gstib_entity-assigneddate        = lst_formid-assigneddate.
**    gstib_entity-assignedtime        = lst_formid-assignedtime.
**    gstib_entity-assignedby          = lst_formid-assignedby.
**
**    GET REFERENCE OF gstib_entity INTO ex_entity.

 ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : Sravan Kumar
* Creation Date          : 28/05/2021
* Transport No.          : ES1K902703
* Program Description    : Method used to Fetch the Approvers
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
* Internal Table
    DATA: lit_formid        TYPE  STANDARD TABLE OF /odsmfe/tb_fapvr,                                      "/odsmfe/tb_fapvr,
          lrt_formid        TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_version       TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_approverid    TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_workordernum  TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_oprnum        TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_notif         TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_notifitem     TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_notiftask     TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_equip         TYPE TABLE OF /odsmfe/st_core_range_str,
          lrt_funlocation   TYPE TABLE OF /odsmfe/st_core_range_str,
          "lst_key_tab       TYPE /iwbep/s_mgw_name_value_pair,
          lrs_it_formid     TYPE /odsmfe/st_core_range_str,
          lrs_it_version    TYPE /odsmfe/st_core_range_str,
          lrs_it_approverid TYPE /odsmfe/st_core_range_str,
          lrs_workordernum  TYPE /odsmfe/st_core_range_str,
          lrs_oprnum        TYPE /odsmfe/st_core_range_str,
          lrs_notif         TYPE /odsmfe/st_core_range_str,
          lrs_notifitem     TYPE /odsmfe/st_core_range_str,
          lrs_notiftask     TYPE /odsmfe/st_core_range_str,
          lrs_equip         TYPE /odsmfe/st_core_range_str,
          lrs_funlocation   TYPE /odsmfe/st_core_range_str.

* Constants
    CONSTANTS: lc_e            TYPE string VALUE 'E',
               lc_i            TYPE string VALUE 'I',
               lc_eq           TYPE string VALUE 'EQ',
               lc_formid       TYPE string VALUE 'FORMID',
               lc_version      TYPE string VALUE 'VERSION',
               lc_workordernum TYPE string VALUE 'WORKORDERNUM',
               lc_oprnum       TYPE string VALUE 'OPRNUM',
               lc_approverid   TYPE string VALUE 'APPROVERID',
               lc_notif        TYPE string VALUE 'NOTIFICATION',
               lc_notifitem    TYPE string VALUE 'NOTIFICATIONITEM',
               lc_notiftask    TYPE string VALUE 'NOTIFICATIONTASK',
               lc_equip        TYPE string VALUE 'EQUIPMENT',
               lc_funlocation  TYPE string VALUE 'FUNCTIONALLOCATION'.
* Variables
    DATA: lv_mobileuser  TYPE string,
          lv_delta_token TYPE timestamp.
* Field Symbols
    FIELD-SYMBOLS <lfsst_form> TYPE /odsmfe/tb_fapvr.
* Internal Table
    DATA: lit_entity_temp TYPE TABLE OF /odsmfe/tb_forsp,
          lst_entity_temp TYPE /odsmfe/tb_forsp,
          lst_valid_wo    TYPE /odsmfe/pm_valid_aufnr_str,
*          lit_valid_wo    TYPE STANDARD TABLE OF /odsmfe/pm_valid_aufnr_str
              lit_valid_wo    TYPE STANDARD TABLE OF /odsmfe/tb_aufnr.

    DATA:  lo_delta_context TYPE REF TO if_rap_query_request,
        "lo_delta_context TYPE REF TO /iwbep/if_mgw_req_entityset,
          lo_ref_exch_data TYPE REF TO data.

    DATA: lo_filter  TYPE REF TO if_rap_query_filter,
           lit_return TYPE  if_rap_query_filter=>tt_name_range_pairs.

    DATA: IM_TECH_REQUEST_CONTEXT type ref to if_rap_query_request,
          ex_response_context TYPE timestamp.


*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

*    IF im_key_tab IS NOT INITIAL.
*
*      LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
*        CASE lst_key_tab-name.
*          WHEN lc_formid.
*            lrs_it_formid-sign = lc_i.
*            lrs_it_formid-option = lc_eq.
*            lrs_it_formid-low = lst_key_tab-value.
*            APPEND lrs_it_formid TO lrt_formid.
*            CLEAR lrs_it_formid.
*
*          WHEN lc_version.
*            lrs_it_version-sign   = lc_i.
*            lrs_it_version-option = lc_eq.
*            lrs_it_version-low    = lst_key_tab-value.
*            APPEND lrs_it_version TO lrt_version.
*            CLEAR lrs_it_version.
*
*          WHEN lc_workordernum.
*            lrs_workordernum-sign   = lc_i.
*            lrs_workordernum-option = lc_eq.
*            lrs_workordernum-low    = lst_key_tab-value.
*            APPEND lrs_workordernum TO lrt_workordernum.
*            CLEAR lrs_workordernum.
*
*          WHEN lc_oprnum.
*            lrs_oprnum-sign   = lc_i.
*            lrs_oprnum-option = lc_eq.
*            lrs_oprnum-low    = lst_key_tab-value.
*            APPEND lrs_oprnum TO lrt_oprnum.
*            CLEAR lrs_oprnum.
*
*          WHEN lc_approverid.
*            lrs_it_approverid-sign   = lc_i.
*            lrs_it_approverid-option = lc_eq.
*            lrs_it_approverid-low    = lst_key_tab-value.
*            APPEND lrs_it_approverid TO lrt_approverid.
*            CLEAR lrs_it_approverid.
*
*          WHEN lc_notif.
*            lrs_notif-sign   = lc_i.
*            lrs_notif-option = lc_eq.
*            lrs_notif-low    = lst_key_tab-value.
*            APPEND lrs_notif TO lrt_notif.
*            CLEAR lrs_notif.
*
*          WHEN lc_notifitem.
*            lrs_notifitem-sign   = lc_i.
*            lrs_notifitem-option = lc_eq.
*            lrs_notifitem-low    = lst_key_tab-value.
*            APPEND lrs_notifitem TO lrt_notifitem.
*            CLEAR lrs_notifitem.
*
*          WHEN lc_notiftask.
*            lrs_notiftask-sign   = lc_i.
*            lrs_notiftask-option = lc_eq.
*            lrs_notiftask-low    = lst_key_tab-value.
*            APPEND lrs_notiftask TO lrt_notiftask.
*            CLEAR lrs_notiftask.
*
*          WHEN lc_equip.
*            lrs_equip-sign   = lc_i.
*            lrs_equip-option = lc_eq.
*            lrs_equip-low    = lst_key_tab-value.
*            APPEND lrs_equip TO lrt_equip.
*            CLEAR lrs_equip.
*
*          WHEN lc_funlocation.
*            lrs_funlocation-sign   = lc_i.
*            lrs_funlocation-option = lc_eq.
*            lrs_funlocation-low    = lst_key_tab-value.
*            APPEND lrs_funlocation TO lrt_funlocation.
*            CLEAR lrs_funlocation.
*        ENDCASE.
        LOOP AT im_filter_select_options INTO DATA(ls_filter_select_options).
          CASE ls_filter_select_options-name.
              WHEN lc_formid.
                 lrt_formid = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_formid WHERE low IS INITIAL.
              when lc_version.
                 lrt_version = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_version WHERE low IS INITIAL.

               when lc_workordernum.
                  lrt_workordernum = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_workordernum WHERE low IS INITIAL.

               when lc_oprnum.
                  lrt_oprnum = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_oprnum WHERE low IS INITIAL.

                when lc_approverid.
                  lrt_approverid = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_approverid WHERE low IS INITIAL.

                when lc_notif.
                  lrt_notif = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_notif WHERE low IS INITIAL.

                when lc_notifitem.
                  lrt_notifitem = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_notifitem WHERE low IS INITIAL.

                 when lc_notiftask.
                  lrt_notiftask = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_notiftask WHERE low IS INITIAL.

                  when lc_equip.
                  lrt_equip = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_equip WHERE low IS INITIAL.

                   when lc_funlocation.
                  lrt_funlocation = CORRESPONDING #(  ls_filter_select_options-range ).
                  DELETE lrt_funlocation WHERE low IS INITIAL.

              ENDCASE.


        ENDLOOP.

      "ENDLOOP.
    "ENDIF.
* Get the Approver list for the assigned work orders
* Incoming delta token
    IF lv_mobileuser IS INITIAL.
      lv_mobileuser = sy-uname.
    ENDIF.

   DATA(lv_user) = lv_mobileuser.

    DATA(lr_getworkorder) = NEW /odsmfe/cl_get_workorder_data( im_entity_name ).

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

    SORT lit_valid_wo.
    DELETE ADJACENT DUPLICATES FROM lit_valid_wo COMPARING ALL FIELDS.

    IF lit_valid_wo IS NOT INITIAL.
* Fetch the Forms associated with work orders
      SELECT * FROM /odsmfe/tb_forsp                            " FETCHING ALL THE FIELDS FROM RESPONSE CAPTURE TABLE
      FOR ALL ENTRIES IN @lit_valid_wo
      WHERE wo_num = @lit_valid_wo-aufnr
      INTO CORRESPONDING FIELDS OF TABLE @lit_entity_temp.                "#EC CI_NOFIELD
      IF sy-subrc = 0.
        SORT lit_entity_temp BY instanceid.
      ENDIF.
    ENDIF.
* Fetch the non associated forms
    SELECT * FROM /odsmfe/tb_forsp                            " FETCHING ALL THE FIELDS FROM RESPONSE CAPTURE TABLE
    WHERE nonobjtype = @abap_true                        "#EC CI_NOFIELD
      AND created_by = @lv_mobileuser
      APPENDING CORRESPONDING FIELDS OF TABLE @lit_entity_temp.
    IF sy-subrc = 0.
      SORT lit_entity_temp BY instanceid.
    ENDIF.

* Get the Approver list for the assigned work orders


* Fetching data
    IF lit_entity_temp IS NOT INITIAL.
      SELECT formid, version, workordernum, oprnum, approverid, notification, notificationitem, notificationtask, equipment,
             functionallocation, formname, formstatus, assigneddate, assignedtime, assignedby
             FROM /odsmfe/tb_fapvr
             FOR ALL ENTRIES IN @lit_entity_temp
             WHERE /odsmfe/tb_fapvr~workordernum     = @lit_entity_temp-wo_num
             AND /odsmfe/tb_fapvr~formid             = @lit_entity_temp-formid
             AND /odsmfe/tb_fapvr~version            = @lit_entity_temp-version
             AND /odsmfe/tb_fapvr~oprnum             = @lit_entity_temp-vornr
             AND /odsmfe/tb_fapvr~approverid         IN @lrt_approverid
             AND /odsmfe/tb_fapvr~notification       IN @lrt_notif
             AND /odsmfe/tb_fapvr~notificationitem   IN @lrt_notifitem
             AND /odsmfe/tb_fapvr~notificationtask   IN @lrt_notiftask
             AND /odsmfe/tb_fapvr~equipment          IN @lrt_equip
             AND /odsmfe/tb_fapvr~functionallocation IN @lrt_funlocation
             INTO CORRESPONDING FIELDS OF TABLE @lit_formid.
    ENDIF.
* Sorting & Deleting duplicates Forms
    IF sy-subrc = 0  AND lit_formid IS NOT INITIAL.
      SORT lit_formid BY formid version workordernum.
      DELETE ADJACENT DUPLICATES FROM lit_formid COMPARING ALL FIELDS.
    ENDIF.

 "TimeStamp Field for supplying Delta token
*    GET TIME STAMP FIELD lv_delta_token.
*
*" Export the delta token
*
*    ex_response_context = lv_delta_token.
*   ex_response_data[] = lit_formid[].

    MOVE-CORRESPONDING lit_formid to ex_response_data.

*    IF lit_formid IS NOT INITIAL.
** Display all data
*      LOOP AT lit_formid ASSIGNING <lfsst_form>.
*        MOVE-CORRESPONDING <lfsst_form> TO gstib_entity.
** Get Entity method is requested
*        "IF im_key_tab IS NOT INITIAL.
*        IF im_filter_select_options IS NOT INITIAL.
*           MOVE-CORRESPONDING gstib_entity to ex_response.
*
*          "GET REFERENCE OF gstib_entity INTO ex_entity.
*        ELSE.
*          APPEND gstib_entity TO gitib_entity.
*          CLEAR gstib_entity.
*        ENDIF.
*      ENDLOOP.
*
** Get EntitySet method is requested
*      "GET REFERENCE OF gitib_entity INTO ex_entityset.
*      MOVE-CORRESPONDING gitib_entity to ex_response_data.
*    ENDIF.

  ENDMETHOD.
ENDCLASS.
