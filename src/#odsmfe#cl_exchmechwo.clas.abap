class /ODSMFE/CL_EXCHMECHWO definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF GTY_ROLES,
          low  TYPE /ODSMFE/DE_MFE_LOW100,
          high TYPE /ODSMFE/DE_MFE_HIGH,
         END OF GTY_ROLES .

  methods EXCH_TABLE_UPDATE
    importing
      !IV_AUFNR type AUFNR .
  methods GET_USERROLE_TAB
    returning
      value(RS_USRROLETAB) type GTY_ROLES .
  class-methods WO_CLASSNAME_GET
    returning
      value(RV_CLASSNAME) type SEOCLSNAME .
  methods EXCH_TABLE_UPDATE_NOTIFICATION
    importing
      !IV_QMNUM type QMNUM .
  class-methods NO_CLASSNAME_GET
    returning
      value(RV_CLASSNAME) type SEOCLSNAME .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_EXCHMECHWO IMPLEMENTATION.


  METHOD exch_table_update.
***********************************************************************************
* Data Declaration
***********************************************************************************
    DATA: lv_date           TYPE datum,        "Date
          lv_time           TYPE uzeit,        "Time
          lv_sys_time_token TYPE string,
          lv_count          TYPE i,
          lv_sys_tzone      TYPE tznzonesys,   "System Time Zone
          lv_timestamp      TYPE timestamp,    "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
          lst_delta         TYPE /odsmfe/tb_wo_ex,
          lv_mobile_app     TYPE string,
          lv_exchange_table TYPE /odsmfe/de_mfe_low100.

    CONSTANTS: lc_formattach TYPE /iwbep/sbdm_node_name     VALUE 'FormAttachmentSet',
               lc_wo         TYPE /odsmfe/de_mfe_fieldname  VALUE 'WORKORDER',
               lc_exchtab    TYPE /odsmfe/de_mfe_tabname    VALUE 'EXCHANGE_TABLE'.
********************************************************************************************
* Main Code
********************************************************************************************
    CLEAR: lv_timestamp,lv_sys_tzone,
    lv_sys_time_token,lv_date,lv_time,lv_count.

*--Get Exchange table name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                      INTO lv_exchange_table
                      WHERE entitysetname = lc_formattach
                      AND tabname = lc_exchtab
                      AND field = lc_wo.
*--Get App Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                   INTO lv_mobile_app
                   WHERE entitysetname = 'MobileAppName'
                   AND field = 'MOBILE_APP_NAME'.

    IF sy-subrc EQ 0 AND lv_exchange_table IS NOT INITIAL.
      SELECT COUNT(*) FROM (lv_exchange_table) INTO lv_count
      WHERE objkey = iv_aufnr.
      IF lv_count = 0.
        lst_delta-action = 'I'.
      ELSE.
        lst_delta-action = 'U'.
      ENDIF.

      " get system time
      GET TIME STAMP FIELD lv_timestamp.
      /syclo/cl_core_bapi_tools=>get_system_time(
      IMPORTING ev_sys_tzone = lv_sys_tzone ).

      CONVERT TIME STAMP lv_timestamp TIME ZONE lv_sys_tzone
      INTO DATE lv_date TIME lv_time.
      CONCATENATE lv_date lv_time INTO lv_sys_time_token.

      " Insert data
      lst_delta-mandt      = sy-mandt.
      lst_delta-mobile_app = lv_mobile_app.
      lst_delta-objkey     = iv_aufnr.
      lst_delta-changed_ts = lv_sys_time_token.
      lst_delta-changed_by = sy-uname.
      " updating work order exchange table
      MODIFY (lv_exchange_table) FROM lst_delta.
      IF sy-subrc = 0 .
        COMMIT WORK .
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD exch_table_update_notification.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS-VSANAGALA
* Creation Date          : 24.02.2023
* Transport No.          : ES1K903619
* Program Description    : Update the Exchange Table for Notification
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                      *
* -----------------------------------------------------------------------*

    "/Tables and Structures
    DATA: lst_delta         TYPE /odsmfe/tb_no_ex.

    "/ Variables
    DATA: lv_date           TYPE datum,
          lv_time           TYPE uzeit,
          lv_sys_time_token TYPE string,
          lv_count          TYPE i,
          lv_sys_tzone      TYPE tznzonesys,
          lv_timestamp      TYPE timestamp,
          lv_mobile_app     TYPE string,
          lv_exchange_table TYPE /odsmfe/de_mfe_low100.

    "/ Constants
    CONSTANTS: lc_formattach   TYPE /iwbep/sbdm_node_name     VALUE 'FormAttachmentSet',
               lc_exchtab      TYPE /odsmfe/de_mfe_fieldname  VALUE 'EXCHANGE_TABLE',
               lc_notification TYPE /odsmfe/de_mfe_tabname    VALUE 'NOTIFICATION'.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*
* -----------------------------------------------------------------------*
*                        M A I N   S E C T I O N                         *
* -----------------------------------------------------------------------*

    CLEAR: lv_timestamp,lv_sys_tzone,
       lv_sys_time_token,lv_date,lv_time,lv_count.

    "/ Get Exchange table name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                      INTO lv_exchange_table
                      WHERE entitysetname = lc_formattach
                      AND   tabname       = lc_notification
                      AND   field         = lc_exchtab.
    "/ Get App Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                   INTO lv_mobile_app
                   WHERE entitysetname = 'MobileAppName'
                   AND   field         = 'MOBILE_APP_NAME'.

    IF sy-subrc EQ 0 AND lv_exchange_table IS NOT INITIAL.
      SELECT COUNT(*) FROM (lv_exchange_table) INTO lv_count
      WHERE objkey = iv_qmnum.
      IF lv_count = 0.
        lst_delta-action = 'I'.
      ELSE.
        lst_delta-action = 'U'.
      ENDIF.

      "/ Get system time
      GET TIME STAMP FIELD lv_timestamp.
      /syclo/cl_core_bapi_tools=>get_system_time(
      IMPORTING ev_sys_tzone = lv_sys_tzone ).

      CONVERT TIME STAMP lv_timestamp TIME ZONE lv_sys_tzone
      INTO DATE lv_date TIME lv_time.
      CONCATENATE lv_date lv_time INTO lv_sys_time_token.

      "/ Insert data
      lst_delta-mandt      = sy-mandt.
      lst_delta-mobile_app = lv_mobile_app.
      lst_delta-objkey     = iv_qmnum.
      lst_delta-changed_ts = lv_sys_time_token.
      lst_delta-changed_by = sy-uname.

      "/ updating Notification exchange table
      MODIFY (lv_exchange_table) FROM lst_delta.
      IF sy-subrc = 0 .
        COMMIT WORK .
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_userrole_tab.
***********************************************************************************
* Data Declaration
***********************************************************************************
    CONSTANTS: lc_formattach TYPE /iwbep/sbdm_node_name     VALUE 'FormAttachmentSet',
               lc_usrroles   TYPE /odsmfe/de_mfe_fieldname  VALUE 'USERROLES',
               lc_exchtab    TYPE /odsmfe/de_mfe_tabname    VALUE 'EXCHANGE_TABLE'.
***********************************************************************************
* Main Code
***********************************************************************************
*--Get userroles table
    SELECT SINGLE low, high
           FROM /odsmfe/tb_filtr
           INTO @rs_usrroletab
           WHERE entitysetname = @lc_formattach
           AND tabname = @lc_exchtab
           AND field = @lc_usrroles.
  ENDMETHOD.


  method NO_CLASSNAME_GET.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS-VSANAGALA
* Creation Date          : 25.02.2023
* Transport No.          : ES1K903619
* Program Description    : Get the Notification class name
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
***********************************************************************************
* Data Declaration
***********************************************************************************
    CONSTANTS: lc_formattach TYPE /iwbep/sbdm_node_name     VALUE 'FormAttachmentSet',
               lc_NO         TYPE /odsmfe/de_mfe_fieldname  VALUE 'NOTIF_CLASS',
               lc_exchtab    TYPE /odsmfe/de_mfe_tabname    VALUE 'ATTACHMENT'.
***********************************************************************************
* Main Code
***********************************************************************************

"/ Get Notification class
    SELECT SINGLE low
           FROM /odsmfe/tb_filtr
           INTO rv_classname
           WHERE entitysetname = lc_formattach
           AND tabname = lc_exchtab
           AND field = lc_No.
  endmethod.


  METHOD wo_classname_get.
***********************************************************************************
* Data Declaration
***********************************************************************************
    CONSTANTS: lc_formattach TYPE /iwbep/sbdm_node_name     VALUE 'FormAttachmentSet',
               lc_wo         TYPE /odsmfe/de_mfe_fieldname  VALUE 'WO_CLASS',
               lc_exchtab    TYPE /odsmfe/de_mfe_tabname    VALUE 'ATTACHMENT'.
***********************************************************************************
* Main Code
***********************************************************************************
*--Get WO class
    SELECT SINGLE low
           FROM /odsmfe/tb_filtr
           INTO rv_classname
           WHERE entitysetname = lc_formattach
           AND tabname = lc_exchtab
           AND field = lc_wo.
  ENDMETHOD.
ENDCLASS.
