CLASS /odsmfe/cl_exchmechwo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF gty_roles,
        low  TYPE /odsmfe/de_mfe_low100,
        high TYPE /odsmfe/de_mfe_high,
      END OF gty_roles .

    METHODS exch_table_update
      IMPORTING
        !iv_aufnr TYPE aufnr .
    METHODS get_userrole_tab
      RETURNING
        VALUE(rs_usrroletab) TYPE gty_roles .
    CLASS-METHODS wo_classname_get
      RETURNING
        VALUE(rv_classname) TYPE char30. "SEOCLSNAME .


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /ODSMFE/CL_EXCHMECHWO IMPLEMENTATION.


  METHOD exch_table_update.
***********************************************************************************
* Data Declaration
***********************************************************************************
    DATA: lv_date           TYPE datn,        "Date
          lv_time           TYPE timn,        "Time
          lv_sys_time_token TYPE string,
          lv_count          TYPE i,
          lv_sys_tzone      TYPE tznzone,   "System Time Zone
          lv_timestamp      TYPE timestamp,    "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
          lst_delta         TYPE /odsmfe/tb_wo_ex,
          lv_mobile_app     TYPE string,
          lv_exchange_table TYPE /odsmfe/de_mfe_low100.

    CONSTANTS: lc_formattach(128) TYPE c  VALUE 'FormAttachmentSet',
               lc_wo              TYPE /odsmfe/de_mfe_fieldname  VALUE 'WORKORDER',
               lc_exchtab         TYPE /odsmfe/de_mfe_tabname    VALUE 'EXCHANGE_TABLE'.
********************************************************************************************
* Main Code
********************************************************************************************
    CLEAR: lv_timestamp,lv_sys_tzone,
    lv_sys_time_token,lv_date,lv_time,lv_count.

*--Get Exchange table name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
*                      INTO @lv_exchange_table
                      WHERE entitysetname = @lc_formattach
                      AND tabname = @lc_exchtab
                      AND field = @lc_wo
                      INTO @lv_exchange_table.
*--Get App Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
*                   INTO @lv_mobile_app
                   WHERE entitysetname = 'MobileAppName'
                   AND field = 'MOBILE_APP_NAME'
                   INTO @lv_mobile_app.

    IF sy-subrc EQ 0 AND lv_exchange_table IS NOT INITIAL.
      SELECT COUNT(*) FROM (lv_exchange_table)
      WHERE objkey = @iv_aufnr
      INTO @lv_count.
      IF lv_count = 0.
        lst_delta-action = 'I'.
      ELSE.
        lst_delta-action = 'U'.
      ENDIF.

      " get system time
      GET TIME STAMP FIELD lv_timestamp.

*      /syclo/cl_core_bapi_tools=>get_system_time(       "commented by LMETTA"
*     IMPORTING ev_sys_tzone = lv_sys_tzone ).


*SOC BY LMETTA


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
               lt_systime(8) TYPE c,
             END OF ty_fields.


      DATA:    "lt_options        TYPE TABLE OF ty_options,
        lt_fields        TYPE TABLE OF ty_fields,
        lt_data          TYPE TABLE OF ty_data,
        lt_systime       TYPE TABLE OF ty_data,
        lv_rfc_dest_name TYPE string.

*DATA :   LT_OPTIONS TYPE TABLE OF RFC_DB_OPT,
*       LT_DATA   TYPE TABLE OF TTZCU,
*       LT_FIELDS  TYPE TABLE OF RFC_DB_FLD.

* DATA : ls_ttzcu type  LIKE  DD02L-TABNAME,
*        lt_ttzcu type table of DD02L-TABNAME .

* DATA LT_SYSTIME TYPE STANDARD TABLE OF TTZCU.

      lt_fields  = VALUE #( ( fieldname = 'TZONESYS' ) ).


      CALL FUNCTION 'RFC_READ_TABLE'
        DESTINATION lv_rfc_dest_name
        EXPORTING
          query_table = 'TTZCU'
*         rowskips    = lv_rowskip
*         rowcount    = lv_rowcount
*        IMPORTING
*         ev_sys_tzone = lv_sys_tzone
        TABLES
*         options     = lt_options
          fields      = lt_fields
          data        = lt_systime.  "query result will be stored in lt_systime"

*EOC BY LMETTA



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
      MODIFY (lv_exchange_table) FROM @lst_delta.
      IF sy-subrc = 0 .
        COMMIT WORK .
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_userrole_tab.
***********************************************************************************
* Data Declaration
***********************************************************************************
    CONSTANTS: " lc_formattach TYPE /iwbep/sbdm_node_name     VALUE 'FormAttachmentSet', 'commented by LMETTA'
      "lc_formattach(128) TYPE c VALUE 'FormAttachmentSet',
      lc_formattach(128) TYPE c VALUE '/ODSMFE/CE_FORMATTACHMENT',                      "added by LMETTA"
      lc_usrroles        TYPE /odsmfe/de_mfe_fieldname  VALUE 'USERROLES',
      lc_exchtab         TYPE /odsmfe/de_mfe_tabname    VALUE 'EXCHANGE_TABLE'.
***********************************************************************************
* Main Code
***********************************************************************************
*--Get userroles table
    SELECT SINGLE low, high
           FROM /odsmfe/tb_filtr
*           INTO @rs_usrroletab
           WHERE entitysetname = @lc_formattach
           AND tabname = @lc_exchtab
           AND field = @lc_usrroles
           INTO @rs_usrroletab.
  ENDMETHOD.


  METHOD wo_classname_get.
***********************************************************************************
* Data Declaration
***********************************************************************************
    CONSTANTS: "lc_formattach TYPE /iwbep/sbdm_node_name     VALUE 'FormAttachmentSet', 'commented by LMETTA'
      "lc_formattach(128) TYPE c VALUE 'FormAttachmentSet',
      lc_formattach(128) TYPE c VALUE '/ODSMFE/CE_FORMATTACHMENT',                     "added by LMETTA"
      lc_wo              TYPE /odsmfe/de_mfe_fieldname  VALUE 'WO_CLASS',
      lc_exchtab         TYPE /odsmfe/de_mfe_tabname    VALUE 'ATTACHMENT'.
***********************************************************************************
* Main Code
***********************************************************************************
*--Get WO class
    SELECT SINGLE low
           FROM /odsmfe/tb_filtr
*           INTO @rv_classname
           WHERE entitysetname = @lc_formattach
           AND tabname = @lc_exchtab
           AND field = @lc_wo
           INTO @rv_classname.
  ENDMETHOD.
ENDCLASS.
