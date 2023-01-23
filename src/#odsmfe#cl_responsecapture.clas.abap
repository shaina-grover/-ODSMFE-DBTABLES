class /ODSMFE/CL_RESPONSECAPTURE definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  type-pools ABAP .

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_RESPONSECAPTURE .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_RESPONSECAPTURE .
  data GVIB_USER type USNAM .
  data GSTIB_DEL_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_RESPONSECAPTURE .
  data GITIB_DEL_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_RESPONSECAPTURE .

  methods GMIB_PARSE_RESPONSEDATA
    importing
      !IM_RESPONSEDATA type XSTRING optional
      !IM_WONUM type AUFNR optional
      !IM_FORMID type /ODSMFE/DE_FORMID optional
      !IM_VERSION type /ODSMFE/DE_VERSION optional
      !IM_QMNUM type QMNUM optional
    exporting
      !EX_RETURN type BAPIRET2_T
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GMIB_GET_XML_CONTENT
    importing
      !IM_RESPONSEDATA type XSTRING
    exporting
      !EX_RETURN type BAPIRET2_T
      !EX_XML_DATA type /ODSMFE/XML_DATA
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION
      /IWBEP/CX_MGW_BUSI_EXCEPTION .
  methods GMIB_CREATE_NOTIF_ITEM_ACT
    importing
      !IM_WONUM type AUFNR optional
      !IM_FORMID type /ODSMFE/DE_FORMID optional
      !IM_VERSION type /ODSMFE/DE_VERSION optional
      !IM_XML_DATA type /ODSMFE/XML_DATA optional
      !IM_QMNUM type QMNUM optional
    exporting
      !EX_RETURN type BAPIRET2_T
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GMIB_DELTA_TABLE_UPDATE
    importing
      !IM_QMNUM type BAPI2080_NOTHDRE-NOTIF_NO optional .
  methods MODIFY_DELTA_TABLE
    importing
      !ORDER_NUMBER type AUFNR optional
      !TIME_TOKEN type STRING optional
      !NOTIFICATION type QMNUM optional .
  methods READ_DELTA_TABLE
    importing
      !DELTA_TOKEN type TIMESTAMP optional
    exporting
      !EX_AUFNR_DATA type /ODSMFE/TT_EX_AUFNR .
  methods EQUIPMENT_CHAR_UPDATE
    importing
      value(IM_WORKORDER) type AUFNR
      value(IM_CHAR) type /ODSMFE/EQ_CHAR_TT
    exporting
      !EX_RETURN type BAPIRET2 .
  methods UPDATE_CHARACTERSTICS
    importing
      value(LV_OBJECT) type BAPI1003_KEY-OBJECT
      !LV_CLASSNUM type BAPI1003_KEY-CLASSNUM
      !LV_KLART type BAPI1003_KEY-CLASSTYPE
      !IM_CHAR type /ODSMFE/EQ_CHAR_TT .

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



CLASS /ODSMFE/CL_RESPONSECAPTURE IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_create_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  ODS
* Creation Date          :  12/05/2020
* Transport No.          : ES1K901774
* Program Description    : Creates forms data in service
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : ODS
* Change Date            : 22/09/2020
* Transport No.          : ES1K902140
* Change Description     : Added logic to update work order exchange table while capturing form responses.
***********************************************************************
* Program Author (SID)   : ODS
* Change Date            : 14/12/2020
* Transport No.          : ES1K902363
* Change Description     : Added logic to check postnotification and
*                          Enabling forms for task list assignment .
***********************************************************************
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------
    TYPES: BEGIN OF ltys_responsecapture,
             instanceid   TYPE c LENGTH 50,
             formid       TYPE /odsmfe/tb_forsp-formid, "++ES1K902499
             version      TYPE c LENGTH 3,
             wo_num       TYPE c LENGTH 12,
             vornr        TYPE c LENGTH 4,
*  SOC by ODS ++ES1K902363
             plnty        TYPE plnty,
             plnnr        TYPE plnnr,
             plnal        TYPE plnal,
*            zaehl        TYPE cim_count,   "++ES1K902499
             zaehl        TYPE char8,       "++ES1K902499
* EOC by ODS ++ES1K902363
             equnr        TYPE c LENGTH 18,
             tplnr        TYPE c LENGTH 30,
             responsedata TYPE string,
             created_on   TYPE timestamp,
             created_by   TYPE c LENGTH 50,
             modified_on  TYPE timestamp,
             modified_by  TYPE c LENGTH 50,
             nonobjtype   TYPE /odsmfe/tb_forsp-nonobjtype,
             isdraft      TYPE c LENGTH 1,
             counter      TYPE /odsmfe/de_counter,
             remarks      TYPE /odsmfe/de_remark,
           END OF ltys_responsecapture.

    DATA: lst_response     TYPE /odsmfe/tb_forsp,
          lst_formresponse TYPE ltys_responsecapture,
          lit_return       TYPE STANDARD TABLE OF bapiret2,
          lit_return1      TYPE STANDARD TABLE OF bapiret2.

* Variables
    DATA: lv_message          TYPE bapi_msg,
          lv_msg              TYPE char128,
          lv_postnotification TYPE char1,
          lv_cdate            TYPE sy-datlo,
          lv_mdate            TYPE sy-datlo,
          lv_ctime            TYPE sy-timlo,
          lv_mtime            TYPE sy-timlo,
          lv_roleid           TYPE /odsmfe/de_roleid,
          lv_instanceid       TYPE char50,
          lv_postnotif        TYPE char1,        "++ODS ES1K902363
          lv_postchar         TYPE char1,        "++ODS ES1K902363
*         SOC by ODS- ES1K902140
          lv_date             TYPE datum,        "Date
          lv_time             TYPE uzeit,        "Time
          lv_sys_time_token   TYPE string,
          lv_count            TYPE i,
          lv_sys_tzone        TYPE tznzonesys,   "System Time Zone
          lv_timestamp        TYPE timestamp.    "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
*         EOC by ODS- ES1K902140
    DATA: lv_aufnr TYPE aufnr,
          lv_qmnum TYPE qmnum.
* Class
    DATA: lo_error           TYPE REF TO /iwbep/if_message_container.

* Constants
    CONSTANTS: lc_i    TYPE char1 VALUE 'I',
               lc_wi   TYPE char2 VALUE 'WI',
               lc_aewi TYPE char4 VALUE 'AEWI',
               lc_w    TYPE char1 VALUE 'W',
               lc_e    TYPE char1 VALUE 'E',
               lc_a    TYPE char1 VALUE 'A',
               lc_low1 TYPE char1 VALUE '1',
               lc_low2 TYPE char1 VALUE '2',
               lc_low3 TYPE char1 VALUE '3',
               lc_pfcg_role TYPE string VALUE 'PFCG_ROLE',
               lc_x   TYPE char1 VALUE 'X',
               lc_true TYPE string VALUE 'TRUE'.

    DATA : lv_date1           TYPE datum,        "Date
           lv_time1           TYPE uzeit,        "Time
           lv_sys_time_token1 TYPE string,
           lv_mobileuser      TYPE string,
           lv_userrole        TYPE /odsmfe/de_roleid,
           lv_sys_tzone1      TYPE tznzonesys,   "System Time Zone
           lv_timestamp1      TYPE timestamp,    "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
           lv_wo              TYPE aufnr,
           lv_form_date       TYPE datum,
           lv_form_time       TYPE uzeit,
           lt_return_email    TYPE TABLE OF bapiret2,
           lv_pfcg_role       TYPE    /ods/value.

     DATA: lo_auth TYPE REF TO /ods/cl_auth_utility."Reference to class for calling role assinment method


*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    im_data_provider->read_entry_data( IMPORTING es_data = lst_formresponse ).

    IF lst_formresponse IS NOT INITIAL.
      lst_response-instanceid = lst_formresponse-instanceid.
** BOC C0035157 ES1K902883 07/03/2022
** when the instance ID is not coming from FE( i.e. through mobile and Web dekstop) then only
** update the instance ID as SAP WO Date time.
      IF lst_response-instanceid IS INITIAL.
        CLEAR : lv_timestamp1,lv_sys_tzone1,lv_date1,lv_time1,lv_sys_time_token1,lv_wo.
        GET TIME STAMP FIELD lv_timestamp1.
        /syclo/cl_core_bapi_tools=>get_system_time(
        IMPORTING ev_sys_tzone = lv_sys_tzone1 ).
        CONVERT TIME STAMP lv_timestamp1 TIME ZONE lv_sys_tzone1
        INTO DATE lv_date1 TIME lv_time1.
        CONCATENATE lv_date1 lv_time1 INTO lv_sys_time_token1.

        lv_wo = lst_formresponse-wo_num.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = lv_wo
          IMPORTING
            output = lv_wo.

        CONCATENATE 'SAP' lv_wo lv_sys_time_token1 INTO lst_formresponse-instanceid SEPARATED BY '_'.
        CONCATENATE 'SAP' lv_wo lv_sys_time_token1 INTO lst_response-instanceid SEPARATED BY '_'.
      ENDIF.
** EOC C0035157 ES1K902883 07/03/2022
      lst_response-formid     = lst_formresponse-formid.
      lst_response-version    = lst_formresponse-version.
      TRANSLATE lst_formresponse-responsedata TO UPPER CASE.
      lst_response-responsedata = lst_formresponse-responsedata.
      lst_response-wo_num       = lst_formresponse-wo_num.
      lst_response-vornr        = lst_formresponse-vornr.
* SOC by ODS ++ES1K902363
      lst_response-plnty = lst_formresponse-plnty.
      lst_response-plnnr = lst_formresponse-plnnr.
      lst_response-plnal = lst_formresponse-plnal.
      lst_response-zaehl = lst_formresponse-zaehl.
* EOC by ODS ++ES1K902363
      lst_response-equnr        = lst_formresponse-equnr.
      lst_response-tplnr = lst_formresponse-tplnr.
      lst_response-created_on  = lst_formresponse-created_on.
      TRANSLATE lst_formresponse-created_by TO UPPER CASE.
      lst_response-created_by  = sy-uname."++ES1K902140
      lst_response-modified_on = lst_formresponse-modified_on.
      TRANSLATE lst_formresponse-modified_by TO UPPER CASE.
      lst_response-modified_by = lst_formresponse-modified_by.
      lst_response-nonobjtype  = lst_formresponse-nonobjtype.
      lst_response-counter     = lst_formresponse-counter.
      lst_response-isdraft     = lst_formresponse-isdraft.      "for saving form in Draft
      lst_response-remarks     = lst_formresponse-remarks.
* Get PFCG and ODS Role ID
*--Start of changes - ES1K902967
      SELECT SINGLE param_value
          FROM /ods/app_config
          INTO lv_pfcg_role
          WHERE param_name = lc_pfcg_role
          AND activeflag = lc_x.

    IF sy-uname IS NOT INITIAL.
      IF sy-uname EQ 'PPRIYANKA' AND lv_pfcg_role EQ lc_true.

        DATA(lv_user) = sy-uname .
      CREATE OBJECT lo_auth.
     TRY.
    CALL METHOD lo_auth->role_assignment    "Get PFCG Role ID
                EXPORTING
                       iv_uname = lv_user
                 IMPORTING
                         ev_field = DATA(lv_role)
                        .
        IF lv_role IS NOT INITIAL.
          lst_response-roleid = lv_role.
        ENDIF.
      CATCH /iwbep/cx_mgw_busi_exception.
      ENDTRY.
      ELSE.
*--Get reference for fetching value of user role table
      DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ).
      DATA(lv_usrroletab) = lr_exchtab->get_userrole_tab( ).
      IF lr_exchtab IS BOUND.
*--Get Role ID
        SELECT SINGLE roleid FROM (lv_usrroletab-low) INTO lv_roleid
            WHERE userid = sy-uname
            AND startdate LE sy-datum
            AND enddate GE sy-datum.
        IF sy-subrc = 0.
          lst_response-roleid = lv_roleid.
        ENDIF.
      ENDIF.
     ENDIF.
     ENDIF.
*--End of changes - ES1K902967

      IF lst_formresponse-created_on IS NOT INITIAL.
* Converting timestamp into date and time
        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
          EXPORTING
            iv_timestamp     = lst_formresponse-created_on
          IMPORTING
            o_date           = lv_cdate
            o_time           = lv_ctime
          EXCEPTIONS
            conversion_error = 1
            OTHERS           = 2.
        IF sy-subrc = 0.
          lst_response-created_date = lv_cdate.
          lst_response-created_time = lv_ctime.
          lv_form_date  = lv_cdate.
          lv_form_time = lv_ctime.
        ENDIF.

      ENDIF.

      IF lst_formresponse-modified_on IS NOT INITIAL.
* Converting timestamp into date and time
        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
          EXPORTING
            iv_timestamp     = lst_formresponse-modified_on
          IMPORTING
            o_date           = lv_mdate
            o_time           = lv_mtime
          EXCEPTIONS
            conversion_error = 1
            OTHERS           = 2.
        IF sy-subrc = 0.
          lst_response-modified_date = lv_mdate.
          lst_response-modified_time = lv_mtime.
          lv_form_date  = lv_mdate.
          lv_form_time = lv_mtime.
        ENDIF.

      ENDIF.

      IF lst_formresponse-instanceid IS NOT INITIAL.
*  Insert / update logic for Isdraft functionality
        SELECT SINGLE instanceid
               FROM /odsmfe/tb_forsp
               INTO lv_instanceid
               WHERE instanceid = lst_formresponse-instanceid.

        IF sy-subrc NE 0 AND lv_instanceid IS INITIAL.
* Insert data to Table /odsmfe/tb_forsp
          INSERT /odsmfe/tb_forsp FROM lst_response.
          IF sy-subrc <> 0.
            CLEAR lst_response.
          ENDIF.
        ELSE.
* Update data to Table /odsmfe/tb_forsp
          UPDATE /odsmfe/tb_forsp FROM lst_response.
          IF sy-subrc <> 0.
            CLEAR lst_response.
          ENDIF.
        ENDIF.
        " updating work order exchange table
        IF lst_response-wo_num IS NOT INITIAL.
*--Start of changes - ES1K902967
          IF lr_exchtab IS BOUND.
            lr_exchtab->exch_table_update( lst_response-wo_num ).
          ENDIF.
*--End of changes - ES1K902967
        ENDIF.

      ENDIF.

      IF lst_formresponse-nonobjtype IS INITIAL.

        SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_postnotification
             WHERE entitysetname = 'ResponseCaptureSet' AND field = 'POSTNOTIFICATION'.

* Wo Number
        lv_aufnr = lst_response-wo_num.

* Get the Notification number from work order number
        SELECT SINGLE qmnum INTO lv_qmnum FROM afih
        WHERE aufnr = lv_aufnr.

* Update Work Order Notification
        IF lv_postnotification = abap_true AND lv_qmnum IS NOT INITIAL.

**          SOC  ODS ++ES1K902363
          SELECT SINGLE postnotification FROM /odsmfe/tb_foass INTO lv_postnotif
                 WHERE formid EQ lst_formresponse-formid
                   AND version EQ lst_formresponse-version.

          IF lv_postnotif IS INITIAL.
            CLEAR:lv_postnotif.
          ENDIF."++ ODS  ES1K902363
*         EOC ODS ++ ES1K902363

          IF lst_response-isdraft IS INITIAL OR lv_postnotif IS NOT INITIAL.                        "Do not update notification activity if flag is set as 'X'

            CALL METHOD me->gmib_parse_responsedata
              EXPORTING
                im_responsedata = lst_response-responsedata
                im_wonum        = lst_response-wo_num
                im_qmnum        = lv_qmnum
                im_formid       = lst_response-formid
                im_version      = lst_response-version
              IMPORTING
                ex_return       = lit_return.
* Error Handling
            IF lit_return IS NOT INITIAL.
              lit_return1 = lit_return.
              DELETE lit_return1 WHERE type NE lc_e OR type = lc_a.
              IF lit_return1 IS NOT INITIAL.
                lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

                lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return1
                  iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
                  iv_add_to_response_header = abap_true ).

                RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                  EXPORTING
                    message_container = lo_error.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

* Send the instance ID to Front end application
      gstib_entity-instanceid = lst_response-instanceid.
      GET REFERENCE OF gstib_entity INTO ex_entity.
*--Update Equipment Characterstics
*--Update the characterstics of Equipment based on the table content
*--KMADHURI
      SELECT SINGLE postcharacteristics FROM /odsmfe/tb_foass INTO lv_postchar
       WHERE formid EQ lst_formresponse-formid
         AND version EQ lst_formresponse-version.
      IF sy-subrc = 0 AND lv_postchar IS NOT INITIAL.
        DATA: lt_return   TYPE bapiret2,
              lt_return1  TYPE bapiret2_t,
              lt_xml_data TYPE TABLE OF smum_xmltb,
              lt_char     TYPE /odsmfe/eq_char_tt,
              ls_char     TYPE /odsmfe/eq_char,
              lo_msg1     TYPE REF TO cx_root,
              lo_msg      TYPE REF TO cx_root,
              lv_msg1     TYPE string,
              lt_return2  TYPE STANDARD TABLE OF bapiret2.                    "Return Parameter
*
        SELECT *
          FROM /odsmfe/tb_focha INTO TABLE @DATA(lt_char1)
                            WHERE formid EQ @lst_formresponse-formid
                            AND version EQ @lst_formresponse-version.
        IF sy-subrc = 0.
          IF NOT lt_char1 IS INITIAL.
*--Convert the repsonse data to internal table data
            CLEAR: lt_return1.
            TRY.
                CALL METHOD me->gmib_get_xml_content
                  EXPORTING
                    im_responsedata = lst_response-responsedata
                  IMPORTING
                    ex_xml_data     = lt_xml_data
                    ex_return       = lt_return1.
                IF lt_return1 IS NOT INITIAL.
*--Catch exceptions
                  DELETE lt_return1 WHERE type NE lc_e OR type = lc_a.
                  IF lt_return1 IS NOT INITIAL.
                    lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

                    lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lt_return2
                      iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
                      iv_add_to_response_header = abap_true ).

                    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                      EXPORTING
                        message_container = lo_error.
                  ENDIF.
                ENDIF.


              CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
                CLEAR lv_msg.
                lo_msg->get_text( RECEIVING result = lv_msg ).
                lo_msg->get_longtext( RECEIVING result = lv_msg ).
                MESSAGE lv_msg TYPE 'I'.

              CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
                CLEAR lv_msg1.
                lo_msg1->get_text( RECEIVING result = lv_msg1 ).
                lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
                MESSAGE lv_msg1 TYPE 'I'.
            ENDTRY.
*--Prepare internal table to send to new class for equipment char update
            LOOP AT lt_xml_data INTO DATA(ls_xml).
              MOVE ls_xml-cname TO ls_char-name.
              MOVE ls_xml-cvalue TO ls_char-value.
              TRANSLATE ls_xml-cname TO UPPER CASE.
              LOOP AT lt_char1 INTO DATA(ls_char1) WHERE field = ls_xml-cname.
                MOVE ls_xml-cvalue  TO ls_char-value.
                MOVE-CORRESPONDING ls_char1 TO ls_char.
                APPEND ls_char TO lt_char.
                CLEAR : ls_char1.
              ENDLOOP.

            ENDLOOP.
*--Method to update characterstics
            CALL METHOD me->equipment_char_update
              EXPORTING
                im_workorder = lst_response-wo_num
                im_char      = lt_char
              IMPORTING
                ex_return    = lt_return.
            CLEAR: lt_return, lt_char.
*--Update Equipment
          ENDIF.
        ENDIF.
*--end of changes
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_delete_entityset.

    DATA: lit_key_tab TYPE /iwbep/t_mgw_name_value_pair,
          lst_key_tab TYPE /iwbep/s_mgw_name_value_pair.

    DATA: lv_instanceid TYPE /odsmfe/de_instanceid,
          lv_aufnr      TYPE aufnr,
          lv_deleted    TYPE char1.

    lit_key_tab = im_key_tab.
    SORT lit_key_tab BY name.

    "/Work Order Num
    READ TABLE lit_key_tab INTO lst_key_tab WITH KEY name = 'InstanceID' BINARY SEARCH.
    IF sy-subrc = 0 AND lst_key_tab-value IS NOT INITIAL.
      lv_instanceid = lst_key_tab-value.
    ENDIF.

    "/Update entry from Response capture as deleted
    lv_deleted = abap_true.
    UPDATE /odsmfe/tb_forsp SET deleted = lv_deleted
      WHERE instanceid =  lv_instanceid.

* Update Exchange Table
    SELECT SINGLE wo_num FROM /odsmfe/tb_forsp INTO lv_aufnr
      WHERE instanceid = lv_instanceid.

    IF lv_aufnr IS NOT INITIAL.
      DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ).
      IF lr_exchtab IS BOUND.
        lr_exchtab->exch_table_update( lv_aufnr ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_modify_entityset.

***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   :  ODS
* Creation Date          :  12/05/2020
* Transport No.          : ES1K901774
* Program Description    : updates forms data in service
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   : ODS
* Change Date            : 29/09/2020
* Transport No.          : ES1K902140
* Change Description     : Added logic to update work order exchange table
***********************************************************************
* Program Author (SID)   : ODS
* Change Date            : 14/12/2020
* Transport No.          : ES1K902363
* Change Description     : Enabling forms for task list assignment
***********************************************************************
*----------------------------------------------------------------------
*  Data declaration
*----------------------------------------------------------------------
    TYPES:
      BEGIN OF ltys_responsecapture,
        instanceid      TYPE c LENGTH 50,
        formid          TYPE /odsmfe/tb_forsp-formid, "++ES1K902499
        version         TYPE c LENGTH 3,
        wo_num          TYPE c LENGTH 12,
        vornr           TYPE c LENGTH 4,
*  SOC by ODS ++ES1K902363
        tasklisttype    TYPE  plnty,
        group           TYPE plnnr,
        groupcounter    TYPE  plnal,
*        internalcounter TYPE cim_count, "++ES1K902499
        internalcounter TYPE char8,      "++ES1K902499
* EOC by ODS ++ES1K902363
        equnr           TYPE c LENGTH 18,
        tplnr           TYPE c LENGTH 30, "ODS- ES1K902140
        responsedata    TYPE string,
        created_on      TYPE timestamp,
        created_by      TYPE c LENGTH 50,
        modified_on     TYPE timestamp,
        modified_by     TYPE c LENGTH 50,
        nonobjtype      TYPE /odsmfe/tb_forsp-nonobjtype,
        isdraft         TYPE c LENGTH 1,
        counter         TYPE /odsmfe/de_counter,
        remarks         TYPE /odsmfe/de_remark,
      END OF ltys_responsecapture.

    DATA: lst_response     TYPE /odsmfe/tb_forsp,
          lst_formresponse TYPE ltys_responsecapture,
          lit_return       TYPE STANDARD TABLE OF bapiret2,
          lit_return1      TYPE STANDARD TABLE OF bapiret2,
          lt_return_email  TYPE STANDARD TABLE OF bapiret2.


* Class
    DATA: lo_error           TYPE REF TO /iwbep/if_message_container.
* Variables
    DATA: lv_message          TYPE bapi_msg,
          lv_msg              TYPE char128,
          lv_postnotification TYPE char1,
          lv_cdate            TYPE sy-datlo,
          lv_mdate            TYPE sy-datlo,
          lv_ctime            TYPE sy-timlo,
          lv_mtime            TYPE sy-timlo,
          lv_roleid           TYPE /odsmfe/de_roleid,
          lv_instanceid       TYPE char50,
          " soc "++ES1K902140
          lv_date             TYPE datum,        "Date
          lv_time             TYPE uzeit,        "Time
          lv_sys_time_token   TYPE string,
          lv_count            TYPE i,
          lv_sys_tzone        TYPE tznzonesys,   "System Time Zone
          lv_postchar         TYPE char1,
          lv_timestamp        TYPE timestamp.    "UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
    " eoc "++ES1K902140
    DATA: lv_aufnr TYPE aufnr,
          lv_qmnum TYPE qmnum.

    DATA: lv_form_date TYPE datum,
          lv_form_time TYPE uzeit.

    DATA: lv_pfcg_role TYPE    /ods/value,
          lo_auth TYPE REF TO /ods/cl_auth_utility.
* Constants
    CONSTANTS: lc_i                  TYPE char1 VALUE 'I',
               lc_wi                 TYPE char2 VALUE 'WI',
               lc_aewi               TYPE char4 VALUE 'AEWI',
               lc_w                  TYPE char1 VALUE 'W',
               lc_e                  TYPE char1 VALUE 'E',
               lc_a                  TYPE char1 VALUE 'A',
               lc_low1               TYPE char1 VALUE '1',
               lc_low2               TYPE char1 VALUE '2',
               lc_low3               TYPE char1 VALUE '3',
               lc_responsecaptureset TYPE /iwbep/sbdm_node_name VALUE 'ResponseCaptureSet',
               lc_postnotification   TYPE /odsmfe/de_mfe_fieldname VALUE 'POSTNOTIFICATION',
               lc_pfcg_role          TYPE string VALUE 'PFCG_ROLE',
               lc_x                  TYPE char1 VALUE 'X',
               lc_true               TYPE string VALUE 'TRUE'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
    im_data_provider->read_entry_data( IMPORTING es_data = lst_formresponse ).
    IF lst_formresponse IS NOT INITIAL.
      lst_response-instanceid = lst_formresponse-instanceid.
      lst_response-formid     = lst_formresponse-formid.
      lst_response-version    = lst_formresponse-version.
      TRANSLATE lst_formresponse-responsedata TO UPPER CASE.
      lst_response-responsedata = lst_formresponse-responsedata.
      lst_response-wo_num       = lst_formresponse-wo_num.
      lst_response-vornr        = lst_formresponse-vornr.
* SOC by ODS ++ES1K902363
      lst_response-plnty = lst_formresponse-tasklisttype.
      lst_response-plnnr = lst_formresponse-group.
      lst_response-plnal = lst_formresponse-groupcounter.
      lst_response-zaehl = lst_formresponse-internalcounter.
* EOC by ODS ++ES1K902363
      lst_response-equnr        = lst_formresponse-equnr.
      lst_response-tplnr = lst_formresponse-tplnr."++ES1K902140
      lst_response-created_on   = lst_formresponse-created_on.
      TRANSLATE lst_formresponse-created_by TO UPPER CASE.
      lst_response-created_by  = sy-uname."++ES1K902140
      lst_response-modified_on = lst_formresponse-modified_on.
      TRANSLATE lst_formresponse-modified_by TO UPPER CASE.
      lst_response-modified_by = sy-uname.
      lst_response-nonobjtype  = lst_formresponse-nonobjtype.
      lst_response-counter     = lst_formresponse-counter.
      lst_response-isdraft     = lst_formresponse-isdraft.
      lst_response-remarks     = lst_formresponse-remarks.
* PFCG and ODS Role ID
*--Start of changes - ES1K902967
       SELECT SINGLE param_value
          FROM /ods/app_config
          INTO lv_pfcg_role
          WHERE param_name = lc_pfcg_role
          AND activeflag = lc_x.

    IF sy-uname IS NOT INITIAL.
      IF sy-uname EQ 'PPRIYANKA' AND lv_pfcg_role EQ lc_true.

        DATA(lv_user) = sy-uname .
      CREATE OBJECT lo_auth.
     TRY.
    CALL METHOD lo_auth->role_assignment    "Get PFCG Role ID
                EXPORTING
                       iv_uname = lv_user
                 IMPORTING
                         ev_field = DATA(lv_role)
                        .
        IF lv_role IS NOT INITIAL.
          lst_response-roleid = lv_role.
        ENDIF.
      CATCH /iwbep/cx_mgw_busi_exception.
      ENDTRY.
      ELSE.
*--Get reference for fetching value of user role table
      DATA(lr_exchtab) = NEW /odsmfe/cl_exchmechwo( ).
      IF lr_exchtab IS BOUND.
        DATA(lv_usrroletab) = lr_exchtab->get_userrole_tab( ).
*--End of changes - ES1K902967
*--Get Role ID
        SELECT SINGLE roleid FROM (lv_usrroletab-low) INTO lv_roleid " ES1K902967
        WHERE userid = sy-uname
        AND startdate LE sy-datum
        AND enddate GE sy-datum.
        IF sy-subrc = 0.
          lst_response-roleid = lv_roleid.
        ENDIF.
      ENDIF.
      ENDIF.
      ENDIF.

      IF lst_formresponse-created_on IS NOT INITIAL.
* Converting timestamp into date and time
        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
          EXPORTING
            iv_timestamp     = lst_formresponse-created_on
          IMPORTING
            o_date           = lv_cdate
            o_time           = lv_ctime
          EXCEPTIONS
            conversion_error = 1
            OTHERS           = 2.
        IF sy-subrc = 0.
          lst_response-created_date = lv_cdate.
          lst_response-created_time = lv_ctime.
          lv_form_date  = lv_cdate.
          lv_form_time = lv_ctime.
        ENDIF.
      ENDIF.

      IF lst_formresponse-modified_on IS NOT INITIAL.
* Converting timestamp into date and time
        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
          EXPORTING
            iv_timestamp     = lst_formresponse-modified_on
          IMPORTING
            o_date           = lv_mdate
            o_time           = lv_mtime
          EXCEPTIONS
            conversion_error = 1
            OTHERS           = 2.
        IF sy-subrc = 0.
          lst_response-modified_date = lv_mdate.
          lst_response-modified_time = lv_mtime.
          lv_form_date  = lv_mdate.
          lv_form_time = lv_mtime.
        ENDIF.

      ENDIF.

*  Insert / update logic for Isdraft functionality
      SELECT SINGLE instanceid FROM /odsmfe/tb_forsp INTO lv_instanceid
      WHERE instanceid = lst_formresponse-instanceid.
* Update Table /odsmfe/tb_forsp
      IF sy-subrc = 0 AND lv_instanceid IS NOT INITIAL.
        MODIFY /odsmfe/tb_forsp FROM lst_response.       "#EC CI_SUBRC.
        IF sy-subrc <> 0.
* Implement suitable error handling here
          CHECK sy-subrc NE 0.
        ENDIF.
        " updating work order exchange table
        IF lst_response-wo_num IS NOT INITIAL.
*--Start of changes - ES1K902967
          IF lr_exchtab IS BOUND.
            lr_exchtab->exch_table_update( lst_response-wo_num ).
          ENDIF.
*--End of changes - ES1K902967
        ENDIF.
      ENDIF.
    ENDIF.

    IF lst_formresponse-nonobjtype IS INITIAL.

* wo number
      lv_aufnr = lst_response-wo_num.

* Get the Notification number from work order number
      SELECT SINGLE qmnum INTO lv_qmnum FROM afih
      WHERE aufnr = lv_aufnr.

      SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_postnotification
             WHERE entitysetname = lc_responsecaptureset AND field = lc_postnotification.
* Update Work Order Notification
      IF lv_postnotification = abap_true AND lv_qmnum IS NOT INITIAL.
        IF lst_response-isdraft IS INITIAL.                        "Do not update notification activity if flag is set as 'X'
          CALL METHOD me->gmib_parse_responsedata
            EXPORTING
              im_responsedata = lst_response-responsedata
              im_wonum        = lst_response-wo_num
              im_qmnum        = lv_qmnum
              im_formid       = lst_response-formid
              im_version      = lst_response-version
            IMPORTING
              ex_return       = lit_return.
* Error Handling
          IF lit_return IS NOT INITIAL.
            lit_return1 = lit_return.
            DELETE lit_return1 WHERE type NE lc_e OR type = lc_a.
            IF lit_return1 IS NOT INITIAL.
              lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

              lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return1
                iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
                iv_add_to_response_header = abap_true ).

              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                EXPORTING
                  message_container = lo_error.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*--update equipment characterstics
*--Update the characterstics of Equipment based on the table content
*--KMADHURI
    SELECT SINGLE postcharacteristics FROM /odsmfe/tb_foass INTO lv_postchar
           WHERE formid EQ lst_formresponse-formid
             AND version EQ lst_formresponse-version.
    IF sy-subrc = 0 AND lv_postchar IS NOT INITIAL.
      DATA: lt_return   TYPE bapiret2,
            lt_return1  TYPE bapiret2_t,
            lt_xml_data TYPE TABLE OF smum_xmltb,
            lt_char     TYPE /odsmfe/eq_char_tt,
            ls_char     TYPE /odsmfe/eq_char,
            lo_msg1     TYPE REF TO cx_root,
            lo_msg      TYPE REF TO cx_root,
            lv_msg1     TYPE string,
            lt_return2  TYPE STANDARD TABLE OF bapiret2.                    "Return Parameter
*
      SELECT *
        FROM /odsmfe/tb_focha INTO TABLE @DATA(lt_char1)
                          WHERE formid EQ @lst_formresponse-formid
                          AND version EQ @lst_formresponse-version.
      IF sy-subrc = 0.
        IF NOT lt_char1 IS INITIAL.
*--Convert the repsonse data to internal table data
          CLEAR: lt_return1.
          TRY.
              CALL METHOD me->gmib_get_xml_content
                EXPORTING
                  im_responsedata = lst_response-responsedata
                IMPORTING
                  ex_xml_data     = lt_xml_data
                  ex_return       = lt_return1.
              IF lt_return1 IS NOT INITIAL.
*--Catch exceptions
                DELETE lt_return1 WHERE type NE lc_e OR type = lc_a.
                IF lt_return1 IS NOT INITIAL.
                  lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

                  lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lt_return2
                    iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
                    iv_add_to_response_header = abap_true ).

                  RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                    EXPORTING
                      message_container = lo_error.
                ENDIF.
              ENDIF.


            CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
              CLEAR lv_msg.
              lo_msg->get_text( RECEIVING result = lv_msg ).
              lo_msg->get_longtext( RECEIVING result = lv_msg ).
              MESSAGE lv_msg TYPE 'I'.

            CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
              CLEAR lv_msg1.
              lo_msg1->get_text( RECEIVING result = lv_msg1 ).
              lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
              MESSAGE lv_msg1 TYPE 'I'.
          ENDTRY.
*--Prepare internal table to send to new class for equipment char update
          LOOP AT lt_xml_data INTO DATA(ls_xml).
            MOVE ls_xml-cname TO ls_char-name.
            MOVE ls_xml-cvalue TO ls_char-value.
            TRANSLATE ls_xml-cname TO UPPER CASE.
            LOOP AT lt_char1 INTO DATA(ls_char1) WHERE field = ls_xml-cname.
              MOVE ls_xml-cvalue  TO ls_char-value.
              MOVE-CORRESPONDING ls_char1 TO ls_char.
              APPEND ls_char TO lt_char.
              CLEAR : ls_char1.
            ENDLOOP.

          ENDLOOP.
*--Method to update characterstics
          CALL METHOD me->equipment_char_update
            EXPORTING
              im_workorder = lst_response-wo_num
              im_char      = lt_char
            IMPORTING
              ex_return    = lt_return.
          CLEAR: lt_return, lt_char.
*--Update Equipment
        ENDIF.
      ENDIF.
*--end of changes
    ENDIF.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  ODS
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Displays forms data in service based on work order and type of WorkOrder
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : ODS
* Change Date            : 05-11-2020
* Transport No.          : ES1K902363
* Change Description     : Display forms data for all technicinas for supervisor view
***********************************************************************
* Program Author (SID)   : ODS
* Change Date            : 07-01-2021
* Transport No.          : ES1K902470
* Change Description     : Changed logic to display supervisor forms alogn with technitian forms.
***********************************************************************
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------
* object reference
    DATA: lo_filter TYPE REF TO /iwbep/if_mgw_req_filter,
          lrt_form  TYPE REF TO /odsmfe/st_core_range_str.

    DATA:lrs_form                       TYPE /odsmfe/st_core_range_str,
         lrs_filter_values              TYPE /odsmfe/st_form_fil_vals,
         lrs_wonum                      TYPE /odsmfe/st_core_range_str,
         lst_workorderset_get_entityset TYPE LINE OF /odsmfe/cl_pr_formui_mpc=>tt_workorder,
         lst_key_tab                    TYPE /iwbep/s_mgw_name_value_pair,
         lst_filter_range               TYPE /iwbep/s_cod_select_option,
         lst_filter                     TYPE /iwbep/s_mgw_select_option,
         lv_source_entity_set_name      TYPE string,
         lv_aufnr                       TYPE aufnr,
         lv_instid                      TYPE /odsmfe/de_instanceid,
         lv_mobileuser                  TYPE string,
         lv_pernr                       TYPE persno,
         lv_parva                       TYPE xuvalue,
         lv_delta_token                 TYPE timestamp,
         lv_formid                      TYPE /odsmfe/de_formid,
         lv_version                     TYPE /odsmfe/de_version.


* SOC by ODS - ES1K902363

    TYPES : BEGIN OF ltys_usrid ,
              pernr TYPE pernr_d,
              usrid TYPE /odsmfe/de_createdby,
            END OF ltys_usrid.
*
    TYPES : BEGIN OF ltys_tech ,
              pernr TYPE pernr_d,
              usrid TYPE sysid,
            END OF ltys_tech.
    DATA : lit_tech TYPE TABLE OF ltys_tech.
    DATA : lit_usrid TYPE TABLE OF ltys_usrid.

* EOC by ODS - ES1K902363

    DATA: lit_entity_temp TYPE TABLE OF /odsmfe/tb_forsp,
          lst_entity_temp TYPE /odsmfe/tb_forsp,
          lit_valid_wo    TYPE STANDARD TABLE OF /odsmfe/pm_valid_aufnr_str.

    DATA: lo_delta_context TYPE REF TO /iwbep/if_mgw_req_entityset,
          lo_ref_exch_data TYPE REF TO data.

    "/Constants
    CONSTANTS:
      lc_agr          TYPE memoryid VALUE 'AGR',
      lc_workorderset TYPE string   VALUE 'WorkOrderSet',
      lc_i            TYPE char1    VALUE 'I',
      lc_eq           TYPE char2    VALUE 'EQ',
      lc_instid       TYPE string   VALUE 'InstanceID',
      lc_createdby    TYPE string   VALUE 'CREATEDBY',
      lc_instanceid   TYPE string   VALUE 'INSTANCEID',
      lc_formid       TYPE string   VALUE 'FORMID',
      lc_version      TYPE string   VALUE 'VERSION'.

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    IF im_tech_request_context IS SUPPLIED.
      lo_filter = im_tech_request_context->get_filter( ).
    ENDIF.

    IF im_key_tab IS NOT INITIAL AND im_tech_request_context IS SUPPLIED.

      lv_source_entity_set_name     = im_tech_request_context->get_source_entity_set_name( ).

      IF  lv_source_entity_set_name = lc_workorderset.
*   Convert keys to appropriate entity set structure
        im_tech_request_context->get_converted_source_keys(
        IMPORTING
          es_key_values  = lst_workorderset_get_entityset ).
        lv_aufnr      = lst_workorderset_get_entityset-aufnr.
        IF lv_aufnr IS NOT INITIAL.
          lrs_wonum-sign = lc_i.
          lrs_wonum-option = lc_eq.
          lrs_wonum-low = lv_aufnr.
          APPEND lrs_wonum TO lrs_filter_values-wo_num[].
        ENDIF.
      ENDIF.
    ENDIF.
    READ TABLE im_key_tab INTO lst_key_tab WITH KEY name = lc_instid.
    IF sy-subrc = 0 AND lst_key_tab-value IS NOT INITIAL.
      lv_instid = lst_key_tab-value.
      lrs_form-sign = lc_i.
      lrs_form-option = lc_eq.
      lrs_form-low = lv_instid.
      APPEND lrs_form TO lrs_filter_values-forminstanceid[].
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
* SOC skammari 08-06-2020
          WHEN lc_createdby.
            READ TABLE lst_filter-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL .
              lv_mobileuser = lst_filter_range-low.
              TRANSLATE lv_mobileuser TO UPPER CASE.
            ENDIF.
* EOC skammari 08-06-2020
          WHEN lc_instanceid.
            READ TABLE lst_filter-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL .
              lv_instid = lst_filter_range-low.
              lrs_form-sign = lc_i.
              lrs_form-option = lc_eq.
              lrs_form-low = lv_instid.
              APPEND lrs_form TO lrs_filter_values-forminstanceid[].
            ENDIF.

          WHEN lc_formid.
            READ TABLE lst_filter-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL .
              lv_formid = lst_filter_range-low.
              lrs_form-sign = lc_i.
              lrs_form-option = lc_eq.
              lrs_form-low = lv_formid.
              APPEND lrs_form TO lrs_filter_values-formid[].
            ENDIF.

          WHEN lc_version.
            READ TABLE lst_filter-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL .
              lv_version = lst_filter_range-low.
              lrs_form-sign = lc_i.
              lrs_form-option = lc_eq.
              lrs_form-low = lv_version.
              APPEND lrs_form TO lrs_filter_values-version[].
            ENDIF.
        ENDCASE.
        CLEAR : lrs_form , lst_filter_range.
      ENDLOOP.
    ENDIF.
* SOC by ODS - ES1K902363
    IF lv_mobileuser IS INITIAL.
      lv_mobileuser = sy-uname.
    ENDIF.

    SELECT SINGLE parva FROM usr05 INTO lv_parva
                  WHERE bname = lv_mobileuser
                   AND  parid = 'RFPNR'.

    DATA(lr_getworkorder) = NEW /odsmfe/cl_get_workorder_data( ).

    IF sy-subrc IS INITIAL AND lv_parva EQ 'X'.

      IF lr_getworkorder IS BOUND.
        lr_getworkorder->gmib_get_technician_data(
        EXPORTING
          im_mobileuser = lv_mobileuser
         IMPORTING
          ex_tech_data = lit_tech ).
      ENDIF.

      IF lit_tech IS NOT INITIAL.
* Fetch forms for all the technicians
        lit_usrid = lit_tech.

        SELECT * FROM /odsmfe/tb_forsp " FETCHING ALL THE FIELDS FROM RESPONSE CAPTURE TABLE
                  INTO CORRESPONDING FIELDS OF TABLE lit_entity_temp
                  FOR ALL ENTRIES IN lit_usrid
                  WHERE instanceid NE space "++ES1K902363
                  AND created_by = lit_usrid-usrid.

        IF sy-subrc IS INITIAL.
          SORT lit_entity_temp BY instanceid.
        ENDIF.
      ENDIF.

      IF lit_entity_temp IS NOT INITIAL .
        LOOP AT lit_entity_temp INTO lst_entity_temp.
          MOVE-CORRESPONDING lst_entity_temp TO gstib_entity.
          APPEND gstib_entity TO gitib_entity.
          CLEAR: lst_entity_temp,gstib_entity.
        ENDLOOP.
      ENDIF.

    ELSE.
* EOC by ODS - ES1K902363
* SOC  fetching response based on Loggedin User.  SKAMMARI ++ ES1K901991
      IF im_key_tab IS INITIAL.
* If EnteredyBy is empty
        IF lv_mobileuser IS INITIAL.
          lv_mobileuser = sy-uname.
        ENDIF.

*SOC by ODS-VSANAGALA - ES1K903413
        "/ Checking whether Work center is assigned to the user or not.
        CLEAR: lv_parva.
        SELECT SINGLE parva FROM usr05 INTO lv_parva
                     WHERE bname = lv_mobileuser
                       AND parid = lc_agr.

        IF sy-subrc = 0 AND lv_parva IS NOT INITIAL.
*EOC by ODS-VSANAGALA - ES1K903413

*--Start of changes - ES1K902967
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
* Fetch Response Capture from /odsmfe/tb_forsp for entered user
          IF lit_valid_wo IS NOT INITIAL.
* Fetch the Forms associated with work orders
            SELECT * FROM /odsmfe/tb_forsp                            " FETCHING ALL THE FIELDS FROM RESPONSE CAPTURE TABLE
            INTO CORRESPONDING FIELDS OF TABLE lit_entity_temp
            FOR ALL ENTRIES IN lit_valid_wo
            WHERE wo_num = lit_valid_wo-aufnr.          "#EC CI_NOFIELD

            IF sy-subrc = 0.
              SORT lit_entity_temp BY instanceid.
            ENDIF.
          ENDIF.
*SOC by ODS-VSANAGALA - ES1K903413
        ENDIF. "IF sy-subrc = 0 AND lv_parva IS NOT INITIAL.
*EOC by ODS-VSANAGALA - ES1K903413

* Fetch the non associated forms
        SELECT * FROM /odsmfe/tb_forsp                            " FETCHING ALL THE FIELDS FROM RESPONSE CAPTURE TABLE
        APPENDING CORRESPONDING FIELDS OF TABLE lit_entity_temp
        WHERE nonobjtype = abap_true                    "#EC CI_NOFIELD
          AND created_by = lv_mobileuser.
        IF sy-subrc = 0.
          SORT lit_entity_temp BY instanceid.
        ENDIF.

      ENDIF.

      IF lit_entity_temp IS INITIAL AND im_key_tab IS NOT INITIAL OR lrs_filter_values IS NOT INITIAL .
* EOC  SKAMMARI ++ ES1K901991
        SELECT * FROM /odsmfe/tb_forsp
        INTO TABLE lit_entity_temp
        WHERE instanceid IN lrs_filter_values-forminstanceid[]
        AND wo_num IN lrs_filter_values-wo_num[]
          AND formid IN lrs_filter_values-formid[]
          AND version IN lrs_filter_values-version[].

        IF sy-subrc = 0.
          SORT lit_entity_temp BY formid version."++ES1K902140
        ENDIF.
      ENDIF.

      IF lit_entity_temp IS NOT INITIAL .
        LOOP AT lit_entity_temp INTO lst_entity_temp.
          MOVE-CORRESPONDING lst_entity_temp TO gstib_entity.
          APPEND gstib_entity TO gitib_entity.
          CLEAR: lst_entity_temp,gstib_entity.
        ENDLOOP.
        LOOP AT lit_entity_temp INTO lst_entity_temp WHERE deleted IS NOT INITIAL.
          MOVE-CORRESPONDING lst_entity_temp TO gstib_del_entity.
          APPEND gstib_del_entity TO gitib_del_entity.
          CLEAR: lst_entity_temp,gstib_del_entity.
        ENDLOOP.
      ENDIF.
    ENDIF." ODS - ES1K902363
* Delete deleted entry from inital entity
    DELETE gitib_entity WHERE deleted IS NOT INITIAL.

* TimeStamp Field for supplying Delta token
    GET TIME STAMP FIELD lv_delta_token.

* Export the delta token
    ex_response_context-deltatoken = lv_delta_token.
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
  ENDMETHOD.


  METHOD equipment_char_update.

*--kmadhuri
    CONSTANTS : lc_equi TYPE bapi1003_key-objecttable VALUE 'EQUI',
                lc_e    TYPE char1 VALUE 'E',
                lc_a    TYPE char1 VALUE 'A',
                lc_x    TYPE char1 VALUE 'X'.

    DATA : ls_char TYPE /odsmfe/eq_char.


    DATA: lv_equnr    TYPE equnr,
          lv_tplnr    TYPE tplnr,
          lv_object   TYPE bapi1003_key-object,
          lit_return  TYPE TABLE OF bapiret2,
          lv_classnum TYPE bapi1003_key-classnum,
          lv_klart    TYPE bapi1003_key-classtype,
          lv_message  TYPE bapi_msg,
          lst_return  TYPE bapiret2,
          lit_char    TYPE TABLE OF bapi1003_alloc_values_char.
    "Classification BAPI - Values of Type CHAR, BOOL

    IF NOT im_workorder IS INITIAL.

      READ TABLE im_char ASSIGNING FIELD-SYMBOL(<lfs_char>) INDEX 1.
      IF sy-subrc = 0.
        MOVE <lfs_char>-class TO lv_classnum.
      ENDIF.
*      IF <lfs_char>-woheadereq = lc_x OR
*      <lfs_char>-wooperationeq = lc_x
*     OR <lfs_char>-notficationeq = lc_x.
*        lv_klart    = '002'."for equipment
**                ELSEIF <lfs_char>-wo_object = lc_x.
**                  lv_klart = '016'."Object link
*      ELSE.
*        lv_klart = '003'."Function location
*      ENDIF.

      SELECT SINGLE aufnr ,equnr,iloan, qmnum
        FROM afih INTO @DATA(ls_equnr)
        WHERE aufnr = @im_workorder.
      IF sy-subrc = 0.
        IF NOT ls_equnr-equnr IS INITIAL.
          MOVE ls_equnr-equnr TO lv_equnr.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_equnr
            IMPORTING
              output = lv_equnr.

          CLEAR: lv_object.

          MOVE lv_equnr TO lv_object.
          IF <lfs_char>-woheadereq = lc_x.
            lv_klart    = '002'.
*--Update charactaerstics of the equipment , functional location
            CALL METHOD me->update_characterstics
              EXPORTING
                lv_object   = lv_object
                lv_classnum = lv_classnum
                lv_klart    = lv_klart
                im_char     = im_char.
            CLEAR: lv_equnr.
          ENDIF.

          IF <lfs_char>-woheaderfl = lc_x.
            lv_klart = '003'."Function location
            SELECT SINGLE iloan, tplnr FROM iloa
              INTO @DATA(ls_iloa)
              WHERE iloan = @ls_equnr-iloan.
            IF sy-subrc = 0.
              MOVE ls_iloa-tplnr TO lv_tplnr.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = lv_tplnr
                IMPORTING
                  output = lv_tplnr.
              MOVE lv_tplnr TO lv_object.
*--update charactaerstics of the equipment , functional location
              CALL METHOD me->update_characterstics
                EXPORTING
                  lv_object   = lv_object
                  lv_classnum = lv_classnum
                  lv_klart    = lv_klart
                  im_char     = im_char.
              CLEAR: lv_tplnr.
            ENDIF.
          ENDIF.
          IF <lfs_char>-notficationeq = lc_x.
            lv_klart    = '002'.
            SELECT SINGLE qmnum,iloan,equnr
              FROM qmih INTO @DATA(ls_qmequi)
              WHERE qmnum = @ls_equnr-qmnum.
            IF sy-subrc = 0.
              IF NOT ls_qmequi IS INITIAL.
                MOVE ls_qmequi-equnr TO lv_equnr.
*                IF NOT ls_equnr IS INITIAL.
*                  MOVE ls_equnr-equnr TO lv_equnr.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = lv_equnr
                    IMPORTING
                      output = lv_equnr.

                  CLEAR:  lv_object.
                  MOVE lv_equnr TO lv_object.
*--Update charactaerstics of the equipment , functional location
                  CALL METHOD me->update_characterstics
                    EXPORTING
                      lv_object   = lv_object
                      lv_classnum = lv_classnum
                      lv_klart    = lv_klart
                      im_char     = im_char.
                  CLEAR: lv_equnr,lv_tplnr.
                  IF <lfs_char>-notficationfl = lc_x.
                    lv_klart = '003'."Function location
                    SELECT SINGLE iloan, tplnr FROM iloa
                    INTO @DATA(ls_iloa1)
                          WHERE iloan = @ls_equnr-iloan.
                    IF sy-subrc = 0.
                      MOVE ls_iloa1-tplnr TO lv_tplnr.
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = lv_tplnr
                        IMPORTING
                          output = lv_tplnr.
                      MOVE lv_tplnr TO lv_object.
*--update charactaerstics of the equipment , functional location
                      CALL METHOD me->update_characterstics
                        EXPORTING
                          lv_object   = lv_object
                          lv_classnum = lv_classnum
                          lv_klart    = lv_klart
                          im_char     = im_char.
                      CLEAR: lv_tplnr.
                    ENDIF.
                  ENDIF.
*                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD gmib_create_notif_item_act.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  12/05/2020
* Transport No.          : ES1K901774
* Program Description    : Method creates Notification Item activity
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
***********************************************************************
* Data Declaration
***********************************************************************
    TYPES:  BEGIN OF ltys_notificationitem,
              qmnum TYPE qmfe-qmnum,
              fenum TYPE qmfe-fenum,
            END OF ltys_notificationitem.
    TYPES : BEGIN OF ltys_notificationact1,
              qmnum TYPE qmma-qmnum,
              manum TYPE qmma-manum,
            END OF ltys_notificationact1.

    TYPES : BEGIN OF ltys_data,
              qmnum TYPE qmnum,                                         "Notification No
              manum TYPE manum,                                         "Sequential Task Number
              name  TYPE char255,                                       "Char255
              value TYPE char255,                                       "Char255
              longt TYPE string,
            END OF ltys_data.
* Variables
    DATA: lv_aufnr         TYPE aufnr,                                         "Order Number
          lv_qmnum         TYPE qmnum,                                         "Notification No
          lv_count         TYPE num4,                                          "Four-digit number
          lv_act_key       TYPE manum,                                         "Sequential Task Number
          lv_item_key      TYPE fenum,                                         "Notification Print Item Number
          lv_formid        TYPE string,                                        "30 Characters
          lv_ver           TYPE string,                                        "30 Characters
          lv_codegruppe    TYPE qcodegrp,                                      "Code Group
          lv_code          TYPE qcode,                                         "Code
          lv_version       TYPE /odsmfe/de_version,                            "ODS Version
          lv_key           TYPE manum,                                         "Sequential Task Number
          lv_actcodegruppe TYPE qcodegrp,                                      "Code Group
          lv_actcode       TYPE qcode,                                         "Code
          lv_longt         TYPE string,
          lv_qmart         TYPE qmart,                                         "Notification Type
          lv_number        TYPE bapi2080_nothdre-notif_no,                     "Notification No
          lv_message       TYPE bapi_msg,                                      "Message Text
          lv_msg           TYPE string,
          lv_msg1          TYPE string.

* Table & Structure
    DATA: lit_notificationitem   TYPE TABLE OF  ltys_notificationitem,
          lit_notitem            TYPE STANDARD TABLE OF bapi2080_notitemi,  "Notification item for creation
          lit_notificationact    TYPE STANDARD TABLE OF qmma,           "Quality notification - activities
          lit_notificationact1   TYPE TABLE OF ltys_notificationact1,
*      lst_notification1      TYPE qmma,                             " added by shyamala
          lst_notification1      TYPE  ltys_notificationact1,
          lst_notifitem          TYPE bapi2080_notitemi,                "Notification item for creation
          lit_notifactv          TYPE STANDARD TABLE OF bapi2080_notactvi,  "Notification activity for creation
          lst_notifact           TYPE bapi2080_notactvi,                "Notification activity for creation
          lst_xml_data           TYPE smum_xmltb,                       "XML Table structure used for retreive and output XML doc
          lit_tq80               TYPE STANDARD TABLE OF tq80,           "Notification Types
          lst_tq80               TYPE tq80,                             "Notification Types
          lit_notifheader_export TYPE bapi2080_nothdre,     "#EC NEEDED
          lit_notifhdtext        TYPE bapi2080_nothdtxte,   "#EC NEEDED
          lit_notifheader        TYPE bapi2080_nothdre,     "#EC NEEDED
          lst_return             TYPE bapiret2.                         "Return Parameter

    FIELD-SYMBOLS : <lfsst_return> TYPE bapiret2.

    DATA :lit_txt TYPE TABLE OF ltys_data,
          lst_txt TYPE ltys_data.
    DATA :lst_lines TYPE tline,                                  "SAPscript: Text Lines
          lit_lines TYPE TABLE OF tline.                         "SAPscript: Text Lines
    DATA :lit_longtext    TYPE TABLE OF swastrtab,               "WF: Display string in 255-character lines
          lst_longtext    TYPE swastrtab,
          lit_return1     TYPE STANDARD TABLE OF bapiret2,
          lo_error        TYPE REF TO /iwbep/if_message_container,
          lv_manum        TYPE aknum,                                  "Consecutive Number of Activity
          lv_actlongtext  TYPE char50,                                 "Comment
          lv_actshorttext TYPE char50,
          lv_name1        TYPE  char255.
    DATA:  lo_msg  TYPE REF TO cx_root,
           lo_msg1 TYPE REF TO cx_root.
**********************************************************
* Main Section
**********************************************************
    TRY.
* Conversion Exit
        lv_aufnr = im_wonum.
        IF lv_aufnr IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_aufnr
            IMPORTING
              output = lv_aufnr.
* Get the Notification number from work order number
          SELECT SINGLE qmnum INTO lv_qmnum FROM afih
          WHERE aufnr = lv_aufnr.
          IF sy-subrc = 0.
            lv_number = lv_qmnum.
          ENDIF.
        ENDIF.

        IF lv_number IS NOT INITIAL.
* Look for Notificaion Items
          SELECT qmnum fenum INTO CORRESPONDING FIELDS OF TABLE lit_notificationitem FROM qmfe
          WHERE qmnum = lv_qmnum.

          IF sy-subrc = 0.
* get the count of notification items
            lv_count = lines( lit_notificationitem ).
          ENDIF.
* increment the count by 1
          lv_item_key = lv_count + 1.

*          CLEAR lst_xml_data.
*          READ TABLE im_xml_data INTO lst_xml_data WITH KEY cname = 'id'.
*          IF sy-subrc = 0.
*            lv_formid = lst_xml_data-cvalue.
*          ENDIF.
*          lv_version = im_version.

          CLEAR lst_xml_data.
          READ TABLE im_xml_data INTO lst_xml_data WITH KEY cname = 'id'.
          IF sy-subrc <> 0.
            READ TABLE im_xml_data INTO lst_xml_data WITH KEY cname = 'name'.
          ENDIF.

          IF lst_xml_data-cvalue IS NOT INITIAL.
            SPLIT lst_xml_data-cvalue AT ' [' INTO lv_formid lv_ver.
            CLEAR lv_ver.
          ENDIF.

          lv_version = im_version.

          CLEAR: lv_codegruppe, lv_code.
* Fetch code Group based on type = 'B'
          SELECT SINGLE qpct~codegruppe qpct~code INTO (lv_codegruppe,lv_code)
          FROM qpct INNER JOIN /odsmfe/tb_fomst ON /odsmfe/tb_fomst~codegruppe = qpct~codegruppe
          WHERE /odsmfe/tb_fomst~formid =  lv_formid
          AND   /odsmfe/tb_fomst~version = lv_version
          AND   qpct~katalogart = 'B'
          AND   qpct~sprache = sy-langu
          AND   qpct~inaktiv NE 'X'.                   "#EC CI_BUFFJOIN

          IF lv_codegruppe IS INITIAL OR lv_code IS INITIAL.
            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                textid  = /iwbep/cx_mgw_busi_exception=>business_error
                message = text-t02.
          ENDIF.

          IF sy-subrc = 0.
            lst_notifitem-dl_codegrp   = lv_codegruppe.
            lst_notifitem-dl_code      = lv_code.
            lst_notifitem-item_key     = lv_item_key.
            lst_notifitem-item_sort_no = lv_item_key.
            APPEND lst_notifitem TO lit_notitem.
            CLEAR: lst_notifitem.
          ENDIF.

          CLEAR: lv_count, lv_act_key.
* Look for Notification Activities
          SELECT qmnum manum INTO CORRESPONDING FIELDS OF TABLE lit_notificationact FROM qmma
          WHERE qmnum = lv_qmnum
          AND   fenum = lv_item_key
          AND mngrp = lv_codegruppe.
          IF sy-subrc = 0.
* get the count of notification activities
            lv_count = lines( lit_notificationact ).
          ENDIF.
* Incremement the count by 1
          lv_act_key = lv_count + 1.
          lv_manum = lv_item_key - 1.

          SELECT qmnum manum INTO CORRESPONDING FIELDS OF TABLE lit_notificationact1 FROM qmma
          WHERE qmnum = lv_qmnum
          AND   fenum = lv_manum.

          IF sy-subrc = 0.
            SORT lit_notificationact1 DESCENDING BY manum.
          ENDIF.
          READ TABLE lit_notificationact1 INTO lst_notification1 INDEX 1.
* Get the original activity key
          lv_key = lst_notification1-manum.

          SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_actlongtext
          WHERE entitysetname = 'WoHeaderSet'
          AND field =    'ACTIVITYLONGTEXTLENGHT'.
          SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_actshorttext
          WHERE entitysetname = 'WoHeaderSet'
          AND field =    'UPDATEACTIVITYSHORTTEXT'.

          CLEAR lst_xml_data.
          LOOP AT im_xml_data INTO lst_xml_data WHERE type = 'V' OR type = '+'.

            IF lst_xml_data-type = 'V'.
              lv_key = lv_key + 1.
            ENDIF.
            lst_notifact-act_key      = lv_act_key.
            lst_notifact-act_sort_no  = lv_act_key.
            lst_notifact-item_sort_no = lv_item_key.
            CLEAR : lv_actcodegruppe , lv_actcode.

* Fetch code Group based on type = 'A'
            SELECT SINGLE codegruppe code FROM qpct INTO (lv_actcodegruppe,lv_actcode)
            WHERE kurztext =  lst_xml_data-cname
            AND codegruppe = lv_codegruppe
            AND katalogart = 'A'
            AND inaktiv NE 'X'
            AND sprache = sy-langu.

            IF sy-subrc = 0.
              IF lv_actcodegruppe IS INITIAL OR lv_actcode IS INITIAL.
                RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                  EXPORTING
                    textid  = /iwbep/cx_mgw_busi_exception=>business_error
                    message = text-t03.
              ENDIF.
              lst_notifact-act_codegrp = lv_actcodegruppe.
              lst_notifact-act_code    = lv_actcode.
              IF lst_xml_data-type = '+'.
                lv_name1 = lst_xml_data-cname.
              ENDIF.
              IF lv_name1 = lst_xml_data-cname.
                CONCATENATE lv_longt lst_xml_data-cvalue INTO lv_longt.
              ELSE.
                CLEAR lv_longt.
              ENDIF.
            ENDIF.
* Fill the activity texts into a table which are more than 40 characters.
            IF lst_xml_data-type = 'V' .

              IF strlen( lst_xml_data-cvalue ) GT lv_actlongtext OR lv_longt IS NOT INITIAL.
                lst_txt-qmnum = lv_qmnum.
                lst_txt-manum = lv_key.
                lst_txt-name  = lst_xml_data-cname.
                lst_txt-value = lst_xml_data-cvalue.
                lst_txt-longt = lv_longt.
                APPEND lst_txt TO lit_txt.
                CLEAR lst_txt.
              ENDIF.

              IF lv_actshorttext = 'X' AND ( strlen( lst_xml_data-cvalue )
              GT lv_actlongtext OR lv_longt IS NOT INITIAL ).
                lst_notifact-acttext = text-t04.
              ELSE.
                lst_notifact-acttext = lst_xml_data-cvalue.
              ENDIF.
              APPEND lst_notifact TO lit_notifactv.
              lv_act_key = lv_act_key + 1.
            ENDIF.
            CLEAR: lst_notifact , lst_xml_data.
          ENDLOOP.

* Look for Notification Type
          SELECT SINGLE qmart FROM qmel INTO lv_qmart WHERE qmnum = lv_number.
          IF sy-subrc = 0.
* look for Notification type and Category
            SELECT qmart qmtyp FROM tq80 INTO CORRESPONDING FIELDS OF TABLE lit_tq80
            WHERE qmart = lv_qmart.
            IF sy-subrc = 0.
              SORT lit_tq80 BY  qmart.
            ENDIF.
            READ TABLE lit_tq80 INTO lst_tq80 WITH KEY qmart = lv_qmart   BINARY SEARCH.
          ENDIF.
          IF sy-subrc = 0 AND lst_tq80-qmtyp = '03'.
            CLEAR ex_return.
* Update Service Notification
            IF lit_notitem IS NOT INITIAL AND lit_notifactv IS NOT INITIAL.
              CALL FUNCTION 'BAPI_SERVNOT_ADD_DATA'
                EXPORTING
                  number      = lv_number
                IMPORTING
                  notifheader = lit_notifheader_export
                TABLES
                  notitem     = lit_notitem
                  notifactv   = lit_notifactv
                  return      = ex_return.
            ENDIF.

            IF ex_return IS NOT INITIAL.
              lit_return1 = ex_return.
              DELETE lit_return1 WHERE type NE 'E' OR type = 'A'.
              IF lit_return1 IS NOT INITIAL.
                lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

                lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return1
                  iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
                  iv_add_to_response_header = abap_true ).

                RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                  EXPORTING
                    message_container = lo_error.
              ENDIF.
            ENDIF.
* Error handling
            IF <lfsst_return> IS ASSIGNED.
              UNASSIGN <lfsst_return>.
            ENDIF.
            LOOP AT ex_return ASSIGNING <lfsst_return>.
              IF <lfsst_return>-type = 'E' OR <lfsst_return>-type = 'A'.

                lv_message = <lfsst_return>-message.
                RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                  EXPORTING
                    textid  = /iwbep/cx_mgw_busi_exception=>business_error
                    message = lv_message.
              ENDIF.
            ENDLOOP.
          ELSE.
* Update Notification
            IF lit_notitem IS NOT INITIAL AND lit_notifactv IS NOT INITIAL.
              REFRESH ex_return[].
              CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_ADD'
                EXPORTING
                  number             = lv_number
                IMPORTING
                  notifhdtext        = lit_notifhdtext
                  notifheader_export = lit_notifheader_export
                TABLES
                  notitem            = lit_notitem
                  notifactv          = lit_notifactv
                  return             = ex_return.
            ENDIF.
* Error handling
            IF ex_return IS NOT INITIAL.
              REFRESH lit_return1[].
              lit_return1 = ex_return.
              DELETE lit_return1 WHERE type NE 'E' OR type = 'A'.
              IF lit_return1 IS NOT INITIAL.
                lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

                lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return1
                  iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
                  iv_add_to_response_header = abap_true ).

                RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                  EXPORTING
                    message_container = lo_error.
              ENDIF.
            ENDIF.
            IF <lfsst_return> IS ASSIGNED.
              UNASSIGN <lfsst_return>.
            ENDIF.

            LOOP AT ex_return ASSIGNING <lfsst_return>.
              IF <lfsst_return>-type = 'E' OR <lfsst_return>-type = 'A'.

                lv_message = <lfsst_return>-message.
                RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                  EXPORTING
                    textid  = /iwbep/cx_mgw_busi_exception=>business_error
                    message = lv_message.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF lv_number IS NOT INITIAL.
            REFRESH ex_return[].
* Save Notification
            CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
              EXPORTING
                number      = lv_number
              IMPORTING
                notifheader = lit_notifheader
              TABLES
                return      = ex_return.

            IF ex_return IS NOT INITIAL.
              REFRESH lit_return1[].
              lit_return1 = ex_return.
              DELETE lit_return1 WHERE type NE 'E' OR type = 'A'.
              IF lit_return1 IS NOT INITIAL.
                lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

                lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return1
                  iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
                  iv_add_to_response_header = abap_true ).

                RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                  EXPORTING
                    message_container = lo_error.
              ENDIF.
            ENDIF.
* Error handling
            IF <lfsst_return> IS ASSIGNED.
              UNASSIGN <lfsst_return>.
            ENDIF.

            LOOP AT ex_return ASSIGNING <lfsst_return>.
              IF <lfsst_return>-type = 'E' OR <lfsst_return>-type = 'A'.
                lv_message = <lfsst_return>-message.
                RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                  EXPORTING
                    textid  = /iwbep/cx_mgw_busi_exception=>business_error
                    message = lv_message.
              ENDIF.
            ENDLOOP.

            CLEAR lst_return.
* Database Commit
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait   = 'X'
              IMPORTING
                return = lst_return.
* Error handling
            IF lst_return-type EQ 'E' OR lst_return-type EQ 'A'.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              lv_message = lst_return-message.
              RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
                EXPORTING
                  textid  = /iwbep/cx_mgw_busi_exception=>business_error
                  message = lv_message.
            ENDIF.

* Update the notification activity longtext
            IF lit_txt IS NOT INITIAL.

              LOOP AT lit_txt INTO lst_txt.

                IF lst_txt-longt IS NOT INITIAL.
                  CALL FUNCTION 'SWA_STRING_SPLIT'
                    EXPORTING
                      input_string                 = lst_txt-longt
                      max_component_length         = '132'
                    TABLES
                      string_components            = lit_longtext
                    EXCEPTIONS
                      max_component_length_invalid = 1
                      OTHERS                       = 2.

                  IF sy-subrc = 0.
                    LOOP AT lit_longtext INTO lst_longtext. "#EC CI_NESTED.
                      lst_lines-tdformat = '*'.
                      lst_lines-tdline   = lst_longtext-str.
                      APPEND lst_lines TO lit_lines.
                      CLEAR lst_lines.
                    ENDLOOP.
                  ENDIF.
                ELSE.

                  lst_lines-tdformat = '*'.
                  lst_lines-tdline   = lst_txt-value+0(132).
                  APPEND lst_lines TO lit_lines.
                  CLEAR lst_lines.

                  lst_lines-tdformat = '*'.
                  lst_lines-tdline   = lst_txt-value+132(123).
                  APPEND lst_lines TO lit_lines.
                  CLEAR lst_lines.
                ENDIF.

                CALL FUNCTION 'IQS0_ADD_ACTIVITY_LONGTEXT'
                  EXPORTING
                    i_qmnum       = lst_txt-qmnum
                    i_manum       = lst_txt-manum
                    i_post        = 'X'
                  TABLES
                    t_inlines     = lit_lines
                  EXCEPTIONS
                    show_messages = 1
                    OTHERS        = 2.

                IF sy-subrc = 0.
                  CALL FUNCTION 'IQS1_REFRESH_ALL'. " To Reset the notification buffer
                ENDIF.
              ENDLOOP.
            ENDIF.
* Update Exchange Table entry
            TRY.
                CALL METHOD me->gmib_delta_table_update
                  EXPORTING
                    im_qmnum = lv_number.

              CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
                CLEAR lv_msg.
                lo_msg->get_text( RECEIVING result = lv_msg ).
                lo_msg->get_longtext( RECEIVING result = lv_msg ).
                MESSAGE lv_msg TYPE 'I'.

              CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
                CLEAR lv_msg1.
                lo_msg1->get_text( RECEIVING result = lv_msg1 ).
                lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
                MESSAGE lv_msg1 TYPE 'I'.
            ENDTRY.
          ENDIF.
        ENDIF.
      CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
        CLEAR lv_msg.
        lo_msg->get_text( RECEIVING result = lv_msg ).
        lo_msg->get_longtext( RECEIVING result = lv_msg ).
        MESSAGE lv_msg TYPE 'I'.
      CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
        CLEAR lv_msg1.
        lo_msg1->get_text( RECEIVING result = lv_msg1 ).
        lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
        MESSAGE lv_msg1 TYPE 'I'.
    ENDTRY.

  ENDMETHOD.


  METHOD gmib_delta_table_update.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  12/05/2020
* Transport No.          : ES1K901774
* Program Description    : Method to update Delta table
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
***********************************************************************
* Data Declaration
***********************************************************************
    DATA: lv_aufnr          TYPE aufnr,
          lv_mobile_app     TYPE string,
          lv_timestamp      TYPE timestamp,
          lv_sys_tzone      TYPE tznzonesys,
          lv_date_1         TYPE datum,
          lv_time_1         TYPE uzeit,
          lv_sys_time_token TYPE string.
    DATA: lit_delta TYPE STANDARD TABLE OF /odsmfe/tb_wo_ex,
          lst_delta TYPE /odsmfe/tb_wo_ex.

******************************************************************
* Main Section
******************************************************************
*--Get App Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                   INTO lv_mobile_app
                   WHERE entitysetname = 'MobileAppName'
                   AND field = 'MOBILE_APP_NAME'.
* Get System time
    GET TIME STAMP FIELD lv_timestamp.
    /syclo/cl_core_bapi_tools=>get_system_time(
    IMPORTING ev_sys_tzone = lv_sys_tzone ).
    CONVERT TIME STAMP lv_timestamp TIME ZONE lv_sys_tzone
    INTO DATE lv_date_1
    TIME lv_time_1.
    CONCATENATE lv_date_1 lv_time_1 INTO lv_sys_time_token.
    REFRESH: lit_delta.
    CLEAR: lst_delta.

    CALL METHOD me->modify_delta_table
      EXPORTING
        notification = im_qmnum
        time_token   = lv_sys_time_token.

* Clear final internal table
    REFRESH: lit_delta.
    lst_delta-mandt = sy-mandt.
    lst_delta-mobile_app = lv_mobile_app.
    lst_delta-changed_ts = lv_sys_time_token.
    lst_delta-changed_by = sy-uname.
* Get WO Num
    SELECT SINGLE aufnr FROM qmel INTO lv_aufnr WHERE qmnum = im_qmnum.
    IF sy-subrc = 0.
      lst_delta-objkey  = lv_aufnr.
* Update Exchange Table

      CALL METHOD me->modify_delta_table
        EXPORTING
          order_number = lv_aufnr
          time_token   = lv_sys_time_token.

    ENDIF.

  ENDMETHOD.


  METHOD GMIB_GET_XML_CONTENT.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 12/05/2020
* Transport No.          : ES1K901774
* Program Description    :
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
************************************************************************
* Data Declaration
************************************************************************
* Class
    DATA: lo_xml TYPE REF TO cl_xml_document,
          lo_error TYPE REF TO /iwbep/if_message_container.

* Tables & Structures
    DATA: lit_data    TYPE swxmlcont,
          lit_retcode TYPE sysubrc ,
          lit_return1 TYPE STANDARD TABLE OF bapiret2.

* Field symbols
    FIELD-SYMBOLS : <lfsst_return> TYPE bapiret2.

* Variables
    DATA: lv_xml_string TYPE xstring,
          lv_message    TYPE bapi_msg,
          lv_subrc      TYPE sy-subrc,                      "#EC NEEDED
          lv_size       TYPE sytabix.                       "#EC NEEDED
* Constants
   CONSTANTS: lc_a TYPE string VALUE 'A',
              lc_e TYPE string VALUE 'E'.
************************************************************************
* Main Section
************************************************************************
    CREATE OBJECT lo_xml.
* Check Response data
    IF im_responsedata IS NOT INITIAL.
      lv_xml_string = im_responsedata.
    ELSE.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid  = /iwbep/cx_mgw_busi_exception=>business_error
          message = text-024.
    ENDIF.

* Convert XString to Binary
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer     = lv_xml_string
      TABLES
        binary_tab = lit_data.
    IF lit_data IS INITIAL.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid  = /iwbep/cx_mgw_busi_exception=>business_error
          message = text-025.
    ENDIF.

* Parse data
    CALL METHOD lo_xml->create_with_table
      EXPORTING
        table   = lit_data
*       SIZE    = 0
      RECEIVING
        retcode = lit_retcode.
* render_2_xstring
    CALL METHOD lo_xml->render_2_xstring
      IMPORTING
        retcode = lv_subrc
        stream  = lv_xml_string
        size    = lv_size.
    IF lv_xml_string IS INITIAL.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid  = /iwbep/cx_mgw_busi_exception=>business_error
          message = text-026.
    ENDIF.

* Convert XML to internal table
    CALL FUNCTION 'SMUM_XML_PARSE'
      EXPORTING
        xml_input = lv_xml_string
      TABLES
        xml_table = ex_xml_data
        return    = ex_return.

    IF ex_return IS NOT INITIAL.
      lit_return1 = ex_return.
      DELETE lit_return1 WHERE type NE lc_e OR type = lc_a.
      IF lit_return1 IS NOT INITIAL.
        lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

        lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return1
          iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
          iv_add_to_response_header = abap_true ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_error.
      ENDIF.
    ENDIF.
* Error handling
    LOOP AT ex_return ASSIGNING <lfsst_return>.
      IF <lfsst_return>-type = lc_e OR <lfsst_return>-type = lc_a.
        lv_message = <lfsst_return>-message.
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = lv_message.
      ENDIF.
    ENDLOOP.
    "Delete the ignorables
    DELETE ex_xml_data WHERE cname+0(7) EQ '_IGNORE'.    "#EC CI_STDSEQ
* Delete Data from XML based on the below conditions
    DELETE ex_xml_data WHERE cname = text-028."instanceID      "#EC CI_STDSEQ
    DELETE ex_xml_data WHERE cname = text-029."start        "#EC CI_STDSEQ
    DELETE ex_xml_data WHERE cname = text-030."end             "#EC CI_STDSEQ
  ENDMETHOD.


  METHOD gmib_parse_responsedata.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 12/05/2020
* Transport No.          : ES1K901774
* Program Description    : This Method is used to pares data and updates the Notification Items
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
************************************************************************
************************************************************************
* Data Declaration
************************************************************************
* Tables & Structures
    DATA :lit_xml_data TYPE TABLE OF smum_xmltb,                            "XML Table structure used for retreive and output XML doc
          lit_return1  TYPE STANDARD TABLE OF bapiret2,                     "Return Parameter
          lo_error     TYPE REF TO /iwbep/if_message_container.
    DATA: lo_msg  TYPE REF TO cx_root,
          lo_msg1 TYPE REF TO cx_root,
          lv_msg  TYPE string,
          lv_msg1 TYPE string.

    CONSTANTS: lc_e TYPE char1 VALUE 'E',
               lc_a TYPE char2 VALUE 'A'.
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
* Convert XML Response into Internal Table
    TRY.
        CALL METHOD me->gmib_get_xml_content
          EXPORTING
            im_responsedata = im_responsedata
          IMPORTING
            ex_xml_data     = lit_xml_data
            ex_return       = ex_return.

        IF ex_return IS NOT INITIAL.
          lit_return1 = ex_return.
          DELETE lit_return1 WHERE type NE lc_e OR type = lc_a.
          IF lit_return1 IS NOT INITIAL.
            lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

            lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return1
              iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
              iv_add_to_response_header = abap_true ).

            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                message_container = lo_error.
          ENDIF.
        ENDIF.

      CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
        CLEAR lv_msg.
        lo_msg->get_text( RECEIVING result = lv_msg ).
        lo_msg->get_longtext( RECEIVING result = lv_msg ).
        MESSAGE lv_msg TYPE 'I'.

      CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
        CLEAR lv_msg1.
        lo_msg1->get_text( RECEIVING result = lv_msg1 ).
        lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
        MESSAGE lv_msg1 TYPE 'I'.
    ENDTRY.

* Create Notification Item & Activities for Form
    TRY.
        REFRESH ex_return[].
        CALL METHOD me->gmib_create_notif_item_act
          EXPORTING
            im_wonum    = im_wonum
            im_qmnum    = im_qmnum
            im_formid   = im_formid
            im_version  = im_version
            im_xml_data = lit_xml_data
          IMPORTING
            ex_return   = ex_return.
        IF ex_return IS NOT INITIAL.
          REFRESH lit_return1[].
          lit_return1 = ex_return.
          DELETE lit_return1 WHERE type NE lc_e OR type = lc_a.
          IF lit_return1 IS NOT INITIAL.
            lo_error = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

            lo_error->add_messages_from_bapi( EXPORTING it_bapi_messages = lit_return1
              iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first
              iv_add_to_response_header = abap_true ).

            RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
              EXPORTING
                message_container = lo_error.
          ENDIF.
        ENDIF.

      CATCH /iwbep/cx_mgw_busi_exception INTO lo_msg. " Business Exception
        CLEAR lv_msg.
        lo_msg->get_text( RECEIVING result = lv_msg ).
        lo_msg->get_longtext( RECEIVING result = lv_msg ).
        MESSAGE lv_msg TYPE 'I'.

      CATCH /iwbep/cx_mgw_tech_exception INTO lo_msg1. " Technical Exception
        CLEAR lv_msg1.
        lo_msg1->get_text( RECEIVING result = lv_msg1 ).
        lo_msg1->get_longtext( RECEIVING result = lv_msg1 ).
        MESSAGE lv_msg1 TYPE 'I'.
    ENDTRY.
  ENDMETHOD.


  METHOD modify_delta_table.

    DATA : lst_delta         TYPE /odsmfe/tb_wo_ex,
           lv_count          TYPE i,
           lv_mobile_app     TYPE string,
           lv_exchange_table TYPE /odsmfe/de_mfe_low100.

    CONSTANTS: lc_formattach TYPE /iwbep/sbdm_node_name     VALUE 'FormAttachmentSet',
               lc_wo         TYPE /odsmfe/de_mfe_fieldname  VALUE 'WORKORDER',
               lc_exchtab    TYPE /odsmfe/de_mfe_tabname    VALUE 'EXCHANGE_TABLE'.
****************************************************************************************
*--Get App Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                   INTO lv_mobile_app
                   WHERE entitysetname = 'MobileAppName'
                   AND field = 'MOBILE_APP_NAME'.

    " Insert data
    lst_delta-mandt      = sy-mandt.
    lst_delta-mobile_app = lv_mobile_app.
    lst_delta-changed_ts = time_token.
    lst_delta-changed_by = sy-uname.

    IF order_number IS NOT INITIAL.

      SELECT SINGLE low FROM /odsmfe/tb_filtr
                          INTO lv_exchange_table
                          WHERE entitysetname = lc_formattach
                          AND tabname = lc_exchtab
                          AND field = lc_wo.

      SELECT COUNT(*) FROM (lv_exchange_table) INTO lv_count
            WHERE objkey = order_number.
      IF lv_count = 0.
        lst_delta-action = 'I'.
      ELSE.
        lst_delta-action = 'U'.
      ENDIF.

      lst_delta-objkey     = order_number.

    ELSEIF notification IS NOT INITIAL.
      CLEAR lv_exchange_table.
      SELECT SINGLE low FROM /odsmfe/tb_filtr INTO lv_exchange_table
                                WHERE entitysetname = 'FormAttachmentSet'
                                  AND tabname = 'NOTIFICATION'
                                  AND field = 'EXCHANGE_TABLE'.

      SELECT COUNT(*) FROM (lv_exchange_table) INTO lv_count
          WHERE objkey = notification.

      IF lv_count = 0.
        lst_delta-action = 'I'.
      ELSE.
        lst_delta-action = 'U'.
      ENDIF.

      lst_delta-objkey     = notification.

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
                      AND field = 'EXCHANGE_TABLE'.

    IF lv_exchange_table IS NOT INITIAL.

      SELECT * FROM (lv_exchange_table) INTO TABLE lit_ex_aufnr
                                     WHERE (lv_filter).


      IF lit_ex_aufnr IS NOT INITIAL.
        ex_aufnr_data[] = lit_ex_aufnr[].
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD update_characterstics.
*--KMADHURI
    CONSTANTS : lc_equi TYPE bapi1003_key-objecttable VALUE 'EQUI',
                lc_iloa TYPE  bapi1003_key-objecttable VALUE 'IFLOT',
                lc_e    TYPE char1 VALUE 'E',
                lc_a    TYPE char1 VALUE 'A'.
    DATA :  lit_return TYPE TABLE OF bapiret2,
            lv_objtab  TYPE bapi1003_key-objecttable,
            lit_char   TYPE TABLE OF bapi1003_alloc_values_char,
            lit_num    TYPE TABLE OF bapi1003_alloc_values_num,
            lit_curr   TYPE TABLE OF bapi1003_alloc_values_curr,
            lv_message TYPE bapi_msg,
            lst_return TYPE bapiret2.


    CLEAR: lit_return,lv_objtab.
    IF lv_klart = '002'.
      lv_objtab = lc_equi.
    ELSE.
      lv_objtab = lc_iloa.
    ENDIF.
* --Get Equipment charateristics
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_object
        objecttable     = lv_objtab
        classnum        = lv_classnum
        classtype       = lv_klart
        keydate         = sy-datum
        language        = sy-langu
      TABLES
        allocvaluesnum  = lit_num
        allocvalueschar = lit_char
        allocvaluescurr = lit_curr
        return          = lit_return.


    LOOP AT im_char ASSIGNING FIELD-SYMBOL(<lfs_char>)." WITH KEY charact = <lfs_char1>-charact.
      READ TABLE lit_char ASSIGNING FIELD-SYMBOL(<lfs_char1>) WITH KEY charact = <lfs_char>-charact.
      IF sy-subrc = 0.
        MOVE <lfs_char>-value TO <lfs_char1>-value_char.
        MOVE <lfs_char>-value TO <lfs_char1>-value_neutral.
      ELSE.
        READ TABLE lit_num ASSIGNING FIELD-SYMBOL(<lfs_char2>) WITH KEY charact = <lfs_char>-charact.
        IF sy-subrc = 0.
          MOVE <lfs_char>-value TO <lfs_char2>-value_from.
        ELSE.
          READ TABLE lit_curr ASSIGNING  FIELD-SYMBOL(<lfs_char3>) WITH KEY charact = <lfs_char>-charact.
          IF sy-subrc = 0.
            MOVE <lfs_char>-value TO <lfs_char3>-value_from.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.
    CLEAR: lit_return.
    CALL FUNCTION 'BAPI_OBJCL_CHANGE'
      EXPORTING
        objectkey          = lv_object
        objecttable        = lv_objtab
        classnum           = lv_classnum
        classtype          = lv_klart
        status             = '1'
        keydate            = sy-datum
      TABLES
        allocvaluesnumnew  = lit_num
        allocvaluescharnew = lit_char
        allocvaluescurrnew = lit_curr
        return             = lit_return.

    LOOP AT lit_return ASSIGNING FIELD-SYMBOL(<lfsst_return>).
      IF <lfsst_return>-type = lc_e OR <lfsst_return>-type = lc_a.
        lv_message = <lfsst_return>-message.

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid  = /iwbep/cx_mgw_busi_exception=>business_error
            message = lv_message.
      ENDIF.
    ENDLOOP.
* Transaction Commit
    CLEAR lst_return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = lst_return.
* Error handling
    IF lst_return-type EQ lc_e OR lst_return-type EQ lc_a.
      lv_message = lst_return-message.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid  = /iwbep/cx_mgw_busi_exception=>business_error
          message = lv_message.
    ENDIF.
    CLEAR: lit_char.


  ENDMETHOD.
ENDCLASS.
