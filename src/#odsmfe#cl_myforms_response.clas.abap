class /ODSMFE/CL_MYFORMS_RESPONSE definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  final
  create public .

public section.
  type-pools ABAP .

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_MYFORMSRESPONSECAPTURE .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_MYFORMSRESPONSECAPTURE .
  data GVIB_USER type USNAM .
  data GSTIB_DEL_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_RESPONSECAPTURE .
  data GITIB_DEL_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_RESPONSECAPTURE .

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



CLASS /ODSMFE/CL_MYFORMS_RESPONSE IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_create_entityset.
*&----------------------------------------------------------------------*
* PROGRAM ID           :/ODSMFE/IF_GET_ENTITYSET_BAPI                   *
* DEVELOPER ID         :PIYYAPPAN                                       *
* SUPPLIER             :OnDevice Solutions                              *
* DATE                 :17.05.2023                                      *
* DEVELOPMENT ID       :/ODSMFE/MA\ HPQC                                *
* CHANGE REQUEST (CTS) :ES1K903808                                      *
*=======================================================================*
* COPIED FROM         : (CLONED PROGRAM)                                *
* TITLE               : (PROGRAM TITLE)                                 *
* OTHER RELATED OBJ   : (OBJECT NAMES)                                  *
*=======================================================================*
* CHANGE HISTORY LOG                                                    *
* CHANGE HISTORY LOG                                                    *
*-----------------------------------------------------------------------*
* CHANGE HISTORY LOG                                                    *
* MOD.NO.| DATE     | NAME           | CORRECTION NUMBER  |CHANGE       *
*                                                          REFERENCE    *
*-----------------------------------------------------------------------*
*        |          |                |                    |             *
*                                                                       *
* DESCRIPTION:                                                          *
*-----------------------------------------------------------------------*
* ----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                     *
* ----------------------------------------------------------------------*

    "/ Types
    TYPES: BEGIN OF ltys_responsecapture,
             instanceid   TYPE c LENGTH 50,                                    "InstanceID
             formid       TYPE /odsmfe/tb_forsp-formid,                        "FormID
             version      TYPE c LENGTH 3,                                     "Version
             wo_num       TYPE c LENGTH 12,                                    "WorkOrder Number
             vornr        TYPE c LENGTH 4,                                     "Operation Number
             plnty        TYPE plnty,                                          "TaskList Type
             plnnr        TYPE plnnr,                                          "Group
             plnal        TYPE plnal,                                          "Group Counter
             zaehl        TYPE char8,                                          "Counter
             equnr        TYPE c LENGTH 18,                                    "Equipment
             tplnr        TYPE c LENGTH 30,                                    "Functional Location
             responsedata TYPE string,                                         "Response Data
             created_on   TYPE timestamp,                                      "CreatedOn
             created_by   TYPE c LENGTH 50,                                    "CreatedBy
             modified_on  TYPE timestamp,                                      "ModifiedOn
             modified_by  TYPE c LENGTH 50,                                    "ModifiedBy
             nonobjtype   TYPE /odsmfe/tb_forsp-nonobjtype,                    "NonObjectType
             isdraft      TYPE c LENGTH 1,                                     "IsDraft
             counter      TYPE /odsmfe/de_counter,                             "Counter
             remarks      TYPE /odsmfe/de_remark,                              "Remarks
             auart        TYPE aufart,                                         "OrderType
           END OF ltys_responsecapture.

    "/ Tables and Structures
    DATA: lst_response     TYPE /odsmfe/tb_forsp,                              "Structure for response capture table
          lst_formresponse TYPE ltys_responsecapture.                          "Types Structure for form response

    "/ Variables
    DATA: lv_cdate       TYPE sy-datlo,                                        "created date
          lv_mdate       TYPE sy-datlo,                                        "modified date
          lv_ctime       TYPE sy-timlo,                                        "Created time
          lv_mtime       TYPE sy-timlo,                                        "Modified time
          lv_instanceid  TYPE char50,                                          "InstanceID
          lv_modified_by TYPE /odsmfe/de_modifiedby.                           "ModifiedBy

* ----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N              *
* ----------------------------------------------------------------------*

* ----------------------------------------------------------------------*
*                   M A I N   S E C T I O N                             *
* ----------------------------------------------------------------------*

    "/ Get the requested date from the gateway service
    im_data_provider->read_entry_data( IMPORTING es_data = lst_formresponse ).

    IF lst_formresponse IS NOT INITIAL.
      lst_response-instanceid = lst_formresponse-instanceid.
      lst_response-formid     = lst_formresponse-formid.
      lst_response-version    = lst_formresponse-version.
      TRANSLATE lst_formresponse-responsedata TO UPPER CASE.
      lst_response-responsedata = lst_formresponse-responsedata.
      lst_response-wo_num       = lst_formresponse-wo_num.
      lst_response-vornr        = lst_formresponse-vornr.
      lst_response-plnty        = lst_formresponse-plnty.
      lst_response-plnnr        = lst_formresponse-plnnr.
      lst_response-plnal        = lst_formresponse-plnal.
      lst_response-zaehl        = lst_formresponse-zaehl.
      lst_response-equnr        = lst_formresponse-equnr.
      lst_response-tplnr        = lst_formresponse-tplnr.
      lst_response-created_on   = lst_formresponse-created_on.
      TRANSLATE lst_formresponse-created_by TO UPPER CASE.
      lst_response-created_by  = sy-uname.
      lst_response-modified_on = lst_formresponse-modified_on.
      TRANSLATE lst_formresponse-modified_by TO UPPER CASE.
      lst_response-modified_by = lst_formresponse-modified_by.
      lst_response-nonobjtype  = lst_formresponse-nonobjtype.
      lst_response-counter     = lst_formresponse-counter.
      lst_response-isdraft     = lst_formresponse-isdraft.
      lst_response-remarks     = lst_formresponse-remarks.

      IF lst_response-created_on IS NOT INITIAL.
        "/Converting timestamp into date and time
        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
          EXPORTING
            iv_timestamp     = lst_response-created_on
          IMPORTING
            o_date           = lv_cdate
            o_time           = lv_ctime
          EXCEPTIONS
            conversion_error = 1
            OTHERS           = 2.
        IF sy-subrc EQ 0.
          lst_response-created_date = lv_cdate.
          lst_response-created_time = lv_ctime.
        ENDIF.                                                                 "/IF sy-subrc EQ 0.
      ENDIF.                                                                   "/IF lst_response-created_on IS NOT INITIAL.

      IF lst_formresponse-modified_on IS NOT INITIAL.
        "/Converting timestamp into date and time
        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
          EXPORTING
            iv_timestamp     = lst_formresponse-modified_on
          IMPORTING
            o_date           = lv_mdate
            o_time           = lv_mtime
          EXCEPTIONS
            conversion_error = 1
            OTHERS           = 2.
        IF sy-subrc EQ 0.
          lst_response-modified_date = lv_mdate.
          lst_response-modified_time = lv_mtime.
        ENDIF.                                                                 "/IF sy-subrc = 0.
      ENDIF.                                                                   "/IF lst_formresponse-modified_on IS NOT INITIAL.

      IF lst_response-instanceid IS NOT INITIAL.
        "/ Update the form response table based on requested data
        SELECT SINGLE instanceid
          FROM /odsmfe/tb_forsp
          INTO lv_instanceid
          WHERE instanceid EQ lst_response-instanceid.

        IF sy-subrc NE 0 AND lv_instanceid IS INITIAL.
          INSERT /odsmfe/tb_forsp FROM lst_response.
          IF sy-subrc <> 0.
            CLEAR: lst_response.
          ENDIF.                                                               "/IF sy-subrc <> 0.
        ELSE.                                                                  " IF SY-SUBRC <> 0.
          UPDATE /odsmfe/tb_forsp FROM lst_response.
          IF sy-subrc <> 0.
            CLEAR: lst_response.
          ENDIF.                                                               "/IF sy-subrc <> 0.
        ENDIF.                                                                 "/IF sy_subrc NE 0 AND lv_instanceid IS INITIAL.

      ENDIF.                                                                   "/IF lst_response-instanceid IS NOT INITIAL.

    ENDIF.                                                                     "/IF lst_formresponse IS NOT INITIAL.

    "/ Mapping the created InstanceID to the Gateway output response table
    gstib_entity-instanceid = lst_response-instanceid.
    GET REFERENCE OF gstib_entity INTO ex_entity.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_delete_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS - PIYYAPPAN
* Creation Date          : 19/04/2023
* Transport No.          : ES1K903808
* Program Description    : To delete Non Object forms response data
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

    "/ Tables and Structures
    DATA: lit_key_tab TYPE /iwbep/t_mgw_name_value_pair,       "name value pair for mgw
          lst_key_tab TYPE /iwbep/s_mgw_name_value_pair.       "name value pair for mgw

    "/ Variables
    DATA: lv_instanceid TYPE /odsmfe/de_instanceid,            "InstanceID
          lv_deleted    TYPE char1.                            "Deleted

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

* -----------------------------------------------------------------------*
*                  M A I N    S E C T I O N                              *
* -----------------------------------------------------------------------*

    "/ Read key values
    lit_key_tab = im_key_tab.
    SORT lit_key_tab BY name.

    READ TABLE lit_key_tab INTO lst_key_tab WITH KEY name = 'InstanceID' BINARY SEARCH.

    IF sy-subrc = 0 AND lst_key_tab-value IS NOT INITIAL.
      SELECT SINGLE instanceid
        FROM /odsmfe/tb_forsp
        INTO lv_instanceid
        WHERE instanceid = lst_key_tab-value.
    ENDIF. "/IF sy-subrc = 0 AND lst_key_tab-value IS NOT INITIAL.

    IF lv_instanceid IS NOT INITIAL.
      "/Update entry from Response capture as deleted
      lv_deleted = abap_true.
      UPDATE /odsmfe/tb_forsp SET deleted = lv_deleted
      WHERE instanceid =  lv_instanceid.
    ENDIF. "/ IF lv_instanceid IS NOT INITIAL.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_modify_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : ODS - PIYYAPPAN
* Creation Date          : 02/05/2023
* Transport No.          : ES1K903808
* Program Description    : updates forms data in service
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                      *
* -----------------------------------------------------------------------*

    "/ Types
    TYPES: BEGIN OF ltys_responsecapture,
             instanceid   TYPE c LENGTH 50,                                    "InstanceID
             formid       TYPE /odsmfe/tb_forsp-formid,                        "FormID
             version      TYPE c LENGTH 3,                                     "Version
             wo_num       TYPE c LENGTH 12,                                    "WorkOrder Number
             vornr        TYPE c LENGTH 4,                                     "Operation Number
             plnty        TYPE plnty,                                          "TaskList Type
             plnnr        TYPE plnnr,                                          "Group
             plnal        TYPE plnal,                                          "Group Counter
             zaehl        TYPE char8,                                          "Counter
             equnr        TYPE c LENGTH 18,                                    "Equipment
             tplnr        TYPE c LENGTH 30,                                    "Functional Location
             responsedata TYPE string,                                         "Response Data
             created_on   TYPE timestamp,                                      "CreatedOn
             created_by   TYPE c LENGTH 50,                                    "CreatedBy
             modified_on  TYPE timestamp,                                      "ModifiedOn
             modified_by  TYPE c LENGTH 50,                                    "ModifiedBy
             nonobjtype   TYPE /odsmfe/tb_forsp-nonobjtype,                    "NonObjectType
             isdraft      TYPE c LENGTH 1,                                     "IsDraft
             counter      TYPE /odsmfe/de_counter,                             "Counter
             remarks      TYPE /odsmfe/de_remark,                              "Remarks
             auart        TYPE aufart,                                         "OrderType
           END OF ltys_responsecapture.

    "/ Tables and Structures
    DATA: lst_response     TYPE /odsmfe/tb_forsp,                              "Structure for response capture table
          lst_formresponse TYPE ltys_responsecapture.                          "Types Structure for form response

    "/ Variables
    DATA: lv_cdate       TYPE sy-datlo,                                        "created date
          lv_mdate       TYPE sy-datlo,                                        "modified date
          lv_ctime       TYPE sy-timlo,                                        "Created time
          lv_mtime       TYPE sy-timlo,                                        "Modified time
          lv_instanceid  TYPE char50,                                          "InstanceID
          lv_modified_by TYPE /odsmfe/de_modifiedby.                           "ModifiedBy

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

* -----------------------------------------------------------------------*
*                   M A I N   S E C T I O N                              *
* -----------------------------------------------------------------------*

    "Get the requested data from the Gateway service
    im_data_provider->read_entry_data( IMPORTING es_data = lst_formresponse ).

    IF lst_formresponse IS NOT INITIAL.
      lst_response-instanceid     = lst_formresponse-instanceid.
      lst_response-formid         = lst_formresponse-formid.
      lst_response-version        = lst_formresponse-version.
      TRANSLATE lst_formresponse-responsedata TO UPPER CASE.
      lst_response-responsedata   = lst_formresponse-responsedata.
      lst_response-wo_num         = lst_formresponse-wo_num.
      lst_response-vornr          = lst_formresponse-vornr.
      lst_response-plnty          = lst_formresponse-plnty.
      lst_response-plnnr          = lst_formresponse-plnnr.
      lst_response-plnal          = lst_formresponse-plnal.
      lst_response-zaehl          = lst_formresponse-zaehl.
      lst_response-equnr          = lst_formresponse-equnr.
      lst_response-tplnr          = lst_formresponse-tplnr.
      lst_response-created_on     = lst_formresponse-created_on.
      TRANSLATE lst_formresponse-created_by TO UPPER CASE.
      lst_response-created_by     = sy-uname.
      lst_response-modified_on    = lst_formresponse-modified_on.
      TRANSLATE lst_formresponse-modified_by TO UPPER CASE.
      lst_response-modified_by    = lst_formresponse-modified_by.
      lst_response-nonobjtype     = lst_formresponse-nonobjtype.
      lst_response-counter        = lst_formresponse-counter.
      lst_response-isdraft        = lst_formresponse-isdraft.      "For saving form in Draft
      lst_response-remarks        = lst_formresponse-remarks.

      IF lst_response-created_on IS NOT INITIAL.
        "/Converting timestamp into date and time
        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
          EXPORTING
            iv_timestamp     = lst_response-created_on
          IMPORTING
            o_date           = lv_cdate
            o_time           = lv_ctime
          EXCEPTIONS
            conversion_error = 1
            OTHERS           = 2.
        IF sy-subrc EQ 0.
          lst_response-created_date  = lv_cdate.
          lst_response-created_time  = lv_ctime.
        ENDIF."/IF sy-subrc EQ 0.
      ENDIF."/IF lst_response-created_on IS NOT INITIAL.

      IF lst_formresponse-modified_on IS NOT INITIAL.
        "/Converting timestamp into date and time
        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
          EXPORTING
            iv_timestamp     = lst_formresponse-modified_on
          IMPORTING
            o_date           = lv_mdate
            o_time           = lv_mtime
          EXCEPTIONS
            conversion_error = 1
            OTHERS           = 2.
        IF sy-subrc EQ 0.
          lst_response-modified_date = lv_mdate.
          lst_response-modified_time = lv_mtime.
        ENDIF. "/IF sy-subrc = 0.
      ENDIF. "/IF lst_formresponse-modified_on IS NOT INITIAL.

      "/ Update the form response table based on the requested data
      SELECT SINGLE instanceid
        FROM /odsmfe/tb_forsp
        INTO lv_instanceid
        WHERE instanceid EQ lst_response-instanceid.

      IF sy-subrc EQ 0 AND lv_instanceid IS NOT INITIAL.
        MODIFY /odsmfe/tb_forsp FROM lst_response.
      ENDIF. "/IF sy-subrc EQ 0 AND lv_instanceid IS NOT INITIAL.

    ENDIF. "/IF lst_formresponse IS NOT INITIAL.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS - PIYYAPPAN
* Creation Date          : 19/04/2023
* Transport No.          : ES1K903808
* Program Description    : Display Non Object forms response data
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

    "/Types
    TYPES:BEGIN OF ltys_foass,
            formid   TYPE /odsmfe/tb_foass-formid,                                  "FormID
            version  TYPE /odsmfe/tb_foass-version,                                 "Version
            category TYPE /odsmfe/tb_foass-category,                                "Form Category
          END OF ltys_foass,

          BEGIN OF ltys_addr,
            bname      TYPE xubname,                                                "Username
            name_textc TYPE ad_namtext,                                             "FullName
          END OF ltys_addr.

    "/Tables and Structures
    DATA: lit_forsp         TYPE TABLE OF /odsmfe/tb_forsp,                         "Table for form response
          lst_forsp         TYPE /odsmfe/tb_forsp,                                  "Structure for form response
          lit_foass         TYPE TABLE OF ltys_foass,                               "Table for form assignment
          lit_addr          TYPE TABLE OF ltys_addr,                                "Table for user address
          lst_addr          TYPE ltys_addr,                                         "Structure for user address
          lst_key_tab       TYPE /iwbep/s_mgw_name_value_pair,                      "Name value pair for mgw
          lst_filter        TYPE /iwbep/s_mgw_select_option,                        "MGW Framework: Selection Option Parameters for db selects
          lst_filter_range  TYPE /iwbep/s_cod_select_option.                        "MGW Framework: Select Options for Queries

    "/Range Tables and Range Structures
    DATA: lrs_core_range_str TYPE /odsmwh/st_core_range_str,                        "Filter Purpose Range Structure
          lrs_filter_range   TYPE /iwbep/s_cod_select_option,                       "MGW Framework: Select Options for Queries
          lrs_date           TYPE /odsmfe/st_core_range_str,                        "Date Range structure
          lrt_date           TYPE TABLE OF /odsmfe/st_core_range_str,               "Date Range table
          lrt_filter_range   TYPE /odsmwh/tt_core_range_tab,                        "Filter range table
          lrs_it_filters     TYPE /odsmfe/st_core_range_str,                        "Filter values Range Structure
          lrt_instanceid     TYPE TABLE OF /odsmfe/st_core_range_str,               "InstanceID Range table
          lrt_formid         TYPE TABLE OF /odsmfe/st_core_range_str,               "FormID Range table
          lrt_version        TYPE TABLE OF /odsmfe/st_core_range_str.               "Version Range table

    "/Variables
    DATA: lv_date      TYPE dats,                                                   "date
          lv_days      TYPE char10,                                                 "days
          lv_days_inst TYPE numc2.                                                  "days instance

    "/Constants
    CONSTANTS: lc_instanceid     TYPE string VALUE 'INSTANCEID',                    "InstanceID
               lc_key_instanceid TYPE string VALUE 'InstanceID',                    "InstanceID for key values
               lc_formid         TYPE string VALUE 'FORMID',                        "FormID
               lc_version        TYPE string VALUE 'VERSION',                       "Version
               lc_nonobject      TYPE string VALUE 'NonObject',                     "NonObject
               lc_entityset      TYPE string VALUE 'myFormsResponseCaptureSet',     "myFormsResponseCaptureSet
               lc_field          TYPE string VALUE 'DAYS_INSTANCE',                 "Days
               lc_month          TYPE numc2  VALUE '00',                            "Month
               lc_sign           TYPE c      VALUE '-',                             "Sign
               lc_years          TYPE numc2  VALUE '00',                            "Years
               lc_i              TYPE string VALUE 'I',                             "Single-Character Indicator
               lc_eq             TYPE string VALUE 'EQ',                            "Version Number Component
               lc_bt             TYPE string VALUE 'BT'.                            "Version Number Component


* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

* -----------------------------------------------------------------------*
*                  M A I N    S E C T I O N                              *
* -----------------------------------------------------------------------*

    "/ Read filter Property Values
    IF im_filter_select_options IS NOT INITIAL.

      LOOP AT im_filter_select_options INTO lst_filter.
        TRANSLATE lst_filter-property TO UPPER CASE.
        CASE lst_filter-property.

          WHEN lc_instanceid.
            READ TABLE lst_filter-select_options INTO lrs_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lrs_filter_range IS NOT INITIAL.
              lrs_it_filters-sign    = lc_i.
              lrs_it_filters-option  = lc_eq.
              lrs_it_filters-low     = lrs_filter_range-low.
              APPEND lrs_it_filters TO lrt_instanceid.
            ENDIF. "/IF sy-subrc EQ 0 AND lrs_filter_range IS NOT INITIAL.

          WHEN lc_formid.
            READ TABLE lst_filter-select_options INTO lrs_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lrs_filter_range IS NOT INITIAL.
              lrs_it_filters-sign      = lc_i.
              lrs_it_filters-option    = lc_eq.
              lrs_it_filters-low       = lrs_filter_range-low.
              APPEND lrs_it_filters TO lrt_formid.
            ENDIF. "/IF sy-subrc EQ 0 AND lrs_filter_range IS NOT INITIAL.

          WHEN lc_version.
            READ TABLE lst_filter-select_options INTO lrs_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lrs_filter_range IS NOT INITIAL.
              lrs_it_filters-sign   = lc_i.
              lrs_it_filters-option = lc_eq.
              lrs_it_filters-low    = lrs_filter_range-low.
              APPEND lrs_it_filters TO lrt_version.
            ENDIF. "/IF sy-subrc EQ 0 AND lrs_filter_range IS NOT INITIAL.

        ENDCASE. "/Case lst_filter-property
        CLEAR: lrs_it_filters.
      ENDLOOP.   "/Loop at im_filter_select_options into lst_filter.

    ENDIF. "/If im_filter_select_options is not initial.

    "/Read the key property values
    IF im_key_tab IS NOT INITIAL.

      LOOP AT im_key_tab INTO lst_key_tab.
        CASE lst_key_tab-name.

          WHEN lc_key_instanceid.
            lrs_it_filters-sign   = lc_i.
            lrs_it_filters-option = lc_eq.
            lrs_it_filters-low    = lst_key_tab-value.
            APPEND lrs_it_filters to lrt_instanceid.
            CLEAR: lrs_it_filters.

        ENDCASE. "/CASE lst_key_tab-name.

      ENDLOOP. "/ LOOP AT im_key_tab INTO lst_key_tab.

    ENDIF. "/IF im_key_tab[] IS NOT INITIAL.

    "/To get configurable days from filter table
    SELECT SINGLE low
      FROM /odsmfe/tb_filtr
      INTO lv_days
      WHERE entitysetname EQ lc_entityset
      AND   field         EQ lc_field.

    "/Instance data will be fetched based on the configurable days
    IF lv_days IS NOT INITIAL.
      lv_days_inst = lv_days.
      "/ Function Module to get the back date based on configurable days
      CALL FUNCTION '/SAPHT/DRM_CALC_DATE'
        EXPORTING
          date      = sy-datum
          days      = lv_days_inst
          months    = lc_month
          sign      = lc_sign
          years     = lc_years
        IMPORTING
          calc_date = lv_date.

      lrs_date-sign   = lc_i.
      lrs_date-option = lc_bt.
      lrs_date-low    = lv_date.
      lrs_date-high   = sy-datum.
      APPEND lrs_date TO lrt_date[].

    ENDIF. "/IF lv_days IS NOT INITIAL.

    "/To get all the non object forms
    SELECT formid
           version
           category
      FROM /odsmfe/tb_foass
      INTO TABLE lit_foass
      WHERE formid    IN lrt_formid[]
      AND   version   IN lrt_version[]
      AND   category  EQ lc_nonobject.

    IF lit_foass[] IS NOT INITIAL.
      "/To get all the non object forms instances
      SELECT mandt
             instanceid
             formid
             version
             wo_num
             vornr
             plnty
             plnnr
             plnal
             zaehl
             equnr
             tplnr
             responsedata
             formhtml
             formmodel
             roleid
             created_on
             created_by
             modified_on
             modified_by
             nonobjtype
             isdraft
             created_date
             created_time
             modified_date
             modified_time
             submitted
             multiple_sub
             counter
             remarks
             deleted
             order_type
        FROM /odsmfe/tb_forsp
        INTO TABLE lit_forsp
        FOR ALL ENTRIES IN lit_foass
        WHERE nonobjtype   EQ abap_true
        AND   instanceid   IN lrt_instanceid[]
        AND   formid       EQ lit_foass-formid
        AND   version      EQ lit_foass-version
        AND   created_date IN lrt_date[].

      IF sy-subrc EQ 0 AND lit_forsp[] IS NOT INITIAL.
        SELECT bname
               name_textc
          FROM user_addr
          INTO TABLE lit_addr[]
          FOR ALL ENTRIES IN lit_forsp
          WHERE bname     EQ lit_forsp-created_by.
       ENDIF. "/IF sy-subrc EQ 0 AND lit_forsp[] IS NOT INITIAL.

        "/Exporting all the non object form response data into final internal table
        LOOP AT lit_forsp INTO lst_forsp.
          gstib_entity-instanceid     = lst_forsp-instanceid.
          gstib_entity-formid         = lst_forsp-formid.
          gstib_entity-version        = lst_forsp-version.
          gstib_entity-auart          = lst_forsp-order_type.
          gstib_entity-counter        = lst_forsp-counter.
          gstib_entity-created_by     = lst_forsp-created_by.
          gstib_entity-created_on     = lst_forsp-created_on.
          gstib_entity-deleted        = lst_forsp-deleted.
          gstib_entity-equnr          = lst_forsp-equnr.
          gstib_entity-isdraft        = lst_forsp-isdraft.
          gstib_entity-modified_by    = lst_forsp-modified_by.
          gstib_entity-modified_on    = lst_forsp-modified_on.
          gstib_entity-nonobjtype     = lst_forsp-nonobjtype.
          gstib_entity-plnal          = lst_forsp-plnal.
          gstib_entity-plnnr          = lst_forsp-plnnr.
          gstib_entity-plnty          = lst_forsp-plnty.
          gstib_entity-remarks        = lst_forsp-remarks.
          gstib_entity-responsedata   = lst_forsp-responsedata.
          gstib_entity-tplnr          = lst_forsp-tplnr.
          gstib_entity-vornr          = lst_forsp-vornr.
          gstib_entity-wo_num         = lst_forsp-wo_num.
          gstib_entity-zaehl          = lst_forsp-zaehl.
          READ TABLE lit_addr INTO lst_addr WITH KEY bname = lst_forsp-created_by.
          IF sy-subrc EQ 0.
            gstib_entity-created_by_name = lst_addr-name_textc.
          ENDIF. "/If sy-subrc eq 0.
          APPEND gstib_entity TO gitib_entity.
          CLEAR: gstib_entity.

        ENDLOOP. "/Loop at lit_forsp into lst_forsp.

       "/Mapping properties from the backend to the Gateway output response table
       IF im_key_tab[] IS NOT INITIAL.
         READ TABLE gitib_entity INTO gstib_entity INDEX 1.
         GET REFERENCE OF gstib_entity INTO ex_entity.
       ELSE.
         GET REFERENCE OF gitib_entity INTO ex_entityset.
       ENDIF. "/IF im_key_tab[] IS NOT INITIAL.

    ENDIF. "/If lit_foass[] is not initial.

  ENDMETHOD.
ENDCLASS.
