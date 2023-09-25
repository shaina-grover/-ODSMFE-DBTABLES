class /ODSMFE/CL_MYFORMS_RESPONSE definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  final
  create public .

public section.
  data GVIB_USER type USNAM .

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
             instanceid   TYPE c LENGTH 50,                                         "InstanceID
             formid       TYPE /odsmfe/tb_forsp-formid,                        "FormID
             version      TYPE c LENGTH 3,                                             "Version
             wo_num       TYPE c LENGTH 12,                                        "WorkOrder Number
             vornr        TYPE c LENGTH 4,                                               "Operation Number
             plnty        TYPE c LENGTH 1,                                               "TaskList Type
             plnnr        TYPE c LENGTH 8,                                               "Group
             plnal        TYPE c LENGTH 2,                                               "Group Counter
             zaehl        TYPE c LENGTH 8,                                                         "Counter
             equnr        TYPE c LENGTH 18,                                            "Equipment
             tplnr        TYPE c LENGTH 30,                                             "Functional Location
             responsedata TYPE string,                                                 "Response Data
             created_on   TYPE timestamp,                                           "CreatedOn
             created_by   TYPE c LENGTH 50,                                        "CreatedBy
             modified_on  TYPE timestamp,                                          "ModifiedOn
             modified_by  TYPE c LENGTH 50,                                       "ModifiedBy
             nonobjtype   TYPE /odsmfe/tb_forsp-nonobjtype,             "NonObjectType
             isdraft      TYPE c LENGTH 1,                                              "IsDraft
             counter      TYPE /odsmfe/de_counter,                             "Counter
             remarks      TYPE /odsmfe/de_remark,                              "Remarks
             auart        TYPE c LENGTH 4,                                                       "OrderType
           END OF ltys_responsecapture.

    "/ Tables and Structures
    DATA: lst_response     TYPE /odsmfe/tb_forsp,                              "Structure for response capture table
          lst_formresponse TYPE ltys_responsecapture.                          "Types Structure for form response

    "/ Variables
    DATA: lv_cdate       TYPE sy-datlo,                                        "created date
          lv_mdate       TYPE sy-datlo,                                        "modified date
          lv_ctime       TYPE sy-timlo,                                        "Created time
          lv_mtime       TYPE sy-timlo,                                        "Modified time
          lv_instanceid  TYPE c LENGTH 50,                                          "InstanceID
          lv_modified_by TYPE /odsmfe/de_modifiedby.                           "ModifiedBy

* --------------------------------------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N                                           *
* --------------------------------------------------------------------------------------------------*

* --------------------------------------------------------------------------------------------------*
*                   M A I N   S E C T I O N                                                                     *
* --------------------------------------------------------------------------------------------------*

*    "/ Get the requested date from the gateway service
*    im_data_provider->read_entry_data( IMPORTING es_data = lst_formresponse ).
*
*    IF lst_formresponse IS NOT INITIAL.
*      lst_response-instanceid = lst_formresponse-instanceid.
*      lst_response-formid     = lst_formresponse-formid.
*      lst_response-version    = lst_formresponse-version.
*      TRANSLATE lst_formresponse-responsedata TO UPPER CASE.
*      lst_response-responsedata = lst_formresponse-responsedata.
*      lst_response-wo_num       = lst_formresponse-wo_num.
*      lst_response-vornr        = lst_formresponse-vornr.
*      lst_response-plnty        = lst_formresponse-plnty.
*      lst_response-plnnr        = lst_formresponse-plnnr.
*      lst_response-plnal        = lst_formresponse-plnal.
*      lst_response-zaehl        = lst_formresponse-zaehl.
*      lst_response-equnr        = lst_formresponse-equnr.
*      lst_response-tplnr        = lst_formresponse-tplnr.
*      lst_response-created_on   = lst_formresponse-created_on.
*      TRANSLATE lst_formresponse-created_by TO UPPER CASE.
*      lst_response-created_by  = sy-uname.
*      lst_response-modified_on = lst_formresponse-modified_on.
*      TRANSLATE lst_formresponse-modified_by TO UPPER CASE.
*      lst_response-modified_by = lst_formresponse-modified_by.
*      lst_response-nonobjtype  = lst_formresponse-nonobjtype.
*      lst_response-counter     = lst_formresponse-counter.
*      lst_response-isdraft     = lst_formresponse-isdraft.
*      lst_response-remarks     = lst_formresponse-remarks.
*
*      IF lst_response-created_on IS NOT INITIAL.
*        "/Converting timestamp into date and time
*        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
*          EXPORTING
*            iv_timestamp     = lst_response-created_on
*          IMPORTING
*            o_date           = lv_cdate
*            o_time           = lv_ctime
*          EXCEPTIONS
*            conversion_error = 1
*            OTHERS           = 2.
*        IF sy-subrc EQ 0.
*          lst_response-created_date = lv_cdate.
*          lst_response-created_time = lv_ctime.
*        ENDIF.                                                                 "/IF sy-subrc EQ 0.
*      ENDIF.                                                                   "/IF lst_response-created_on IS NOT INITIAL.
*
*      IF lst_formresponse-modified_on IS NOT INITIAL.
*        "/Converting timestamp into date and time
*        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
*          EXPORTING
*            iv_timestamp     = lst_formresponse-modified_on
*          IMPORTING
*            o_date           = lv_mdate
*            o_time           = lv_mtime
*          EXCEPTIONS
*            conversion_error = 1
*            OTHERS           = 2.
*        IF sy-subrc EQ 0.
*          lst_response-modified_date = lv_mdate.
*          lst_response-modified_time = lv_mtime.
*        ENDIF.                                                                 "/IF sy-subrc = 0.
*      ENDIF.                                                                   "/IF lst_formresponse-modified_on IS NOT INITIAL.
*
*      IF lst_response-instanceid IS NOT INITIAL.
*        "/ Update the form response table based on requested data
*        SELECT SINGLE instanceid
*          FROM /odsmfe/tb_forsp
*          INTO lv_instanceid
*          WHERE instanceid EQ lst_response-instanceid.
*
*        IF sy-subrc NE 0 AND lv_instanceid IS INITIAL.
*          INSERT /odsmfe/tb_forsp FROM lst_response.
*          IF sy-subrc <> 0.
*            CLEAR: lst_response.
*          ENDIF.                                                               "/IF sy-subrc <> 0.
*        ELSE.                                                                  " IF SY-SUBRC <> 0.
*          UPDATE /odsmfe/tb_forsp FROM lst_response.
*          IF sy-subrc <> 0.
*            CLEAR: lst_response.
*          ENDIF.                                                               "/IF sy-subrc <> 0.
*        ENDIF.                                                                 "/IF sy_subrc NE 0 AND lv_instanceid IS INITIAL.
*
*      ENDIF.                                                                   "/IF lst_response-instanceid IS NOT INITIAL.
*
*    ENDIF.                                                                     "/IF lst_formresponse IS NOT INITIAL.
*
*    "/ Mapping the created InstanceID to the Gateway output response table
*    gstib_entity-instanceid = lst_response-instanceid.
*    GET REFERENCE OF gstib_entity INTO ex_entity.

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
*    DATA: lit_key_tab TYPE /iwbep/t_mgw_name_value_pair,       "name value pair for mgw
*          lst_key_tab TYPE /iwbep/s_mgw_name_value_pair.       "name value pair for mgw

    "/ Variables
    DATA: lv_instanceid TYPE /odsmfe/de_instanceid,            "InstanceID
          lv_deleted    TYPE char1.                            "Deleted

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

* -----------------------------------------------------------------------*
*                  M A I N    S E C T I O N                              *
* -----------------------------------------------------------------------*

*    "/ Read key values
*    lit_key_tab = im_key_tab.
*    SORT lit_key_tab BY name.
*
*    READ TABLE lit_key_tab INTO lst_key_tab WITH KEY name = 'InstanceID' BINARY SEARCH.
*
*    IF sy-subrc = 0 AND lst_key_tab-value IS NOT INITIAL.
*      SELECT SINGLE instanceid
*        FROM /odsmfe/tb_forsp
*        INTO lv_instanceid
*        WHERE instanceid = lst_key_tab-value.
*    ENDIF. "/IF sy-subrc = 0 AND lst_key_tab-value IS NOT INITIAL.
*
*    IF lv_instanceid IS NOT INITIAL.
*      "/Update entry from Response capture as deleted
*      lv_deleted = abap_true.
*      UPDATE /odsmfe/tb_forsp SET deleted = lv_deleted
*      WHERE instanceid =  lv_instanceid.
*    ENDIF. "/ IF lv_instanceid IS NOT INITIAL.
*
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
             plnty        TYPE c LENGTH 1,                                               "TaskList Type
             plnnr        TYPE c LENGTH 8,                                               "Group
             plnal        TYPE c LENGTH 2,                                               "Group Counter
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
             auart        TYPE c LENGTH 4,                                         "OrderType
           END OF ltys_responsecapture.

    "/ Tables and Structures
    DATA: lst_response     TYPE /odsmfe/tb_forsp,                              "Structure for response capture table
          lst_formresponse TYPE ltys_responsecapture.                          "Types Structure for form response

    "/ Variables
    DATA: lv_cdate       TYPE sy-datlo,                                        "created date
          lv_mdate       TYPE sy-datlo,                                        "modified date
          lv_ctime       TYPE sy-timlo,                                        "Created time
          lv_mtime       TYPE sy-timlo,                                        "Modified time
          lv_instanceid  TYPE c LENGTH 50,                                          "InstanceID
          lv_modified_by TYPE /odsmfe/de_modifiedby.                           "ModifiedBy

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

* -----------------------------------------------------------------------*
*                   M A I N   S E C T I O N                              *
* -----------------------------------------------------------------------*

*    "Get the requested data from the Gateway service
*    im_data_provider->read_entry_data( IMPORTING es_data = lst_formresponse ).
*
*    IF lst_formresponse IS NOT INITIAL.
*      lst_response-instanceid     = lst_formresponse-instanceid.
*      lst_response-formid         = lst_formresponse-formid.
*      lst_response-version        = lst_formresponse-version.
*      TRANSLATE lst_formresponse-responsedata TO UPPER CASE.
*      lst_response-responsedata   = lst_formresponse-responsedata.
*      lst_response-wo_num         = lst_formresponse-wo_num.
*      lst_response-vornr          = lst_formresponse-vornr.
*      lst_response-plnty          = lst_formresponse-plnty.
*      lst_response-plnnr          = lst_formresponse-plnnr.
*      lst_response-plnal          = lst_formresponse-plnal.
*      lst_response-zaehl          = lst_formresponse-zaehl.
*      lst_response-equnr          = lst_formresponse-equnr.
*      lst_response-tplnr          = lst_formresponse-tplnr.
*      lst_response-created_on     = lst_formresponse-created_on.
*      TRANSLATE lst_formresponse-created_by TO UPPER CASE.
*      lst_response-created_by     = sy-uname.
*      lst_response-modified_on    = lst_formresponse-modified_on.
*      TRANSLATE lst_formresponse-modified_by TO UPPER CASE.
*      lst_response-modified_by    = lst_formresponse-modified_by.
*      lst_response-nonobjtype     = lst_formresponse-nonobjtype.
*      lst_response-counter        = lst_formresponse-counter.
*      lst_response-isdraft        = lst_formresponse-isdraft.      "For saving form in Draft
*      lst_response-remarks        = lst_formresponse-remarks.
*
*      IF lst_response-created_on IS NOT INITIAL.
*        "/Converting timestamp into date and time
*        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
*          EXPORTING
*            iv_timestamp     = lst_response-created_on
*          IMPORTING
*            o_date           = lv_cdate
*            o_time           = lv_ctime
*          EXCEPTIONS
*            conversion_error = 1
*            OTHERS           = 2.
*        IF sy-subrc EQ 0.
*          lst_response-created_date  = lv_cdate.
*          lst_response-created_time  = lv_ctime.
*        ENDIF."/IF sy-subrc EQ 0.
*      ENDIF."/IF lst_response-created_on IS NOT INITIAL.
*
*      IF lst_formresponse-modified_on IS NOT INITIAL.
*        "/Converting timestamp into date and time
*        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
*          EXPORTING
*            iv_timestamp     = lst_formresponse-modified_on
*          IMPORTING
*            o_date           = lv_mdate
*            o_time           = lv_mtime
*          EXCEPTIONS
*            conversion_error = 1
*            OTHERS           = 2.
*        IF sy-subrc EQ 0.
*          lst_response-modified_date = lv_mdate.
*          lst_response-modified_time = lv_mtime.
*        ENDIF. "/IF sy-subrc = 0.
*      ENDIF. "/IF lst_formresponse-modified_on IS NOT INITIAL.
*
*      "/ Update the form response table based on the requested data
*      SELECT SINGLE instanceid
*        FROM /odsmfe/tb_forsp
*        INTO lv_instanceid
*        WHERE instanceid EQ lst_response-instanceid.
*
*      IF sy-subrc EQ 0 AND lv_instanceid IS NOT INITIAL.
*        MODIFY /odsmfe/tb_forsp FROM lst_response.
*      ENDIF. "/IF sy-subrc EQ 0 AND lv_instanceid IS NOT INITIAL.
*
*    ENDIF. "/IF lst_formresponse IS NOT INITIAL.

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
            category TYPE /odsmfe/tb_foass-category,                             "Form Category
          END OF ltys_foass,

          BEGIN OF ltys_addr,
            bname      TYPE c LENGTH 12,                                                  "Username
            name_textc TYPE c LENGTH 80,                                               "FullName
          END OF ltys_addr,

         BEGIN OF ty_data,
             wa(512) TYPE c,
         END OF ty_data,

        BEGIN OF ty_options,
             text(72) TYPE c,
        END OF ty_options,

        BEGIN OF ty_fields,
             fieldname(30) TYPE c,
             offset(6)     TYPE n,
             length(6)     TYPE n,
             type(1)       TYPE c,
             fieldtext(60) TYPE c,
        END OF ty_fields.

    "/Tables and Structures
    DATA: lit_forsp         TYPE TABLE OF /odsmfe/tb_forsp,                         "Table for form response
              lit_foass         TYPE TABLE OF ltys_foass,                                           "Table for form assignment
              lit_addr          TYPE TABLE OF ltys_addr,                                             "Table for user address
              lit_myformsresponse  TYPE TABLE OF /odsmfe/ce_myformsresponse,
              lst_myformsresponse TYPE /odsmfe/ce_myformsresponse,
              lit_options TYPE TABLE OF ty_options,
              lit_fields  TYPE TABLE OF ty_fields,
              lit_data    TYPE TABLE OF ty_data.


    "/Range Tables and Range Structures
    DATA: lrs_core_range_str TYPE /odsmfe/st_core_range_str,                        "Filter Purpose Range Structure
          lrs_date           TYPE /odsmfe/st_core_range_str,                                     "Date Range structure
          lrt_date           TYPE TABLE OF /odsmfe/st_core_range_str,                      "Date Range table
          lrt_filter_range   TYPE /odsmfe/tt_core_range_tab,                                   "Filter range table
          lrs_it_filters     TYPE /odsmfe/st_core_range_str,                                      "Filter values Range Structure
          lrt_instanceid     TYPE TABLE OF /odsmfe/st_core_range_str,                   "InstanceID Range table
          lrt_formid         TYPE TABLE OF /odsmfe/st_core_range_str,                      "FormID Range table
          lrt_version        TYPE TABLE OF /odsmfe/st_core_range_str.                      "Version Range table

    "/Variables
    DATA: lv_date           TYPE dats,                                                  "date
               lv_days          TYPE  c LENGTH 10,                                    "days
               lv_days_inst  TYPE numc2,                                              "days instance
              actual_date TYPE d,
              days TYPE p.

    "/Constants
    CONSTANTS: lc_instanceid            TYPE    string VALUE 'INSTANCEID',                    "InstanceID
                          lc_key_instanceid    TYPE    string VALUE 'InstanceID',                             "InstanceID for key values
                          lc_formid                  TYPE   string VALUE 'FORMID',                                        "FormID
                          lc_version                TYPE    string VALUE 'VERSION',                                        "Version
                          lc_nonobject            TYPE   string VALUE 'NonObject',                                 "NonObject
                          lc_entityset              TYPE   string VALUE '/ODSMFE/CE_MYFORMSRESPONSE',     "myFormsResponseCaptureSet
                          lc_field                     TYPE   string VALUE 'DAYS_INSTANCE',                                 "Days
                          lc_month                  TYPE   numc2  VALUE '00',                                                "Month
                          lc_sign                      TYPE  c      VALUE '-',                                                              "Sign
                          lc_years                    TYPE  numc2  VALUE '00',                                                     "Years
                          lc_i                            TYPE  string VALUE 'I',                                                             "Single-Character Indicator
                          lc_eq                         TYPE  string VALUE 'EQ',                                                        "Version Number Component
                          lc_bt                         TYPE   string VALUE 'BT'.                                                         "Version Number Component


* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N         *
* -----------------------------------------------------------------------*

* -----------------------------------------------------------------------*
*                  M A I N    S E C T I O N                                    *
* -----------------------------------------------------------------------*

    "/ Read filter Property Values
    IF im_filter_select_options IS NOT INITIAL.

      LOOP AT im_filter_select_options INTO DATA(lst_filter_select_options).
        TRANSLATE lst_filter_select_options-name TO UPPER CASE.
        CASE lst_filter_select_options-name.

          WHEN lc_instanceid.
            lrt_instanceid = CORRESPONDING #( lst_filter_select_options-range ).
            DELETE lrt_instanceid WHERE low IS INITIAL.

          WHEN lc_formid.
            lrt_formid = CORRESPONDING #( lst_filter_select_options-range ).
            DELETE lrt_formid WHERE low IS INITIAL.

          WHEN lc_version.
            lrt_version = CORRESPONDING #( lst_filter_select_options-range ).
            DELETE lrt_version WHERE low IS INITIAL.

        ENDCASE. "/Case lst_filter-property

"/ New ABAP Syntax for CASE-ENDCASE Statements
*         lrt_instanceid = SWITCH #( lst_filter_select_options-name WHEN lc_instanceid THEN CORRESPONDING #( lst_filter_select_options-range )
*                                                                                                         WHEN lc_formid THEN CORRESPONDING #( lst_filter_select_options-range )
*                                                                                                         WHEN lc_version THEN CORRESPONDING #( lst_filter_select_options-range ) ).

      CLEAR: lst_filter_select_options.
      ENDLOOP.   "/Loop at im_filter_select_options into lst_filter.

    ENDIF. "/If im_filter_select_options is not initial.

    "/To get configurable days from filter table
    SELECT SINGLE low
      FROM /odsmfe/tb_filtr
      WHERE entitysetname EQ @lc_entityset
      AND   field         EQ @lc_field
      INTO @lv_days.

    "/Instance data will be fetched based on the configurable days
    IF lv_days IS NOT INITIAL.
      lv_days_inst = lv_days.
     DATA(date) = sy-datum.

       days = - lv_days_inst.
      actual_date = date + days.

      lrs_date-sign   = lc_i.
      lrs_date-option = lc_bt.
      lrs_date-low    = actual_date.
      lrs_date-high   = sy-datum.
      APPEND lrs_date TO lrt_date[].

    ENDIF. "/IF lv_days IS NOT INITIAL.

    "/To get all the non object forms
    SELECT formid,
           version,
           category
      FROM /odsmfe/tb_foass
      WHERE formid    IN @lrt_formid[]
      AND   version   IN @lrt_version[]
      AND   category  EQ @lc_nonobject
      INTO TABLE @lit_foass.

    IF lit_foass[] IS NOT INITIAL.
      "/To get all the non object forms instances
      SELECT mandt,
             instanceid,
             formid,
             version,
             wo_num,
             vornr,
             plnty,
             plnnr,
             plnal,
             zaehl,
             equnr,
             tplnr,
             responsedata,
             formhtml,
             formmodel,
             roleid,
             created_on,
             created_by,
             modified_on,
             modified_by,
             nonobjtype,
             isdraft,
             created_date,
             created_time,
             modified_date,
             modified_time,
             submitted,
             multiple_sub,
             counter,
             remarks,
             deleted,
             order_type
        FROM /odsmfe/tb_forsp
        FOR ALL ENTRIES IN @lit_foass
        WHERE nonobjtype   EQ @abap_true
        AND   instanceid   IN @lrt_instanceid[]
        AND   formid       EQ @lit_foass-formid
        AND   version      EQ @lit_foass-version
        AND   created_date IN @lrt_date[]
         INTO TABLE @lit_forsp.

      IF sy-subrc EQ 0 AND lit_forsp[] IS NOT INITIAL.

       LOOP AT lit_forsp INTO DATA(lst_forsp).

       DATA(lr_rfc) = NEW /odsmfe/cl_get_ent_super_bapi( im_entity_name ).

       lr_rfc->get_cloud_dest(
         IMPORTING
           ex_dest = DATA(lv_rfc)
       ).

      DATA(lv_top)     = im_request->get_paging( )->get_page_size( ).
      DATA(lv_skip)    = im_request->get_paging( )->get_offset( ).

      lit_fields = VALUE #( ( fieldname = 'BNAME' )
                                        ( fieldname = 'NAME_TEXTC' ) ).

      lit_options = VALUE #( ( text = |BNAME| & | | & |EQ| & | | & |'| & |{ lst_forsp-created_by }| & |'|  ) ) .

      CALL FUNCTION 'RFC_READ_TABLE'
            DESTINATION lv_rfc
            EXPORTING
              query_table = 'USER_ADDR'
            TABLES
              options     = lit_options
              fields        = lit_fields
              data          = lit_data.

        lit_addr = VALUE #( FOR lst_data IN lit_data
                          ( bname = lst_data+0(12) )
                          ( name_textc = lst_data+12(80) ) ).

       ENDLOOP. "/ LOOP AT lit_forsp INTO DATA(lst_forsp).
       ENDIF. "/IF sy-subrc EQ 0 AND lit_forsp[] IS NOT INITIAL.

        "/Exporting all the non object form response data into final internal table
*        lit_myformsresponse =
*           VALUE #( FOR lst_frsp IN lit_forsp
*                       ( InstanceID = lst_frsp-instanceid
*                         FormID      = lst_frsp-formid
*                         Version     = lst_frsp-version
*                         WoNum    = lst_frsp-wo_num
*                         OperationNum = lst_frsp-vornr
*                         TaskListType = lst_frsp-plnty
*                         Groups = lst_frsp-plnnr
*                         GroupCounter = lst_frsp-plnal
*                         InternalCounter = lst_frsp-zaehl
*                         Equipment = lst_frsp-equnr
*                         FunctionLocation = lst_frsp-tplnr
*                          ResponseData = lst_frsp-responsedata
*                          CreatedOn = lst_frsp-created_on
*                          CreatedBy = lst_frsp-created_by
*                          ModifiedOn = lst_frsp-modified_on
*                          ModifiedBy = lst_frsp-modified_by
*                          NonObjType = lst_frsp-nonobjtype
*                          IsDraft = lst_frsp-isdraft
*                          Counter = lst_frsp-counter
*                          Remarks = lst_frsp-remarks
*                          Deleted = lst_frsp-deleted
*                          OrderType = lst_frsp-order_type
*                           CreatedByName =  lit_addr[ bname = lst_frsp-created_by ]-name_textc
*                           ) ).

      LOOP AT lit_forsp INTO lst_forsp.
            lst_myformsresponse-InstanceID = lst_forsp-instanceid.
            lst_myformsresponse-FormID  = lst_forsp-formid.
            lst_myformsresponse-Version = lst_forsp-version.
            lst_myformsresponse-WoNum = lst_forsp-wo_num.
            lst_myformsresponse-OperationNum = lst_forsp-vornr.
            lst_myformsresponse-TaskListType = lst_forsp-plnty.
            lst_myformsresponse-Groups = lst_forsp-plnnr.
            lst_myformsresponse-GroupCounter = lst_forsp-plnal.
            lst_myformsresponse-InternalCounter = lst_forsp-zaehl.
            lst_myformsresponse-Equipment = lst_forsp-equnr.
            lst_myformsresponse-FunctionLocation = lst_forsp-tplnr.
            lst_myformsresponse-ResponseData = lst_forsp-responsedata.
            lst_myformsresponse-CreatedOn = lst_forsp-created_on.
            lst_myformsresponse-CreatedBy = lst_forsp-created_by.
            lst_myformsresponse-ModifiedOn = lst_forsp-modified_on.
            lst_myformsresponse-ModifiedBy = lst_forsp-modified_by.
            lst_myformsresponse-NonObjType = lst_forsp-nonobjtype.
            lst_myformsresponse-IsDraft = lst_forsp-isdraft.
            lst_myformsresponse-Counter = lst_forsp-counter.
            lst_myformsresponse-Remarks = lst_forsp-remarks.
            lst_myformsresponse-Deleted = lst_forsp-deleted.
            lst_myformsresponse-OrderType = lst_forsp-order_type.
            READ  TABLE lit_addr INTO DATA(lst_addr) WITH KEY bname = lst_forsp-created_by.
            lst_myformsresponse-CreatedByName = lst_addr-name_textc.
            APPEND lst_myformsresponse TO lit_myformsresponse.
            clear : lst_myformsresponse.

      ENDLOOP.

       "/Mapping properties from the backend to the Gateway output response table
       ex_response_data = CORRESPONDING #( lit_myformsresponse ).


    ENDIF. "/If lit_foass[] is not initial.

  ENDMETHOD.
ENDCLASS.
