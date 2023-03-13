class /ODSMFE/CL_FORMMETADATA definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  type-pools ABAP .

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_FORMMETADATA .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_FORMMETADATA .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_CREATE_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_FORMMETADATA IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_create_entityset.
***********************************************************************
********************** CREATED HISTORY ********************************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 11/07/2020
* Transport No.          : ES1K902140
* Program Description    : Creates forms data from UI5
***********************************************************************
********************** CHANGE HISTORY *********************************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------
    TYPES: BEGIN OF ltys_formmetadata,
             instanceid   TYPE c LENGTH 50,
             formid       TYPE c LENGTH 30,
             version      TYPE c LENGTH 3,
             wo_num       TYPE c LENGTH 12,
             vornr        TYPE c LENGTH 4,
             responsedata TYPE string,
             roleid       TYPE /odsmfe/de_roleid, "++ES1K902140
             created_on   TYPE timestamp,
             created_by   TYPE c LENGTH 50,
             nonobjtype   TYPE /odsmfe/tb_forsp-nonobjtype,
             isdraft      TYPE c LENGTH 1,
           END OF ltys_formmetadata.

    DATA: lst_response     TYPE /odsmfe/tb_forsp,
          lst_formmetadata TYPE ltys_formmetadata,
          lit_return       TYPE STANDARD TABLE OF bapiret2,
          lit_return1      TYPE STANDARD TABLE OF bapiret2.

    " Variables
    DATA: lv_cdate      TYPE sy-datlo,
          lv_ctime      TYPE sy-timlo,
          lv_instanceid TYPE char50,
          lv_occur      TYPE /odsmfe/tb_foass-occur.

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    im_data_provider->read_entry_data( IMPORTING es_data = lst_formmetadata ).
    IF lst_formmetadata IS NOT INITIAL.
      lst_response-instanceid = lst_formmetadata-instanceid.
      lst_response-formid     = lst_formmetadata-formid.
      lst_response-version    = lst_formmetadata-version.
      TRANSLATE lst_formmetadata-responsedata TO UPPER CASE.
      lst_response-responsedata = lst_formmetadata-responsedata.
      lst_response-roleid = lst_formmetadata-roleid. "++ES1K902140
      lst_response-wo_num       = lst_formmetadata-wo_num.
      lst_response-vornr        = lst_formmetadata-vornr.
      lst_response-created_on  = lst_formmetadata-created_on.
      TRANSLATE lst_formmetadata-created_by TO UPPER CASE.
      lst_response-created_by  = sy-uname.
      lst_response-isdraft  = lst_formmetadata-isdraft.

      IF lst_formmetadata-created_on IS NOT INITIAL.
* Converting timestamp into date and time
        CALL FUNCTION 'ABI_TIMESTAMP_CONVERT_FROM'
          EXPORTING
            iv_timestamp     = lst_formmetadata-created_on
          IMPORTING
            o_date           = lv_cdate
            o_time           = lv_ctime
          EXCEPTIONS
            conversion_error = 1
            OTHERS           = 2.
        IF sy-subrc = 0.
          lst_response-created_date = lv_cdate.
          lst_response-created_time = lv_ctime.
        ENDIF.

      ENDIF.
      IF lst_formmetadata-instanceid IS NOT INITIAL.
*  Insert / update logic for Isdraft functionality
        SELECT SINGLE instanceid
        FROM /odsmfe/tb_forsp
        INTO lv_instanceid
        WHERE instanceid = lst_formmetadata-instanceid.

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
      ENDIF.
* Send the instance ID to Front end application
      gstib_entity-instanceid = lst_response-instanceid.
      IF lst_response-formid IS NOT INITIAL AND lst_response-version IS NOT INITIAL.

        SELECT SINGLE occur
        FROM /odsmfe/tb_foass
        INTO lv_occur
        WHERE formid = lst_response-formid
        AND version = lst_response-version.
        IF lv_occur IS NOT INITIAL.
          gstib_entity-occur = lv_occur.
        ENDIF.
      ENDIF.
      GET REFERENCE OF gstib_entity INTO ex_entity.
    ENDIF.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Creation Date          : 24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Displays form metatada for forms based on instanceId/formId
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   : SKAMMARI
* Change Date            : 09/09/2020
* Transport No.          : ES1K902140
* Change Description     : Added logic to handle create screnario from Ui5
***********************************************************************
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------
    TYPES: BEGIN OF ltys_formmast,
             formid    TYPE /odsmfe/de_formid,                         "ODS Form ID
             version   TYPE /odsmfe/de_version,                        "ODS Version
             formhtml  TYPE /odsmfe/de_formhtml,                       "ODSMFE FORMHTML
             formmodel TYPE /odsmfe/de_formmodel,                      "ODSMFE FormModel
             roleid    TYPE /odsmfe/de_roleid,                         "++ES1K902140
           END OF ltys_formmast.

    TYPES: BEGIN OF ltys_formass,
             formid     TYPE /odsmfe/de_formid,                           "ODS Form ID
             version    TYPE /odsmfe/de_version,                          "ODS Version
             ordertype  TYPE aufart,
             mutiplesub TYPE /odsmfe/de_multiplesub,
             occur      TYPE /odsmfe/de_occur,
           END   OF ltys_formass.
    TYPES: BEGIN OF ltys_form_mast,
             formid    TYPE /odsmfe/de_formid,                         "ODS Form ID
             version   TYPE /odsmfe/de_version,                        "ODS Version
             formhtml  TYPE /odsmfe/de_formhtml,                       "ODSMFE FORMHTML
             formmodel TYPE /odsmfe/de_formmodel,                      "ODSMFE FormModel
           END OF ltys_form_mast.
    DATA: lst_formset_get_entityset     TYPE LINE OF /odsmfe/cl_pr_formui_mpc=>tt_responsecapture,
          lst_formmetaset_get_entityset TYPE LINE OF /odsmfe/cl_pr_formui_mpc=>tt_formmetadata,   " YSINDHU - ES1K902140
          lv_source_entity_set_name     TYPE string,                       "
          lv_instid                     TYPE /odsmfe/de_instanceid.        "ODS MFE InstanceId

* SOC by YSINDHU - ES1K902140
    DATA : lv_formid    TYPE /odsmfe/de_formid,
           lv_version   TYPE /odsmfe/de_version,
           lv_lines(4)  TYPE c,
           lv_wonum     TYPE aufnr,
           lv_ordertype TYPE aufart,   "++ES1K902140
           lv_index     TYPE sy-index. "++ES1K902140

    DATA : lst_key_tab     TYPE /iwbep/s_mgw_name_value_pair,
           lit_wo_resp_cap TYPE TABLE OF /odsmfe/tb_forsp,
           lst_response    TYPE /odsmfe/tb_forsp.
* EOC by YSINDHU - ES1K902140

    DATA: lit_form_ass           TYPE STANDARD TABLE OF ltys_formass,         "Table to Capture Response
          lst_form_ass           TYPE ltys_formass,
          lit_resp_cap           TYPE TABLE OF /odsmfe/tb_forsp,                 "Table to Capture Response
          lit_response           TYPE TABLE OF /odsmfe/tb_forsp,                 "Table to Capture Response
          lst_respdata           TYPE /odsmfe/tb_forsp,                          "Table to Capture Response
          lit_form_master        TYPE STANDARD TABLE OF ltys_form_mast,       "
          lit_form_master_create TYPE STANDARD TABLE OF ltys_form_mast,
          lit_formmast           TYPE STANDARD TABLE OF ltys_formmast,        "
          lst_formmast           TYPE ltys_formmast.                          "

    FIELD-SYMBOLS : <lfsst_form_master> TYPE  ltys_form_mast.

    CONSTANTS: lc_ResponseCaptureSet TYPE string VALUE 'ResponseCaptureSet',
               lc_FormMetaDataSet    TYPE string VALUE 'FormMetaDataSet',
               lc_FormId             TYPE string VALUE 'FormId',
               lc_Version            TYPE string VALUE 'Version',
               lc_WorkOrderSet       TYPE string VALUE 'WorkOrderSet',
               lc_WorkOrderNumber    TYPE string VALUE 'WorkOrderNumber',
               lc_OrderType          TYPE string VALUE 'OrderType'.

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

* Maps key fields to function module parameters
    IF im_key_tab IS NOT INITIAL.

* SOC by YSINDHU - ES1K902140
      IF im_tech_request_context_entity IS NOT INITIAL.
        lv_source_entity_set_name     = im_tech_request_context_entity->get_source_entity_set_name( ).
      ELSEIF im_tech_request_context IS NOT INITIAL.
        lv_source_entity_set_name     = im_tech_request_context->get_source_entity_set_name( ).
      ENDIF.
      " In case of navigation from ResponseCaptureSet , get data based on InstanceId
      IF  lv_source_entity_set_name = lc_ResponseCaptureSet.

        IF im_tech_request_context_entity IS NOT INITIAL.
* Convert keys to appropriate entity set structure
          im_tech_request_context_entity->get_converted_source_keys(
                                          IMPORTING es_key_values  = lst_formset_get_entityset ).
        ELSE.
          im_tech_request_context->get_converted_source_keys(
                                          IMPORTING es_key_values  = lst_formset_get_entityset ).
        ENDIF.

        lv_instid = lst_formset_get_entityset-instanceid.
        IF lv_instid IS NOT INITIAL.
* Getting responce data based on instance id
          SELECT * FROM /odsmfe/tb_forsp
                   INTO TABLE lit_resp_cap
                   WHERE instanceid EQ lv_instid .
          IF sy-subrc = 0.
            SORT lit_resp_cap BY instanceid.
          ENDIF.
        ENDIF.
        " In case of running independent FormMetadtaSet, Fetch data based on FormId and Version ++SKAMMARI ES1K902140
      ELSEIF  lv_source_entity_set_name IS INITIAL AND im_entity_set_name = lc_FormMetaDataSet.
        READ TABLE im_key_tab INTO lst_key_tab WITH KEY name = lc_FormId.
        IF sy-subrc IS INITIAL.
          lv_formid = lst_key_tab-value.
        ENDIF.

        READ TABLE im_key_tab INTO lst_key_tab WITH KEY name = lc_Version.
        IF sy-subrc IS INITIAL.
          lv_version = lst_key_tab-value.
        ENDIF.
*        EOC ++SKAMMARI ES1K902140
* Fetching master data for captured form based on form id and version.
        SELECT formid version formhtml formmodel
        FROM /odsmfe/tb_fomst
        INTO TABLE lit_form_master_create
        UP TO 1 ROWS
        WHERE formid = lv_formid
        AND version = lv_version.  " ##SELECT_FAE_WITH_LOB[FORMHTML]
        IF sy-subrc EQ 0.
          SORT lit_form_master_create BY formid.
        ENDIF.
* filling final table
        LOOP AT lit_form_master_create ASSIGNING <lfsst_form_master>.

          MOVE-CORRESPONDING <lfsst_form_master> TO gstib_entity.
          MOVE <lfsst_form_master>-formid   TO gstib_entity-formid.
          MOVE <lfsst_form_master>-version  TO gstib_entity-version.
          MOVE <lfsst_form_master>-formhtml TO gstib_entity-formhtml.
          MOVE <lfsst_form_master>-formmodel TO gstib_entity-formmodel.
        ENDLOOP.
        IF gstib_entity IS NOT INITIAL.
          GET REFERENCE OF gstib_entity INTO ex_entity.
        ENDIF.
        IF ex_entity IS NOT INITIAL.
          RETURN.
        ENDIF.

*     " EOC ++SKAMMARI ES1K902140
        " In case of Navigation from Workoderset service
      ELSEIF lv_source_entity_set_name = lc_WorkOrderSet
         AND im_entity_set_name = lc_FormMetaDataSet.

        READ TABLE im_key_tab INTO lst_key_tab WITH KEY name = lc_WorkOrderNumber.
        IF sy-subrc IS INITIAL.
          lv_wonum = lst_key_tab-value.
        ENDIF.

        READ TABLE im_key_tab INTO lst_key_tab WITH KEY name = lc_OrderType.
        IF sy-subrc IS INITIAL.
          lv_ordertype = lst_key_tab-value.
        ENDIF.
* Getting responce data based on work order num
        IF lv_wonum IS NOT INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_wonum
            IMPORTING
              output = lv_wonum.

          CLEAR: lit_wo_resp_cap.
          SELECT * FROM /odsmfe/tb_forsp
                   INTO TABLE lit_wo_resp_cap
                   WHERE instanceid NE space
                        AND wo_num = lv_wonum.

          IF sy-subrc = 0.
            SORT lit_wo_resp_cap BY instanceid.
          ENDIF.
        ENDIF.
*     " EOC SKAMMARI ES1K902140
      ENDIF.
    ENDIF.
* EOC by YSINDHU - ES1K902140

    IF lit_resp_cap IS NOT INITIAL.
      CLEAR : lit_form_master.

* Fetching master data for captured form based on form id and version.
      SELECT formid version formhtml formmodel
             FROM /odsmfe/tb_fomst
             INTO TABLE lit_form_master
               UP TO 1 ROWS
               FOR ALL ENTRIES IN lit_resp_cap
               WHERE formid = lit_resp_cap-formid
               AND version = lit_resp_cap-version  .  " ##SELECT_FAE_WITH_LOB[FORMHTML] 701-nshyamala.
      IF sy-subrc <> 0.
        CLEAR:lit_form_master.
      ENDIF.
* filling final table
      LOOP AT lit_form_master ASSIGNING <lfsst_form_master>.

        MOVE-CORRESPONDING <lfsst_form_master> TO gstib_entity.
        MOVE <lfsst_form_master>-formid   TO gstib_entity-formid.
        MOVE <lfsst_form_master>-version  TO gstib_entity-version.
        MOVE <lfsst_form_master>-formhtml TO gstib_entity-formhtml.
        MOVE <lfsst_form_master>-formmodel TO gstib_entity-formmodel.
        IF lv_source_entity_set_name IS INITIAL AND im_entity_set_name = lc_FormMetaDataSet.
          CLEAR gstib_entity-responsedata.
        ELSE.
          SORT lit_resp_cap by formid.
          READ TABLE lit_resp_cap INTO lst_respdata WITH KEY formid = <lfsst_form_master>-formid BINARY SEARCH.
          IF sy-subrc EQ 0.
            gstib_entity-responsedata = lst_respdata-responsedata.
          ENDIF.
        ENDIF.
        IF lv_source_entity_set_name = lc_ResponseCaptureSet. "++SKAMMARI
          gstib_entity-instanceid = lv_instid.
        ENDIF.                                              "++SKAMMARI
      ENDLOOP.                                                         " LOOP AT LIT_FORM_MASTER

      IF gstib_entity IS NOT INITIAL.
        GET REFERENCE OF gstib_entity INTO ex_entity.
      ENDIF.                                                           " IF GSTIB_ENTITY IS NOT INITIAL

    ELSE.                                                              " IF SY-SUBRC EQ 0 AND LIT_RESP_CAP IS NOT INITIAL
*  Fetching form Assignment data
      IF lv_ordertype IS NOT INITIAL.                       "++SKAMMARI
        SELECT formid version ordertype multiplesub occur
              FROM /odsmfe/tb_foass                     "#EC CI_NOWHERE
              INTO TABLE lit_form_ass
              WHERE ordertype EQ lv_ordertype
              AND active NE space.
        IF sy-subrc <> 0.
          CLEAR : lit_form_ass.
        ENDIF.
      ELSE.                                                 "++SKAMMARI
        SELECT formid version ordertype multiplesub occur
               FROM /odsmfe/tb_foass                    "#EC CI_NOWHERE
               INTO TABLE lit_form_ass
               WHERE active NE space.
        IF sy-subrc <> 0.
          CLEAR lit_form_ass.
        ENDIF.
      ENDIF.

* Fetching master data for captured form based on form id and version.
      IF lit_form_ass IS NOT INITIAL.
        CLEAR: lit_formmast.
        SELECT formid version formhtml formmodel
               FROM /odsmfe/tb_fomst
               INTO TABLE lit_formmast
               FOR ALL ENTRIES IN lit_form_ass
               WHERE formid = lit_form_ass-formid
               AND version = lit_form_ass-version . " ##SELECT_FAE_WITH_LOB[FORMMODEL]-701-nshyamala.
        IF sy-subrc NE 0.
          CLEAR lit_formmast.
        ENDIF.
        IF lit_wo_resp_cap IS INITIAL.                      "++SKAMMARI ES1K902140
          SELECT formid version FROM /odsmfe/tb_forsp
          INTO CORRESPONDING FIELDS OF TABLE lit_resp_cap
          FOR ALL ENTRIES IN lit_form_ass
          WHERE instanceid NE space
            AND formid = lit_form_ass-formid
            AND version = lit_form_ass-version.
          IF sy-subrc <> 0.
            CLEAR lit_resp_cap.
          ENDIF.
        ELSE.
          CLEAR: lit_resp_cap.
          lit_resp_cap[] = lit_wo_resp_cap[].
          IF sy-subrc = 0.
            SORT lit_resp_cap BY formid version.
          ENDIF.
        ENDIF.

      ENDIF.                                                           " IF LIT_FORM_ASS IS NOT INITIAL
      IF lit_formmast IS NOT INITIAL.
* Filling final internal table
        LOOP AT lit_formmast INTO lst_formmast .
          gstib_entity-formid     = lst_formmast-formid.
          gstib_entity-version    = lst_formmast-version.
          IF lv_source_entity_set_name NE lc_WorkOrderSet.
            gstib_entity-formhtml   = lst_formmast-formhtml.
            gstib_entity-formmodel  = lst_formmast-formmodel.
          ENDIF.
* Fetching no.of submitted forms
          lit_response[] = lit_resp_cap[].

          DELETE lit_response WHERE formid NE lst_formmast-formid.
          IF lit_response IS NOT INITIAL.
            DELETE lit_response WHERE version NE lst_formmast-version.
          ENDIF.

          DELETE lit_response WHERE roleid NE lst_formmast-roleid.
          CLEAR lv_lines.
          DESCRIBE TABLE lit_response LINES lv_lines.
          gstib_entity-submitted = lv_lines.
* fetching WO number
          gstib_entity-wo_num = lv_wonum. "++ SKAMMARI ES1K902140
* Fetching the multple submission flag , Order type
          CLEAR lst_form_ass.
          SORT lit_form_ass BY formid version.
          READ TABLE lit_form_ass INTO lst_form_ass
          WITH KEY formid = lst_formmast-formid
          version = lst_formmast-version BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            gstib_entity-multiple_sub = lst_form_ass-mutiplesub.
            gstib_entity-ordertype = lst_form_ass-ordertype.
            gstib_entity-occur = lst_form_ass-occur.
          ENDIF.

          APPEND gstib_entity TO gitib_entity.
          CLEAR: gstib_entity.
        ENDLOOP.                                                       " LOOP AT LIT_FORMMAST

        IF gitib_entity IS NOT INITIAL.
          GET REFERENCE OF gitib_entity INTO ex_entityset.
        ENDIF.                                                         " IF GITIB_ENTITY IS NOT INITIAL

      ENDIF.                                                           " IF LIT_FORMMAST IS NOT INITIAL
    ENDIF.                                                             " IF SY-SUBRC EQ 0 AND LIT_RESP_CAP IS NOT INITIAL

  ENDMETHOD.
ENDCLASS.
