class /ODSMFE/CL_INSTANCE_METADATA definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  type-pools ABAP .

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC_EXT=>TS_INSTANCEMETADATA .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC_EXT=>TT_INSTANCEMETADATA .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_INSTANCE_METADATA IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS - PIYYAPPAN
* Creation Date          : 10/04/2023
* Transport No.          : ES1K903465
* Program Description    : To display the Instance Metadata
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
    TYPES: BEGIN OF ltys_forsp,
             instanceid  TYPE /odsmfe/tb_forsp-instanceid,                      "InstanceID
             formid      TYPE /odsmfe/tb_forsp-formid,                          "FormID
             version     TYPE /odsmfe/tb_forsp-version,                         "Version
             wonum       TYPE aufnr,                                            "WorkOrderNumber
             vornr       TYPE vornr,                                            "OperationNumber
             createddate TYPE dats,                                             "CreatedDate
             createdby   TYPE /odsmfe/tb_forsp-created_by,                      "CreatedBy
           END OF ltys_forsp,

           BEGIN OF ltys_afih,
             wonum TYPE aufnr,                                                  "WorkOrderNumber
             equnr TYPE equnr,                                                  "EquipmentNumber
             iloan TYPE iloan,                                                  "Location/Account assignment
           END OF ltys_afih,

           BEGIN OF ltys_iloa,
             iloan TYPE iloan,                                                  "Location/Account assignment
             tplnr TYPE tplnr,                                                  "Functional Location
           END OF ltys_iloa.

    "/ Tables and Structures
    DATA: lit_forsp TYPE TABLE OF ltys_forsp,                                   "Types structure for form response
          lst_forsp TYPE ltys_forsp,                                            "Types structure for form resposne
          lit_afih  TYPE TABLE OF ltys_afih,                                    "Types structure for Maintenance order header
          lst_afih  TYPE ltys_afih,                                             "Types structure for Maintenance order header
          lit_iloa  TYPE TABLE OF ltys_iloa,                                    "Types structure for PM Object Location and Account Assignment
          lst_iloa  TYPE ltys_iloa,                                             "Types structure for PM Object Location and Account Assignment
          lit_final TYPE TABLE OF /odsmfe/st_instance_metadata,                 "Structure for Instance Metadata
          lst_final TYPE /odsmfe/st_instance_metadata.                          "Structure for Instance Metadata

    DATA: lst_filter       TYPE /iwbep/s_mgw_select_option,                     "MGW Framework: Selection Option Parameters for db selects
          lst_filter_range TYPE /iwbep/s_cod_select_option.                     "MGW Framework: Select Options for Queries

    "/Range Tables and Range Structures
    DATA: lrs_core_range_str TYPE /odsmwh/st_core_range_str,                    "Filter Purpose Range Structure
          lrs_filter_range   TYPE /iwbep/s_cod_select_option,                   "MGW Framework: Select Options for Queries
          lrt_filter_range   TYPE /odsmwh/tt_core_range_tab,                    "Filter range table
          lst_key_tab        TYPE /iwbep/s_mgw_name_value_pair.                 "Name value pair for mgw

    "/ Variables
    DATA : lrs_it_instanceid TYPE /odsmfe/st_core_range_str,                    "InstanceID Range Structure
           lrt_instanceid    TYPE TABLE OF /odsmfe/st_core_range_str,           "InstanceID Range Table
           lrs_it_formid     TYPE /odsmfe/st_core_range_str,                    "FormID Range Structure
           lrt_formid        TYPE TABLE OF /odsmfe/st_core_range_str,           "FormID Range Table
           lrs_it_version    TYPE /odsmfe/st_core_range_str,                    "FormID Range Structure
           lrt_version       TYPE TABLE OF /odsmfe/st_core_range_str,           "FormID Range Table
           lrt_date          TYPE TABLE OF /odsmfe/st_core_range_str,           "Date Range Structure
           lrs_it_date       TYPE /odsmfe/st_core_range_str,                    "Date Range Structure
           lv_startdate      TYPE dats,
           lv_enddate        TYPE dats.

    "/Constants
    CONSTANTS: lc_i                 TYPE string     VALUE 'I',                     "Single-Character Indicator
               lc_bt                TYPE string     VALUE 'BT',                    "Version Number Component
               lc_eq                TYPE string     VALUE 'EQ',                    "Version Number Component
               lc_startdate         TYPE string     VALUE 'STARTDATE',             "StartDate
               lc_enddate           TYPE string     VALUE 'ENDDATE',               "EndDate
               lc_key_instanceid    TYPE string     VALUE 'InstanceId',            "Key Values - InstanceID
               lc_filtr_instanceid  TYPE string     VALUE 'INSTANCEID',            "Filter Values - InstanceID
               lc_formid            TYPE string     VALUE 'FORMID',                "FormId
               lc_version           TYPE string     VALUE 'VERSION'.               "Version

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

    "/ Read filter Property Values
    IF im_filter_select_options[] IS NOT INITIAL.

      LOOP AT im_filter_select_options INTO lst_filter.
        TRANSLATE lst_filter-property TO UPPER CASE.
        CASE lst_filter-property.

          WHEN lc_startdate.
            READ TABLE lst_filter-select_options INTO lrs_filter_range INDEX 1.
            lv_startdate = lrs_filter_range-low.

          WHEN lc_enddate.
            READ TABLE lst_filter-select_options INTO lrs_filter_range INDEX 1.
            lv_enddate = lrs_filter_range-low.

          WHEN lc_filtr_instanceid.
            READ TABLE lst_filter-select_options INTO lrs_filter_range INDEX 1.
            lrs_it_instanceid-sign   = lc_i.
            lrs_it_instanceid-option = lc_eq.
            lrs_it_instanceid-low    = lrs_filter_range-low.
            APPEND lrs_it_instanceid to lrt_instanceid.
            CLEAR: lrs_it_instanceid.

          WHEN lc_formid.
            READ TABLE lst_filter-select_options INTO lrs_filter_range INDEX 1.
            lrs_it_formid-sign    = lc_i.
            lrs_it_formid-option  = lc_eq.
            lrs_it_formid-low     = lrs_filter_range-low.
            APPEND lrs_it_formid to lrt_formid.
            CLEAR: lrs_it_formid.

          WHEN lc_version.
            READ TABLE lst_filter-select_options INTO lrs_filter_range INDEX 1.
            lrs_it_version-sign    = lc_i.
            lrs_it_version-option  = lc_eq.
            lrs_it_version-low     = lrs_filter_range-low.
            APPEND lrs_it_version to lrt_version.
            CLEAR: lrs_it_version.

        ENDCASE. "/ Case lst_filter-property.
      ENDLOOP. "/Loop at im_filter_select_options into lst_filter.

      IF lv_startdate IS NOT INITIAL AND lv_enddate IS NOT INITIAL.
        "/Filling the range table
        lrs_it_date-sign    = lc_i.
        lrs_it_date-option  = lc_bt.
        lrs_it_date-low     = lv_startdate.
        lrs_it_date-high    = lv_enddate.
        APPEND lrs_it_date TO lrt_date.
        CLEAR: lrs_it_date.
      ENDIF. "/If lv_startdate and lv_enddate is not initial.

    ENDIF. "/If im_filter_select_options[] is not initial.

    IF im_key_tab[] IS NOT INITIAL.

      LOOP AT im_key_tab INTO lst_key_tab.
        CASE lst_key_tab-name.
          WHEN lc_key_instanceid.
            lrs_it_instanceid-sign    = lc_i.
            lrs_it_instanceid-option  = lc_eq.
            lrs_it_instanceid-low     = lst_key_tab-value.
            APPEND lrs_it_instanceid TO lrt_instanceid.
            CLEAR: lrs_it_instanceid.
        ENDCASE. "/Case lst_key_tab-name.
      ENDLOOP. "/Loop at im_key_tab into lst_key_tab.

    ENDIF. "/ If im_key_tab[] is not initial.

    "/To get the data from Response table based on startdate, enddate and instanceid
    SELECT instanceid
         formid
         version
         wo_num
         vornr
         created_date
         created_by
    FROM /odsmfe/tb_forsp
    INTO TABLE lit_forsp
    WHERE created_date IN lrt_date[]
    AND   instanceid   IN lrt_instanceid[]
    AND   formid       IN lrt_formid[]
    AND   version      IN lrt_version[].

    IF lit_forsp[] IS NOT INITIAL.
      "/To get equipment number, Location/Account assignment based on workorder number
      SELECT aufnr
             equnr
             iloan
        FROM afih
        INTO TABLE lit_afih
        FOR ALL ENTRIES IN lit_forsp
        WHERE aufnr = lit_forsp-wonum.

      IF lit_afih[] IS NOT INITIAL.
        "/To get functional location based on Location/Account assignment
        SELECT iloan
               tplnr
          FROM iloa
          INTO TABLE lit_iloa
          FOR ALL ENTRIES IN lit_afih
          WHERE iloan = lit_afih-iloan.
      ENDIF. "/If lst_afih is not initial.
    ENDIF. "/If lit_forsp[] is not initial.

    IF sy-subrc EQ 0.
      "/Exporting all the values into final internal table
      LOOP AT lit_forsp INTO lst_forsp.
        lst_final-instanceid      = lst_forsp-instanceid.
        lst_final-formid          = lst_forsp-formid.
        lst_final-version         = lst_forsp-version.
        lst_final-wo_num          = lst_forsp-wonum.
        lst_final-vornr           = lst_forsp-vornr.
        lst_final-created_date    = lst_forsp-createddate.
        lst_final-start_date      = lv_startdate.
        lst_final-end_date        = lv_enddate.
        lst_final-created_by      = lst_forsp-createdby.
        READ TABLE lit_afih INTO lst_afih WITH KEY wonum = lst_forsp-wonum.
        lst_final-equnr           = lst_afih-equnr.
        READ TABLE lit_iloa INTO lst_iloa WITH KEY iloan = lst_afih-iloan.
        lst_final-tplnr           = lst_iloa-tplnr.
        APPEND lst_final TO gitib_entity.
        CLEAR: lst_final.
      ENDLOOP. "/Loop at lit_forsp into lst_forsp.
    ENDIF. "/If sy-subrc eq 0.

    "/Mapping properties from the backend to the Gateway output response table
    IF im_key_tab[] IS NOT INITIAL.
      READ TABLE gitib_entity INTO gstib_entity INDEX 1.
      GET REFERENCE OF gstib_entity INTO ex_entity.
    ELSE.
      GET REFERENCE OF gitib_entity INTO ex_entityset.
    ENDIF. "/If im_key_tab[] is not initial.

  ENDMETHOD.
ENDCLASS.
