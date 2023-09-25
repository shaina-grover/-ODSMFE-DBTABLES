class /ODSMFE/CL_INSTANCE_DATA definition
  public
  create private .

public section.
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_INSTANCE_DATA IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS - HSANGAM
* Creation Date          : 11/04/2023
* Transport No.          : ES1K903465
* Program Description    : To display all the questions and its response of an InstanceId
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

    "/ Types
    TYPES : BEGIN OF ltys_form_data,
              lang              TYPE string,
              label             TYPE string,
              pre_group_id      TYPE string,
              group_id          TYPE string,
              repeat_group_id   TYPE string,
              question_id       TYPE string,
              question_index    TYPE string,
              pre_group_name    TYPE string,
              group_name        TYPE string,
              repeat_group_name TYPE string,
              question          TYPE string,
            END OF ltys_form_data .

    TYPES : BEGIN OF ltys_response_data,
              formid         TYPE /odsmfe/de_formid,
              version        TYPE /odsmfe/de_version,
              instanceid     TYPE /odsmfe/de_instanceid,
              wo_num         TYPE aufnr,
              vornr          TYPE vornr,
              equnr          TYPE equnr,
              tplnr          TYPE tplnr,
              created_date   TYPE dats,
              created_by     TYPE /odsmfe/de_createdby,
              group          TYPE string,
              repeat_group   TYPE string,
              sub_group      TYPE string,
              question_id    TYPE string,
              question_index TYPE string,
              questionname   TYPE string,
              responsedata   TYPE string,
            END OF ltys_response_data.

    "/ Tables and structures
    DATA : lit_questions TYPE TABLE OF ltys_form_data,                                           "internal table for ltys_form_data
           lst_questions TYPE ltys_form_data,                                                    "structure for ltys_form_data
           lit_response  TYPE TABLE OF ltys_response_data,                                       "internal table for ltys_response_data
           lst_response  TYPE ltys_response_data,                                                "structure for ltys_response_data
           lit_forsp1    TYPE TABLE OF ltys_response_data,                                       "internal table for ltys_response_data
           lst_forsp1    TYPE ltys_response_data,                                                "structure for ltys_response_data
           lit_forsp2    TYPE TABLE OF ltys_response_data,                                       "internal table for ltys_response_data
           lst_forsp2    TYPE ltys_response_data,                                                "structure for ltys_response_data
           lit_forsp3    TYPE TABLE OF ltys_response_data,                                       "internal table for ltys_response_data
           lst_forsp3    TYPE ltys_response_data,                                                "structure for ltys_response_data
           lit_fomst     TYPE TABLE OF /odsmfe/tb_fomst,                                         "internal table for /odsmfe/tb_fomst
           lst_fomst     TYPE /odsmfe/tb_fomst,                                                  "structure for /odsmfe/tb_fomst
           lit_forsp     TYPE TABLE OF /odsmfe/tb_forsp,                                         "internal table for /odsmfe/tb_forsp
           lst_forsp     TYPE /odsmfe/tb_forsp,                                                  "structure for /odsmfe/tb_forsp
           lo_ref        TYPE REF TO /odsmfe/cl_form_data_parse.                                 "class reference for /odsmfe/cl_form_data_parse

    "/ Range tables and structures
    DATA : lst_key_tab               TYPE /iwbep/s_mgw_name_value_pair,                          "structure for name value pair for mgw
           lrs_it_formguid           TYPE /odsmfe/st_core_range_str,                             "structure for ODS MFE: Filter Purpose Range Structure
           lrt_instanceid            TYPE TABLE OF /odsmfe/st_core_range_str,                    "internal table for ODS MFE: Filter Purpose Range Structure
           lrs_it_instanceid         TYPE /odsmfe/st_core_range_str,                             "structure for ODS MFE: Filter Purpose Range Structure
           lrt_groupname             TYPE TABLE OF /odsmfe/st_core_range_str,                    "internal table for ODS MFE: Filter Purpose Range Structure
           lrs_it_groupname          TYPE /odsmfe/st_core_range_str,                             "structure for ODS MFE: Filter Purpose Range Structure
           lrt_repeatnumber          TYPE TABLE OF /odsmfe/st_core_range_str,                    "internal table for ODS MFE: Filter Purpose Range Structure
           lrs_it_repeatnumber       TYPE /odsmfe/st_core_range_str,                             "structure for ODS MFE: Filter Purpose Range Structure
           lrt_datakey               TYPE TABLE OF /odsmfe/st_core_range_str,                    "internal table for ODS MFE: Filter Purpose Range Structure
           lrs_it_datakey            TYPE /odsmfe/st_core_range_str,                             "structure for ODS MFE: Filter Purpose Range Structure
           lst_filter_select_options TYPE /iwbep/s_mgw_select_option,                            "structure for MGW Framework: Selection Option Parameters for db selects
           lst_select_options        TYPE /iwbep/s_cod_select_option,                            "structure for MGW Framework: Select Options for Queries
           lst_inst_metadata         TYPE /odsmfe/cl_pr_formui_mpc_ext=>ts_instancemetadata.     "InstanceMetadata properties

    "/ variables
    DATA : lv_count                  TYPE i,                                                     "Count
           lv_val                    TYPE string,                                                "value string
           lv_index                  TYPE i,                                                     "index
           lv_rgroup                 TYPE string,                                                "repeat group
           lv_itr                    TYPE i,                                                     "iterator
           lv_source_entity_set_name TYPE string,                                                "Source Entityset name
           lv_instanceid             TYPE /odsmfe/de_instanceid.                                 "ODS MFE InstanceId

    "/ constants
    CONSTANTS : lc_instanceid        TYPE string VALUE 'InstanceId',                             "ODS MFE InstanceId
                lc_groupname         TYPE string VALUE 'GroupName',                              "GroupName
                lc_repeatnumber      TYPE string VALUE 'RepeatNumber',                           "RepeatNumber
                lc_datakey           TYPE string VALUE 'DataKey',                                "Datakey
                lc_i                 TYPE string VALUE 'I',                                      "Single-Character Indicator
                lc_eq                TYPE string VALUE 'EQ',                                     "Version Number Component
                lc_instance_metadata TYPE string VALUE 'InstanceMetadataSet'.                    "InstanceMetadataSet

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

*------------------------------------------------------------------------*
*                       M A I N   S E C T I O N                          *
*------------------------------------------------------------------------*
    "/ Read key values
    IF im_key_tab IS NOT INITIAL.

      lv_source_entity_set_name = im_tech_request_context->get_source_entity_set_name( ).

      IF lv_source_entity_set_name = lc_instance_metadata.
        "/Convert keys to appropriate entity set structure
        im_tech_request_context->get_converted_source_keys(
          IMPORTING
            es_key_values  = lst_inst_metadata ).

        lv_instanceid = lst_inst_metadata-instanceid.

        lrs_it_instanceid-sign = lc_i.
        lrs_it_instanceid-option = lc_eq.
        lrs_it_instanceid-low = lv_instanceid.
        APPEND lrs_it_instanceid TO lrt_instanceid.
        CLEAR lrs_it_instanceid.
      ENDIF. " IF  lv_source_entity_set_name = lc_instance_metadata.

      LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
        CASE lst_key_tab-name.
          WHEN lc_instanceid.
            lrs_it_instanceid-sign = lc_i.
            lrs_it_instanceid-option = lc_eq.
            lrs_it_instanceid-low = lst_key_tab-value.
            APPEND lrs_it_instanceid TO lrt_instanceid.
            CLEAR lrs_it_instanceid.

          WHEN lc_groupname.
            lrs_it_groupname-sign = lc_i.
            lrs_it_groupname-option = lc_eq.
            lrs_it_groupname-low = lst_key_tab-value.
            APPEND lrs_it_groupname TO lrt_groupname.
            CLEAR lrs_it_groupname.

          WHEN lc_repeatnumber.
            lrs_it_repeatnumber-sign = lc_i.
            lrs_it_repeatnumber-option = lc_eq.
            lrs_it_repeatnumber-low = lst_key_tab-value.
            APPEND lrs_it_repeatnumber TO lrt_repeatnumber.
            CLEAR lrs_it_repeatnumber.

          WHEN lc_datakey.
            lrs_it_datakey-sign = lc_i.
            lrs_it_datakey-option = lc_eq.
            lrs_it_datakey-low = lst_key_tab-value.
            APPEND lrs_it_datakey TO lrt_datakey.
            CLEAR lrs_it_datakey.
        ENDCASE."/ CASE lst_key_tab-name.
      ENDLOOP."/ LOOP AT im_key_tab INTO lst_key_tab WHERE value IS NOT INITIAL.
    ENDIF."/ IF im_key_tab IS NOT INITIAL.

    "/ Read filter values
    IF im_filter_select_options IS NOT INITIAL.
      LOOP AT im_filter_select_options INTO lst_filter_select_options.
        CASE lst_filter_select_options-property.
          WHEN lc_instanceid.
            READ TABLE lst_filter_select_options-select_options INTO lst_select_options INDEX 1.
            lrs_it_instanceid-sign = lst_select_options-sign.
            lrs_it_instanceid-option = lst_select_options-option.
            lrs_it_instanceid-low = lst_select_options-low.
            APPEND lrs_it_instanceid TO lrt_instanceid.
            CLEAR lrs_it_instanceid.
        ENDCASE."/ CASE lst_filter_select_options-property.
      ENDLOOP."/ LOOP AT im_filter_select_options INTO lst_filter_select_options.
    ENDIF."/ IF im_filter_select_options IS NOT INITIAL.

    "/ Fetching the data from Response Capture table based the filters
    SELECT SINGLE *
           FROM /odsmfe/tb_forsp
           INTO lst_forsp
           WHERE instanceid IN lrt_instanceid .

    IF lst_forsp IS NOT INITIAL.
      "create object for calss gmib_get_response_data
      CREATE OBJECT lo_ref.
      "/ calling gmib_get_response_data with ref lo_ref
      lo_ref->gmib_get_response_data(
        EXPORTING
            im_formid        =    lst_forsp-formid   " ODS Form ID
            im_version       =    lst_forsp-version   " ODS Version
            im_instanceid    =    lst_forsp-instanceid " ODS MFE InstanceId
        IMPORTING
          ex_response_data =  lit_response
      ).

      lit_forsp1[] = lit_response[].
      DELETE ADJACENT DUPLICATES FROM lit_forsp1 COMPARING sub_group.

      lv_itr = 1.
      lv_count = 1.
      LOOP AT lit_response INTO lst_response.
        gstib_entity-instanceid = lst_forsp-instanceid.
        gstib_entity-groupname = lst_response-sub_group.

        IF lst_response-sub_group IS NOT INITIAL.
          IF lst_response-repeat_group IS INITIAL.
            lv_count = 0.
            READ TABLE lit_forsp1 INTO lst_forsp1 WITH KEY sub_group = lst_response-sub_group. "read
            lv_count = lv_count + 1.
            gstib_entity-repeatnumber = lv_count.
          ENDIF."/ IF lst_response-repeat_group IS INITIAL.
        ELSE."/ IF lst_response-sub_group IS NOT INITIAL.
          gstib_entity-repeatnumber = lv_count.
        ENDIF."/ IF lst_response-sub_group IS NOT INITIAL.

        IF lst_response-repeat_group IS NOT INITIAL.

          IF lst_response-repeat_group IS NOT INITIAL AND lst_response-questionname EQ lv_val.
            lv_count = lv_count + 1.
          ELSEIF lst_response-repeat_group NE lv_rgroup."/ IF lst_response-repeat_group IS NOT INITIAL AND lst_response-questionname EQ lv_val.
            lv_count = 1.
          ENDIF."/ IF lst_response-repeat_group IS NOT INITIAL AND lst_response-questionname EQ lv_val.

          gstib_entity-repeatnumber = lv_count.
          gstib_entity-datakey = lst_response-questionname.
          gstib_entity-datavalue = lst_response-responsedata.

          IF lv_val IS INITIAL.
            lv_val = lst_response-questionname.
            lv_rgroup = lst_response-repeat_group.
          ELSEIF lst_response-repeat_group NE lv_rgroup."/ IF lv_val IS INITIAL.
            CLEAR : lv_val , lv_index.
            lv_val = lst_response-questionname.
          ENDIF."/ IF lv_val IS INITIAL.

          IF lst_response-repeat_group EQ lv_rgroup.
            APPEND gstib_entity TO gitib_entity.
          ENDIF."/ IF lst_response-repeat_group EQ lv_rgroup.
          lv_rgroup = lst_response-repeat_group.
        ELSE."/ IF lst_response-repeat_group IS NOT INITIAL.
          gstib_entity-datakey = lst_response-questionname.
          gstib_entity-datavalue = lst_response-responsedata.
          APPEND gstib_entity TO gitib_entity.
        ENDIF."/ IF lst_response-repeat_group IS NOT INITIAL.
        CLEAR : gstib_entity .
      ENDLOOP."/ LOOP AT lit_response INTO lst_response.
    ENDIF."/ IF lst_forsp IS NOT INITIAL.

    "/ Mapping properties from the backend to the Gateway output response table
    IF gitib_entity IS NOT INITIAL.
      GET REFERENCE OF gitib_entity INTO ex_entityset.
    ENDIF."/ IF gitib_entity IS NOT INITIAL.
  ENDMETHOD.
ENDCLASS.
