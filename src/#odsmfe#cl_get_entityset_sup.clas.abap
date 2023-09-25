CLASS /odsmfe/cl_get_entityset_sup DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /odsmfe/if_get_entityset .
    INTERFACES /odsmfe/if_get_entityset_main .

    CLASS-DATA gvsb_note_id_func_import_0_1(10) TYPE n.
    CLASS-DATA gvsb_note_id_cache_control(10) TYPE n.
    METHODS constructor
      IMPORTING
        !im_entity_name TYPE string
        !im_entity_set_name TYPE string OPTIONAL
        !im_data_ext_class TYPE REF TO object OPTIONAL
        !im_request               TYPE REF TO if_rap_query_request OPTIONAL      !im_service_document_name TYPE REF TO string OPTIONAL
        !im_service_version TYPE REF TO numc4 OPTIONAL
        !im_service_namespace TYPE REF TO string OPTIONAL.
  PROTECTED SECTION.

    TYPES:
      BEGIN OF gtys_select,
        select TYPE REF TO data,
      END OF gtys_select .
    TYPES:
      BEGIN OF gtys_from,
        query_count TYPE int2,
        from        TYPE string,
      END OF gtys_from .
    TYPES:
      BEGIN OF gtys_where,
        query_count TYPE int2,
        where       TYPE string,
      END OF gtys_where .

    DATA:
      gitio_fetch_config TYPE TABLE OF /odsmfe/tb_fetch .
    DATA:
      gitio_join_config TYPE TABLE OF /odsmfe/tb_joinc .
*    DATA goio_model TYPE REF TO /iwbep/cl_mgw_odata_model .
    DATA gvio_entityset_name(40) TYPE c .
    DATA gvio_entity_name(40) TYPE c ..
*    DATA goio_data_ext_class TYPE REF TO /iwbep/cl_mgw_abs_data .
*    DATA goio_context TYPE REF TO /iwbep/if_mgw_context .
    DATA goio_request_details TYPE REF TO if_rap_query_request.
    DATA goio_service_document_name TYPE REF TO string .
    DATA goio_service_version TYPE REF TO numc4 .
    DATA goio_service_namespace TYPE REF TO string .
*    DATA goio_injection TYPE REF TO /iwbep/if_sb_gen_dpc_injection .
    DATA gvio_where_clause TYPE c .
    DATA gvio_select TYPE c .
    DATA gvio_from TYPE c .
  PRIVATE SECTION.
ENDCLASS.



CLASS /ODSMFE/CL_GET_ENTITYSET_SUP IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_main~gmib_get_entityset_config_data.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :05/03/2021
* Transport No.          :ES1K901576
* Program Description    :Method to Get Application Configuration Data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*

    DATA: lv_param_value TYPE /odsmfe/de_mfe_value.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

    SELECT SINGLE param_value FROM /odsmfe/tb_apcon WHERE param_name EQ @im_param_name INTO @lv_param_value. "#EC CI_NOFIELD
    IF sy-subrc = 0.
      ex_param_value = lv_param_value.
    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_main~gmib_get_entityset_data.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :05/03/2021
* Transport No.          :ES1K902573
* Program Description    :Method to Get EntitySet Data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

    "/Data Provider for ODATA Services
    CALL METHOD me->/odsmfe/if_get_entityset~gmib_get_entityset_data
      EXPORTING
        im_entity_name           = im_entity_name
        im_entity_set_name       = im_entity_set_name
*       im_source_name           = im_source_name
        im_filter_select_options = im_filter_select_options
*       im_paging                = im_paging
*       im_key_tab               = im_key_tab
*       im_navigation_path       = im_navigation_path
*       im_order                 = im_order
        im_filter_string         = im_filter_string
*       im_search_string         = im_search_string
*       im_tech_request_context  = im_tech_request_context
*       im_data_ext_class        = im_data_ext_class
      IMPORTING
        !ex_response             = ex_response
        !ex_response_data        = ex_response_data.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_main~gmib_get_entityset_filter_data.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :05/03/2021
* Transport No.          :ES1K902573
* Program Description    :Method to Get Mobile filter data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*

    DATA: lo_filter_values TYPE REF TO data,
          lst_filter_val   TYPE /odsmfe/st_core_range_str,
          lit_filters      TYPE TABLE OF /odsmfe/tb_filtr,
          lst_filters      TYPE /odsmfe/tb_filtr,
          lit_filter_val   TYPE /odsmfe/tt_core_range_tab.

    FIELD-SYMBOLS: <lfsst_filter_val>    TYPE /odsmfe/tt_core_range_tab,
                   <lfsst_filter_values> TYPE any,
                   <lfsst_field>         TYPE /odsmfe/tb_filtr-field.

    CONSTANTS: lc_abap_on TYPE abap_bool VALUE 'X'.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

    IF ex_filter_values IS SUPPLIED.

      "/Select all the filters from config table for the respective entityset
      SELECT mandt, entitysetname, tabname, field, recordno, field_descr, sign, options, low, high, active
         FROM /odsmfe/tb_filtr
                WHERE active = @lc_abap_on
                AND   entitysetname = @im_entity_set_name
                INTO TABLE @lit_filters .

      IF sy-subrc EQ 0.
        GET REFERENCE OF ex_filter_values INTO lo_filter_values.
        ASSIGN lo_filter_values->* TO  <lfsst_filter_values>.

        IF <lfsst_filter_values> IS ASSIGNED.
          LOOP AT lit_filters INTO lst_filters.

            IF lst_filters-low IS INITIAL.
              CONTINUE.
            ENDIF.
            lst_filters-sign = lst_filter_val-sign.
            lst_filters-options = lst_filter_val-option.
            lst_filters-low = lst_filter_val-low.
            lst_filters-high = lst_filter_val-high.
            lst_filter_val-option = lst_filters-options.
            APPEND lst_filter_val TO lit_filter_val.
            CLEAR lst_filter_val.

            ASSIGN COMPONENT 'FIELD' OF STRUCTURE lst_filters TO <lfsst_field>.
            IF <lfsst_field> IS ASSIGNED.
              ASSIGN COMPONENT <lfsst_field> OF STRUCTURE <lfsst_filter_values> TO <lfsst_filter_val>.
              IF <lfsst_filter_val> IS ASSIGNED.
                APPEND LINES OF lit_filter_val  TO <lfsst_filter_val>.
                UNASSIGN <lfsst_filter_val>.
              ENDIF.
              UNASSIGN <lfsst_field>.
            ENDIF.

            CLEAR lit_filter_val[].
          ENDLOOP.

          ex_filter_values = <lfsst_filter_values>.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_main~gmib_get_entityset_serv_config.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset_main~gmib_set_global_variable.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :05/03/2021
* Transport No.          :ES1K902573
* Program Description    :Method to Set Global Variables
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

*    goio_context                 =     im_context                 .
*    goio_request_details         =     im_request_details         .
*    goio_service_document_name   =     im_service_document_name   .
*    goio_service_version         =     im_service_version         .
*    goio_service_namespace       =     im_service_namespace       .
*    goio_injection               =     im_injection               .
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset~gmib_get_additional_where.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :16/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Get the Additional where Clause
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

    CHECK im_fnam IS NOT INITIAL.

    IF ch_where IS NOT INITIAL.
      CONCATENATE ch_where im_and_or   im_fnam im_operator im_fval
      INTO ch_where
      SEPARATED BY space.
    ELSE.
      CONCATENATE  ch_where  im_fnam im_operator im_fval
      INTO ch_where
      SEPARATED BY space.
    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset~gmib_get_entityset_data.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :16/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Get The Entity Data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

    "/ Data Provider for ODATA Services
    CALL METHOD me->/odsmfe/if_get_entityset~gmib_execute_query
      EXPORTING
        im_entity_name           = im_entity_name
        im_entity_set_name       = im_entity_set_name
*       im_source_name           = im_source_name
        im_filter_select_options = im_filter_select_options
*       im_paging                = im_paging
*       im_key_tab               = im_key_tab
*       im_navigation_path       = im_navigation_path
*       im_order                 = im_order
        im_filter_string         = im_filter_string
*       im_search_string         = im_search_string
*       im_tech_request_context  = im_tech_request_context
*       im_data_ext_class        = im_data_ext_class
        im_query_index           = 1
        im_request               = im_request
      IMPORTING
        !ex_response             = !ex_response
        !ex_response_data        = ex_response_data.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset~gmib_get_from_clause.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :16/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Fetch the from Clause from th econfig
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*

    DATA: lv_first_on  TYPE abap_bool VALUE abap_true,
          lv_join_rank TYPE int4.

    FIELD-SYMBOLS :<lfsst_join> TYPE /odsmfe/tb_joinc.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*


    SORT gitio_join_config BY join_rank ASCENDING.
    LOOP AT gitio_join_config ASSIGNING <lfsst_join>.

      "/We are not using Outer Join condition here
      IF lv_join_rank NE <lfsst_join>-join_rank.

        IF re_from IS INITIAL AND <lfsst_join>-rt_tab IS INITIAL.
          CONCATENATE re_from ` ` <lfsst_join>-left_tab INTO  re_from.
          CONTINUE.
        ELSEIF re_from IS INITIAL AND <lfsst_join>-rt_tab IS NOT INITIAL.
          CONCATENATE re_from ` ` <lfsst_join>-left_tab ` Inner Join ` <lfsst_join>-rt_tab ` on ` INTO re_from.
        ELSE.
          CONCATENATE re_from ` ` ` Inner Join ` <lfsst_join>-rt_tab  ` on ` INTO re_from.
        ENDIF.

        lv_join_rank = <lfsst_join>-join_rank.

        lv_first_on  = abap_true.

      ENDIF.

      "/We are not using OR condition here
      IF lv_first_on = abap_false.
        CONCATENATE re_from ` and ` INTO re_from.
      ELSE.
        lv_first_on = abap_false.
      ENDIF.

      CONCATENATE re_from <lfsst_join>-left_tab `~` <lfsst_join>-l_fnam ` = ` <lfsst_join>-rt_tab `~`  <lfsst_join>-r_fnam
               INTO re_from.

    ENDLOOP.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset~gmib_get_model_instance.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :16/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Get the instanc  for  the model Class
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*
*
*    DATA: lo_metadata_provider TYPE  REF TO /iwbep/if_mgw_med_provider,
*          lo_model             TYPE REF TO /iwbep/if_mgw_odata_fw_model.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*
*    IF goio_model IS NOT BOUND.
*
*      lo_metadata_provider = /iwbep/cl_mgw_med_provider=>get_med_provider( ).
*
*      lo_model = lo_metadata_provider->get_service_model(
*            iv_svc_ext_name  = goio_service_document_name->*
*            iv_svc_namespace = goio_service_namespace->*
*            iv_svc_version   = goio_service_version->*
*            iv_do_check_for_extension = abap_true  ).
*
*      CHECK lo_model IS BOUND.
*      goio_model ?= lo_model.
*    ENDIF.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset~gmib_get_odata_parameter.

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset~gmib_get_selection_fields.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :16/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Get the fields to be selected
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*

*    DATA: lit_select        TYPE TABLE OF edpline,
*          lst_sublist       TYPE edpline,
*          lst_entity_detail TYPE /iwbep/if_mgw_med_odata_types=>ty_s_med_entity_type.
*
*    DATA: lo_model          TYPE REF TO /iwbep/cl_mgw_odata_model.
*
*    DATA: lv_entity         TYPE /iwbep/if_mgw_med_odata_types=>ty_e_med_entity_id,
*          lv_ent_properties TYPE  /iwbep/if_mgw_med_odata_types=>ty_t_med_properties,
*          lv_sublist        TYPE edpline,
*          lv_lines          TYPE i.
*
*    FIELD-SYMBOLS :<lfsst_fetch> TYPE /odsmfe/tb_fetch,
*                   <lfsst_prop>  TYPE /iwbep/if_mgw_med_odata_types=>ty_s_med_property,
*                   <lfsst_comp>  TYPE edpline.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

*    CHECK goio_model IS BOUND.
*
*    lv_entity = im_entity.
*    lo_model = goio_model.
*
*
*    READ TABLE lo_model->mt_entities INTO lst_entity_detail
*          WITH KEY name = im_entity.
*
*    lv_ent_properties = lst_entity_detail-properties .
*
*    LOOP AT lst_entity_detail-properties ASSIGNING <lfsst_prop>.
*
*      "/ Very Low number of fields so sorting and binary search not required
*      READ TABLE gitio_fetch_config ASSIGNING <lfsst_fetch> WITH KEY properties = <lfsst_prop>-name .
*
*      CHECK sy-subrc IS INITIAL.
*
*      CONCATENATE <lfsst_fetch>-tabname '~' <lfsst_fetch>-fieldname ` as ` <lfsst_prop>-name  INTO lst_sublist.
*
*      APPEND lst_sublist TO lit_select.
*      CLEAR lst_sublist.
*    ENDLOOP.
*
*
*    "/ Export the select fields
*    ch_select_fld =  lit_select .
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset~gmib_get_where_clause.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :16/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Build the SQL Where Clause for the
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************


*    re_where_cls = im_tech_request_context->get_osql_where_clause_convert( ).

  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset~gmib_set_global_variable.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :16/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Set the Global Variable in the class
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
*
*    goio_context                 =     im_context                 .
*    goio_request_details         =     im_request_details         .
*    goio_service_document_name   =     im_service_document_name   .
*    goio_service_version         =     im_service_version         .
*    goio_service_namespace       =     im_service_namespace       .
*    goio_injection               =     im_injection               .
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset~gmib_sort_data.
  ENDMETHOD.


  METHOD constructor.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :16/03/2020
* Transport No.          :ES1K901576
* Program Description    :Constructor method
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* -----------------------------------------------------------------------*

    DATA: lit_joinc   TYPE TABLE OF /odsmfe/tb_joinc,
          lit_tb_fetc TYPE TABLE OF /odsmfe/tb_fetch.

    DATA: lv_entity_set_name(40)  TYPE c.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

    gvio_entityset_name  = im_entity_set_name.
    gvio_entity_name     = im_entity_name.
*    goio_data_ext_class ?= im_data_ext_class.

    "/ Fetch the Config data for the Select Query

    lv_entity_set_name = gvio_entityset_name.

    SELECT * FROM  /odsmfe/tb_joinc
    WHERE entityset    = @lv_entity_set_name
    INTO TABLE @lit_joinc.
    IF sy-subrc EQ 0.
      gitio_join_config = lit_joinc.
    ENDIF.

    SELECT * FROM  /odsmfe/tb_fetch
    WHERE entityset    = @lv_entity_set_name
    INTO TABLE @lit_tb_fetc.
    IF sy-subrc EQ 0.
      gitio_fetch_config = lit_tb_fetc.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
