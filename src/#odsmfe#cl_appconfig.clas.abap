class /ODSMFE/CL_APPCONFIG definition
  public
  inheriting from /ODSMFE/CL_GET_ENTITYSET_SUP
  create public .

public section.

  methods /ODSMFE/IF_GET_ENTITYSET~GMIB_EXECUTE_QUERY
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET~GMIB_GET_ADDITIONAL_WHERE
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET~GMIB_GET_WHERE_CLAUSE
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET~GMIB_SORT_DATA
    redefinition .
protected section.
private section.

  data GITII_ENTITYSET type /ODSMFE/CL_PR_APPSTORE_MPC=>TT_APPLICATIONCONFIG .
ENDCLASS.



CLASS /ODSMFE/CL_APPCONFIG IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset~gmib_execute_query.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :23/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Execute the select query
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :ODS-VSANAGALA
* Change Date            :02.01.2023
* Transport No.          :ES1K903398
* Change Description     :Mapping system userid to the related parameter to get the relevant data based on the UserID in the related services.
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :ODS-PIYYAPPAN
* Change Date            :17.05.2023
* Transport No.          :ES1K903808
* Change Description     :To get the configurable days for myForms Response Capture Service
***********************************************************************
* -----------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                      *
* -----------------------------------------------------------------------*

    "/Tables and Structures
    DATA: lst_filter       TYPE /iwbep/s_mgw_select_option,                         "MGW Framework: Selection Option Parameters for db selects
          lst_filter_range TYPE /iwbep/s_cod_select_option,                         "MGW Framework: Select Options for Queries
          lit_appconfig    TYPE /odsmfe/cl_pr_appstore_mpc=>tt_applicationconfig.   "Gateway properties for appconfig

    "/ Reference Object
    DATA: lo_super  TYPE REF TO /odsmfe/cl_get_ent_super_bapi.                      "Super Class For implementing the getentityset with BAPI

    "/ Variables
    DATA: lv_active        TYPE string,                                             "Active flag
          lv_days          TYPE char10,                                                  "Configured days
          lv_select        TYPE /odsmfe/if_get_entityset~gtyt_select,               "EDIC: Program editor line
          lv_where_clause  TYPE string,                                             "Where Clause
          lv_from          TYPE string.                                             "From Clause

    "/ Constants
    CONSTANTS: lc_user       TYPE string VALUE 'BackEndUser',                       "BackEndUser
               lc_instance   TYPE string VALUE 'MYFORMS_INSTANCES_DAYS',            "myForms Instance Days
               lc_entityset  TYPE string VALUE 'myFormsResponseCaptureSet',         "myForms Response Capture Set
               lc_field      TYPE string VALUE 'DAYS_INSTANCE'.                     "Instance Days

    "/ Field Symbols
    FIELD-SYMBOLS: <lfsst_select> TYPE /odsmfe/if_get_entityset~gtyt_select,        "EDIC: Program editor line
                   <lfsst_entity> TYPE /odsmfe/tb_apcon.                            "Structure for Application Configuration

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

* -----------------------------------------------------------------------*
*                  M A I N    S E C T I O N                              *
* -----------------------------------------------------------------------*

    TRY.
        "/Instantiate super class
        CREATE OBJECT lo_super
          EXPORTING
            im_entity_name = im_entity_name.

        "/Get Service Config Data
        CALL METHOD lo_super->/odsmfe/if_get_entityset_main~gmib_get_entityset_serv_config
          EXPORTING
            im_entity_set_name = im_entity_set_name
          IMPORTING
            ex_active          = lv_active.
        IF lv_active IS INITIAL.
          RETURN.
        ENDIF. " IF lv_active IS INITIAL.

        "/ Fetch The Model Instance
        me->/odsmfe/if_get_entityset~gmib_get_model_instance( ).

        "/ Get The Wehre Clause
        lv_where_clause = /odsmfe/if_get_entityset~gmib_get_where_clause( im_tech_request_context ).

        "/Get The Fields For Selection From The Config Table
        me->/odsmfe/if_get_entityset~gmib_get_selection_fields(
        EXPORTING im_entity     = im_entity_name
        CHANGING  ch_select_fld = lv_select ).

        "/ Build From Clause From The Config Table
        lv_from = /odsmfe/if_get_entityset~gmib_get_from_clause( im_entity_name ).

        TRY .
            SELECT (lv_select)
            INTO CORRESPONDING FIELDS OF TABLE gitii_entityset
            FROM (lv_from)
            WHERE (lv_where_clause).

            IF sy-subrc EQ 0.
              me->/odsmfe/if_get_entityset~gmib_sort_data( ).

*SOC by ODS-VSANAGALA - ES1K903398.
              "/ Mapping userid to the related parameter.
              READ TABLE gitii_entityset ASSIGNING <lfsst_entity> WITH KEY param_name = lc_user.
              IF sy-subrc = 0 .
                <lfsst_entity>-param_value = sy-uname.
              ENDIF. "/IF sy-subrc = 0 .
*EOC by ODS-VSANAGALA - ES1K903398.

*SOC by ODS-PIYYAPPAN - ES1K903808.
              READ TABLE gitii_entityset ASSIGNING <lfsst_entity> WITH KEY param_name = lc_instance.
              IF sy-subrc = 0.
                "/ Get Configurable days from filter table
                SELECT SINGLE low
                  FROM /odsmfe/tb_filtr
                  INTO lv_days
                  WHERE entitysetname = lc_entityset
                  AND   field         = lc_field.

                IF sy-subrc = 0 AND lv_days IS NOT INITIAL.
                  <lfsst_entity>-param_value = lv_days.
                ENDIF. "/IF sy-subrc = 0 AND lv_days IS NOT INITIAL.

              ENDIF. "/IF sy-subrc = 0.
*EOC by ODS-PIYYAPPAN - ES1K903808.

              "/ Data Provider for ODATA Services
              GET REFERENCE OF gitii_entityset INTO ch_entityset.
            ENDIF. "/IF sy-subrc EQ 0.

          CATCH cx_sy_dynamic_osql_syntax. " Open SQL Error
            CHECK sy-subrc EQ 0.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.


  METHOD /odsmfe/if_get_entityset~gmib_get_additional_where.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :24/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Get the Additional where Clause
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

    DATA: lv_fnam TYPE char30,
          lv_fval	TYPE char30.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

*    lv_fnam = 'ACTIVE'.
*    lv_fval = abap_true.
*    CONCATENATE `'` lv_fval `'` INTO lv_fval.
*
*    "/ Below method can be called here Number of times.
*    "/ Data Provider for ODATA Services.
*    super->/odsmfe/if_get_entityset~gmib_get_additional_where(
*    EXPORTING
*      im_fnam     = lv_fnam
*      im_fval     = lv_fval
*    CHANGING
*      ch_where    = ch_where ).
  ENDMETHOD.


  method /ODSMFE/IF_GET_ENTITYSET~GMIB_GET_WHERE_CLAUSE.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :24/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to get Build the SQL Where Clause
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

    TRY.
        "/ Data Provider for ODATA Services
        re_where_cls = super->/odsmfe/if_get_entityset~gmib_get_where_clause( im_tech_request_context ).

        "/get additional where condition for query
        me->/odsmfe/if_get_entityset~gmib_get_additional_where(
        CHANGING
          ch_where    = re_where_cls ).

      CATCH cx_static_check.


    ENDTRY.
  endmethod.


  method /ODSMFE/IF_GET_ENTITYSET~GMIB_SORT_DATA.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :24/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Sort Entityset data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

    SORT gitii_entityset BY recordnum.
  endmethod.
ENDCLASS.
