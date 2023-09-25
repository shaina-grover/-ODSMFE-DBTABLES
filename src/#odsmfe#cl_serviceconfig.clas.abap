class /ODSMFE/CL_SERVICECONFIG definition
  public
  inheriting from /ODSMFE/CL_GET_ENTITYSET_SUP
  create public .

public section.
  "type-pools ABAP .  "Changed by Pratheesh

  methods /ODSMFE/IF_GET_ENTITYSET~GMIB_EXECUTE_QUERY
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET~GMIB_GET_WHERE_CLAUSE
    redefinition .
  methods /ODSMFE/IF_GET_ENTITYSET~GMIB_SORT_DATA
    redefinition .
protected section.
private section.

  "data GITII_ENTITYSET type /ODSMFE/CL_PR_APPSTORE_MPC=>TT_SERVICECONFIG . "Changed by Pratheesh
ENDCLASS.



CLASS /ODSMFE/CL_SERVICECONFIG IMPLEMENTATION.


  method /ODSMFE/IF_GET_ENTITYSET~GMIB_EXECUTE_QUERY.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :25/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Execute the select query
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
   DATA:  "lv_user          TYPE usnam,
*          lv_select        TYPE /odsmfe/if_get_entityset~gtyt_select,
*          lv_where_clause  TYPE string,
*          lv_from          TYPE string,
          lit_serviceconfig TYPE TABLE of /ODSMFE/CE_ServiceConfig.
*          lst_filter       TYPE /iwbep/s_mgw_select_option,
*          lst_filter_range TYPE /iwbep/s_cod_select_option.
         " lit_appstore     TYPE /odsmfe/cl_pr_appstore_mpc=>tt_serviceconfig.

    "/Field Symbols
    FIELD-SYMBOLs:  <lfsst_select>  TYPE /odsmfe/if_get_entityset~gtyt_select.

    DATA: lv_active  TYPE string.

    DATA: lo_super  TYPE REF TO /odsmfe/cl_get_ent_super_bapi.
* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

    "TRY.
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
 "SOC Prateesh

        "/ Fetch The Model Instance
*        me->/odsmfe/if_get_entityset~gmib_get_model_instance( ).
*
*        "/ Get The Wehre Clause
*        lv_where_clause = /odsmfe/if_get_entityset~gmib_get_where_clause( im_request ). "im_tech_request_context ).
*
*        "/ Get The Fields For Selection From The Config Table
*        me->/odsmfe/if_get_entityset~gmib_get_selection_fields(
*        EXPORTING
*          im_entity     = im_entity_name
*        CHANGING
*          ch_select_fld = lv_select ).
*
*        "/ Build From Clause From The Config Table
*        "lv_from = /odsmfe/if_get_entityset~gmib_get_from_clause( im_entity_name ).
*
*        "/ Get the fields for selection from the config table
*        "TRY .
*            SELECT (lv_select)
*            FROM (lv_from)
*            WHERE (lv_where_clause)
*            INTO CORRESPONDING FIELDS OF TABLE @ex_response_data.  "gitii_entityset.
*          CATCH cx_sy_dynamic_osql_syntax.
*            CHECK sy-subrc EQ 0.
*        ENDTRY.

             SELECT
                ReqID, entityset_name, AppStoreName, RoleID, EnteredBy, entity_type,  ClassName, Object,
                AppStoreID, AppStore, Url, DisplayName, Active, Keys
                from /ODSMFE/TB_SRCON  into TABLE @lit_serviceconfig .

        IF sy-subrc EQ 0.

        sort lit_serviceconfig by appstoreid.

        MOVE-CORRESPONDING lit_serviceconfig TO ex_response_data.

        "ex_response_data[] = lt_tab[].


        "  me->/odsmfe/if_get_entityset~gmib_sort_data( ).
            "GET REFERENCE OF gitii_entityset INTO ch_entityset.
        ENDIF. "/IF sy-subrc EQ 0.
        "/ Business Exception
      "CATCH /iwbep/cx_mgw_busi_exception .
       " CHECK sy-subrc EQ 0.
        "/ Technical Exception
      "CATCH /iwbep/cx_mgw_tech_exception .
      "  CHECK sy-subrc EQ 0.
   "ENDTRY.

 "EOC Pratheesh
  endmethod.


  METHOD /ODSMFE/IF_GET_ENTITYSET~GMIB_GET_WHERE_CLAUSE.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :25/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Build the SQL Where Clause
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
    TRY.
        "/ Data Provider for ODATA Services
        re_where_cls = super->/odsmfe/if_get_entityset~gmib_get_where_clause( IO_REQUEST ). "im_tech_request_context ).

        "/get additional where condition for query
        me->/odsmfe/if_get_entityset~gmib_get_additional_where(
        CHANGING
          ch_where    = re_where_cls ).
      CATCH cx_static_check.

    ENDTRY.
  ENDMETHOD.


  METHOD /ODSMFE/IF_GET_ENTITYSET~GMIB_SORT_DATA.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :25/03/2020
* Transport No.          :ES1K901576
* Program Description    :Method to Sort Entityset data
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

    "SORT  gitii_entityset  BY appstoreid.
  ENDMETHOD.
ENDCLASS.
