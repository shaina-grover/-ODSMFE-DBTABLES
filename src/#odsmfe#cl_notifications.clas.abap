class /ODSMFE/CL_NOTIFICATIONS definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  type-pools ABAP .

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC_EXT=>TS_NOTIFICATIONS .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC_EXT=>TT_NOTIFICATIONS .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_NOTIFICATIONS IMPLEMENTATION.


    METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS-VSANAGALA
* Creation Date          : 24.02.2023
* Transport No.          : ES1K903619
* Program Description    : Get the Notifications in service to fetch forms
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************
*---------------------------------------------------------------------*
*                  D A T A    D E C L A R A T I O N                   *
*---------------------------------------------------------------------*

      "/ Tables and Structures
      DATA: lst_filter_select_options TYPE /iwbep/s_mgw_select_option,
            lst_key_tab               TYPE /iwbep/s_mgw_name_value_pair.

      "/ Range Tabkes and Range Structures
      DATA: lrs_filter_range TYPE /iwbep/s_cod_select_option,
            lrs_filter       TYPE /iwbep/s_cod_select_option,
            lrt_notification TYPE /iwbep/t_cod_select_options,
            lrt_ordertype    TYPE /iwbep/t_cod_select_options.

      "/ Constants
      CONSTANTS: lc_notification TYPE string VALUE 'NOTIFICATION',
                 lc_ordertype    TYPE string VALUE 'ORDERTYPE',
                 lc_notif        TYPE string VALUE 'Notification',
                 lc_i            TYPE char1  VALUE 'I',
                 lc_eq           TYPE char2  VALUE 'EQ'.

*---------------------------------------------------------------------*
*           E N D   O F   D A T A   D E C L A R A T I O N             *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*                    M A I N    S E C T I O N                         *
*---------------------------------------------------------------------*

      "/ Read filter values
      IF im_filter_select_options IS NOT INITIAL.
        LOOP AT im_filter_select_options INTO lst_filter_select_options.
          TRANSLATE lst_filter_select_options-property TO UPPER CASE.
          CASE lst_filter_select_options-property.

            WHEN lc_notification.
              READ TABLE lst_filter_select_options-select_options INTO lrs_filter_range INDEX 1.
              IF sy-subrc = 0 AND lrs_filter_range IS NOT INITIAL.
                lrs_filter-sign   = lc_i.
                lrs_filter-option = lc_eq.
                lrs_filter-low    = lrs_filter_range-low.
                APPEND lrs_filter_range TO lrt_notification.
              ENDIF.

            WHEN lc_ordertype.
              READ TABLE lst_filter_select_options-select_options INTO lrs_filter_range INDEX 1.
              IF sy-subrc = 0 AND lrs_filter_range IS NOT INITIAL.
                lrs_filter-sign   = lc_i.
                lrs_filter-option = lc_eq.
                lrs_filter-low    = lrs_filter_range-low.
                APPEND lrs_filter_range TO lrt_ordertype.
              ENDIF.

          ENDCASE. "/ CASE lst_filter_select_options-property.
          CLEAR: lrs_filter_range, lrs_filter, lst_filter_select_options.
        ENDLOOP. "/ LOOP AT im_filter_select_options INTO lst_filter_select_options.
      ENDIF. "/ IF im_filter_select_options IS NOT INITIAL.

      "/ Read key values
      IF im_key_tab IS NOT INITIAL.
        READ TABLE im_key_tab INTO lst_key_tab WITH KEY name = lc_notif.
        IF sy-subrc = 0 AND lst_key_tab IS NOT INITIAL.
          lrs_filter-sign   = lc_i.
          lrs_filter-option = lc_eq.
          lrs_filter-low    = lrs_filter_range-low.
          APPEND lrs_filter_range TO lrt_notification.
          CLEAR: lrs_filter_range, lrs_filter, lst_key_tab.
        ENDIF. "/ IF sy-subrc = 0 AND lst_key_tab IS NOT INITIAL.
      ENDIF. "/ IF im_key_tab IS NOT INITIAL.

      "/ Get the Notification details from the relevant table
      SELECT qmnum qmart qmtxt
        FROM qmel
        INTO TABLE gitib_entity
        WHERE qmnum IN lrt_notification[]
          AND qmart IN lrt_ordertype[].

      IF sy-subrc NE 0.
        CLEAR gitib_entity.
      ENDIF.

      "/ Mapping the properties to export in the gateway service.
      IF im_key_tab IS NOT INITIAL.
        READ TABLE gitib_entity INTO gstib_entity INDEX 1.
        GET REFERENCE OF gstib_entity INTO ex_entity.
      ELSE.
        GET REFERENCE OF gitib_entity INTO ex_entityset.
      ENDIF.

    ENDMETHOD.
ENDCLASS.
