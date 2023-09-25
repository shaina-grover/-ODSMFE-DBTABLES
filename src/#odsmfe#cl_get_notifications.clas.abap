class /ODSMFE/CL_GET_NOTIFICATIONS definition
  public
  create private .

public section.
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_GET_NOTIFICATIONS IMPLEMENTATION.


  METHOD gmib_get_notification_data.
***********************************************************************************
* Data Declaration
***********************************************************************************
* Object reference
    DATA: lo_filter    TYPE REF TO /iwbep/if_mgw_req_filter,
          lo_ref       TYPE REF TO object,
          lv_classname TYPE seoclsname.

    DATA: lst_filter_range TYPE /iwbep/s_cod_select_option,
          lst_filter       TYPE /iwbep/s_mgw_select_option,
          lv_mobileuser    TYPE string,
          lv_mobile_app    TYPE /syclo/core_mobile_app_dte,
          lv_exchobj       TYPE /odsmfe/exchobj_dte,
          lv_component     TYPE boolean,
          lv_delta_token   TYPE timestamp.

    DATA: lo_data_provider TYPE REF TO /odsmfe/if_get_entityset_main.

    DATA: lit_notification_header  TYPE STANDARD TABLE OF /odsmfe/cs_notif_header_str,
          lst_notification_header  TYPE /odsmfe/cs_notif_header_str,
          lst_valid_nos            TYPE /odsmfe/pm_valid_qmnum_str,
          lit_valid_nos            TYPE STANDARD TABLE OF /odsmfe/pm_valid_qmnum_str,
          lit_notif_header_temp    TYPE STANDARD TABLE OF /odsmfe/cs_notif_header_str,
          lst_notif_header_temp    TYPE /odsmfe/cs_notif_header_str,
          lst_notification_headers TYPE /odsmfe/cs_notif_header_str,
          lit_notification_headers TYPE /odsmfe/cs_notif_header_tab.

    DATA: lo_delta_context TYPE REF TO /iwbep/if_mgw_req_entityset,
          lo_ref_exch_data TYPE REF TO data.

    "/ Constants
    CONSTANTS: lc_tzone     TYPE tznzonesys VALUE 'UTC',
               lc_i         TYPE string VALUE 'I',
               lc_u         TYPE string VALUE 'U',
               lc_createdby TYPE string VALUE 'CREATEDBY'.

    "/ Field Symbols
    FIELD-SYMBOLS: <lfsit_delta_tab> TYPE STANDARD TABLE,
                   <lfs_no>          LIKE lit_valid_nos,
                   <lfsst_delta_str> TYPE /odsmfe/tb_no_ex.

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    "/ Get App Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                   INTO lv_mobile_app
                   WHERE entitysetname = 'MobileAppName'
                   AND field = 'MOBILE_APP_NAME'.

    "/ Get Exchange Object Name
    SELECT SINGLE low FROM /odsmfe/tb_filtr
                   INTO lv_exchobj
                   WHERE entitysetname = 'No_Exchange'
                   AND field = 'EXCHANGE_OBJECT_NAME'.

    IF im_tech_request_context IS SUPPLIED.
      lo_filter = im_tech_request_context->get_filter( ).
    ENDIF.

    "/ Read filter values
    IF im_filter_select_options IS NOT INITIAL.
      LOOP AT im_filter_select_options INTO lst_filter.
        TRANSLATE lst_filter-property TO UPPER CASE.
        CASE lst_filter-property.
          WHEN lc_createdby.
            READ TABLE lst_filter-select_options INTO lst_filter_range INDEX 1.
            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL .
              lv_mobileuser = lst_filter_range-low.
              TRANSLATE lv_mobileuser TO UPPER CASE.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    IF lv_mobileuser IS INITIAL.
      lv_mobileuser = sy-uname.
    ENDIF.

    "/ Incoming delta token
    lo_delta_context ?= im_tech_request_context.
    lv_delta_token = lo_delta_context->get_deltatoken( ).

    DATA: o_ref TYPE REF TO object.
    lv_classname = /odsmfe/cl_exchmechwo=>no_classname_get( ).
    TRANSLATE lv_classname TO UPPER CASE.
    CREATE OBJECT o_ref TYPE (lv_classname).

    "/ Fetch Notification Header Details
    IF  lv_mobileuser IS NOT INITIAL AND lv_delta_token IS INITIAL.

      CALL METHOD O_REF->('GET_NOTIFICATION_COMMON')
        EXPORTING
          iv_mobileuser = lv_mobileuser.

      ASSIGN ('O_REF->GT_QMNUM_DELTA[]') TO <lfs_no>.
      IF <lfs_no> IS ASSIGNED.
        lit_valid_nos[] = <lfs_no>.
      ENDIF.
    ENDIF.

    "/ Determine delta objects
    IF lv_delta_token IS NOT INITIAL.

      CALL METHOD /smfnd/cl_core_exobj_tools=>determine_delta_objkeys
        EXPORTING
          iv_mobile_app  = lv_mobile_app
          iv_exchobj     = lv_exchobj
          iv_time_token  = lv_delta_token
          iv_time_zone   = lc_tzone
        IMPORTING
          eref_exch_data = lo_ref_exch_data.

      IF lo_ref_exch_data IS NOT INITIAL.
        ASSIGN lo_ref_exch_data->* TO <lfsit_delta_tab>.
        LOOP AT <lfsit_delta_tab> ASSIGNING <lfsst_delta_str>.
          IF <lfsst_delta_str>-action = lc_i OR <lfsst_delta_str>-action = lc_u.
            lst_notification_header-aufnr = <lfsst_delta_str>-objkey.
            APPEND lst_notification_header TO lit_notif_header_temp.
            CLEAR lst_notification_header.
          ENDIF.
        ENDLOOP.

        LOOP AT lit_notif_header_temp  INTO lst_notif_header_temp .
          MOVE-CORRESPONDING lst_notif_header_temp TO lst_notification_headers .
          APPEND lst_notification_headers  TO lit_notification_headers .
          CLEAR : lst_notif_header_temp ,lst_notification_headers .
        ENDLOOP.

        IF lit_notification_headers IS NOT INITIAL.
          "/ Fetch Notification Header Details

          CALL METHOD O_REF->('GET_NOTIFICATION_COMMON')
            EXPORTING
              iv_mobileuser          = lv_mobileuser
              it_notification_header = lit_notification_headers.

          ASSIGN ('O_REF->GT_QMNUM_DELTA[]') TO <lfs_no>.
          IF <lfs_no> IS ASSIGNED.
            lit_valid_nos[] = <lfs_no>.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    "/ Mapping to Final Table
    ex_valid_notifications[] = lit_valid_nos[].


  ENDMETHOD.


  METHOD gmib_get_technician_data.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS-VSANAGALA
* Creation Date          : 25.02.2023
* Transport No.          : ES1K903619
* Program Description    : Get the Technician data
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

    "/ Types
    TYPES : BEGIN OF ltys_pernr,
              pernr TYPE pernr_d,
            END OF ltys_pernr.

    "/ Tables and Structures
    DATA : lit_pa0105 TYPE TABLE OF pa0105,
           lst_pa0105 TYPE pa0105,
           lit_pa0001 TYPE TABLE OF ltys_pernr,
           lst_pa0001 TYPE ltys_pernr.

    "/ Variables
    DATA : lv_mstbr TYPE mstbr,
           lv_parva TYPE xuvalue.

*---------------------------------------------------------------------*
*           E N D   O F   D A T A   D E C L A R A T I O N             *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*                    M A I N    S E C T I O N                         *
*---------------------------------------------------------------------*

    "/ Fetch pernr of supervisor user
    SELECT * FROM pa0105
       INTO CORRESPONDING FIELDS OF TABLE lit_pa0105    "#EC CI_NOFIRST
       WHERE endda GE sy-datum
       AND   begda LE sy-datum
       AND   usrid EQ im_mobileuser.

    IF sy-subrc = 0 AND lit_pa0105 IS NOT INITIAL.
      SORT lit_pa0105 BY pernr.
      DELETE ADJACENT DUPLICATES FROM lit_pa0105 COMPARING pernr.

      READ TABLE lit_pa0105 INTO lst_pa0105 INDEX 1.
      IF sy-subrc = 0.
        lv_mstbr = lst_pa0105-pernr.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lv_mstbr
        IMPORTING
          output = lv_mstbr.

      "/ Get Personal details - HR Master Record: Infotype 0001 (Org. Assignment)
      SELECT pernr
        FROM pa0001
        INTO CORRESPONDING FIELDS OF TABLE lit_pa0001   "#EC CI_NOFIELD
        WHERE pa0001~endda GE sy-datum
        AND   pa0001~begda LE sy-datum
        AND   pa0001~mstbr EQ lv_mstbr.

      IF sy-subrc = 0.
        SORT lit_pa0001 BY pernr.
      ENDIF.

    ENDIF.
    lst_pa0001-pernr = lv_mstbr.
    APPEND lst_pa0001 TO lit_pa0001.
    CLEAR lst_pa0001.
    IF lit_pa0001 IS NOT INITIAL.

      SELECT pernr usrid FROM pa0105
                         INTO TABLE gitib_tech
                         FOR ALL ENTRIES IN lit_pa0001
                         WHERE pernr = lit_pa0001-pernr.
    ENDIF.

    ex_tech_data[] = gitib_tech[].

  ENDMETHOD.
ENDCLASS.
