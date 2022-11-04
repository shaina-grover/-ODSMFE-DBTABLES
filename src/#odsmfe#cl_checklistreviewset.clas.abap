class /ODSMFE/CL_CHECKLISTREVIEWSET definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  type-pools ABAP .

  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_RESPONSECAPTURE .
  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_RESPONSECAPTURE .
  data GVIB_USER type USNAM .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_CHECKLISTREVIEWSET IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS
* Creation Date          :
* Transport No.          :
* Program Description    :
***********************************************************************
*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------
* object reference
    DATA: lo_filter TYPE  REF TO /iwbep/if_mgw_req_filter,
          lrt_form  TYPE REF TO /odsmfe/st_core_range_str.

    DATA:lrs_form          TYPE /odsmfe/st_core_range_str,
         lrs_filter_values TYPE /odsmfe/st_form_fil_vals,
         lst_key_tab       TYPE /iwbep/s_mgw_name_value_pair,
         lst_filter_range  TYPE /iwbep/s_cod_select_option,
         lst_filter        TYPE /iwbep/s_mgw_select_option,
         lv_mobileuser     TYPE string,
         lv_delta_token    TYPE timestamp,
         lt_fapvr          TYPE STANDARD TABLE OF /odsmfe/tb_fapvr,
         lt_respcap        TYPE STANDARD TABLE OF /odsmfe/tb_forsp,
         lwa_rescap        TYPE /odsmfe/tb_forsp,
         lt_finst          TYPE STANDARD TABLE OF /odsmfe/tb_finst,
         lwa_finst         TYPE /odsmfe/tb_finst.

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

    IF im_tech_request_context IS SUPPLIED.
      lo_filter = im_tech_request_context->get_filter( ).
    ENDIF.

    IF im_filter_select_options IS NOT INITIAL.
      LOOP AT im_filter_select_options INTO lst_filter.
        TRANSLATE lst_filter-property TO UPPER CASE.
        CASE lst_filter-property.
          WHEN 'CreatedBy'.
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

* Form Approval Check
    SELECT formid version workordernum oprnum approverid notification notificationitem
           notificationtask equipment functionallocation formname FROM /odsmfe/tb_fapvr
      INTO CORRESPONDING FIELDS OF TABLE lt_fapvr
      WHERE approverid = lv_mobileuser.
* Form Response Capture Check
    IF lt_fapvr IS NOT INITIAL.
      SELECT * FROM /odsmfe/tb_forsp
        INTO CORRESPONDING FIELDS OF TABLE lt_respcap
        FOR ALL ENTRIES IN lt_fapvr
        WHERE wo_num = lt_fapvr-workordernum
        AND formid = lt_fapvr-formid
        AND version = lt_fapvr-version
        AND isdraft = space.
      IF sy-subrc = 0.
        SELECT * FROM /odsmfe/tb_finst
                 INTO CORRESPONDING FIELDS OF TABLE lt_finst
                 FOR ALL ENTRIES IN lt_respcap
                 WHERE forminstanceid EQ lt_respcap-instanceid
                 AND formid = lt_respcap-formid
                 AND version = lt_respcap-version
                 AND counter = lt_respcap-counter
                 AND approverid = lv_mobileuser.
        LOOP AT lt_respcap INTO lwa_rescap.
          READ TABLE lt_finst INTO lwa_finst WITH KEY forminstanceid = lwa_rescap-instanceid.
          IF sy-subrc NE 0.
            MOVE-CORRESPONDING lwa_rescap TO gstib_entity.
* Get Entity method is requested
            IF im_key_tab IS NOT INITIAL.
              GET REFERENCE OF gstib_entity INTO ex_entity.
            ELSE.
              APPEND gstib_entity TO gitib_entity.
            ENDIF.
          ENDIF.
          CLEAR : lwa_rescap.
        ENDLOOP.
* Get EntitySet method is requested
        IF gitib_entity IS NOT INITIAL.
          GET REFERENCE OF gitib_entity INTO ex_entityset.
        ENDIF.
      ENDIF.
    ENDIF.
* Third Table --> /ODSMFE/TB_FINST
  ENDMETHOD.
ENDCLASS.
