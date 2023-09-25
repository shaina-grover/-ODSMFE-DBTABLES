CLASS /odsmfe/cl_checklistreviewset DEFINITION
  PUBLIC
  INHERITING FROM /odsmfe/cl_get_ent_super_bapi
  CREATE PUBLIC .

  PUBLIC SECTION.

*SOC BY LMETTA

*  type-pools ABAP .

*  data GSTIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TS_RESPONSECAPTURE .
*  data GITIB_ENTITY type /ODSMFE/CL_PR_FORMUI_MPC=>TT_RESPONSECAPTURE .

*EOC BY LMETTA

    DATA gvib_user TYPE usnam .


    METHODS /odsmfe/if_get_entityset_bapi~gmib_read_entityset
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
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
    DATA: lo_filter TYPE  REF  TO if_rap_query_filter,
          lrt_form  TYPE REF TO /odsmfe/st_core_range_str.

    DATA:lrs_form          TYPE /odsmfe/st_core_range_str,

         lrs_filter_values TYPE /odsmfe/st_form_fil_vals,
*         lst_key_tab       TYPE /iwbep/s_mgw_name_value_pair,
*         lst_filter_range  TYPE if_rap_query_filter=>tt_name_range_pairs,       "/iwbep/s_cod_select_option,
*         lst_filter        TYPE if_rap_query_filter=>ty_name_range_pairs,      "/iwbep/s_mgw_select_option,
         lv_mobileuser     TYPE string,
         lv_delta_token    TYPE timestamp,
         lt_fapvr          TYPE STANDARD TABLE OF /odsmfe/tb_fapvr,
         lt_respcap        TYPE STANDARD TABLE OF /odsmfe/tb_forsp,
         lwa_rescap        TYPE /odsmfe/tb_forsp,
         lt_finst          TYPE STANDARD TABLE OF /odsmfe/tb_finst,
         lwa_finst         TYPE /odsmfe/tb_finst,
         lrt_createdby     TYPE TABLE OF /odsmfe/st_core_range_str,
         lst_createdby     TYPE /odsmfe/tb_forsp.

data: lt_finst_temp  type table of /ODSMFE/CE_REVIEWERFORMRESP,    "by LMETTA
*      lst_finst_temp type /ODSMFE/CE_REVIEWERFORMRESP,
      lwa_finst_temp type /ODSMFE/CE_REVIEWERFORMRESP.

*     lst_finst type /odsmfe/tb_finst.

    CONSTANTS:
         lc_createdby   TYPE string VALUE 'CREATEDBY'.


*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

*    IF im_tech_request_context IS SUPPLIED.

*     lo_filter = im_tech_request_context->get_filter( ).
*      lo_filter = !im_request->get_filter( ).

*    ENDIF.

*    IF im_filter_select_options IS NOT INITIAL.
*      LOOP AT im_filter_select_options INTO lst_filter.
*      TRANSLATE lst_filter-property TO UPPER CASE.
*TRY.
*  IF im_request->is_data_requested( ).
*     lo_filter = im_request->get_filter( ).
*    DATA(im_filter_select_options) = lo_filter->get_as_ranges( ).

*    LOOP AT im_filter_select_options INTO DATA(lst_filter).
*      TRANSLATE lst_filter-name TO UPPER CASE.    "property

*SOC BY LMETTA
    LOOP AT im_filter_select_options INTO DATA(ls_filter_select_options).
      CASE ls_filter_select_options-name.

*       CASE lst_filter-name.
        WHEN lc_createdby.
          Lrt_createdby = CORRESPONDING #( ls_filter_select_options-range ).
          DELETE lrt_createdby WHERE low IS INITIAL.

          READ TABLE lrt_createdby INTO DATA(lrs_createdby) INDEX 1.

*          IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL .

          IF sy-subrc EQ 0 AND lrs_createdby-low IS NOT INITIAL .  "BY LMETTA

            lv_mobileuser = lrs_createdby-low.

            TRANSLATE lv_mobileuser TO UPPER CASE.

          ENDIF.
      ENDCASE.
    ENDLOOP.
*EOC BY LMETTA

*        CASE lst_filter-name.
*          WHEN 'CreatedBy'.
*            READ TABLE lst_filter-select_options INTO lst_filter_range INDEX 1.
*            IF sy-subrc EQ 0 AND lst_filter_range-low IS NOT INITIAL .
*              lv_mobileuser = lst_filter_range-low.
*              TRANSLATE lv_mobileuser TO UPPER CASE.
*            ENDIF.
*        ENDCASE.
*      ENDLOOP.
*    ENDIF.
*ENDTRY.
    IF lv_mobileuser IS INITIAL.
      lv_mobileuser = sy-uname.
    ENDIF.

* Form Approval Check
    SELECT formid, version, workordernum, oprnum, approverid,notification, notificationitem,
           notificationtask, equipment, functionallocation,formname FROM /odsmfe/tb_fapvr
*      INTO CORRESPONDING FIELDS OF TABLE @lt_fapvr
      WHERE approverid = @lv_mobileuser INTO CORRESPONDING FIELDS OF TABLE @lt_fapvr.
* Form Response Capture Check
    IF lt_fapvr IS NOT INITIAL.

      SELECT * FROM /odsmfe/tb_forsp
*        INTO CORRESPONDING FIELDS OF TABLE @lt_respcap.
        FOR ALL ENTRIES IN @lt_fapvr
        WHERE wo_num = @lt_fapvr-workordernum
        AND formid = @lt_fapvr-formid
        AND version = @lt_fapvr-version
        AND isdraft = @space INTO CORRESPONDING FIELDS OF TABLE @lt_respcap.

      IF sy-subrc = 0.
        SELECT * FROM /odsmfe/tb_finst
*                 INTO CORRESPONDING FIELDS OF TABLE @lt_finst
                 FOR ALL ENTRIES IN @lt_respcap
                 WHERE forminstanceid EQ @lt_respcap-instanceid
                 AND formid = @lt_respcap-formid
                 AND version = @lt_respcap-version
                 AND counter = @lt_respcap-counter
                 AND approverid = @lv_mobileuser INTO CORRESPONDING FIELDS OF TABLE @lt_finst.
        LOOP AT lt_respcap INTO lwa_rescap.
          READ TABLE lt_finst INTO lwa_finst WITH KEY forminstanceid = lwa_rescap-instanceid.

LOOP AT lt_finst into lwa_finst.

lwa_finst_temp-FormID = lwa_finst-formid.
lwa_finst_temp-Version = lwa_finst-version.
lwa_finst_temp-InstanceID = lwa_finst-forminstanceid.
lwa_finst_temp-Counter =  lwa_finst-counter.
lwa_finst_temp-Remarks = lwa_finst-remarks.

append lwa_finst_temp to lt_finst_temp.

clear lwa_finst_temp.

ENDLOOP.
 MOVE-CORRESPONDING lt_finst_temp to ex_response_data. "BY LMETTA
          IF sy-subrc NE 0.

*            MOVE-CORRESPONDING lwa_rescap TO gstib_entity.    "BY LMETTA
** Get Entity method is requested
*            IF lst_key_tab IS NOT INITIAL.
*              GET REFERENCE OF gstib_entity INTO ex_entity.
*            ELSE.
*              APPEND gstib_entity TO gitib_entity.
*            ENDIF.
          ENDIF.
          CLEAR : lwa_rescap.
        ENDLOOP.
      ENDIF.
    ENDIF.
*    ENDLOOP.         "BY LMETTA
* Get EntitySet method is requested
*        IF gitib_entity IS NOT INITIAL.
*          GET REFERENCE OF gitib_entity INTO ex_entityset.
*        ENDIF.
*      ENDIF.
*    ENDIF.
* Third Table --> /ODSMFE/TB_FINST

   ENDMETHOD.
ENDCLASS.
