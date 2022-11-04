class /ODSMFE/CL_PM_UTIL definition
  public
  create public .

public section.

  class-methods METH_STAT_PUB_DYN_CONV_EXIT
    importing
      !IM_FNAM type BDCDATA-FNAM
    changing
      !CH_VALUE type ANY .
  class-methods METH_STAT_PUB_MOVE_CORR
    importing
      !IM_DR_SOURCE type ref to DATA
      !IM_INITIAL_CHK type FLAG optional
      !IM_INITIAL_CHK_SRC type FLAG optional
    changing
      !CH_DR_DESTINATION type ref to DATA .
  class-methods METH_DYNAMIC_FCAT
    importing
      !IM_DATA type ref to DATA optional
      !IM_FTEXT type SCRTEXT_L optional
      !IM_FNAM type FIELDNAM optional
    changing
      !CH_I_FCAT type LVC_T_FCAT optional .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_PM_UTIL IMPLEMENTATION.


  METHOD meth_dynamic_fcat.

    "/ Data for RTTS
    DATA: l_o_elemdescr   TYPE REF TO cl_abap_elemdescr,               " Class For Element Description
          l_o_typedescr   TYPE REF TO cl_abap_typedescr,               " Class For Type Description
          l_o_tabledescr  TYPE REF TO cl_abap_tabledescr,              " Class For Table Description
          l_o_structdescr TYPE REF TO cl_abap_structdescr,             " Class For Structure Description
          l_o_refdescr    TYPE REF TO cl_abap_refdescr,                " Class For Reference Description
          l_o_datadescr   TYPE REF TO cl_abap_datadescr,               " Class For Data Description
          l_o_clsdescr    TYPE REF TO cl_abap_classdescr.              " Class For Class Description

    "/ Method Data.
    DATA: l_i_components   TYPE abap_compdescr_tab,                    " structure component description
          l_i_comptab      TYPE abap_component_tab,                    " structure component description
          l_i_comptab_tmp  TYPE abap_component_tab,                    " structure component description
          l_i_comptab_tmp1 TYPE abap_component_tab,                    " structure component description
          lv_colpos        TYPE int4,                                  "Natural Number
          l_dr_dfies       TYPE REF TO dfies.                          " reference to data description type


    "/Field Symbols

    FIELD-SYMBOLS: <l_fs_comptab>    TYPE abap_componentdescr,         "
                   <l_fs_components> TYPE abap_compdescr,              "
                   <l_fs_param>      TYPE abap_parmdescr,              "
                   <l_fs_meth>       TYPE abap_methdescr,              "
                   <l_fs_fcat>       TYPE lvc_s_fcat.                  "ALV control: Field catalog

* ----------------------------------------------------------------------*
* --- END: Section DATA  * --- * --- * --- * --- * --- * --- * --- *****
* --- * --- * --- * --- * --- * --- * --- * --- * --- * --- * --- * ----*
* ----------------------------------------------------------------------*

*-------------------------------------------------------------------------------------------------------

    IF im_data IS BOUND.

* ----------------------------------------------------------------------*
* --- START: Section LOGIC  * --- * --- * --- * --- * --- * --- * --- **
* --- * --- * --- * --- * --- * --- * --- * --- * --- * --- * --- * ----*
* ----------------------------------------------------------------------*
      TRY .

          l_o_tabledescr ?= cl_abap_datadescr=>describe_by_data_ref( im_data ).

          IF l_o_tabledescr IS BOUND.
            l_o_structdescr ?= l_o_tabledescr->get_table_line_type( ).
          ENDIF.                                                       " IF L_O_TABLEDESCR IS BOUND Line No. :79

        CATCH cx_sy_move_cast_error .
* ----------------------------------------------------------------------*
* --- Find the attributes of Structure using Data Ref..-----------------*
* ----------------------------------------------------------------------*
          TRY .
* ----------------------------------------------------------------------*
* --- Describe the type get the components------------------------------*
* ----------------------------------------------------------------------*
              l_o_structdescr ?= cl_abap_datadescr=>describe_by_data_ref( im_data ).
            CATCH cx_sy_move_cast_error.
* ----------------------------------------------------------------------*
* --- Describe the type get the element---------------------------------*
* ----------------------------------------------------------------------*
              TRY.
                  l_o_elemdescr ?= cl_abap_datadescr=>describe_by_data_ref( im_data ).
                CATCH cx_sy_move_cast_error.
              ENDTRY.
          ENDTRY.
      ENDTRY.

* ----------------------------------------------------------------------*
* --- Create Data to hold the information of the data description-------*
* ----------------------------------------------------------------------*
      CREATE DATA l_dr_dfies.

      IF l_o_structdescr IS BOUND.
* ----------------------------------------------------------------------*
* --- Describe the type get the components------------------------------*
* ----------------------------------------------------------------------*
        l_i_comptab_tmp1 = l_i_comptab = l_o_structdescr->get_components( ).


        DELETE l_i_comptab WHERE as_include IS NOT INITIAL.

        LOOP AT l_i_comptab_tmp1 ASSIGNING <l_fs_comptab> WHERE as_include IS NOT INITIAL.

          CLEAR l_o_structdescr.

          TRY .
              l_o_structdescr ?= <l_fs_comptab>-type.
            CATCH cx_sy_move_cast_error.

          ENDTRY.


          CHECK l_o_structdescr IS BOUND.
          l_i_comptab_tmp = l_o_structdescr->get_components( ).


          APPEND LINES OF l_i_comptab_tmp TO l_i_comptab.

          CLEAR :l_i_comptab_tmp.


        ENDLOOP.                                                       " LOOP AT L_I_COMPTAB_TMP1 Line No. :117


        LOOP AT l_i_comptab ASSIGNING <l_fs_comptab>.

* ----------------------------------------------------------------------*
* --- Describe the Component Element.-----------------------------------*
* ----------------------------------------------------------------------*
          CLEAR l_o_elemdescr.
          TRY .
              l_o_typedescr ?= <l_fs_comptab>-type.
              IF l_o_typedescr->is_ddic_type( ) = abap_true.
                l_o_elemdescr ?= <l_fs_comptab>-type.
              ENDIF.                                                   " IF L_O_TYPEDESCR->IS_DDIC_TYPE( ) = ABAP_TRUE Line No. :148
            CATCH cx_sy_move_cast_error.

          ENDTRY.

* ----------------------------------------------------------------------*
* --- Describe the DDIC type of the elements.---------------------------*
* ----------------------------------------------------------------------*
          IF l_o_elemdescr IS BOUND.
            l_dr_dfies->* = l_o_elemdescr->get_ddic_field( sy-langu ).
            UNASSIGN <l_fs_fcat>.
            APPEND INITIAL LINE TO ch_i_fcat ASSIGNING <l_fs_fcat>.

* ----------------------------------------------------------------------*
* --- Move all the Info Needed for creating the Field Catalog-----------*
* ----------------------------------------------------------------------*
            IF  <l_fs_fcat> IS ASSIGNED.

              IF l_o_tabledescr IS BOUND.
                READ TABLE l_o_tabledescr->key WITH KEY name = <l_fs_comptab>-name TRANSPORTING NO FIELDS.

                IF sy-subrc IS INITIAL.
                  <l_fs_fcat>-key = abap_true.

                ENDIF.                                                 " IF SY-SUBRC IS INITIAL Line No. :171
              ENDIF.                                                   " IF L_O_TABLEDESCR IS BOUND Line No. :168
*          <l_fs_fcat>-tabname =    l_dr_dfies->*-tabname.
              <l_fs_fcat>-col_pos   = lv_colpos + 1.
              <l_fs_fcat>-fieldname = <l_fs_comptab>-name.
              <l_fs_fcat>-inttype   = l_dr_dfies->*-inttype.           "
              <l_fs_fcat>-seltext   = l_dr_dfies->*-scrtext_s.

              IF <l_fs_fcat>-seltext IS INITIAL.
                <l_fs_fcat>-seltext = <l_fs_fcat>-fieldname.
              ENDIF.                                                   " IF <L_FS_FCAT>-SELTEXT IS INITIAL Line No. :182

              <l_fs_fcat>-scrtext_s = l_dr_dfies->*-scrtext_s.
              <l_fs_fcat>-scrtext_m = l_dr_dfies->*-scrtext_m.
              <l_fs_fcat>-scrtext_l = l_dr_dfies->*-scrtext_l.
              <l_fs_fcat>-intlen    = l_dr_dfies->*-intlen.
* ----------------------------------------------------------------------*
* --- If fields are made Editable Below things will be usefull----------*
* ----------------------------------------------------------------------*
              <l_fs_fcat>-outputlen = l_dr_dfies->*-outputlen.
              <l_fs_fcat>-edit_mask = l_o_elemdescr->edit_mask.

              IF l_dr_dfies->*-inttype  EQ 'T'.
* ----------------------------------------------------------------------*
* --- This referencing will provide the F4 Help for the field-----------*
* ----------------------------------------------------------------------*
                <l_fs_fcat>-ref_field = 'UZEIT'.
                <l_fs_fcat>-ref_table = 'SYST'.
              ELSEIF l_dr_dfies->*-inttype  EQ 'D'.                          " IF L_DR_DFIES->*-INTTYPE EQ 'T' Line No. :196
* ----------------------------------------------------------------------*
* --- This referencing will provide the F4 Help for the field-----------*
* ----------------------------------------------------------------------*
                <l_fs_fcat>-ref_field = 'DATUM'.
                <l_fs_fcat>-ref_table = 'SYST'.
              ENDIF.                                                   " IF L_DR_DFIES->*-INTTYPE EQ 'T' Line No. :196

            ENDIF.                                                     " IF <L_FS_FCAT> IS ASSIGNED Line No. :166
          ENDIF.                                                       " IF L_O_ELEMDESCR IS BOUND Line No. :158

        ENDLOOP.                                                       " LOOP AT L_I_COMPTAB Line No. :140

      ELSEIF l_o_elemdescr IS BOUND.                                   " IF L_O_STRUCTDESCR IS BOUND Line No. :108

* ----------------------------------------------------------------------*
* --- Describe the DDIC type of the elements.---------------------------*
* ----------------------------------------------------------------------*
        l_dr_dfies->* = l_o_elemdescr->get_ddic_field( sy-langu ).
        UNASSIGN <l_fs_fcat>.
        APPEND INITIAL LINE TO ch_i_fcat ASSIGNING <l_fs_fcat>.

* ----------------------------------------------------------------------*
* --- Move all the Info Needed for creating the Field Catalog-----------*
* ----------------------------------------------------------------------*
        IF  <l_fs_fcat> IS ASSIGNED.
          <l_fs_fcat>-tabname   = l_dr_dfies->*-tabname.
          <l_fs_fcat>-fieldname = l_dr_dfies->*-fieldname.
          <l_fs_fcat>-inttype   = l_dr_dfies->*-inttype.               "
          <l_fs_fcat>-intlen    = l_dr_dfies->*-intlen.
          <l_fs_fcat>-outputlen = l_dr_dfies->*-outputlen.
* ----------------------------------------------------------------------*
* --- If Field Text is there use it else Use DDIC Text------------------*
* ----------------------------------------------------------------------*
          IF im_ftext IS INITIAL.
            <l_fs_fcat>-scrtext_s = l_dr_dfies->*-scrtext_s.
            <l_fs_fcat>-scrtext_m = l_dr_dfies->*-scrtext_m.
            <l_fs_fcat>-scrtext_l = l_dr_dfies->*-scrtext_l.
            <l_fs_fcat>-seltext   = l_dr_dfies->*-scrtext_s.

            IF <l_fs_fcat>-seltext IS INITIAL.
              <l_fs_fcat>-seltext = <l_fs_fcat>-fieldname.
            ENDIF.                                                     " IF <L_FS_FCAT>-SELTEXT IS INITIAL Line No. :242

          ELSE.                                                        " IF IM_FTEXT IS INITIAL Line No. :236
            <l_fs_fcat>-scrtext_s = im_ftext.
            <l_fs_fcat>-scrtext_m = im_ftext.
            <l_fs_fcat>-scrtext_l = im_ftext.

            <l_fs_fcat>-seltext   = im_ftext.                                                         " IF <L_FS_FCAT>-SELTEXT IS INITIAL Line No. :170

          ENDIF.                                                       " IF IM_FTEXT IS INITIAL Line No. :236
        ENDIF.                                                         " IF <L_FS_FCAT> IS ASSIGNED Line No. :227
      ENDIF.                                                           " IF L_O_STRUCTDESCR IS BOUND Line No. :108
    ENDIF.                                                             " IF IM_DATA IS BOUND Line No. :69

* ----------------------------------------------------------------------*
* --- Handle the custom text if any.------------------------------------*
* ----------------------------------------------------------------------*
    IF ch_i_fcat IS NOT INITIAL.
      IF im_ftext IS NOT INITIAL AND im_fnam IS NOT INITIAL.
        "/Not using binary search to avoid sort timing as there will be very few rows
        READ TABLE ch_i_fcat WITH KEY fieldname =  im_fnam ASSIGNING <l_fs_fcat>.

        IF sy-subrc IS INITIAL.
          <l_fs_fcat>-scrtext_s = im_ftext.
          <l_fs_fcat>-scrtext_m = im_ftext.
          <l_fs_fcat>-scrtext_l = im_ftext.
          <l_fs_fcat>-seltext   = im_ftext.                            " IF <L_FS_FCAT>-SELTEXT IS INITIAL Line No. :170

        ENDIF.                                                         " IF SY-SUBRC IS INITIAL Line No. :266
      ENDIF.                                                           " IF IM_FTEXT IS NOT INITIAL AND IM_FNAM IS NOT INITIAL Line No.
    ENDIF.                                                             " IF CH_I_FCAT IS NOT INITIAL Line No. :261

* ----------------------------------------------------------------------*
* --- END: Section LOGIC * --- * --- * --- * --- * --- * --- * --- ****
* --- * --- * --- * --- * --- * --- * --- * --- * --- * --- * --- * ----*
* ----------------------------------------------------------------------*

  ENDMETHOD.


METHOD meth_stat_pub_dyn_conv_exit.
* ----------------------------------------------------------------------*
* --- this method will apply the conversion exit dynamically -----------*
* --- if it is there on the domain -------------------------------------*
* ----------------------------------------------------------------------*


* ---------------------------------------------------------------------*
*                   D A T A   D E C L A R A T I O N                    *
* ---------------------------------------------------------------------*

* ----------------------------------------------------------------------*
* --- Internal Tables ---  --------------------------------------------*
* ----------------------------------------------------------------------*

* ----------------------------------------------------------------------*
* --- Variables ---  --------------------------------------------------*
* ----------------------------------------------------------------------*

  DATA :l_v_convexit      TYPE funcname,                             "Function name
        l_v_length        TYPE i,                                    "
        l_v_length_flg    TYPE char1,                                "Single-Character Indicator
        l_v_mask          TYPE string,                               "
        l_v_date_internal TYPE string,                               "
        l_v_time          TYPE sy-uzeit,                             "Current Time of Application Server
        l_wa_desc         TYPE dfies,                                "DD Interface: Table Fields for DDIF_FIELDINFO_GET
        l_v_char40        TYPE char40.                               "Character field of length 40

  DATA: l_v_name       TYPE eu_lname,                                "ABAP Workbench: LIMU Name
        l_wa_interface TYPE rsfbintfv,                               "Interface of a Function Module
        l_i_ptab       TYPE abap_func_parmbind_tab,                  "
        l_wa_ptab      TYPE abap_func_parmbind,                      "
        l_i_etab       TYPE abap_func_excpbind_tab,                  "
        l_wa_etab      TYPE abap_func_excpbind.                      "

  FIELD-SYMBOLS: <l_fs_excep> TYPE rsfbpara.


* ----------------------------------------------------------------------*
* --- Object References ---  ------------------------------------------*
* ----------------------------------------------------------------------*
  DATA :l_o_elemdescr TYPE REF TO cl_abap_elemdescr,                 "Run Time Type Services
        l_o_datadescr TYPE REF TO cl_abap_typedescr.                 "Run Time Type Services

* ----------------------------------------------------------------------*
* --- Data References ---  --------------------------------------------*
* ----------------------------------------------------------------------*
  DATA : l_o_data       TYPE REF TO data.

* ----------------------------------------------------------------------*
* --- Field Symbols ---  ----------------------------------------------*
* ----------------------------------------------------------------------*
  FIELD-SYMBOLS :<l_fs_ddic> TYPE x031l,                             "Nametab Structure, Database Structure DDNTF
                 <l_fs_type> TYPE any.                               "


* ---------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* ---------------------------------------------------------------------*


  TRY.

      CALL METHOD cl_abap_typedescr=>describe_by_name
        EXPORTING
          p_name         = im_fnam
        RECEIVING
          p_descr_ref    = l_o_datadescr
        EXCEPTIONS
          type_not_found = 1
          OTHERS         = 2.

      IF sy-subrc = 0.

        l_o_elemdescr ?= l_o_datadescr.

      ENDIF.                                                         " IF SY-SUBRC = 0 Line No. :102



    CATCH cx_sy_move_cast_error.
      " No Message needed as this conversion will not affect the Upload directly
  ENDTRY.

  IF l_o_elemdescr IS BOUND AND  l_o_elemdescr->edit_mask IS NOT INITIAL AND ch_value IS NOT INITIAL.

    l_v_length = l_o_elemdescr->output_length.
    l_v_mask   = l_o_elemdescr->edit_mask.

    REPLACE '==' IN l_v_mask WITH space.
    CONDENSE l_v_mask NO-GAPS.

    CREATE DATA l_o_data TYPE c LENGTH l_v_length.

    IF l_o_data IS BOUND.

      ASSIGN l_o_data->* TO <l_fs_type>.

      IF <l_fs_type> IS NOT ASSIGNED.

        EXIT.

      ENDIF.                                                         " IF <L_FS_TYPE> IS NOT ASSIGNED Line No. :128

    ELSE.                                                            " IF L_O_DATA IS BOUND Line No. :124

      EXIT.

    ENDIF.                                                           " IF L_O_DATA IS BOUND Line No. :124

    CONCATENATE 'CONVERSION_EXIT_' l_v_mask '_INPUT' INTO l_v_convexit.


    CLEAR l_i_ptab.

    l_wa_ptab-name = 'INPUT'.
    l_wa_ptab-kind = abap_func_exporting .
    GET REFERENCE OF ch_value INTO l_wa_ptab-value.
    INSERT l_wa_ptab INTO TABLE l_i_ptab.

    l_wa_ptab-name = 'OUTPUT'.
    l_wa_ptab-kind = abap_func_importing.
    GET REFERENCE OF <l_fs_type> INTO l_wa_ptab-value.
    INSERT l_wa_ptab INTO TABLE l_i_ptab.


    l_v_name = l_v_convexit.

    cl_fb_function_utility=>meth_get_interface(  EXPORTING
                                                     im_name             = l_v_name
                                                    IMPORTING
                                                      ex_interface        = l_wa_interface
                                                    EXCEPTIONS
                                                      error_occured       = 1
                                                      object_not_existing = 2
                                                      OTHERS              = 3
                                                        ).
    IF sy-subrc = 0.

      CLEAR l_i_etab.

      LOOP AT l_wa_interface-except ASSIGNING <l_fs_excep>.

        CLEAR l_wa_etab.

        l_wa_etab-name  = <l_fs_excep>-parameter .
        l_wa_etab-value = sy-tabix .

        INSERT l_wa_etab INTO TABLE l_i_etab.

      ENDLOOP.                                                       " LOOP AT L_WA_INTERFACE-EXCEPT Line No. :171

    ENDIF.                                                           " IF SY-SUBRC = 0 Line No. :167

* ----------------------------------------------------------------------*
* --- cALL THE fm DYNAMICALLY ------------------------------------------*
* ----------------------------------------------------------------------*

    CALL FUNCTION l_v_convexit
      PARAMETER-TABLE
      l_i_ptab
      EXCEPTION-TABLE
      l_i_etab.

    IF sy-subrc EQ 0.

* ----------------------------------------------------------------------*
* --- If there is no conversion exit on the domain or ------------------*
* --- if any error is there in initializing the description class ------*
* --- then we will send the same data back else new data ---------------*
* ----------------------------------------------------------------------*
      ch_value = <l_fs_type>.

    ENDIF.                                                           " IF SY-SUBRC EQ 0 Line No. :199

    FREE l_o_data.

    UNASSIGN <l_fs_type>.

*******
    " write the conversions for the date fields
  ELSEIF l_o_elemdescr IS BOUND AND l_o_elemdescr->type_kind EQ 'D'.                          " IF L_O_ELEMDESCR IS BOUND AND L_O_ELEMDESCR->EDIT_MASK IS NOT L

    IF strlen( ch_value ) GT 8.

      REPLACE ALL OCCURRENCES OF '/' IN ch_value WITH '.'.
      REPLACE ALL OCCURRENCES OF '-' IN ch_value WITH '.'.

* ----------------------------------------------------------------------*
* --- Call the FM to get the date in internal format -------------------*
* ----------------------------------------------------------------------*
      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
        EXPORTING
          date_external            = ch_value
          accept_initial_date      = abap_true
        IMPORTING
          date_internal            = l_v_date_internal
        EXCEPTIONS
          date_external_is_invalid = 1
          OTHERS                   = 2.

      IF sy-subrc EQ 0.
* ----------------------------------------------------------------------*
* --- No Error is required  we just need to prevent the Dump -----------*
* ----------------------------------------------------------------------*
        ch_value = l_v_date_internal.

      ENDIF.                                                         " IF SY-SUBRC EQ 0 Line No. :235
    ENDIF.                                                           " IF STRLEN( CH_VALUE ) GT 8 Line No. :217

  ELSEIF l_o_elemdescr IS BOUND AND l_o_elemdescr->type_kind EQ 'T'.                          " IF L_O_ELEMDESCR IS BOUND AND L_O_ELEMDESCR->EDIT_MASK IS NOT L

* ----------------------------------------------------------------------*
* --- Call the FM to get the TIME in internal format -------------------*
* ----------------------------------------------------------------------*
    TRY.

        CALL FUNCTION 'CONVERT_TIME_INPUT'
          EXPORTING
            input                     = ch_value
            plausibility_check        = abap_true
          IMPORTING
            output                    = l_v_time
          EXCEPTIONS
            plausibility_check_failed = 1
            wrong_format_in_input     = 2
            OTHERS                    = 3.
      CATCH cx_root.

    ENDTRY.
    IF sy-subrc = 0.
      ch_value = l_v_time.
    ENDIF.                                                           " IF SY-SUBRC = 0 Line No. :264
* ----------------------------------------------------------------------*
* --- No Error is required  we just need to prevent the Dump -----------*
* ----------------------------------------------------------------------*
  ELSEIF l_o_elemdescr IS BOUND AND l_o_elemdescr->type_kind EQ 'P'.                          " IF L_O_ELEMDESCR IS BOUND AND L_O_ELEMDESCR->EDIT_MASK IS NOT L

    CONDENSE ch_value NO-GAPS.

    IF l_o_elemdescr->absolute_name CS 'TSEGEVTFRS' OR  l_o_elemdescr->absolute_name CS 'TSEGEVTTOS' .

      l_v_char40 = ch_value.

      CALL FUNCTION 'SF_SPECIALCHAR_DELETE'
        EXPORTING
          with_specialchar    = l_v_char40
        IMPORTING
          without_specialchar = l_v_char40
        EXCEPTIONS
          result_word_empty   = 1
          OTHERS              = 2.

      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.                                                         " IF SY-SUBRC <> 0 Line No. :287

      ch_value =   l_v_char40.

    ENDIF.                                                           " IF L_O_ELEMDESCR->ABSOLUTE_NAME CS 'TSEGEVTFRS' OR L_O_ELEMDE L

  ELSEIF  l_o_elemdescr IS BOUND AND l_o_elemdescr->type_kind EQ 'C'.                          " IF L_O_ELEMDESCR IS BOUND AND L_O_ELEMDESCR->EDIT_MASK IS NOT L

    l_wa_desc =  l_o_elemdescr->get_ddic_field( ).

    IF  l_wa_desc-lowercase IS INITIAL.

      TRANSLATE ch_value TO UPPER CASE.


    ENDIF.                                                           " IF L_WA_DESC-LOWERCASE IS INITIAL Line No. :299

  ENDIF.                                                             " IF L_O_ELEMDESCR IS BOUND AND L_O_ELEMDESCR->EDIT_MASK IS NOT L


ENDMETHOD.


METHOD meth_stat_pub_move_corr.

***** START: Section DATA  *********************************************
***** Data Declarations  ***********************************************
************************************************************************

* ----------------------------------------------------------------------*
* --- Internal Tables ---  *********************************************
* ----------------------------------------------------------------------*



* ----------------------------------------------------------------------*
* --- Object References ---  *******************************************
* ----------------------------------------------------------------------*
  " Data for RTTS
  DATA: l_o_structdescr TYPE REF TO cl_abap_structdescr.             " Class For Structure Description

* ----------------------------------------------------------------------*
* --- Data References ---  *********************************************
* ----------------------------------------------------------------------*
  DATA: l_i_components TYPE abap_compdescr_tab,                      " structure component description
        l_i_comptab    TYPE abap_component_tab,                      " structure component description
        l_dr_dfies     TYPE REF TO dfies.                            " reference to data description type

* ----------------------------------------------------------------------*
* --- Field Symbols ---  ***********************************************
* ----------------------------------------------------------------------*
  FIELD-SYMBOLS: <l_fs_comptab>      TYPE abap_compdescr,            " Comments
                 <l_fs_struc_dest>   TYPE any,                       " Comments
                 <l_fs_struc_source> TYPE any,                       " Source Structure
                 <l_fs_data_dest>    TYPE any,                       " Destination Data
                 <l_fs_data_source>  TYPE any.                       " Source Data.

***** END: Section DATA  ***********************************************
***** Data Declarations  ***********************************************
************************************************************************
  CHECK im_dr_source IS NOT INITIAL.
  CHECK ch_dr_destination IS NOT INITIAL.
* ----------------------------------------------------------------------*
* --- Find the attributes of Structure using Data Ref.. ----------------*
* ----------------------------------------------------------------------*
  TRY .
* ----------------------------------------------------------------------*
* --- Describe the type get the components -----------------------------*
* ----------------------------------------------------------------------*
      l_o_structdescr ?= cl_abap_datadescr=>describe_by_data_ref( im_dr_source ).
    CATCH cx_sy_move_cast_error.

  ENDTRY.
* ----------------------------------------------------------------------*
* --- Assign the Structure from source and destination -----------------*
* ----------------------------------------------------------------------*
  ASSIGN im_dr_source->* TO <l_fs_struc_source>.
  ASSIGN ch_dr_destination->* TO <l_fs_struc_dest>.

  CHECK l_o_structdescr IS BOUND AND
    <l_fs_struc_source> IS ASSIGNED AND
      <l_fs_struc_dest> IS ASSIGNED .

* ----------------------------------------------------------------------*
* --- loop on the components -------------------------------------------*
* ----------------------------------------------------------------------*
  LOOP AT l_o_structdescr->components ASSIGNING <l_fs_comptab>.
    UNASSIGN <l_fs_data_source>.
* ----------------------------------------------------------------------*
* --- Try to assign the component of same anme and the move the data. --*
* ----------------------------------------------------------------------*
    ASSIGN COMPONENT <l_fs_comptab>-name OF STRUCTURE <l_fs_struc_source> TO <l_fs_data_source>.                " Get the Component from the source
    IF  <l_fs_data_source> IS ASSIGNED.
      " Check if the souce is initial or not
      IF im_initial_chk_src EQ abap_true.
        IF  <l_fs_data_source> IS INITIAL OR <l_fs_data_source> EQ space.
          CONTINUE.
        ENDIF.                                                       " IF <L_FS_DATA_DEST> IS INITIAL Line No. :100

      ENDIF.                                                         " IF IM_INITIAL_CHK_SRC EQ ABAP_TRUE Line No. :99

      UNASSIGN <l_fs_data_dest>.
      ASSIGN COMPONENT <l_fs_comptab>-name OF STRUCTURE <l_fs_struc_dest> TO <l_fs_data_dest>.                  " Get the Component from the destination
      CHECK  <l_fs_data_dest> IS ASSIGNED.
      CASE im_initial_chk.                                           " this check will move the data only if destination is not Empty else it will retail the old Data
        WHEN abap_true.                                              " CASE IM_INITIAL_CHK Line No. :109
          IF  <l_fs_data_dest> IS INITIAL.
            <l_fs_data_dest> = <l_fs_data_source>.
          ENDIF.                                                     " IF <L_FS_DATA_DEST> IS INITIAL Line No. :111
        WHEN abap_false.                                             " CASE IM_INITIAL_CHK Line No. :109
          <l_fs_data_dest> = <l_fs_data_source>.
      ENDCASE.                                                       " CASE IM_INITIAL_CHK Line No. :109
    ENDIF.                                                           " IF <L_FS_DATA_SOURCE> IS ASSIGNED Line No. :97
  ENDLOOP.                                                           " LOOP AT L_O_STRUCTDESCR->COMPONENTS Line No. :91

ENDMETHOD.
ENDCLASS.
