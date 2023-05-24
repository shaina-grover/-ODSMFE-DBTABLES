class /ODSMFE/CL_APPUSER definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  final
  create public .

public section.
  type-pools ABAP .

  data GSTIB_ENTITY type /ODSMFE/CL_PR_MASTER_S_MPC_EXT=>TS_APPUSERTABLE .
  data GITIB_ENTITY type /ODSMFE/CL_PR_MASTER_S_MPC_EXT=>TT_APPUSERTABLE .

  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_APPUSER IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS - PIYYAPPAN
* Creation Date          :05/04/2023
* Transport No.          :ES1K903727
* Program Description    :To display the user data based on the user id
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
    TYPES:BEGIN OF ltys_addr,
            userid    TYPE user_addr-bname,                                    "User ID
            firstname TYPE user_addr-name_first,                               "FirstName
            lastname  TYPE user_addr-name_last,                                "LastName
            fullname  TYPE user_addr-name_textc,                               "FullName
          END OF ltys_addr,

          BEGIN OF ltys_appuser,
             firstname TYPE user_addr-name_first,                              "FirstName
             lastname  TYPE user_addr-name_last,                               "LastName
             fullname  TYPE user_addr-name_textc,                              "FullName
             emailid   TYPE adr6-smtp_addr,                                    "EmailID
           END OF ltys_appuser.

    "/ Tables and Structures
    DATA: lst_filter  TYPE  /iwbep/s_mgw_select_option,                        "MGW Framework: Selection Option Parameters for db selects
          lit_appuser TYPE TABLE OF ltys_appuser,                              "Types structure for AppUser
          lst_appuser TYPE ltys_appuser,                                       "Types structure for AppUser
          lit_final   TYPE TABLE OF /odsmfe/st_user_details,                   "Structure for user details
          lst_final   TYPE /odsmfe/st_user_details,                            "Structure for user details
          lst_addr    TYPE ltys_addr.                                          "Local type structure for user address data

    "/Range Tables and Range Structures
    DATA: lrs_filter_range   TYPE /iwbep/s_cod_select_option.                  "MGW Framework: Select Options for Queries

    "/ Variables
    DATA: lv_recno        TYPE i,                                              "Record Number
          lv_persnumber   TYPE usr21-persnumber,                               "Person number
          lv_emailid      TYPE adr6-smtp_addr,                                 "Email ID
          lv_mobileuser   TYPE XUBNAME.                                        "User Name

    DATA:  lit_defaults  TYPE STANDARD TABLE OF bapidefaul,                     "Structure with User Defaults
           lst_defaults  TYPE bapidefaul,                                       "Structure with User Defaults
           lit_parameter TYPE STANDARD TABLE OF bapiparam,                      "Table with User Parameters
           lst_parameter TYPE bapiparam,                                        "Table with User Parameters
           lit_parva     TYPE STANDARD TABLE OF bapiparam,                      "Table with User Parameters
           lst_parva     TYPE bapiparam,                                        "Table with User Parameters
           lit_return    TYPE STANDARD TABLE OF bapiret2.                       "Return Structure

    "/Constants
    CONSTANTS: lc_hr_user           TYPE string    VALUE 'HR.USER',             "HR.User
               lc_param_firstname   TYPE string    VALUE 'FIRSTNAME',           "FirstName
               lc_param_lastname    TYPE string    VALUE 'LASTNAME',            "LastName
               lc_param_fullname    TYPE string    VALUE 'FULLNAME',            "FullName
               lc_param_emailid     TYPE string    VALUE 'EMAILID',             "EmailID
               lc_enteredby         TYPE string    VALUE 'ENTEREDBY'.           "EnteredBy

    "/Field Symbols
    FIELD-SYMBOLS: <lfsst_appuser> LIKE LINE OF lit_appuser,
                   <lfsst_final>   LIKE LINE OF lit_final.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

    "/ Read filter Property Values
    IF im_filter_select_options IS NOT INITIAL.

      LOOP AT im_filter_select_options INTO lst_filter.
        TRANSLATE lst_filter-property TO UPPER CASE.
        CASE lst_filter-property.
          WHEN lc_enteredby.
            READ TABLE lst_filter-select_options INTO lrs_filter_range INDEX 1.
            lv_mobileuser = lrs_filter_range-low.
        ENDCASE.  "/ Case lst_filter-property.
      ENDLOOP.  "/Loop at im_filter_select_options into lst_filter.

    ENDIF.  "/IF im_filter_select_options is not initial.

    IF lv_mobileuser IS INITIAL.
      lv_mobileuser = sy-uname.
    ENDIF. "/If lv_mobileuser is initial.

    "/Fetch firstname, lastname and fullname based on userid
    SELECT SINGLE bname                 ##WARN_OK
                  name_first
                  name_last
                  name_textc
      FROM user_addr
      INTO lst_addr
      WHERE bname eq lv_mobileuser.

    IF lst_addr IS NOT INITIAL.
      "/Fetch person number based on userid
      SELECT SINGLE persnumber      ##WARN_OK
        FROM usr21
        INTO lv_persnumber
        WHERE bname = lst_addr-userid.

      IF lv_persnumber IS NOT INITIAL.
        "/Fetch emailid based on person number
        SELECT SINGLE smtp_addr        ##WARN_OK
          FROM adr6
          INTO lv_emailid
          WHERE persnumber = lv_persnumber.    "#EC CI_NOFIRST.
      ENDIF. "/If lv_persnumber is not initial.

    ENDIF. "/If lst_addr is not initial.

    lst_appuser-firstname = lst_addr-firstname.
    lst_appuser-lastname  = lst_addr-lastname.
    lst_appuser-fullname  = lst_addr-fullname.
    lst_appuser-emailid   = lv_emailid.
    APPEND lst_appuser TO lit_appuser.
    CLEAR: lst_appuser, lv_emailid, lst_addr, lv_persnumber.

    "/Get all the user parameters based on the userid
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username  = lv_mobileuser
      IMPORTING
        defaults  = lst_defaults
      TABLES
        parameter = lit_parameter
        return    = lit_return.

    IF lit_appuser[] IS NOT INITIAL.
      READ TABLE lit_appuser ASSIGNING <lfsst_appuser> INDEX 1.

      "/Exporting the data to final internal table
      APPEND INITIAL LINE TO lit_final ASSIGNING <lfsst_final>.
      ADD 1 TO lv_recno.
      <lfsst_final>-recordid       = lv_recno.
      <lfsst_final>-settinggrp     = lc_hr_user.
      <lfsst_final>-settingname    = lc_param_firstname.
      <lfsst_final>-settingvalue   = <lfsst_appuser>-firstname.

      APPEND INITIAL LINE TO lit_final ASSIGNING <lfsst_final>.
      ADD 1 TO lv_recno.
      <lfsst_final>-recordid       = lv_recno.
      <lfsst_final>-settinggrp     = lc_hr_user.
      <lfsst_final>-settingname    = lc_param_lastname.
      <lfsst_final>-settingvalue   = <lfsst_appuser>-lastname.

      APPEND INITIAL LINE TO lit_final ASSIGNING <lfsst_final>.
      ADD 1 TO lv_recno.
      <lfsst_final>-recordid       = lv_recno.
      <lfsst_final>-settinggrp     = lc_hr_user.
      <lfsst_final>-settingname    = lc_param_fullname.
      <lfsst_final>-settingvalue   = <lfsst_appuser>-fullname.

      APPEND INITIAL LINE TO lit_final ASSIGNING <lfsst_final>.
      ADD 1 TO lv_recno.
      <lfsst_final>-recordid       = lv_recno.
      <lfsst_final>-settinggrp     = lc_hr_user.
      <lfsst_final>-settingname    = lc_param_emailid.
      <lfsst_final>-settingvalue   = <lfsst_appuser>-emailid.

      LOOP AT lit_parameter INTO lst_parameter.
        SPLIT lst_parameter-parva AT ',' INTO TABLE lit_parva.
        LOOP AT lit_parva INTO lst_parva.
          APPEND INITIAL LINE TO lit_final ASSIGNING <lfsst_final>.
          ADD 1 TO lv_recno.
          <lfsst_final>-recordid     = lv_recno.
          <lfsst_final>-settinggrp   = lc_hr_user.
          <lfsst_final>-settingname  = lst_parameter-parid.
          <lfsst_final>-settingvalue = lst_parva.
        ENDLOOP. "/Loop at lit_parva into lst_parva.
      ENDLOOP. "/Loop at lit_parameter into lst_parameter.

    ENDIF. "/If lit_appuser is not initial.

    "/Mapping properties from the backend to the Gateway output response table
    gitib_entity[] = lit_final[].
    GET REFERENCE OF gitib_entity INTO ex_entityset.


  ENDMETHOD.
ENDCLASS.
