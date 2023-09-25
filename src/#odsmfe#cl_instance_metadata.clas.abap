class /ODSMFE/CL_INSTANCE_METADATA definition
  public
  inheriting from /ODSMFE/CL_GET_ENT_SUPER_BAPI
  create public .

public section.
  methods /ODSMFE/IF_GET_ENTITYSET_BAPI~GMIB_READ_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_INSTANCE_METADATA IMPLEMENTATION.


  METHOD /odsmfe/if_get_entityset_bapi~gmib_read_entityset.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   : ODS - PIYYAPPAN
* Creation Date          : 10/04/2023
* Transport No.          : ES1K903465
* Program Description    : To display the Instance Metadata
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
    TYPES: BEGIN OF ltys_forsp,
             instanceid  TYPE /odsmfe/tb_forsp-instanceid,                      "InstanceID
             formid      TYPE /odsmfe/tb_forsp-formid,                          "FormID
             version     TYPE /odsmfe/tb_forsp-version,                         "Version
             wonum       TYPE aufnr,                                            "WorkOrderNumber
             vornr       TYPE c LENGTH 4,                                            "OperationNumber
             createddate TYPE dats,                                             "CreatedDate
             createdby   TYPE /odsmfe/tb_forsp-created_by,                      "CreatedBy
           END OF ltys_forsp,

           BEGIN OF ltys_afih,
             wonum TYPE aufnr,                                                  "WorkOrderNumber
             equnr TYPE c LENGTH 18,                                                  "EquipmentNumber
             iloan TYPE c LENGTH 12,                                                  "Location/Account assignment
           END OF ltys_afih,

           BEGIN OF ltys_iloa,
             iloan TYPE c LENGTH 12,                                                  "Location/Account assignment
             tplnr TYPE c LENGTH 30,                                                  "Functional Location
           END OF ltys_iloa.

     TYPES: BEGIN OF ty_data,
                      wa TYPE c LENGTH 512,
                   END OF ty_data,

                   BEGIN OF ty_options,
                       text(72) TYPE c,
                   END OF ty_options,

                   BEGIN OF ty_fields,
                        fieldname(30) TYPE c,
                        offset(6)     TYPE n,
                        length(6)     TYPE n,
                        type(1)       TYPE c,
                        fieldtext(60) TYPE c,
                   END OF ty_fields.


    "/ Tables and Structures
    DATA: lit_forsp TYPE TABLE OF ltys_forsp,                                   "Types structure for form response
          lst_forsp TYPE ltys_forsp,                                            "Types structure for form resposne
          lit_afih  TYPE TABLE OF ltys_afih,                                    "Types structure for Maintenance order header
          lst_afih  TYPE ltys_afih,                                             "Types structure for Maintenance order header
          lit_iloa  TYPE TABLE OF ltys_iloa,                                    "Types structure for PM Object Location and Account Assignment
          lst_iloa  TYPE ltys_iloa,                                             "Types structure for PM Object Location and Account Assignment
          lit_final TYPE TABLE OF /ODSMFE/CE_INSTANCE_METADATA,                 "Structure for Instance Metadata
          lst_final TYPE /ODSMFE/CE_INSTANCE_METADATA,                          "Structure for Instance Metadata
          lr_rfc    TYPE REF TO  /ODSMFE/CL_GET_ENT_SUPER_BAPI.

      DATA : lit_fields TYPE TABLE OF ty_fields,
                 lit_options TYPE TABLE OF ty_options,
                 lit_data TYPE TABLE OF ty_data.

    "/ Variables
    DATA :
           lrt_instanceid    TYPE TABLE OF /odsmfe/st_core_range_str,           "InstanceID Range Table
           lrt_formid        TYPE TABLE OF /odsmfe/st_core_range_str,           "FormID Range Table
           lrt_version       TYPE TABLE OF /odsmfe/st_core_range_str,           "FormID Range Table
           lrt_date            TYPE TABLE OF /odsmfe/st_core_range_str,
           lrs_it_date       TYPE /odsmfe/st_core_range_str,                    "Date Range Structure
           lv_startdate      TYPE dats,
           lv_enddate        TYPE dats.

    "/Constants
    CONSTANTS: lc_i                 TYPE string     VALUE 'I',                     "Single-Character Indicator
               lc_bt                TYPE string     VALUE 'BT',                    "Version Number Component
               lc_eq                TYPE string     VALUE 'EQ',                    "Version Number Component
               lc_startdate         TYPE string     VALUE 'STARTDATE',             "StartDate
               lc_enddate           TYPE string     VALUE 'ENDDATE',               "EndDate
               lc_key_instanceid    TYPE string     VALUE 'InstanceId',            "Key Values - InstanceID
               lc_filtr_instanceid  TYPE string     VALUE 'INSTANCEID',            "Filter Values - InstanceID
               lc_formid            TYPE string     VALUE 'FORMID',                "FormId
               lc_version           TYPE string     VALUE 'VERSION'.               "Version

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

    "/ Read filter Property Values
    IF im_filter_select_options[] IS NOT INITIAL.

      LOOP AT im_filter_select_options INTO DATA(lst_filter).
        TRANSLATE lst_filter-name TO UPPER CASE.
        CASE lst_filter-name.

          WHEN lc_startdate.
            READ TABLE lst_filter-range INTO data(lst_filter_values) INDEX 1.
            lv_startdate = lst_filter_values-low.

          WHEN lc_enddate.
            READ TABLE lst_filter-range INTO lst_filter_values INDEX 1.
            lv_enddate = lst_filter_values-low.

          WHEN lc_filtr_instanceid.
            lrt_instanceid = CORRESPONDING #( lst_filter-range ).
            DELETE lrt_instanceid WHERE low IS INITIAL.

          WHEN lc_formid.
            lrt_formid = CORRESPONDING #( lst_filter-range ).
            DELETE lrt_formid WHERE low IS INITIAL.

          WHEN lc_version.
            lrt_version = CORRESPONDING #( lst_filter-range ).
            DELETE lrt_version WHERE low IS INITIAL.

        ENDCASE. "/ Case lst_filter-property.
      ENDLOOP. "/Loop at im_filter_select_options into lst_filter.

      IF lv_startdate IS NOT INITIAL AND lv_enddate IS NOT INITIAL.
        "/Filling the range table
        lrs_it_date-sign    = lc_i.
        lrs_it_date-option  = lc_bt.
        lrs_it_date-low     = lv_startdate.
        lrs_it_date-high    = lv_enddate.
        APPEND lrs_it_date TO lrt_date.
        CLEAR: lrs_it_date.
      ENDIF. "/If lv_startdate and lv_enddate is not initial.

    ENDIF. "/If im_filter_select_options[] is not initial.

    "/To get the data from Response table based on startdate, enddate and instanceid
    SELECT instanceid,
         formid,
         version,
         wo_num,
         vornr,
         created_date,
         created_by
    FROM /odsmfe/tb_forsp
    WHERE created_date IN @lrt_date[]
    AND   instanceid   IN @lrt_instanceid[]
    AND   formid       IN @lrt_formid[]
    AND   version      IN @lrt_version[]
     INTO TABLE @lit_forsp.

    IF lit_forsp[] IS NOT INITIAL.
      "/To get equipment number, Location/Account assignment based on workorder number
*      SELECT aufnr,
*             equnr,
*             iloan
*        FROM afih
*        INTO TABLE @lit_afih
*        FOR ALL ENTRIES IN @lit_forsp
*        WHERE aufnr = @lit_forsp-wonum.

     CREATE OBJECT lr_rfc
       EXPORTING
         im_entity_name           =  im_entity_name .

      CALL METHOD lr_rfc->get_cloud_dest
        IMPORTING
          ex_dest = DATA(lv_rfc).

       LOOP AT lit_forsp INTO lst_forsp.

              lit_fields = VALUE #( ( fieldname = 'AUFNR' )
                                               ( fieldname = 'EQUNR' )
                                               ( fieldname = 'ILOAN' ) ).

              IF lst_forsp-wonum IS NOT INITIAL.

                   lit_options = VALUE #( ( text = |AUFNR| & | | & |EQ| & | | & |'| & |{ lst_forsp-wonum }| & |'|  ) ).

              ENDIF. "/  IF lst_forsp-wonum IS NOT INITIAL.

              CALL FUNCTION 'RFC_READ_TABLE'
              DESTINATION lv_rfc
              EXPORTING
              query_table = 'AFIH'
              TABLES
              data = lit_data
              options = lit_options
              fields = lit_fields .

             lit_afih = VALUE #( FOR lst_data IN lit_data
                                  ( wonum = lst_data+0(12) )
                                  ( equnr = lst_data+12(18) )
                                  ( iloan = lst_data+30(12) ) ).

       ENDLOOP. "/LOOP AT lit_forsp INTO lst_forsp.

      IF lit_afih[] IS NOT INITIAL.
        "/To get functional location based on Location/Account assignment

        LOOP AT lit_afih INTO lst_afih.

            lit_fields = VALUE #( ( fieldname = 'ILOAN' )
                                              ( fieldname = 'TPLNR' ) ).

           IF lst_afih-iloan IS NOT INITIAL.

                lit_options = VALUE #( ( text = |ILOAN| & | | & |EQ| & | | & |'| & |{ lst_afih-iloan }| & |'|  ) ).

           ENDIF. "/ IF lst_afih-iloan IS NOT INITIAL.

           CALL FUNCTION 'RFC_READ_TABLE'
           DESTINATION lv_rfc
           EXPORTING
           query_table  = 'ILOA'
           TABLES
           data = lit_data
           fields = lit_fields
           options = lit_options .

           lit_iloa  = VALUE #( FOR lst_data in lit_data
                                  ( iloan = lst_data+0(12) )
                                  ( tplnr = lst_data+12(30) ) ).


        ENDLOOP. "/LOOP AT lit_afih INTO lst_afih.

*        SELECT iloan
*               tplnr
*          FROM iloa
*          INTO TABLE lit_iloa
*          FOR ALL ENTRIES IN lit_afih
*          WHERE iloan = lit_afih-iloan.

      ENDIF. "/If lit_afih is not initial.

    ENDIF. "/If lit_forsp[] is not initial.

    IF sy-subrc EQ 0.
      "/Exporting all the values into final internal table
      LOOP AT lit_forsp INTO lst_forsp.
        lst_final-instanceid      = lst_forsp-instanceid.
        lst_final-formid          = lst_forsp-formid.
        lst_final-version         = lst_forsp-version.
        lst_final-WorkOrderNum          = lst_forsp-wonum.
        lst_final-OperationNum           = lst_forsp-vornr.
        lst_final-CreatedDate    = lst_forsp-createddate.
        lst_final-StartDate     = lv_startdate.
        lst_final-EndDate       = lv_enddate.
        lst_final-CreatedBy      = lst_forsp-createdby.
        READ TABLE lit_afih INTO lst_afih WITH KEY wonum = lst_forsp-wonum.
        lst_final-EquipmentNum           = lst_afih-equnr.
        READ TABLE lit_iloa INTO lst_iloa WITH KEY iloan = lst_afih-iloan.
        lst_final-FunctionalLocation           = lst_iloa-tplnr.
        APPEND lst_final TO lit_final.
        CLEAR : lst_final.

      ENDLOOP. "/Loop at lit_forsp into lst_forsp.
    ENDIF. "/If sy-subrc eq 0.

    "/Mapping properties from the backend to the Gateway output response table
    ex_response_data = CORRESPONDING #( lit_final ).

  ENDMETHOD.
ENDCLASS.
