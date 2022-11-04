class /ODSMFE/CL_MODEL_EXTENSION definition
  public
  create public .

public section.

  interfaces /ODSMFE/IF_MODEL_EXTENSION .

  methods CONSTRUCTOR
    importing
      !IM_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL
      !IM_PROJECT type ref to /IWBEP/CL_MGW_PUSH_ABS_MODEL .
  PROTECTED SECTION.
private section.

  data GVII_ENTITY_NAME type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_ENTITY_NAME .
  data:
    gt_mdl     TYPE TABLE OF /odsmfe/tb_mdp .                          "Model Provider Class Extentions
  data GOII_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL .
  data GOII_PROJECT type ref to /IWBEP/CL_MGW_PUSH_ABS_MODEL .

  methods GMII_SET_TYPE
    importing
      !IM_PROP_TYPE type /IWBEP/SBOD_EDM_CORE_TYPE
      !IM_PROPERTY type ref to /IWBEP/IF_MGW_ODATA_PROPERTY .
ENDCLASS.



CLASS /ODSMFE/CL_MODEL_EXTENSION IMPLEMENTATION.


  METHOD /odsmfe/if_model_extension~gmib_add_properties.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :30.06.2021
* Transport No.          :ES1K902703
* Program Description    :
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

    FIELD-SYMBOLS: <lfsst_mdl> TYPE /odsmfe/tb_mdp.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

* ----------------------------------------------------------------------*
* --- loop through all the properties that needs to be added------------*
* --- and properties foe each one---------------------------------------*
* ----------------------------------------------------------------------*

    LOOP AT   gt_mdl ASSIGNING <lfsst_mdl> WHERE entity = im_entity_name.


      me->/odsmfe/if_model_extension~gmib_add_property( EXPORTING im_entity_name = im_entity_name
                                                                  im_entity_type = im_entity_type
                                                                  im_project = im_project
                                                                  im_model = im_model
                                                                  im_mdl = im_mdl ).

    ENDLOOP.

    "/Please Create a Z class in client enviornment and then maintain the /odsmfe/tb_mdp table
    "/with the structure name to bind here, same structure will have to be reffered in the data provider as well
    DATA: lv_struct TYPE string.
    DATA: iv_bind_conversions TYPE abap_bool.

    lv_struct = im_mdl-structure.

    CONDENSE lv_struct.

    TRY .

        im_entity_type->bind_structure( iv_structure_name = lv_struct
                                        iv_bind_conversions =  'X' ). "#EC NOTEXT

      CATCH /iwbep/cx_mgw_med_exception.

    ENDTRY.

***********************************************************************************************************************************
*   ENTITY SETS
***********************************************************************************************************************************
    DATA: lv_setname    TYPE /iwbep/if_mgw_med_odata_types=>ty_e_med_entity_name,
          lo_entity_set TYPE REF TO /iwbep/if_mgw_odata_entity_set. "#EC NEEDED

    CONCATENATE im_entity_name 'Set' INTO lv_setname.

    TRY .

        lo_entity_set = im_entity_type->create_entity_set( lv_setname ). "#EC NOTEXT

      CATCH /iwbep/cx_mgw_med_exception.

        EXIT.
    ENDTRY.

    lo_entity_set->set_creatable( abap_false ).
    lo_entity_set->set_updatable( abap_false ).
    lo_entity_set->set_deletable( abap_false ).
    lo_entity_set->set_pageable( abap_false ).
    lo_entity_set->set_addressable( abap_true ).
    lo_entity_set->set_has_ftxt_search( abap_false ).
    lo_entity_set->set_subscribable( abap_false ).
    lo_entity_set->set_filter_required( abap_false ).

  ENDMETHOD.


  METHOD /odsmfe/if_model_extension~gmib_add_property.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :30.06.2021
* Transport No.          :ES1K902703
* Program Description    :
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

    DATA: lo_annotation   TYPE REF TO /iwbep/if_mgw_odata_annotation, "#EC NEEDED
          lo_complex_type TYPE REF TO /iwbep/if_mgw_odata_cmplx_type, "#EC NEEDED
          lo_property     TYPE REF TO /iwbep/if_mgw_odata_property, "#EC NEEDED
          lo_entity_set   TYPE REF TO /iwbep/if_mgw_odata_entity_set. "#EC NEEDED


* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

    CHECK im_mdl IS NOT INITIAL.

    lo_property = im_entity_type->create_property( iv_property_name = im_mdl-property iv_abap_fieldname = im_mdl-abap_field ). "#EC NOTEXT

    lo_property->set_is_key( im_mdl-is_key ).

    IF im_mdl-edm_core_type IS NOT INITIAL.
      me->gmii_set_type( EXPORTING im_prop_type = im_mdl-edm_core_type im_property = lo_property ).

    ENDIF.                                                             " IF IM_MDL-EDM_CORE_TYPE IS NOT INITIAL Line No. :178

    IF im_mdl-edm_core_type IS NOT INITIAL AND im_mdl-prop_precision IS NOT INITIAL.                                      " IF IV_SET_TYPE_EDM IS NOT INITIAL Line No. :51
      lo_property->set_precison( iv_precision = im_mdl-prop_precision ). "#EC NOTEXT
    ENDIF.                                                             " IF IM_MDL-EDM_CORE_TYPE IS NOT INITIAL AND IM_MDL-PROP_PRECISI

*
*      lo_property->set_conversion_exit( abap_true ).                   "#EC NOTEXT


    lo_property->set_maxlength( iv_max_length = im_mdl-max_length ). "#EC NOTEXT
    lo_property->set_semantic(  im_mdl-semantics ).         "#EC NOTEXT
    lo_property->set_creatable( im_mdl-creatable ).
    lo_property->set_updatable( im_mdl-updatable ).
    lo_property->set_sortable( im_mdl-sortable ).
    lo_property->set_nullable( im_mdl-is_nullable  ).
    lo_property->set_filterable( im_mdl-filterable ).

    lo_property->set_as_title( im_mdl-as_title ).
    IF im_mdl-as_etag IS NOT INITIAL.

      lo_property->set_as_etag( ).
    ENDIF.                                                             " IF IM_MDL-AS_ETAG IS NOT INITIAL Line No. :200
    lo_property->set_as_updated( im_mdl-as_updated ).
    lo_property->set_as_published( im_mdl-as_published ).

  ENDMETHOD.


  METHOD /odsmfe/if_model_extension~gmib_fetch_entity_modification.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :30.06.2021
* Transport No.          :ES1K902703
* Program Description    :
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
    DATA: lit_mdp_ext_details TYPE TABLE OF /odsmfe/tb_mdp.
* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

    SELECT * FROM /odsmfe/tb_mdp
    INTO TABLE lit_mdp_ext_details.


    IF lit_mdp_ext_details IS NOT INITIAL.

      gt_mdl = lit_mdp_ext_details .

    ENDIF.                                                             " IF lit_mdp_ext_details IS NOT INITIAL Line No. :264

  ENDMETHOD.


  METHOD /odsmfe/if_model_extension~gmib_modify_entity.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :30.06.2021
* Transport No.          :ES1K902703
* Program Description    :
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

    DATA: lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
          lv_entity      TYPE /iwbep/if_mgw_med_odata_types=>ty_e_med_entity_name,
          lo_model       TYPE REF TO /iwbep/if_mgw_odata_model.

    DATA: lv_class TYPE classname.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*

    CHECK gt_mdl IS NOT INITIAL.

    lo_model  = im_model.
    lv_entity = im_entity_name.

    TRY .
        lo_entity_type = lo_model->get_entity_type( iv_entity_name = lv_entity ). "#EC NOTEXT
      CATCH /iwbep/cx_mgw_med_exception.

    ENDTRY.

    IF lo_entity_type IS NOT BOUND.

      TRY .
          lo_entity_type = lo_model->create_entity_type( iv_entity_type_name = lv_entity ). "#EC NOTEXT

        CATCH /iwbep/cx_mgw_med_exception.

      ENDTRY.

    ENDIF.

    CHECK lo_entity_type IS  BOUND.

    me->/odsmfe/if_model_extension~gmib_add_properties( EXPORTING im_entity_type = lo_entity_type
                                                                  im_project = im_project
                                                                  im_model = im_model
                                                                  im_mdl = im_mdl
                                                                  im_entity_name = im_entity_name ).

  ENDMETHOD.


  METHOD /odsmfe/if_model_extension~gmib_process_entity.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :30.06.2021
* Transport No.          :ES1K902703
* Program Description    :
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

    FIELD-SYMBOLS: <lfsst_mdl> TYPE /odsmfe/tb_mdp.
    DATA: lit_entity TYPE TABLE OF  /odsmfe/tb_mdp.

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N               *
* -----------------------------------------------------------------------*
* ----------------------------------------------------------------------*
* --- Find out the number of entities and loop through them-------------*
* --- and properties foe each one---------------------------------------*
* ----------------------------------------------------------------------*

    lit_entity = gt_mdl .

    SORT lit_entity BY entity.

    DELETE ADJACENT DUPLICATES FROM lit_entity COMPARING entity.

    LOOP AT   lit_entity ASSIGNING <lfsst_mdl>  WHERE srv_name = im_srv_name.

      me->/odsmfe/if_model_extension~gmib_modify_entity( im_project = goii_project im_model = goii_model im_entity_name = <lfsst_mdl>-entity im_mdl = <lfsst_mdl> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :30.06.2021
* Transport No.          :ES1K902703
* Program Description    :
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

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

    goii_project = im_project.
    goii_model   = im_model.

    me->/odsmfe/if_model_extension~gmib_fetch_entity_modification( ).




  ENDMETHOD.


  METHOD GMII_SET_TYPE.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :ODS
* Creation Date          :30.06.2021
* Transport No.          :ES1K902703
* Program Description    :
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

* -----------------------------------------------------------------------*
*            E N D   O F   D A T A   D E C L A R A T I O N             *
* -----------------------------------------------------------------------*

case im_prop_type.
  WHEN  'Edm.Binary'.                                                  " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_binary( ).
  WHEN  'Edm.Boolean'.                                                 " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_boolean( ).

  WHEN  'Edm.Byte'.                                                    " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_byte( ).
  WHEN  'Edm.DateTime'.                                                " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_datetime( ).
  WHEN  'Edm.DateTimeOffset'.                                          " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_datetimeoffset( ).
  WHEN  'Edm.Decimal'.                                                 " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_decimal( ).
  WHEN  'Edm.Double'.                                                  " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_double( ).
  WHEN  'Edm.Float'.                                                   " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_float( ).
  WHEN  'Edm.Guid'.                                                    " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_guid( ).
  WHEN  'Edm.Int16'.                                                   " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_int16( ).
  WHEN  'Edm.Int32'.                                                   " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_int32( ).
  WHEN  'Edm.Int64'.                                                   " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_int64( ).
  WHEN  'Edm.SByte'.                                                   " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_sbyte( ).
  WHEN  'Edm.Single'.                                                  " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_single( ).
  WHEN  'Edm.String'.                                                  " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_string( ).
  WHEN  'Edm.Time'.                                                    " CASE IM_PROP_TYPE Line No. :524
    im_property->set_type_edm_time( ).

ENDCASE.                                                               " CASE IM_PROP_TYPE Line No. :524




ENDMETHOD.
ENDCLASS.
