class /ODSMFE/CL_MODEL_FACTORY definition
  public
  create private .

public section.
  type-pools ABAP .

  interfaces /ODSMFE/IF_MODEL_FACTORY
      all methods final .

  class-data GOSB_OBJ type ref to /ODSMFE/CL_MODEL_FACTORY .

  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS /ODSMFE/CL_MODEL_FACTORY IMPLEMENTATION.


  METHOD /odsmfe/if_model_factory~gmib_get_instance_model.

***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : Factory Method To Provide Data Model Instance
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------

    DATA: lv_entity TYPE char30,
          lo_obj    TYPE REF TO object,
          lo_exref  TYPE REF TO cx_root,        "-EC NEEDED
          lv_mesg   TYPE string.
    CONSTANTS: lc_i TYPE string VALUE 'I'.

    FIELD-SYMBOLS: <lfsst_clss_config> TYPE /odsmfe/tb_model.

*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------
    IF /odsmfe/if_model_factory~gitsb_handler IS INITIAL.

      SELECT *  FROM /odsmfe/tb_model                   "#EC CI_NOWHERE
      INTO TABLE /odsmfe/if_model_factory~gitsb_handler.

    ENDIF.

    IF /odsmfe/if_model_factory~gitsb_handler IS NOT INITIAL.

      lv_entity = im_entity_set_name.

      TRANSLATE lv_entity TO UPPER CASE.

      READ TABLE /odsmfe/if_model_factory~gitsb_handler
      ASSIGNING <lfsst_clss_config>
      WITH KEY entityset = lv_entity.

      CHECK <lfsst_clss_config> IS ASSIGNED.

      TRY.

          CREATE OBJECT lo_obj TYPE (<lfsst_clss_config>-clsname)
          EXPORTING
            im_entity_name     =   im_entity_name
            im_entity_set_name =   im_entity_set_name
            im_data_ext_class  =   im_data_ext_class.

        CATCH cx_sy_create_object_error.
          lv_mesg = lo_exref->get_text( ).
          MESSAGE lv_mesg TYPE lc_i.
      ENDTRY.

      CHECK lo_obj IS BOUND.
      re_obj ?= lo_obj.

    ENDIF.

  ENDMETHOD.


METHOD class_constructor.
***********************************************************************
********************** CREATED HISTORY **********************
* Program Author (SID)   :  SKAMMARI
* Creation Date          :  24/02/2020
* Transport No.          : ES1K901528
* Program Description    : method to create an objct.
***********************************************************************
********************** CHANGE HISTORY **********************
* Program Author (SID)   :
* Change Date            :
* Transport No.          :
* Change Description     :
***********************************************************************

*-------------------------------------------------------------
*  Data declaration
*-------------------------------------------------------------
*-------------------------------------------------------------
* Main Section
*-------------------------------------------------------------

  IF gosb_obj IS NOT BOUND.

    CREATE OBJECT gosb_obj.

  ENDIF.
ENDMETHOD.
ENDCLASS.
