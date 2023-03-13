FUNCTION /odsmfe/fm_forms.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_AUFNR) TYPE  AUFNR OPTIONAL
*"     REFERENCE(IM_AUART) TYPE  AUFART OPTIONAL
*"     REFERENCE(IM_PLNTY) TYPE  PLNTY OPTIONAL
*"     REFERENCE(IM_PLNNR) TYPE  PLNNR OPTIONAL
*"     REFERENCE(IM_PLNAL) TYPE  PLNAL OPTIONAL
*"     REFERENCE(IM_ZAEHL) TYPE  CIM_COUNT OPTIONAL
*"     REFERENCE(IM_QMNUM) TYPE  QMNUM OPTIONAL
*"     REFERENCE(IM_QMART) TYPE  QMART OPTIONAL
*"----------------------------------------------------------------------

* Iniiate controller
  CREATE OBJECT go_control.
*
* Get the object from Control
  CALL METHOD go_control->gmib_get_object
    EXPORTING
      im_name = '/ODSMFE/CL_MODEL'.

  CALL METHOD go_control->gvib_model->gmib_get_data
    EXPORTING
      im_qmnum = im_qmnum     "Added by ODS-VSANAGALA - ES1K903619
      im_qmart = im_qmart     "Added by ODS-VSANAGALA - ES1K903619
      im_aufnr = im_aufnr
      im_auart = im_auart
*   SOC by ODS ES1K902363
      im_plnty = im_plnty
      im_plnnr = im_plnnr
      im_plnal = im_plnal
      im_zaehl = im_zaehl.
*   EOC by ODS ES1K902363
  CALL SCREEN 300 STARTING AT 7    5
                  ENDING   AT 160  20.


ENDFUNCTION.
