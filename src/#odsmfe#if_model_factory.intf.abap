interface /ODSMFE/IF_MODEL_FACTORY
  public .


  class-data:
    gitsb_handler TYPE TABLE OF /odsmfe/tb_model .

  methods GMIB_GET_INSTANCE_MODEL
    importing
      !IM_ENTITY_NAME type STRING optional
      !IM_ENTITY_SET_NAME type STRING
      !IM_DATA_EXT_CLASS type ref to OBJECT optional
    returning
      value(RE_OBJ) type ref to /ODSMFE/IF_GET_ENTITYSET_MAIN .
endinterface.
