interface /ODSMFE/IF_CREATE_INSTANCE
  public .


  class-data GS_CLASS_CONFIG type /ODSMFE/TB_ENTCL .

  class-methods CREATE_OBJECT_FACTORY
    importing
      !IM_ENTITY type STRING
    returning
      value(RE_OBJ) type ref to OBJECT.
*    raising
*      /IWBEP/CX_MGW_BUSI_EXCEPTION
*      /IWBEP/CX_MGW_TECH_EXCEPTION .

endinterface.
