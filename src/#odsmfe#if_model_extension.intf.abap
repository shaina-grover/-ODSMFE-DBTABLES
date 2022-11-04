interface /ODSMFE/IF_MODEL_EXTENSION
  public .


  methods GMIB_FETCH_ENTITY_MODIFICATION .
  methods GMIB_MODIFY_ENTITY
    importing
      !IM_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL
      !IM_PROJECT type ref to /IWBEP/CL_MGW_PUSH_ABS_MODEL
      !IM_ENTITY_NAME type /IWBEP/SBDM_NODE_NAME
      !IM_MDL type /ODSMFE/TB_MDP
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods GMIB_ADD_PROPERTY
    importing
      !IM_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL
      !IM_PROJECT type ref to /IWBEP/CL_MGW_PUSH_ABS_MODEL
      !IM_ENTITY_TYPE type ref to /IWBEP/IF_MGW_ODATA_ENTITY_TYP
      !IM_ENTITY_NAME type /IWBEP/SBDM_NODE_NAME
      !IM_MDL type /ODSMFE/TB_MDP
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods GMIB_ADD_PROPERTIES
    importing
      !IM_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL
      !IM_PROJECT type ref to /IWBEP/CL_MGW_PUSH_ABS_MODEL
      !IM_ENTITY_TYPE type ref to /IWBEP/IF_MGW_ODATA_ENTITY_TYP
      !IM_ENTITY_NAME type /IWBEP/SBDM_NODE_NAME
      !IM_MDL type /ODSMFE/TB_MDP
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
  methods GMIB_PROCESS_ENTITY
    importing
      !IM_SRV_NAME type STRING
    raising
      /IWBEP/CX_MGW_MED_EXCEPTION .
endinterface.
