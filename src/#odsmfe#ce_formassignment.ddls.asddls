@ObjectModel.query.implementedBy:'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'formassignment data definition'
define root custom entity /ODSMFE/CE_FORMASSIGNMENT

{

  key FormID          : /odsmfe/de_formid;

  key Version         : /odsmfe/de_version;

  key OrderType       : abap.char(4);

  key ControlKey      : abap.char(4);

  key TaskListType    : abap.char(1);

      @EndUserText.label: 'Key for Task List Group'
  key Groups           : abap.char(8);

  key GroupCounter    : abap.char(2);

  key InternalCounter : /odsmfe/de_intrncounter;

  key EquipCategory   : abap.char(1);

  key FuncLocCategory : abap.char(1);

      Category        : /odsmfe/de_formcategory;

      JobType         : /odsmfe/de_jobtype;

      Mandatory       : /odsmfe/de_mandatory;

      FlowSequence    : /odsmfe/de_flowsequence;

      MultipleSub     : /odsmfe/de_multiplesub;

      Occur           : /odsmfe/de_occur;

      Theme           : abap.char(123);

      StyleSheet      : abap.char(123);

      CreatedOn       : /odsmfe/de_createdon;

      CreatedBy       : /odsmfe/de_createdby;

      ModifiedOn      : abap.dec(15);

      ModifiedBy      : /odsmfe/de_modifiedby;

      Deleted         : /odsmfe/de_roleid;



}
