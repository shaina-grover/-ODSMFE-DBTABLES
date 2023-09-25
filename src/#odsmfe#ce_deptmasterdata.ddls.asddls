@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'DeptMasterData'
define custom entity /ODSMFE/CE_DeptMasterData 
  {
  key DepartmentID :/odsmfe/de_departmentid;
  key DepartmentName : /odsmfe/de_departmentname;
  key Plant : werks_d;
  Active : /odsmfe/de_active;
  
}
