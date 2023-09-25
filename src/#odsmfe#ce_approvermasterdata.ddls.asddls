 @ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'ApproverMasterData'
define custom entity /ODSMFE/CE_ApproverMasterData 
 {
  key UserSystemID : /odsmfe/de_approverid;
  key PersonnelNum : /odsmfe/de_userpersonnelnum;
   @EndUserText.label: 'emailid'
  key EmailID : abap.char(241);
  Active : /odsmfe/de_active;
  FirstName : /odsmfe/de_firstname;
  LastName : /odsmfe/de_lastname;
  Contact :  /odsmfe/de_contact;
  DepartmentID : /odsmfe/de_departmentid;
  DepartmentName : /odsmfe/de_departmentname;
  ApproverRole : /odsmfe/de_roleid;
  ApproverLevel : /odsmfe/de_approverlevel;
  Plant : werks_d;
  @EndUserText.label: 'WorkCenter'
  WorkCenter : abap.char( 8 );
  
}
