@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'ServiceConfig'
define custom entity /ODSMFE/CE_ServiceConfig 
  {   
  key ReqID : /odsmfe/reqid;
  key EntitySetName : /odsmfe/entityset;
  AppStoreName : /odsmfe/appstore_name;
  RoleID : /odsmfe/roleid;
   @EndUserText.label: 'ENTEREDBY'
  EnteredBy : abap.char(12);
  EntityType : /odsmfe/entity;
  ClassName : /odsmfe/class;
  Object : /odsmfe/object;
  AppStoreID : /odsmfe/appstoreid;
  AppStore : /odsmfe/appstore_indicator;
  Url : /odsmfe/url;
  DisplayName : /odsmfe/dispnam;
  Active : /odsmfe/active;
  Keys : /odsmfe/keys;  
}
