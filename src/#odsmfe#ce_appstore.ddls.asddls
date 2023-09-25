@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'AppStore'
define custom entity /ODSMFE/CE_AppStore 
  {
  key Appstorename : /odsmfe/de_mfe_appstorename;
  key Appstoreid : /odsmfe/de_mfe_appstore_id;
      Sapstore : /odsmfe/de_mfe_sapstore;
      Baseurl : /odsmfe/de_mfe_burl;
      Active : /odsmfe/de_mfe_active;
      Servicename : /odsmfe/de_mfe_sname;
      Flush : /odsmfe/de_flush;
      Refresh : /odsmfe/de_refresh;
      wc_clear : /odsmfe/de_cs_clear;
       syncseq          : abap.char(3);
  
}
