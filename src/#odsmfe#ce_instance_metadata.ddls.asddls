@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'ODSMFE: EntitySet Properties for InstanceMetaData'
define custom entity /ODSMFE/CE_INSTANCE_METADATA
{
  key InstanceId                    : /odsmfe/de_instanceid;
        FormId                           : /odsmfe/de_formid;
        Version                          : /odsmfe/de_version;
        WorkOrderNum             : aufnr;
        OperationNum              : abap.char(4);
        EquipmentNum             : abap.char(18);
        FunctionalLocation       : abap.char(30);
        StartDate                      : abap.dats;
        EndDate                       :  abap.dats;
        CreatedDate                 :  abap.dats;
        CreatedBy                    : /odsmfe/de_createdby;
   
}
