@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'ODSMFE: EntitySet Properties for InstanceData'
define custom entity /ODSMFE/CE_INSTANCE_DATA
{
  key InstanceId            :       /odsmfe/de_instanceid;
        GroupName           :     abap.char(255);
        RepeatNumber      :     abap.int4;
        DataKey                 :    abap.char(255);
        DataValue              :    abap.char(255);
  
}
