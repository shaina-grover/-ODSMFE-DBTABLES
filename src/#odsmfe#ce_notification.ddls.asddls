@ObjectModel.query.implementedBy: 'ABAP:/ODSMFE/CL_QUERY_IMPLEMENT'
@EndUserText.label: 'Custom Entity for Notification Set'
define custom entity /ODSMFE/CE_NOTIFICATION

{
  key Notification : abap.char(12);
         OrderType   : abap.char(4);
         Description : abap.char(40);
}
