@EndUserText.label: 'ZTEST004'
@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
define view entity ZI_Ztest004
  as select from ZTEST004
  association to parent ZI_Ztest004_S as _Ztest004All on $projection.SingletonID = _Ztest004All.SingletonID
{
  key ID as Id,
  DATA as Data_000,
  @Consumption.hidden: true
  1 as SingletonID,
  _Ztest004All
  
}
