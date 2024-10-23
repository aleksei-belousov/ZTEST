@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '##GENERATED ZTEST004'
define root view entity ZR_TEST004000
  as select from ztest004
{
  key id as ID,
  data as Data,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  local_last_changed as LocalLastChanged,
  @Semantics.systemDateTime.lastChangedAt: true
  last_changed as LastChanged
  
}
