@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_TEST_002'
define root view entity ZI_TEST_002 as select from ztest002
{
    key id as ID,
    data as Data,
    country as Country
}
