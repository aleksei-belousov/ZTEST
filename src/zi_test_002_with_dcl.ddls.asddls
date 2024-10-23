@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'ZI_TEST_002'
define root view entity ZI_TEST_002_WITH_DCL as select from ztest002
{
    key id as ID,
    data as Data,
    country as Country
}
