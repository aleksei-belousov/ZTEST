@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DEMO CDS SYSTEM FIELDS2'
define view entity ZDEMO_CDS_SYSTEM_FIELDS2
with parameters
@Environment.systemField : #SYSTEM_LANGUAGE p_langu : abap.lang,
p_test : abap.string
as select from zdemo_cds_system_fields(p_langu:$parameters.p_langu, p_mandt:'000', p_datum:$session.system_date, p_uzeit:'000000', p_uname:'000')  
{
    key id,
    data,
    langu,
    mandant,
    datum,
    uzeit,
    uname,
    $parameters.p_test as test,
    $session.client as session_client  
}

