@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DEMO CDS SYSTEM FIELDS3'
define view entity ZDEMO_CDS_SYSTEM_FIELDS3
with parameters
@Environment.systemField : #SYSTEM_LANGUAGE p_langu : abap.lang
as select from zdemo_cds_system_fields(p_langu:$parameters.p_langu, p_mandt:'000', p_datum:'00000000', p_uzeit:'000000', p_uname:'000')  
{
    max( id ) as max_id,
    sum( cast ( data as abap.int4 ) ) as sum_data,
    langu,
    mandant,
    datum,
    uzeit,
    uname
}
group by
    langu,
    mandant,
    datum,
    uzeit,
    uname


