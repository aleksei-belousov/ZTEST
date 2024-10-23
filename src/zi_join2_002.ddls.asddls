@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZI_JOIN2_002'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_JOIN2_002 as select from
    (
        (
            ztest001 as a  /* scarr */  left outer join // 1. join
            ztest002 as b  /* scounter */
            on ( a.id = b.id )
        ) left outer join // 2. join
        ztest003 as c /* sairport */
        on ( c.id = b.id )
    )
{
    a.id as A,
    b.id as B,
    c.id as C
}
