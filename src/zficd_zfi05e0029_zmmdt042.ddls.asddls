@AbapCatalog.sqlViewName: 'ZFIVT00121'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'Program ZFI05E0029 - Table ZMMDT00042'
/*+[hideWarning] { "IDS" : [ "KEY_CHECK" ]  } */
define view ZFICD_ZFI05E0029_ZMMDT042
  as select distinct from zmmdt00042 as a
{
  key a.mandt,
  key a.bpartner,
      //  key lpad( a.bpartner, 10, '0' ) as bpartner_b,

  key case when a.bpartner like '%[^0-9.]%'
     then lpad( cast(a.bpartner as lifnr), 10, '0' )
     else cast(a.bpartner as lifnr)
  end as bpartner_b,

      a.fityp
}
where
      a.bpartner != ''
  and a.fityp != '';
