@AbapCatalog.sqlViewName: 'ZFIVT00123'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'Program ZFI05E0029 - Table BSET'
define view ZFICD_ZFI05E0029_BSET
  as select distinct from bset as a
{
  key a.mandt,
  key a.bukrs,
  key a.gjahr,
  key a.belnr,

  key a.mwskz,

      sum(a.fwbas) as fwbas,
      sum(a.fwste) as fwste

}
group by
  a.mandt,
  a.bukrs,
  a.gjahr,
  a.belnr,
  a.mwskz
