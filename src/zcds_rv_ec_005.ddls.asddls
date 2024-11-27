//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Clase de Condiciones' // Roow View Interface'
define root view entity ZCDS_RV_EC_005
  as select from zdt_ec_005 as TypeCondition
  association [1..1] to I_CompanyCode              as _Company          on  _Company.CompanyCode = TypeCondition.companycode
  association [1..1] to I_PricingConditionTypeText as _PricingCondition on  _PricingCondition.ConditionType        = TypeCondition.typecondition
                                                                        and _PricingCondition.Language             = $session.system_language
                                                                        and _PricingCondition.ConditionApplication = TypeCondition.conditionapplication
{
  key companycode          as Companycode,
  key ccondition           as Ccondition,
  key conditionapplication as conditionapplication,
      typecondition        as Typecondition,
      _Company,
      _PricingCondition
}
