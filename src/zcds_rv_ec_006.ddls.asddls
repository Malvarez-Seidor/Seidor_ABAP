//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Payment Method - Roow View Interface'
define root view entity ZCDS_RV_EC_006 
as select from zdt_ec_006 as PaymentConditions
association [1..1] to I_CompanyCode                    as _Company          on  _Company.CompanyCode = PaymentConditions.companycode
association [1..1] to I_PaymentMethodText             as _PaymentMethod     on  _PaymentMethod.PaymentMethod = PaymentConditions.paymentmethod
                                                                           and  _PaymentMethod.Language = $session.system_language
                                                                           and  _PaymentMethod.Country = 'EC'
                                                                          
{
    key companycode   as Companycode,
    key paymentmethod as Paymentmethod,
    key paymentsri    as Paymentsri, 
        _Company,
        _PaymentMethod
        
}
