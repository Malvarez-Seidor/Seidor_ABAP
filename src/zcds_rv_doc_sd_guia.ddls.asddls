//@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Transport Guides - Roow View Interface'

define root view entity ZCDS_RV_DOC_SD_GUIA
  as select from    I_DeliveryDocument
    left outer join zdt_sd_doc_guia            as TransportGuides         on TransportGuides.deliverydocument            = I_DeliveryDocument.DeliveryDocument
                                                                         and TransportGuides.deliverydocumenttype        = I_DeliveryDocument.DeliveryDocumentType
                                                                         and TransportGuides.fiscalyear                  = cast( I_DeliveryDocument.DeliveryDate   as abap.char( 4 ) ) 
    inner join      I_BusinessPartner          as I_BusinessPartner       on I_BusinessPartner.BusinessPartner           = I_DeliveryDocument.SoldToParty
    inner join      I_BusinessUserVH           as I_BusinessUser          on I_BusinessUser.UserID                       = I_DeliveryDocument.CreatedByUser
    inner join      I_SalesOrganization        as I_SalesOrganization     on I_SalesOrganization.SalesOrganization       = I_DeliveryDocument.SalesOrganization
    inner join      I_SalesOrganizationText    as I_SalesOrganizationText on I_SalesOrganizationText.SalesOrganization   = I_DeliveryDocument.SalesOrganization
                                                                         and I_SalesOrganizationText.Language            = $session.system_language
    inner join      zdt_ec_001                 as ElectronicDocuments     on ElectronicDocuments.companycode             = I_SalesOrganization.CompanyCode
                                                                         and ElectronicDocuments.documenttype            = I_DeliveryDocument.DeliveryDocumentType
                                                                         and ElectronicDocuments.documentsri             = '06'
                                                                         and ElectronicDocuments.sequence                = '01'
    inner join      I_CompanyCode              as I_Company               on I_Company.CompanyCode                       = I_SalesOrganization.CompanyCode
    inner join      I_DeliveryDocumentTypeText as I_DeliveryDocumentType  on I_DeliveryDocumentType.DeliveryDocumentType = I_DeliveryDocument.DeliveryDocumentType
                                                                         and I_DeliveryDocumentType.Language             = $session.system_language
    left outer join ZSH_STATUS                 as I_Status                on I_Status.value_low =   TransportGuides.documentstatus
  composition [0..*] of ZCDS_RV_EC_010 as _TransportData
{
  
  key
        case
            when TransportGuides.companycode is not initial
            then TransportGuides.companycode
          else   I_SalesOrganization.CompanyCode
          end                      as Companycode,

  key
        case
            when TransportGuides.fiscalyear is not initial
            then TransportGuides.fiscalyear
          else cast( I_DeliveryDocument.DeliveryDate as abap.char( 4 ) )
          end                      as Fiscalyear,
  
  key   case
        when TransportGuides.deliverydocument is not initial
        then TransportGuides.deliverydocument
      else   I_DeliveryDocument.DeliveryDocument
      end                          as Deliverydocument,

  key   case
        when TransportGuides.deliverydocumenttype is not initial
        then TransportGuides.deliverydocumenttype
      else   I_DeliveryDocument.DeliveryDocumentType
     end                           as Deliverydocumenttype,

        case
                 when TransportGuides.shiptoparty is not initial
                 then TransportGuides.shiptoparty
               else   I_DeliveryDocument.ShipToParty
               end                 as Shiptoparty,

        case
            when TransportGuides.businessname is not initial
            then TransportGuides.businessname
          else   I_BusinessPartner.BusinessPartnerFullName
          end                      as Businessname,

        TransportGuides.typeid     as Typeid,
        TransportGuides.idnumber   as Idnumber,


        case
              when TransportGuides.establishment is not initial
              then TransportGuides.establishment
            else   '000'
            end                    as Establishment,

        case
          when TransportGuides.emissionpoint is not initial
          then TransportGuides.emissionpoint
        else   '000'
        end                        as Emissionpoint,

        case
          when TransportGuides.sequential is not initial
          then TransportGuides.sequential
        else   '000000000'
        end                        as Sequential,

        TransportGuides.accesskey as Accesskey,

        case
            when TransportGuides.documenttype is not initial
            then TransportGuides.documenttype
          else   ElectronicDocuments.documentsri
          end                      as Documenttype,

        TransportGuides.issuedate as Issuedate,
        
        @ObjectModel.text.element: [ 'Description' ]
        @Search.defaultSearchElement: true
        @Search.fuzzinessThreshold: 0.8
        case
            when TransportGuides.documentstatus is not initial
                then TransportGuides.documentstatus
                else 'PENDING'
          end                             as Documentstatus,


        case TransportGuides.documentstatus
           when 'PENDING'    then 0
           when 'AUTHORIZED' then 3
           when 'CANCELED'   then 1
           when 'ERROR'      then 1
           when 'PROCESS'    then 2
           else 0
           end                            as criticality,
        
        TransportGuides.messagedocument   as Messagedocument,
        TransportGuides.authorizationdate as Authorizationdate,
        TransportGuides.xml               as Xml,
        TransportGuides.mimetype          as Mimetype,
        TransportGuides.filename          as Filename,
        TransportGuides.documentsupplier  as Documentsupplier,
        
        I_DeliveryDocument.CreatedByUser,
        I_DeliveryDocument.CreationDate,
        I_DeliveryDocument.SalesOrganization,
        I_DeliveryDocument.BillingDocumentDate,
        I_DeliveryDocument.DeliveryDate,
        I_DeliveryDocument.CompleteDeliveryIsDefined,
        I_DeliveryDocument.DeliveryBlockReason,
        I_DeliveryDocument.IsExportDelivery,
        I_DeliveryDocument.PickingDate,
        
        @Semantics.amount.currencyCode: 'TransactionCurrency'
        I_DeliveryDocument.TotalNetAmount,
        
        I_DeliveryDocument.TransactionCurrency,
        @Semantics.amount.currencyCode: 'TransactionCurrency'
        I_DeliveryDocument.ReleasedCreditAmount,
        I_DeliveryDocument.OverallSDProcessStatus,
        I_DeliveryDocument.OverallGoodsMovementStatus,
        I_DeliveryDocument.OverallDelivReltdBillgStatus,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_Company.CompanyCodeName,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_DeliveryDocumentType.DeliveryDocumentTypeName,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_BusinessUser.PersonFullName     as UserFullName,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_SalesOrganizationText.SalesOrganizationName,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_Status.Description,

        _TransportData

} where I_DeliveryDocument.OverallGoodsMovementStatus = 'C'
