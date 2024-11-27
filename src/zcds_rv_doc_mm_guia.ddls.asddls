//@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'Transfer Guides - Roow View Interface'

define root view entity ZCDS_RV_DOC_MM_GUIA
  as select from    I_GoodsMovementCube
    left outer join zdt_mm_doc_guia      as TransferGuides      on  TransferGuides.materialdocument     = I_GoodsMovementCube.MaterialDocument
                                                                and TransferGuides.materialdocumentyear = I_GoodsMovementCube.FiscalYear
                                                                and TransferGuides.companycode          = I_GoodsMovementCube.CompanyCode
                                                                and TransferGuides.goodsmovementtype    = I_GoodsMovementCube.GoodsMovementType
    inner join      zdt_ec_001           as ElectronicDocuments on  ElectronicDocuments.companycode  = I_GoodsMovementCube.CompanyCode
                                                                and ElectronicDocuments.documenttype = I_GoodsMovementCube.GoodsMovementType
                                                                and ElectronicDocuments.documentsri  = '06'
                                                                and ElectronicDocuments.sequence     = '01'
    inner join      I_CompanyCode        as I_Company           on I_Company.CompanyCode = I_GoodsMovementCube.CompanyCode
    inner join      I_GoodsMovementTypeT as I_GoodsMovementType on  I_GoodsMovementType.GoodsMovementType = I_GoodsMovementCube.GoodsMovementType
                                                                and I_GoodsMovementType.Language          = $session.system_language
    left outer join I_Plant              as I_Plant             on  I_Plant.Plant    = I_GoodsMovementCube.Plant
                                                                and I_Plant.Language = $session.system_language
    left outer join I_StorageLocation    as I_StorageLocation   on I_StorageLocation.StorageLocation = I_GoodsMovementCube.StorageLocation
    left outer join ZSH_STATUS                 as I_Status                  
            on I_Status.value_low =   TransferGuides.documentstatus
  composition [0..*] of ZCDS_RV_EC_011 as _TransportData
{

  key
        case
            when TransferGuides.companycode is not initial
            then TransferGuides.companycode
          else   I_GoodsMovementCube.CompanyCode
          end                           as Companycode,

  key
        case
            when TransferGuides.materialdocumentyear is not initial
            then TransferGuides.materialdocumentyear
          else   I_GoodsMovementCube.MaterialDocumentYear
          end                           as MaterialDocumentyear,

  key   case
        when TransferGuides.materialdocument is not initial
        then TransferGuides.materialdocument
      else   I_GoodsMovementCube.MaterialDocument
      end                               as Materialdocument,

  key   case
        when TransferGuides.goodsmovementtype is not initial
        then TransferGuides.goodsmovementtype
      else   I_GoodsMovementCube.GoodsMovementType
     end                                as GoodsMovementType,

        case
            when TransferGuides.establishment is not initial
            then TransferGuides.establishment
          else   '000'
          end                           as Establishment,

        case
          when TransferGuides.emissionpoint is not initial
          then TransferGuides.emissionpoint
        else   '000'
        end                             as Emissionpoint,

        case
          when TransferGuides.sequential is not initial
          then TransferGuides.sequential
        else   '000000000'
        end                             as Sequential,

        TransferGuides.accesskey        as Accesskey,

        case
            when TransferGuides.documenttype is not initial
            then TransferGuides.documenttype
          else   ElectronicDocuments.documentsri
          end                           as Documenttype,

        TransferGuides.issuedate        as Issuedate,

        @ObjectModel.text.element: [ 'Description' ]
        @Search.defaultSearchElement: true
        @Search.fuzzinessThreshold: 0.8
        case
            when TransferGuides.documentstatus is not initial
                then TransferGuides.documentstatus
                else 'PENDING'
          end                           as Documentstatus,


        case TransferGuides.documentstatus
           when 'PENDING'    then 0
           when 'AUTHORIZED' then 3
           when 'CANCELED'   then 1
           when 'ERROR'      then 1
           when 'PROCESS'    then 2
           else 0
           end                          as criticality,

        TransferGuides.messagedocument   as Messagedocument,
        TransferGuides.authorizationdate as Authorizationdate,
        TransferGuides.idnumber         as Idnumber,
        TransferGuides.xml              as Xml,
        TransferGuides.mimetype         as Mimetype,
        TransferGuides.filename         as Filename,
        TransferGuides.documentsupplier as Documentsupplier,

        I_GoodsMovementCube.MaterialDocumentItem,
        I_GoodsMovementCube.MaterialDocumentRecordType,
        I_GoodsMovementCube.DocumentDate,
        I_GoodsMovementCube.Plant,
        I_GoodsMovementCube.PostingDate,
        I_GoodsMovementCube.GoodsMovementIsCancelled,
        I_GoodsMovementCube.StorageLocation,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_Company.CompanyCodeName,

        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_GoodsMovementType.GoodsMovementTypeName,

        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_Plant.PlantName,

        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_StorageLocation.StorageLocationName,
        
        @Search: { defaultSearchElement: true, fuzzinessThreshold: 0.8 }
        I_Status.Description,

        _TransportData

}
where
  I_GoodsMovementCube.MaterialDocumentItem = '0001'
