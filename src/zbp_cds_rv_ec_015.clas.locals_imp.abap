CLASS lhc_ElectronicDocuments DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR ElectronicDocuments RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR electronicdocuments RESULT result.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR electronicdocuments RESULT result.

    METHODS documentlogistics FOR MODIFY
      IMPORTING keys FOR ACTION electronicdocuments~documentlogistics RESULT result.

    METHODS financialdocument FOR MODIFY
      IMPORTING keys FOR ACTION electronicdocuments~financialdocument RESULT result.

ENDCLASS.

CLASS lhc_ElectronicDocuments IMPLEMENTATION.

  METHOD get_instance_authorizations.

  ENDMETHOD.

  METHOD get_global_authorizations.

  ENDMETHOD.

  METHOD get_instance_features.

    READ ENTITIES OF zcds_rv_ec_015  IN LOCAL MODE
      ENTITY ElectronicDocuments
      FIELDS ( Companycode Fiscalyear Supplierinvoice Purchasingdocument Accountingdocument Accountingfiscalyear Documentstatus )
      WITH CORRESPONDING #( keys )
      RESULT DATA(ElectronicDocuments)
      FAILED failed.

    result = VALUE #( FOR ElectronicDocument IN ElectronicDocuments
                    ( %tky = ElectronicDocument-%tky
                      %features-%action-DocumentLogistics
          = COND #( WHEN ElectronicDocument-Documentstatus EQ '01' THEN if_abap_behv=>fc-o-enabled
                    ELSE if_abap_behv=>fc-o-disabled )
          %features-%action-FinancialDocument
          = COND #( WHEN ElectronicDocument-Documentstatus EQ '01' THEN if_abap_behv=>fc-o-enabled
                    ELSE if_abap_behv=>fc-o-disabled )
                     ) ).

  ENDMETHOD.

  METHOD DocumentLogistics.

*I_GoodsMovementDocument = MSEG
*I_GoodsMovementCube     = MSEG
*I_GoodsMovementType
*I_GoodsMovementTypeStdVH
*I_PurchaseRequisitionItemAPI01 = EKKP
*I_PurchaseRequisitionItemBasic = EBAN
*I_Withholdingtaxitem      = WITH_ITEMS
*I_SupplierInvoiceTaxAPI01 = RSET
*I_SupplierInvoicePI01     = RSEG
*I_SupplierInvoice     = RBKP

*    DATA ls_invoice TYPE STRUCTURE FOR ACTION IMPORT i_supplierinvoicetp~create.
*DATA lt_invoice TYPE TABLE FOR ACTION IMPORT i_supplierinvoicetp~create.
*DATA ld_sinv_item_id TYPE n LENGTH 6 VALUE '1'.
*
*TRY.
*  DATA(ld_cid_invo_hdr) = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
*  CATCH cx_uuid_error.
* ENDTRY.
*
**lt_incinvappr = table with additional data
*READ TABLE lt_incinvappr ASSIGNING FIELD-SYMBOL(<ls_incinvappr>) INDEX 1.
*
*  ls_invoice-%cid = ld_cid_invo_hdr.
*  ls_invoice-%param-supplierinvoiceiscreditmemo   = abap_false.
*  ls_invoice-%param-companycode                   = <ls_incinvappr>-CompanyId.
*  ls_invoice-%param-invoicingparty                = <ls_incinvappr>-SupplierId.
*  ls_invoice-%param-postingdate                   = <ls_incinvappr>-IssueDate.
*  ls_invoice-%param-documentdate                  = <ls_incinvappr>-IssueDate.
*  ls_invoice-%param-documentcurrency              = <ls_incinvappr>-Currency.
*  ls_invoice-%param-invoicegrossamount            = <ls_incinvappr>-GrossAmount.
*  ls_invoice-%param-taxiscalculatedautomatically  = abap_true.
*  ls_invoice-%param-SupplierInvoiceIDByInvcgParty = <ls_incinvappr>-ExternalId.
*  ls_invoice-%param-DocumentHeaderText            = 'SAP RAP Action'.
*  ls_invoice-%param-TaxDeterminationDate          = <ls_incinvappr>-TaxDate.
*  ls_invoice-%param-DueCalculationBaseDate        = <ls_incinvappr>-DueDate.
*  ls_invoice-%param-SupplierInvoiceStatus         = cl_mmiv_rap_ext_c=>supplierinvoicestatus-held.
*
** lt_purchaseorder_item = table with PO reference items
*  LOOP AT lt_purchaseorder_item ASSIGNING FIELD-SYMBOL(<ls_pur_item>).
*
*    APPEND VALUE #( supplierinvoiceitem = ld_sinv_item_id
*                    purchaseorder       = <ls_incinvappr>-OrderNumber
*                    purchaseorderitem   = <ls_pur_item>-PurchaseOrderItem
*                    documentcurrency    = <ls_incinvappr>-Currency
*                    supplierinvoiceitemamount   = <ls_pur_item>-NetAmount
*                    purchaseOrderQuantityUnit   = <ls_pur_item>-PurchaseOrderQuantityUnit
*                    quantityinpurchaseorderunit = <ls_pur_item>-OrderQuantity
*                    taxcode                     = 'V1'
*                          )
*
*    TO ls_invoice-%param-_itemswithporeference.
*          ld_sinv_item_id = ld_sinv_item_id + 1.
*  ENDLOOP.

*  INSERT ls_invoice INTO TABLE lt_invoice.
*
** Create BO - Supplier invoice with PO ref
*
*  MODIFY ENTITIES OF I_SupplierInvoiceTP PRIVILEGED
*    ENTITY SupplierInvoice
*    EXECUTE Create FROM lt_invoice
*
*    FAILED      DATA(ls_sinvoice_create_failed)
*    REPORTED    DATA(ls_sinvoice_create_reported)
*    MAPPED      DATA(ls_sinvoice_create_mapped).

  ENDMETHOD.

  METHOD FinancialDocument.

  ENDMETHOD.

ENDCLASS.
