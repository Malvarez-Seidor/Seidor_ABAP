CLASS zcl_create_ats DEFINITION

  PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_canceled      TYPE STANDARD TABLE OF zts_canceled,
           ty_export        TYPE STANDARD TABLE OF zts_export,
           ty_purchases     TYPE STANDARD TABLE OF zts_purchases,
           ty_sales         TYPE STANDARD TABLE OF zts_sales,
           ty_tot_sales     TYPE STANDARD TABLE OF zts_total_sales,
           ty_withholdings  TYPE STANDARD TABLE OF zts_withholdings,
           ty_support       TYPE STANDARD TABLE OF zts_support,
           ty_pagos         TYPE STANDARD TABLE OF zts_pago.

    DATA: gv_CompanyCode     TYPE bukrs,   "Sociedad
          gv_FiscalYear      TYPE gjahr,   "Ejercicio de Docuemnto
          gv_FiscalPeriod    TYPE monat.   "Periodo Contable

    DATA: gs_informant     TYPE zts_informant,
          gs_canceled      TYPE zts_canceled,
          gs_export        TYPE zts_export,
          gs_purchases     TYPE zts_purchases,
          gs_sales         TYPE zts_sales,
          gs_tot_sales     TYPE zts_total_sales,
          gs_withholdings  TYPE zts_withholdings,
          gs_support       TYPE zts_support,
          gs_pagos         TYPE zts_pago.

    DATA: gt_canceled     TYPE STANDARD TABLE OF zts_canceled,
          gt_export       TYPE STANDARD TABLE OF zts_export,
          gt_purchases    TYPE STANDARD TABLE OF zts_purchases,
          gt_sales        TYPE STANDARD TABLE OF zts_sales,
          gt_tot_sales    TYPE STANDARD TABLE OF zts_total_sales,
          gt_withholdings TYPE STANDARD TABLE OF zts_withholdings,
          gt_support      TYPE STANDARD TABLE OF zts_support,
          gt_pagos        TYPE STANDARD TABLE OF zts_pago.

    METHODS constructor        IMPORTING  CompanyCode     TYPE bukrs   "Sociedad
                                          FiscalYear      TYPE gjahr   "Ejercicio de Docuemnto
                                          FiscalPeriod    TYPE monat.  "Periodo Contable

    METHODS callInformation    EXPORTING  is_informant    TYPE zts_informant
                                          it_canceled     TYPE zcl_create_ats=>ty_canceled
                                          it_export       TYPE zcl_create_ats=>ty_export
                                          it_purchases    TYPE zcl_create_ats=>ty_purchases
                                          it_sales        TYPE zcl_create_ats=>ty_sales
                                          it_total_sales  TYPE zcl_create_ats=>ty_tot_sales
                                          it_withholdings TYPE zcl_create_ats=>ty_withholdings
                                          it_support      TYPE zcl_create_ats=>ty_support.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: gs_CompanyCode              TYPE I_CompanyCode,
          gs_AddlInformation          TYPE I_AddlCompanyCodeInformation,
          gs_CountryText              TYPE I_CountryText,
          gs_Gastos_Exp               TYPE zcds_vc_fob,
          gs_Compra                   TYPE zcds_p_compras,
          gs_Venta                    TYPE zcds_p_ventas,
          gs_Venta_ret                TYPE zcds_vc_det_ret,
          gs_Compra_ret               TYPE zcds_vc_det_ret,
          gs_Venta_imp                TYPE zcds_vc_tax,
          gs_Venta_ice                TYPE zcds_vc_ice,
          gs_Compra_imp               TYPE zcds_vc_tax,
          gs_ec_001                   TYPE zdt_ec_001,
          gs_ec_003                   TYPE zdt_ec_003,
          gs_ec_006                   TYPE zdt_ec_006,
          gs_ec_008                   TYPE zdt_ec_008,
          gs_ec_009                   TYPE zdt_ec_009,
          gs_ec_013                   TYPE zdt_ec_013.

   DATA:  gt_AddlInformations         TYPE STANDARD TABLE OF I_AddlCompanyCodeInformation,
          gt_CountryText              TYPE STANDARD TABLE OF I_CountryText,
          gt_Gastos_Exp               TYPE STANDARD TABLE OF zcds_vc_fob,
          gt_Compras                  TYPE STANDARD TABLE OF zcds_p_compras,
          gt_Ventas                   TYPE STANDARD TABLE OF zcds_p_ventas,
          gt_Ventas_ret               TYPE STANDARD TABLE OF zcds_vc_det_ret,
          gt_Compras_ret              TYPE STANDARD TABLE OF zcds_vc_det_ret,
          gt_Ventas_imp               TYPE STANDARD TABLE OF zcds_vc_tax,
          gt_Ventas_ice               TYPE STANDARD TABLE OF zcds_vc_ice,
          gt_Compras_imp              TYPE STANDARD TABLE OF zcds_vc_tax,
          gt_ec_001                   TYPE STANDARD TABLE OF zdt_ec_001,
          gt_ec_003                   TYPE STANDARD TABLE OF zdt_ec_003,
          gt_ec_006                   TYPE STANDARD TABLE OF zdt_ec_006,
          gt_ec_008                   TYPE STANDARD TABLE OF zdt_ec_008,
          gt_ec_009                   TYPE STANDARD TABLE OF zdt_ec_009,
          gt_ec_013                   TYPE STANDARD TABLE OF zdt_ec_013.

    METHODS get_data.

    METHODS get_informant       CHANGING  is_informant    TYPE zts_informant.

    METHODS limpia_string       CHANGING  iv_texto TYPE string.

    METHODS get_Compras         CHANGING  it_purchases    TYPE zcl_create_ats=>ty_purchases.

    METHODS get_Ventas          CHANGING  it_sales        TYPE zcl_create_ats=>ty_sales
                                          it_total_sales  TYPE zcl_create_ats=>ty_tot_sales.

    METHODS get_TotalVentas     IMPORTING is_total_sales  TYPE zts_total_sales
                                CHANGING  it_total_sales  TYPE zcl_create_ats=>ty_tot_sales.

    METHODS get_Exportaciones   CHANGING  it_export       TYPE zcl_create_ats=>ty_export
                                          it_total_sales  TYPE zcl_create_ats=>ty_tot_sales.

    METHODS get_Anulados        CHANGING  it_canceled     TYPE zcl_create_ats=>ty_canceled.

ENDCLASS.

CLASS zcl_create_ats IMPLEMENTATION.

  METHOD constructor.

    gv_CompanyCode  = CompanyCode.  "Sociedad
    gv_FiscalYear   = FiscalYear.   "Ejercicio de Docuemnto
    gv_FiscalPeriod = FiscalPeriod. "Periodo Contable

  ENDMETHOD.

  METHOD callInformation.

    CLEAR: gs_informant, gt_canceled[], gt_export[], gt_purchases[], gt_sales[], gt_tot_sales[], gt_withholdings[], gt_support[].

    me->get_data(  ).

    me->get_compras( CHANGING it_purchases            = me->gt_purchases ).

    me->get_ventas( CHANGING it_sales                 = me->gt_sales
                             it_total_sales           = me->gt_tot_sales ).

    me->get_exportaciones( CHANGING it_export         = me->gt_export
                                    it_total_sales    = me->gt_tot_sales ).

    me->get_anulados( CHANGING it_canceled            = me->gt_canceled ).

    me->get_informant( CHANGING is_informant          = me->gs_informant ).

    is_informant      = me->gs_informant.
    it_canceled[]     = me->gt_canceled[].
    it_export[]       = me->gt_export[].
    it_purchases[]    = me->gt_purchases[].
    it_sales[]        = me->gt_sales[].
    it_total_sales[]  = me->gt_tot_sales[].
    it_withholdings[] = me->gt_withholdings[].
    it_support[]      = me->gt_support[].

  ENDMETHOD.

  METHOD get_data.

    SELECT SINGLE CompanyCode, CompanyCodeName, CityName, Country, Currency, Language, ChartOfAccounts, FiscalYearVariant, Company,
                  CreditControlArea, CountryChartOfAccounts, FinancialManagementArea, AddressID, TaxableEntity, VATRegistration,
                  ExtendedWhldgTaxIsActive, ControllingArea, FieldStatusVariant, NonTaxableTransactionTaxCode, DocDateIsUsedForTaxDetn,
                  TaxRptgDateIsActive, CashDiscountBaseAmtIsNetAmt
      FROM I_CompanyCode
      WHERE companycode = @me->gv_companycode
        AND Language    = @sy-langu
      INTO @me->gs_CompanyCode.

    SELECT CompanyCode, CompanyCodeParameterType, CompanyCodeParameterValue
      FROM I_AddlCompanyCodeInformation
      WHERE companycode = @me->gv_companycode
      INTO TABLE @me->gt_AddlInformations.

    SELECT client, companycode, documenttype, documentsri, sequence, export, refunds, reason
      FROM zdt_ec_001
      WHERE companycode  EQ @me->gv_companycode
      INTO TABLE @gt_ec_001.

    SELECT client, companycode, taxcode, notax, tax0, exempttax, tax, taxsupportid, taxsidrate, taxratepercent, supporttaxcode
      FROM zdt_ec_003
      WHERE companycode  EQ @me->gv_companycode
      INTO TABLE @gt_ec_003.

    SELECT client, companycode, paymentmethod, paymentsri
      FROM zdt_ec_006
      WHERE companycode  EQ @me->gv_companycode
      INTO TABLE @gt_ec_006.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence, accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
      FROM zdt_ec_008
      WHERE companycode  EQ @me->gv_companycode
      INTO TABLE @gt_ec_008.

    SELECT client, country, countrysri, taxhavencountry, taxagreement, taxregime
      FROM zdt_ec_009
      WHERE country NE @space
      INTO TABLE @gt_ec_009.

    IF sy-subrc EQ 0.

      SELECT a~Country, a~Language, a~CountryName, a~NationalityName, a~NationalityLongName, a~CountryShortName
        FROM I_CountryText AS a
        INNER JOIN @gt_ec_009 AS b
         ON a~country  EQ b~country
        AND a~Language EQ @sy-langu
      INTO TABLE @me->gt_CountryText.

    ENDIF.

    SELECT companycode, FiscalYear, AccountingDocument, AccountingDocumentType, Supplier, Businessname, TypeId, IdNumber, Establishment, Emissionpoint,
           Sequential, Accesskey, DocumentType, IssueDate, AuthorizationDate, DocumentStatus, EstablishmentWith, EmissionpointWith, SequentialWith, AccesskeyWith,
           DocumentTypeWith, IssueDateWith, AuthorizationDateWith, DocumentStatusWith, PaymentTerms, PaymentMethod, NetAmount, TaxAmountInCoCodeCrcy,
           TaxBaseAmountInCoCodeCrcy, CompanyCodeCurrency, FiscalPeriod, PostingDate, AccountingDocumentHeaderText, DocumentReferenceID, ReverseDocument,
           AccountingDocCreatedByUser, Reference1InDocumentHeader, UserFullName, BusinessPartnerCategory, BusinessPartnerGrouping, IsNaturalPerson, CompanyCodeName,
           AccountingDocumentTypeName, PaymentTermsConditionDesc, Description, DescriptionW, IsReversed, SupplierAccountNote, Country, EstablishmentRef,
           EmissionpointRef, SequentialRef, AccesskeyRef, DocumentTypeRef
      FROM zcds_p_compras
      WHERE CompanyCode    EQ @me->gv_companycode
        AND FiscalYear     EQ @me->gv_fiscalyear
        AND FiscalPeriod   EQ @me->gv_fiscalperiod
        AND DocumentStatus EQ 'AUTHORIZED'
      INTO TABLE @gt_Compras.

    IF sy-subrc EQ 0.

      SELECT a~client, a~companycode, a~fiscalyear, a~accountingdocument, a~accountingdocumenttype, a~draftuuid, a~typeid, a~idnumber,
             a~documenttype, a~establishment, a~emissionpoint, a~sequential, a~accesskey, a~issuedate, a~taxcode, a~amountbasetax,
             a~amountbasetax0, a~amountbasenotax, a~amountbaseexetax, a~amounttax, a~amountice, a~total_price, a~currency, a~last_changed_by
        FROM zdt_ec_013 as a
        INNER JOIN @gt_Compras AS b
           ON a~companycode             EQ b~CompanyCode
          AND a~FiscalYear              EQ b~FiscalYear
          AND a~AccountingDocument      EQ b~AccountingDocument
          AND a~AccountingDocumentType  EQ b~AccountingDocumentType
        INTO TABLE @gt_ec_013.

      SELECT a~CompanyCode, a~FiscalYear, a~AccountingDocument, a~AccountingDocumentItem, a~WithholdingTaxType, a~WithholdingTaxCode,
             a~OfficialWhldgTaxCode, a~WithholdingType, a~WhldgTaxBaseAmtInCoCodeCrcy, a~WhldgTaxAmtInCoCodeCrcy, a~WithholdingTaxPercent,
             a~CompanyCodeCurrency, a~Country, a~WhldgTaxReferenceText, a~WhldgTaxCodeName
        FROM zcds_vc_det_ret AS a
        INNER JOIN @gt_Compras AS b
           ON a~companycode             EQ b~CompanyCode
          AND a~FiscalYear              EQ b~FiscalYear
          AND a~AccountingDocument      EQ b~AccountingDocument
        INTO TABLE @gt_Compras_ret.

      SELECT a~CompanyCode, a~AccountingDocument, a~FiscalYear, a~TaxCode, a~TaxBaseAmountInCoCodeCrcy, a~TaxAmountInCoCodeCrcy, a~CompanyCodeCurrency
        FROM zcds_vc_tax as a
        INNER JOIN @gt_Compras AS b
           ON a~companycode             EQ b~CompanyCode
          AND a~FiscalYear              EQ b~FiscalYear
          AND a~AccountingDocument      EQ b~AccountingDocument
        INTO TABLE @gt_Compras_imp.


    ENDIF.

    SELECT CompanyCode, FiscalYear, BillingDocument, BillingDocumentType, AccountingDocument, AccountingDocumentType, Customer, Businessname, Typeid, Idnumber,
           Establishment, Emissionpoint, Sequential, Accesskey, Documenttype, Issuedate, Documentstatus, AuthorizationDate, BillingDocumentIsCancelled, CustomerPaymentTerms,
           PaymentMethod, TotalNetAmount, TransactionCurrency, FiscalPeriod, PostingDate, AccountingDocumentHeaderText, DocumentReferenceID, ReverseDocument,
           AccountingDocCreatedByUser, Reference1InDocumentHeader, AccountingDocumentWith, FiscalYearWith, AccountingDocumentTypeWith, PostingDateRWith, DocumentHeaderTextWith,
           DocumentReferenceIDWith, ReverseDocumentWith, AccountingDocCreatedByUserWith, Reference1InDocumentHeaderWith, TotalTaxAmount, CreatedByUser, UserFullName,
           PersonFullName, PersonFullNameWith, BusinessPartnerCategory, BusinessPartnerGrouping, IsNaturalPerson, CompanyCodeName, BillingDocumentTypeName, AccountingDocumentTypeName,
           DocumentTypeNameWith, PaymentTermsConditionDesc, Description, IsReversed, Export, CustomerAccountNote
      FROM zcds_p_ventas
      WHERE CompanyCode    EQ @me->gv_companycode
        AND FiscalYear     EQ @me->gv_fiscalyear
        AND FiscalPeriod   EQ @me->gv_fiscalperiod
        AND DocumentStatus EQ 'AUTHORIZED'
      INTO TABLE @gt_Ventas.

    IF sy-subrc EQ 0.

      SELECT a~CompanyCode, a~FiscalYear, a~AccountingDocument, a~AccountingDocumentItem, a~WithholdingTaxType, a~WithholdingTaxCode,
             a~OfficialWhldgTaxCode, a~WithholdingType, a~WhldgTaxBaseAmtInCoCodeCrcy, a~WhldgTaxAmtInCoCodeCrcy, a~WithholdingTaxPercent,
             a~CompanyCodeCurrency, a~Country, a~WhldgTaxReferenceText, a~WhldgTaxCodeName
        FROM zcds_vc_det_ret AS a
        INNER JOIN @gt_Ventas AS b
           ON a~CompanyCode              EQ b~CompanyCode
          AND a~FiscalYear               EQ b~FiscalYear
          AND a~AccountingDocument       EQ b~AccountingDocument
        INTO TABLE @gt_ventas_ret.

      SELECT a~CompanyCode, a~AccountingDocument, a~FiscalYear, a~TaxCode, a~TaxBaseAmountInCoCodeCrcy, a~TaxAmountInCoCodeCrcy, a~CompanyCodeCurrency
        FROM zcds_vc_tax AS a
        INNER JOIN @gt_Ventas AS b
           ON a~CompanyCode              EQ b~CompanyCode
          AND a~FiscalYear               EQ b~FiscalYearWith
          AND a~AccountingDocument       EQ b~AccountingDocumentWith
        INTO TABLE @gt_ventas_imp.

      SELECT a~CompanyCode, a~FiscalYear, a~AccountingDocument, a~BillingDocument, a~BillingDocumentType, a~TaxCode, a~ConditionType, a~TaxSupportId,
             a~TaxsIdRate, a~ConditionAmount, a~ConditionRateValue, a~ConditionCurrency
        FROM zcds_vc_ice AS a
        INNER JOIN @gt_Ventas AS b
           ON a~CompanyCode              EQ b~CompanyCode
          AND a~FiscalYear               EQ b~FiscalYear
          AND a~AccountingDocument       EQ b~AccountingDocument
          AND a~BillingDocument          EQ b~BillingDocument
          AND a~BillingDocumentType      EQ b~BillingDocumentType
        INTO TABLE @gt_ventas_ice.

      SELECT a~CompanyCode, a~FiscalYear, a~AccountingDocument, a~BillingDocument, a~BillingDocumentType, a~TaxCode, a~ConditionType, a~TaxSupportId,
             a~TaxsIdRate, a~ConditionAmount, a~ConditionRateValue, a~ConditionCurrency
        FROM zcds_vc_fob as a
        INNER JOIN @gt_Ventas AS b
           ON a~CompanyCode              EQ b~CompanyCode
          AND a~FiscalYear               EQ b~FiscalYearWith
          AND a~AccountingDocument       EQ b~AccountingDocumentWith
        INTO TABLE @gt_Gastos_Exp.

    ENDIF.

  ENDMETHOD.

  METHOD get_informant.

    DATA: lv_numestabruc TYPE n LENGTH 3,
          lv_texto       TYPE string.

    IF me->gt_AddlInformations[] IS NOT INITIAL."Ruc
      READ TABLE me->gt_AddlInformations INTO gs_AddlInformation WITH KEY CompanyCode =  me->gv_companycode CompanyCodeParameterType = 'CGIID'.
      IF sy-subrc EQ 0.
        is_informant-idinformante = gs_AddlInformation-CompanyCodeParameterValue.
        is_informant-typeid       = 'R'.
      ENDIF.
    ENDIF.

    lv_numestabruc = LINES( gt_tot_sales ).

    IF lv_numestabruc IS INITIAL.
      lv_numestabruc = 1.
    ENDIF.

    is_informant-anio            = gv_fiscalyear.
    is_informant-mes             = gv_fiscalperiod.
    lv_texto = gs_CompanyCode-CompanyCodeName.
    me->limpia_string( CHANGING iv_texto = lv_texto ).
    is_informant-razonsocial     = lv_texto.
    is_informant-codigooperativo = 'IVA'.
    is_informant-numestabruc     = lv_numestabruc.
    is_informant-totalventas     = '0.00'.

  ENDMETHOD.


  METHOD get_compras.

    DATA: lv_fecha       TYPE string,
          lv_TotalImp    TYPE navnw,
          lv_TotalNoI    TYPE navnw,
          lv_TotalExI    TYPE navnw,
          lv_TotalBas    TYPE navnw,
          lv_Total0I     TYPE navnw,
          lv_Total       TYPE navnw,
          lv_navnw       TYPE navnw,
          lv_retimp10    TYPE navnw,
          lv_retimp20    TYPE navnw,
          lv_retimp30    TYPE navnw,
          lv_retimp50    TYPE navnw,
          lv_retimp70    TYPE navnw,
          lv_retimp100   TYPE navnw,
          lv_porcentaje  TYPE string,
          lv_decimal     TYPE string,
          lv_entero      TYPE string,
          lv_sustentos   TYPE i,
          lv_length      TYPE i,
          lv_texto       TYPE string.

    DATA: ls_ec_003                   TYPE zdt_ec_003,
          lt_ec_003                   TYPE STANDARD TABLE OF zdt_ec_003,
          lt_ec_003_aux               TYPE STANDARD TABLE OF zdt_ec_003.

    LOOP AT gt_compras INTO gs_compra WHERE IsReversed IS INITIAL.



      CLEAR: lt_ec_003, lt_ec_003_aux, lv_sustentos.

      LOOP AT gt_compras_imp INTO gs_compra_imp WHERE CompanyCode        EQ gs_compra-CompanyCode
                                                  AND FiscalYear         EQ gs_compra-FiscalYear
                                                  AND AccountingDocument EQ gs_compra-AccountingDocument.

        READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY taxcode = gs_compra_imp-TaxCode CompanyCode = gs_compra_imp-CompanyCode.
        IF sy-subrc EQ 0.
          APPEND gs_ec_003 TO lt_ec_003.
        ENDIF.

      ENDLOOP.

      lt_ec_003_aux[] = lt_ec_003[].

      SORT lt_ec_003 BY supporttaxcode.
      DELETE ADJACENT DUPLICATES FROM lt_ec_003 COMPARING supporttaxcode. "Solo Codigos de Sustentos Distintos

      lv_sustentos = LINES( lt_ec_003 ).

      LOOP AT lt_ec_003 INTO ls_ec_003.

        gs_purchases-AccountingDocument     = gs_compra-AccountingDocument.
        gs_purchases-AccountingDocumentType = gs_compra-AccountingDocumentType.
        gs_purchases-tipocomprobante        = gs_compra-DocumentType.
        gs_purchases-establecimiento        = gs_compra-Establishment.
        gs_purchases-puntoemision           = gs_compra-Emissionpoint.
        gs_purchases-secuencial             = gs_compra-Sequential.
        gs_purchases-autorizacion           = gs_compra-Accesskey.
        gs_purchases-idprov                 = gs_compra-IdNumber.
        gs_purchases-codsustento            = ls_ec_003-supportTaxCode.

        lv_fecha = gs_compra-IssueDate.
        CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO gs_purchases-fecharegistro SEPARATED BY '/'.

        lv_fecha = gs_compra-AuthorizationDate.
        CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO gs_purchases-fechaemision SEPARATED BY '/'.

        IF gs_compra-DocumentTypeWith IS NOT INITIAL.

*          gs_purchases-tipocomprobante = gs_compra-DocumentTypeWith.
          gs_purchases-estabretencion1  = gs_compra-EstablishmentWith.
          gs_purchases-ptoemiretencion1 = gs_compra-EmissionpointWith.
          gs_purchases-secretencion1    = gs_compra-SequentialWith.
          gs_purchases-autretencion1    = gs_compra-AccesskeyWith.
          lv_fecha = gs_compra-IssueDateWith.
          CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO gs_purchases-fechaemiret1 SEPARATED BY '/'.

        ENDIF.

        IF gs_compra-DocumentTypeRef IS NOT INITIAL.

          gs_purchases-docmodificado    = gs_compra-DocumentTypeRef.
          gs_purchases-estabmodificado  = gs_compra-EstablishmentRef.
          gs_purchases-ptoemimodificado = gs_compra-EmissionpointRef.
          gs_purchases-secmodificado    = gs_compra-SequentialRef.
          gs_purchases-autretencion1    = gs_compra-AccesskeyRef.

        ENDIF.

        case gs_compra-Typeid.
          WHEN '04'.
            gs_purchases-tpidprov = '01'.
          WHEN '05'.
            gs_purchases-tpidprov = '02'.
          WHEN '06'.
            gs_purchases-tpidprov = '03'.
          WHEN '08'.
            gs_purchases-tpidprov = '03'.
          WHEN OTHERS.
            gs_purchases-tpidprov = gs_compra-Typeid.
        ENDCASE.

        IF gs_compra-Typeid EQ '06' OR gs_compra-Typeid EQ '08'.

          lv_texto = gs_compra-Businessname.
          me->limpia_string( CHANGING iv_texto = lv_texto ).
          gs_purchases-denopr = lv_texto.

        ENDIF.

        IF gs_compra-Country EQ 'EC'.

          gs_purchases-pagolocext         = '01'.
          gs_purchases-paisefecpago       = 'NA'.
          gs_purchases-paisefecpagogen    = 'NA'.
          gs_purchases-aplicconvdobtrib   = 'NA'.
          gs_purchases-pagextsujretnorleg = 'NA'.

        ELSE.

          READ TABLE gt_ec_009 INTO gs_ec_009 WITH KEY country = gs_compra-Country.
          IF sy-subrc EQ 0.
            gs_purchases-tiporegi = gs_ec_009-taxregime.
          ENDIF.

          gs_purchases-pagolocext         = '02'.

          IF gs_ec_009-taxagreement IS NOT INITIAL.
            gs_purchases-aplicconvdobtrib   = 'SI'.
          ELSE.
            gs_purchases-aplicconvdobtrib   = 'NO'.
            gs_purchases-pagextsujretnorleg = 'NO'.
          ENDIF.

          IF gs_ec_009-taxregime EQ '03'.
            READ TABLE gt_CountryText INTO gs_CountryText WITH KEY Country = gs_ec_009-Country.
            gs_purchases-denopago = gs_CountryText-CountryName.
          ENDIF.

          IF gs_ec_009-taxregime EQ '01'.

            gs_purchases-paisefecpago       = gs_ec_009-countrysri.
            gs_purchases-paisefecpagogen    = gs_ec_009-countrysri.

          ELSEIF gs_ec_009-taxregime EQ '02' OR gs_ec_009-taxregime EQ '03'.

            IF gs_ec_009-taxregime EQ '02'.
              gs_purchases-paisefecpago       = gs_ec_009-countrysri.
              gs_purchases-paisefecpagoparfis = gs_ec_009-taxhavencountry.
            ELSEIF gs_ec_009-taxregime EQ '03'.
              gs_purchases-paisefecpago       = gs_ec_009-taxhavencountry.
            ENDIF.

            gs_purchases-paisefecpagogen    = 'NA'."gs_ec_009-countrysri.

          ENDIF.

        ENDIF.

        IF gs_compra-BusinessPartnerCategory EQ '1'.
          gs_purchases-tipoprov = '01'.
        ELSE.
          gs_purchases-tipoprov = '02'.
        ENDIF.

        IF gs_compra-SupplierAccountNote IS NOT INITIAL.
          gs_purchases-parterel = 'SI'.
        ELSE.
          gs_purchases-parterel = 'NO'.
        ENDIF.

        CLEAR: lv_TotalExI, lv_TotalNoI, lv_Total0I, lv_TotalBas, lv_TotalImp, lv_Total.

        LOOP AT lt_ec_003_aux INTO gs_ec_003 WHERE companycode  EQ ls_ec_003-companycode
                                               AND taxsupportid EQ ls_ec_003-taxsupportid.

          READ TABLE gt_compras_imp INTO gs_compra_imp WITH KEY CompanyCode = gs_compra-CompanyCode
                                                                 FiscalYear = gs_compra-FiscalYear
                                                         AccountingDocument = gs_compra-AccountingDocument
                                                                    taxcode = gs_ec_003-TaxCode.
          IF sy-subrc EQ 0.

            CASE abap_true.
              WHEN gs_ec_003-exempttax.
                lv_TotalExI += gs_compra_imp-TaxBaseAmountInCoCodeCrcy.
              WHEN gs_ec_003-notax.
                lv_TotalNoI += gs_compra_imp-TaxBaseAmountInCoCodeCrcy.
              WHEN gs_ec_003-tax0.
                lv_Total0I  += gs_compra_imp-TaxBaseAmountInCoCodeCrcy.
              WHEN gs_ec_003-tax.
                lv_TotalImp += gs_compra_imp-TaxAmountInCoCodeCrcy.
                lv_TotalBas += gs_compra_imp-TaxBaseAmountInCoCodeCrcy.
              WHEN OTHERS.
            ENDCASE.

          ENDIF.

        ENDLOOP.

        gs_purchases-baseimpexe    = lv_TotalExI.
        gs_purchases-basenograiva  = lv_TotalNoI.
        gs_purchases-baseimponible = lv_Total0I.
        gs_purchases-baseimpgrav   = lv_TotalBas.
        gs_purchases-montoice      = '0.00'.
        gs_purchases-montoiva      = lv_TotalImp.

        lv_Total = lv_TotalBas + lv_Total0I + lv_TotalNoI + lv_TotalExI.

        CLEAR: lv_retimp10, lv_retimp20, lv_retimp30, lv_retimp50, lv_retimp70, lv_retimp100.

        LOOP AT gt_compras_ret INTO gs_compra_ret WHERE CompanyCode        EQ gs_compra-CompanyCode
                                                    AND FiscalYear         EQ gs_compra-FiscalYear
                                                    AND AccountingDocument EQ gs_compra-AccountingDocument.

          CASE gs_compra_ret-withholdingtype.
            WHEN '1'. "Fuente

              gs_withholdings-accountingdocument     = gs_compra-AccountingDocument.
              gs_withholdings-accountingdocumenttype = gs_compra-AccountingDocumentType.
              gs_withholdings-codretair              = gs_compra_ret-OfficialWhldgTaxCode.
              lv_porcentaje                          = gs_compra_ret-WithholdingTaxPercent.
              SPLIT lv_porcentaje AT '.' INTO lv_entero lv_decimal.
              CLEAR: lv_porcentaje.
              IF lv_decimal IS NOT INITIAL.

                lv_length = strlen( lv_decimal ).

                IF lv_length GT 2.
                  IF lv_decimal+2(1) EQ '0'.
                    lv_decimal = lv_decimal(2).
                  ELSE.
                    lv_decimal = lv_decimal(3).
                  ENDIF.
                ELSEIF lv_decimal EQ 2.
                  lv_decimal = '00'.
                ELSE.
                  lv_decimal = '00'.
                ENDIF.

              ENDIF.

              lv_porcentaje = |{ lv_entero }.{ lv_decimal }|.
              gs_withholdings-porcentajeair          = lv_porcentaje.

              IF lv_sustentos EQ 1.
                gs_withholdings-baseimpair             = gs_compra_ret-WhldgTaxBaseAmtInCoCodeCrcy.
                gs_withholdings-valretair              = gs_compra_ret-WhldgTaxAmtInCoCodeCrcy.
              ELSE.
                gs_withholdings-baseimpair             = lv_Total.
                gs_withholdings-valretair              = ( lv_Total * gs_compra_ret-WithholdingTaxPercent ) / 100.
              ENDIF.

              APPEND gs_withholdings TO gt_withholdings.
              CLEAR: gs_withholdings.

            WHEN '2'. "IVA

              IF lv_sustentos GT 1.
                gs_compra_ret-WhldgTaxAmtInCoCodeCrcy              = ( lv_TotalImp * gs_compra_ret-WithholdingTaxPercent ) / 100.
              ENDIF.

              CASE gs_compra_ret-WithholdingTaxPercent.
                WHEN '10'.
                  lv_retimp10  += gs_compra_ret-WhldgTaxAmtInCoCodeCrcy.
                WHEN '20'.
                  lv_retimp20  += gs_compra_ret-WhldgTaxAmtInCoCodeCrcy.
                WHEN '30'.
                  lv_retimp30  += gs_compra_ret-WhldgTaxAmtInCoCodeCrcy.
                WHEN '50'.
                  lv_retimp50  += gs_compra_ret-WhldgTaxAmtInCoCodeCrcy.
                WHEN '70'.
                  lv_retimp70  += gs_compra_ret-WhldgTaxAmtInCoCodeCrcy.
                WHEN '100'.
                  lv_retimp100 += gs_compra_ret-WhldgTaxAmtInCoCodeCrcy.
                WHEN OTHERS.
              ENDCASE.

            WHEN OTHERS.

          ENDCASE.

        ENDLOOP.

        lv_Total += lv_TotalImp.

        IF gs_compra-DocumentType NE '04'.
          READ TABLE gt_ec_006 INTO gs_ec_006 WITH KEY CompanyCode   = gs_compra-CompanyCode
                                                       PaymentMethod = gs_compra-PaymentMethod.
          IF sy-subrc EQ 0.
            gs_purchases-formapago          = gs_ec_006-paymentsri.
          ELSEIF lv_Total GE 500.
            gs_purchases-formapago          = '20'.
          ENDIF.
        ENDIF.

        gs_purchases-valretbien10      = lv_retimp10.
        gs_purchases-valretserv20      = lv_retimp20.
        gs_purchases-valorretbienes    = lv_retimp30.
        gs_purchases-valretserv50      = lv_retimp50.
        gs_purchases-valorretservicios = lv_retimp70.
        gs_purchases-valretserv100     = lv_retimp100.

        CLEAR: lv_Total.

        READ TABLE gt_support INTO gs_support WITH KEY AccountingDocument = gs_compra-AccountingDocument
                                                   accountingdocumenttype = gs_compra-AccountingDocumentType.
        IF sy-subrc NE 0.

          LOOP AT gt_ec_013 INTO gs_ec_013 WHERE CompanyCode            EQ gs_compra-CompanyCode
                                             AND FiscalYear             EQ gs_compra-FiscalYear
                                             AND AccountingDocument     EQ gs_compra-AccountingDocument
                                             AND accountingdocumenttype EQ gs_compra-AccountingDocumentType.


            CASE gs_ec_013-typeid.
              WHEN '04'.
                gs_support-tpidprovreemb          = '01'.
              WHEN '05'.
                gs_support-tpidprovreemb          = '02'.
              WHEN '06'.
                gs_support-tpidprovreemb          = '03'.
            ENDCASE.

            gs_support-accountingdocument     = gs_ec_013-AccountingDocument.
            gs_support-accountingdocumenttype = gs_ec_013-AccountingDocumentType.
            gs_support-tipocomprobantereemb   = gs_ec_013-DocumentType.
            gs_support-idprovreemb            = gs_ec_013-idnumber.
            gs_support-establecimientoreemb   = gs_ec_013-establishment.
            gs_support-puntoemisionreemb      = gs_ec_013-emissionpoint.
            gs_support-secuencialreemb        = gs_ec_013-sequential.
            gs_support-autorizacionreemb      = gs_ec_013-accesskey.
            gs_support-montoivaremb           = gs_ec_013-amounttax.
            gs_support-montoicereemb          = gs_ec_013-amountice.
            gs_support-montoivaremb           = gs_ec_013-amounttax.
            gs_support-montoicereemb          = gs_ec_013-amountice.
            gs_support-baseimponiblereemb     = gs_ec_013-amountbasetax0.
            gs_support-baseimpgravreemb       = gs_ec_013-amountbasetax.
            gs_support-basenograivareemb      = gs_ec_013-amountbasenotax.
            gs_support-baseimpexereemb        = gs_ec_013-amountbaseexetax.

            gs_support-totbasesimpreemb       = gs_ec_013-amountbasetax0 + gs_ec_013-amountbasetax
                                              + gs_ec_013-amountbasenotax + gs_ec_013-amountbaseexetax.
            lv_fecha = gs_ec_013-issuedate.
            CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO gs_support-fechaemisionreemb SEPARATED BY '/'.

            lv_Total = gs_ec_013-amountbasetax0 + gs_ec_013-amountbasetax + gs_ec_013-amountbasenotax + gs_ec_013-amountbaseexetax.

            APPEND gs_support TO gt_support.
            CLEAR: gs_support.

          ENDLOOP.

        ENDIF.

        gs_purchases-totbasesimpreemb  = lv_Total.

        APPEND gs_purchases TO it_purchases.
        CLEAR: gs_purchases.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_exportaciones.

    DATA: lv_texto       TYPE string,
          lv_fecha       TYPE string,
          lv_Total_fob   TYPE navnw,
          lv_Total_fac   TYPE navnw.

    LOOP AT gt_ventas INTO gs_venta WHERE IsReversed IS INITIAL
                                      AND Export     IS NOT INITIAL.
      CLEAR: gs_tot_sales.
      gs_tot_sales-establishment      = gs_venta-Establishment.
*      gs_tot_sales-salesestablishment = gs_venta-TotalNetAmount + gs_venta-TotalTaxAmount.
      gs_tot_sales-salesestablishment = '0.00'.
      gs_tot_sales-taxcompensated     = '0.00'.

      gs_export-idclienteex           = gs_venta-Idnumber.
      gs_export-tpidclienteex         = gs_venta-Typeid.
      gs_export-tipocomprobante       = gs_venta-Documenttype.
      gs_export-establecimiento       = gs_venta-Establishment.
      gs_export-puntoemision          = gs_venta-Emissionpoint.
      gs_export-secuencial            = gs_venta-Sequential.
      gs_export-autorizacion          = gs_venta-Accesskey.

      lv_fecha = gs_venta-Issuedate.
      CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO gs_export-fechaemision SEPARATED BY '/'.

      IF gs_venta-CustomerAccountNote IS NOT INITIAL.
        gs_export-parterel = 'SI'.
      ELSE.
        gs_export-parterel = 'NO'.
      ENDIF.

      IF gs_venta-Typeid EQ '21'.

        IF gs_venta-BusinessPartnerCategory EQ '1'.
          gs_export-tipocli = '01'.
        ELSE.
          gs_export-tipocli = '02'.
        ENDIF.

        lv_texto = gs_venta-Businessname.
        me->limpia_string( CHANGING iv_texto = lv_texto ).
        gs_export-denoexpcli = lv_texto.

      ENDIF.

      READ TABLE gt_ec_009 INTO gs_ec_009 WITH KEY country = gs_venta-Country.
      IF sy-subrc EQ 0.

        gs_export-tiporegi = gs_ec_009-taxregime.

        IF gs_ec_009-taxregime EQ '01'.

          gs_export-paisefecpagogen    = gs_ec_009-countrysri.
          gs_export-paisefecexp        = gs_ec_009-countrysri.

        ELSEIF gs_ec_009-taxregime EQ '02'.

          gs_export-paisefecpagoparfis = gs_ec_009-taxhavencountry.
          gs_export-paisefecexp        = gs_ec_009-taxhavencountry.

        ELSEIF gs_ec_009-taxregime EQ '03'.

          READ TABLE gt_CountryText INTO gs_CountryText WITH KEY Country = gs_ec_009-Country.
          gs_export-denopagoregfis     = gs_CountryText-CountryName.
          gs_export-paisefecexp        = gs_ec_009-countrysri.

        ENDIF.

      ENDIF.

*      gs_export-exportacionde = Tabla 10

      IF gs_export-exportacionde EQ '03'.
*        gs_export-tipingext = tabla 18
*        gs_export-impuestootropais = 'SI' OR 'NO'.
      ENDIF.

      IF gs_export-impuestootropais EQ 'SI'.
*        gs_export-ingextgravotropais =
      ENDIF.

      IF gs_export-exportacionde EQ '01'.
*        gs_export-distaduanero = Tabla 6
*        gs_export-anio
*        gs_export-regimen = Tabla 7 or Tabla 7,1
*        gs_export-correlativo
*        gs_export-doctransp
      ENDIF.

*      lv_fecha = gs_venta-Issuedate. "Cambiar fecha
*      CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO gs_export-fechaembarque SEPARATED BY '/'.

      CLEAR: lv_Total_fob.
      LOOP AT gt_Gastos_Exp INTO gs_Gastos_Exp WHERE CompanyCode        EQ gs_venta-CompanyCode
                                                 AND FiscalYear         EQ gs_venta-FiscalYearWith
                                                 AND AccountingDocument EQ gs_venta-AccountingDocumentWith.

        lv_Total_fob += abs( gs_Gastos_Exp-ConditionAmount ).

      ENDLOOP.

      lv_Total_fac = gs_venta-TotalNetAmount + gs_venta-TotalTaxAmount.

      lv_Total_fob = lv_Total_fac - lv_Total_fob.

      gs_export-valorfobcomprobante = lv_Total_fac.

      me->get_totalventas( EXPORTING is_total_sales = gs_tot_sales CHANGING it_total_sales    = me->gt_tot_sales ).

      APPEND gs_export TO it_export.
      CLEAR: gs_export.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_ventas.

    DATA: lv_texto TYPE string.

    LOOP AT gt_ventas INTO gs_venta WHERE IsReversed IS INITIAL
                                      AND DocumentStatus EQ 'AUTHORIZED'
                                      AND Export     IS INITIAL.

      CLEAR: gs_tot_sales.
      gs_tot_sales-establishment      = gs_venta-Establishment.
*      gs_tot_sales-salesestablishment = gs_venta-TotalNetAmount + gs_venta-TotalTaxAmount.
      gs_tot_sales-salesestablishment = '0.00'.
      gs_tot_sales-taxcompensated     = '0.00'.

      gs_sales-idcliente            = gs_venta-Idnumber.
      gs_sales-tpidcliente          = gs_venta-Typeid.
      gs_sales-tipocomprobante      = gs_venta-Documenttype.
*      gs_sales-tipocompe            = .
      gs_sales-tipoem               = 'E'.
      gs_sales-numerocomprobantes   = 1.

      gs_sales-montoiva             = gs_venta-TotalTaxAmount.

      READ TABLE gt_ec_006 INTO gs_ec_006 WITH KEY paymentmethod = gs_venta-PaymentMethod.
      IF sy-subrc EQ 0 AND gs_venta-Documenttype NE '04'.
        gs_sales-formapago = gs_ec_006-paymentsri.
      ELSEIF sy-subrc EQ 0 AND gs_venta-Documenttype EQ '04'.
        gs_sales-formapago = ''.
      ELSE.
        gs_sales-formapago = '20'.
      ENDIF.

      IF gs_venta-CustomerAccountNote IS NOT INITIAL.
        gs_sales-parterel = 'SI'.
      ELSE.
        gs_sales-parterel = 'NO'.
      ENDIF.

      IF gs_venta-Typeid EQ '06'.
        IF gs_venta-BusinessPartnerCategory EQ '1'.
          gs_sales-tipocliente = '01'.
        ELSE.
          gs_sales-tipocliente = '02'.
        ENDIF.
        lv_texto = gs_venta-Businessname.
        me->limpia_string( CHANGING iv_texto = lv_texto ).
        gs_sales-denocli = lv_texto.
      ENDIF.

      LOOP AT gt_ventas_imp INTO gs_venta_imp WHERE CompanyCode        EQ gs_venta-CompanyCode
                                                AND FiscalYear         EQ gs_venta-FiscalYearWith
                                                AND AccountingDocument EQ gs_venta-AccountingDocumentWith.

        READ TABLE gt_ec_003 INTO gs_ec_003 WITH KEY companycode  = gs_venta_imp-CompanyCode
                                                     taxcode      = gs_venta_imp-TaxCode.
        IF sy-subrc EQ 0.
          CASE abap_true.
            WHEN gs_ec_003-exempttax.
              gs_sales-basenograiva   += gs_venta_imp-TaxBaseAmountInCoCodeCrcy.
            WHEN gs_ec_003-notax.
              gs_sales-baseimponible  += gs_venta_imp-TaxBaseAmountInCoCodeCrcy.
            WHEN gs_ec_003-tax0.
              gs_sales-baseimponible  += gs_venta_imp-TaxBaseAmountInCoCodeCrcy.
            WHEN gs_ec_003-tax.
              gs_sales-baseimpgrav    += gs_venta_imp-TaxBaseAmountInCoCodeCrcy.
          ENDCASE.

        ENDIF.

      ENDLOOP.

      LOOP AT gt_ventas_ice INTO gs_venta_ice WHERE CompanyCode        EQ gs_venta-CompanyCode
                                                AND FiscalYear         EQ gs_venta-FiscalYearWith
                                                AND AccountingDocument EQ gs_venta-AccountingDocumentWith.

        gs_sales-montoice  += gs_venta_ice-ConditionRateValue.


      ENDLOOP.

      LOOP AT gt_ventas_ret INTO gs_venta_ret WHERE CompanyCode        EQ gs_venta-CompanyCode
                                                AND FiscalYear         EQ gs_venta-FiscalYearWith
                                                AND AccountingDocument EQ gs_venta-AccountingDocumentWith.

        gs_sales-valorretiva   += gs_venta_ret-WhldgTaxAmtInCoCodeCrcy.
        gs_sales-valorretrenta += gs_venta_ret-WhldgTaxBaseAmtInCoCodeCrcy.

      ENDLOOP.

      IF gs_sales-basenograiva IS INITIAL.
        gs_sales-basenograiva = '0.00'.
      ENDIF.

      IF gs_sales-baseimponible IS INITIAL.
        gs_sales-baseimponible = '0.00'.
      ENDIF.

      IF gs_sales-baseimpgrav IS INITIAL.
        gs_sales-baseimpgrav = '0.00'.
      ENDIF.

      IF gs_sales-montoice IS INITIAL.
        gs_sales-montoice = '0.00'.
      ENDIF.

      IF gs_sales-valorretiva IS INITIAL.
        gs_sales-valorretiva = '0.00'.
      ENDIF.

      IF gs_sales-valorretrenta IS INITIAL.
        gs_sales-valorretrenta = '0.00'.
      ENDIF.

      me->get_totalventas( EXPORTING is_total_sales = gs_tot_sales CHANGING it_total_sales    = me->gt_tot_sales ).

      READ TABLE it_sales ASSIGNING FIELD-SYMBOL(<fs_sales>) WITH KEY idcliente     = gs_sales-idcliente
                                                                         tipoem     = gs_sales-tipoem
                                                                      tipocliente   = gs_sales-tipocliente
                                                                    tipocomprobante = gs_sales-tipocomprobante.
      IF sy-subrc EQ 0.
        <fs_sales>-basenograiva  += gs_sales-basenograiva.
        <fs_sales>-baseimponible += gs_sales-baseimponible.
        <fs_sales>-baseimpgrav   += gs_sales-baseimpgrav.
        <fs_sales>-montoice      += gs_sales-montoice.
        <fs_sales>-valorretiva   += gs_sales-valorretiva.
        <fs_sales>-valorretrenta += gs_sales-valorretrenta.
      ELSE.
        APPEND gs_sales TO it_sales.
        CLEAR: gs_sales.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_totalventas.

    READ TABLE it_total_sales ASSIGNING FIELD-SYMBOL(<fs_total_sales>) WITH KEY establishment =  is_total_sales-establishment.
    IF sy-subrc EQ 0.
      <fs_total_sales>-salesestablishment += is_total_sales-salesestablishment.
    ELSE.
      APPEND is_total_sales TO it_total_sales.
    ENDIF.

  ENDMETHOD.

  METHOD get_anulados.

    LOOP AT gt_Ventas INTO gs_Venta WHERE IsReversed IS NOT INITIAL.

      gs_canceled-accountingdocument       = gs_Venta-AccountingDocument.
      gs_canceled-accountingdocumenttype   = gs_Venta-AccountingDocumentType.
      gs_canceled-billingdocument          = gs_Venta-BillingDocument.
      gs_canceled-billingdocumenttype      = gs_Venta-BillingDocumentType.
      gs_canceled-tipocomprobante          = gs_Venta-DocumentType.
      gs_canceled-establecimiento          = gs_Venta-Establishment.
      gs_canceled-puntoemision             = gs_Venta-Emissionpoint.
      gs_canceled-secuencialinicio         = gs_canceled-secuencialfin            = gs_Venta-Sequential.
      gs_canceled-autorizacion             = gs_Venta-Accesskey.
      APPEND gs_canceled TO it_canceled.
      CLEAR: gs_canceled.

    ENDLOOP.

    LOOP AT gt_Compras INTO gs_Compra WHERE IsReversed IS NOT INITIAL
                                        AND (  (  DocumentType  EQ '03' OR
                                                  DocumentType  EQ '41' )
                                             OR ( DocumentType    EQ '01' AND
                                                  EstablishmentWith IS NOT INITIAL ) ).

      IF gs_Compra-EstablishmentWith IS NOT INITIAL.
        gs_canceled-accountingdocument       = gs_Compra-AccountingDocument.
        gs_canceled-accountingdocumenttype   = gs_Compra-AccountingDocumentType.
        gs_canceled-tipocomprobante          = gs_Compra-DocumentTypeWith.
        gs_canceled-establecimiento          = gs_Compra-EstablishmentWith.
        gs_canceled-puntoemision             = gs_Compra-EmissionpointWith.
        gs_canceled-secuencialinicio         = gs_canceled-secuencialfin            = gs_Compra-SequentialWith.
        gs_canceled-autorizacion             = gs_Compra-AccesskeyWith.
        APPEND gs_canceled TO it_canceled.
        CLEAR: gs_canceled.
      ENDIF.

      IF gs_Compra-DocumentType  EQ '03' OR gs_Compra-DocumentType  EQ '41'.
        gs_canceled-accountingdocument       = gs_Compra-AccountingDocument.
        gs_canceled-accountingdocumenttype   = gs_Compra-AccountingDocumentType.
        gs_canceled-tipocomprobante          = gs_Compra-DocumentType.
        gs_canceled-establecimiento          = gs_Compra-Establishment.
        gs_canceled-puntoemision             = gs_Compra-Emissionpoint.
        gs_canceled-secuencialinicio         = gs_canceled-secuencialfin            = gs_Compra-Sequential.
        gs_canceled-autorizacion             = gs_Compra-Accesskey.
        APPEND gs_canceled TO it_canceled.
        CLEAR: gs_canceled.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.

  METHOD limpia_string.

    REPLACE ALL OCCURRENCES OF 'Ñ' IN iv_texto WITH 'N' .
    REPLACE ALL OCCURRENCES OF 'ñ' IN iv_texto WITH 'n' .
    REPLACE ALL OCCURRENCES OF '.' IN iv_texto WITH '' .
    REPLACE ALL OCCURRENCES OF ',' IN iv_texto WITH '' .
    REPLACE ALL OCCURRENCES OF '-' IN iv_texto WITH '' .
    REPLACE ALL OCCURRENCES OF '_' IN iv_texto WITH '' .
    REPLACE ALL OCCURRENCES OF 'Ú' IN iv_texto WITH 'U' .
    REPLACE ALL OCCURRENCES OF 'ú' IN iv_texto WITH 'u' .
    REPLACE ALL OCCURRENCES OF 'Ó' IN iv_texto WITH 'O' .
    REPLACE ALL OCCURRENCES OF 'ó' IN iv_texto WITH 'o' .
    REPLACE ALL OCCURRENCES OF 'é' IN iv_texto WITH 'e' .
    REPLACE ALL OCCURRENCES OF 'É' IN iv_texto WITH 'E' .
    REPLACE ALL OCCURRENCES OF 'Í' IN iv_texto WITH 'I' .
    REPLACE ALL OCCURRENCES OF 'Á' IN iv_texto WITH 'A' .
    REPLACE ALL OCCURRENCES OF '.' IN iv_texto WITH ' '.
    REPLACE ALL OCCURRENCES OF '&' IN iv_texto WITH ' '.
    REPLACE ALL OCCURRENCES OF '(' IN iv_texto WITH ''.
    REPLACE ALL OCCURRENCES OF ')' IN iv_texto WITH ''.
    REPLACE ALL OCCURRENCES OF '[' IN iv_texto WITH ''.
    REPLACE ALL OCCURRENCES OF ']' IN iv_texto WITH ''.
    REPLACE ALL OCCURRENCES OF '/' IN iv_texto WITH ''.
    REPLACE ALL OCCURRENCES OF '\' IN iv_texto WITH ''.
    REPLACE ALL OCCURRENCES OF '#' IN iv_texto WITH ''.
    REPLACE ALL OCCURRENCES OF '>' IN iv_texto WITH ''.
    REPLACE ALL OCCURRENCES OF '<' IN iv_texto WITH ''.
    REPLACE ALL OCCURRENCES OF '¡' IN iv_texto WITH ''.
    REPLACE ALL OCCURRENCES OF '!' IN iv_texto WITH ''.
    REPLACE ALL OCCURRENCES OF '|' IN iv_texto WITH ''.
    REPLACE ALL OCCURRENCES OF '$' IN iv_texto WITH ''.
*    REPLACE ALL OCCURRENCES OF ' IN iv_texto WITH ''.

  ENDMETHOD.

ENDCLASS.
