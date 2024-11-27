CLASS zcl_create_guia_traslado DEFINITION

  PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.


    TYPES: ty_detalle_g TYPE STANDARD TABLE OF zts_guia_detalle,
           ty_det_add   TYPE STANDARD TABLE OF zts_det_add,
           ty_head_add  TYPE STANDARD TABLE OF zts_head_add.

    DATA: gv_companycode            TYPE bukrs,
          gv_documenttype           TYPE zde_trsri,
          gv_usuname                TYPE c LENGTH 12,
          gv_materialdocumentyear   TYPE mjahr,
          gv_materialdocument       TYPE mblnr,
          gv_goodsmovementtype      TYPE bwart.

    METHODS constructor IMPORTING companycode            TYPE bukrs
                                  materialdocumentyear   TYPE mjahr
                                  materialdocument       TYPE mblnr
                                  goodsmovementtype      TYPE bwart.

    METHODS callDocumentType IMPORTING documenttype TYPE zde_trsri             "Tipo de Documento SRI
                             EXPORTING inf_tribu    TYPE zts_inf_tribu
                                       guia         TYPE zts_guia_header
                                       t_detalle_g  TYPE zcl_create_guia_traslado=>ty_detalle_g
                                       t_det_add    TYPE zcl_create_guia_traslado=>ty_det_add
                                       t_head_add   TYPE zcl_create_guia_traslado=>ty_head_add.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: gs_header_g    TYPE zts_guia_header,
          gs_inf_tribu   TYPE zts_inf_tribu,
          gs_detalle_g   TYPE zts_guia_detalle,
          gs_det_add     TYPE zts_det_add,
          gs_head_add    TYPE zts_head_add,
          gs_ec_001      TYPE zdt_ec_001,
          gs_ec_002      TYPE zdt_ec_002,
          gs_ec_003      TYPE zdt_ec_003,
          gs_ec_004      TYPE zdt_ec_004,
          gs_ec_005      TYPE zdt_ec_005,
          gs_ec_006      TYPE zdt_ec_006,
          gs_ec_007      TYPE zdt_ec_007,
          gs_ec_008      TYPE zdt_ec_008,
          gs_ec_011      TYPE zdt_ec_011.

    DATA: gt_detalle_g   TYPE STANDARD TABLE OF zts_guia_detalle,
          gt_det_add     TYPE STANDARD TABLE OF zts_det_add,
          gt_head_add    TYPE STANDARD TABLE OF zts_head_add,
          gt_ec_001      TYPE STANDARD TABLE OF zdt_ec_001,
          gt_ec_002      TYPE STANDARD TABLE OF zdt_ec_002,
          gt_ec_003      TYPE STANDARD TABLE OF zdt_ec_003,
          gt_ec_004      TYPE STANDARD TABLE OF zdt_ec_004,
          gt_ec_005      TYPE STANDARD TABLE OF zdt_ec_005,
          gt_ec_006      TYPE STANDARD TABLE OF zdt_ec_006,
          gt_ec_007      TYPE STANDARD TABLE OF zdt_ec_007,
          gt_ec_008      TYPE STANDARD TABLE OF zdt_ec_008.

    DATA: gs_guia                     TYPE zdt_sd_doc_guia,
          gs_guia_t                   TYPE zdt_mm_doc_guia,
          gs_GoodsMovement            TYPE I_GoodsMovementCube,
          gs_Product                  TYPE I_ProductDescription_2,
          gs_CompanyCode              TYPE I_CompanyCode,
          gs_AddlInformation          TYPE I_AddlCompanyCodeInformation,
          gs_SupplierWithHoldingTax   TYPE I_SupplierWithHoldingTax,
          gs_BusinessPartner          TYPE I_BusinessPartner,
          gs_Address                  TYPE i_address_2,
          gs_Plant                    TYPE I_Plant,
          gs_OrganizationAddress      TYPE I_OrganizationAddress,
          gs_Businesspartnertaxnumber TYPE I_Businesspartnertaxnumber,
          gs_email                    TYPE I_AddressEmailAddress_2,
          gs_telefono                 TYPE I_AddressPhoneNumber_2,
          gs_BusPartAddress           TYPE I_BusPartAddress.

    DATA: gt_GoodsMovement            TYPE STANDARD TABLE OF I_GoodsMovementCube,
          gt_Product                  TYPE STANDARD TABLE OF I_ProductDescription_2,
          gt_CompanyCode              TYPE STANDARD TABLE OF I_CompanyCode,
          gt_AddlInformation          TYPE STANDARD TABLE OF I_AddlCompanyCodeInformation,
          gt_BusinessPartner          TYPE STANDARD TABLE OF I_BusinessPartner,
          gt_ADDRESS                  TYPE STANDARD TABLE OF i_address_2,
          gt_Plant                    TYPE STANDARD TABLE OF I_Plant,
          gt_OrganizationAddress      TYPE STANDARD TABLE OF I_OrganizationAddress,
          gt_Businesspartnertaxnumber TYPE STANDARD TABLE OF I_Businesspartnertaxnumber,
          gt_email                    TYPE STANDARD TABLE OF I_AddressEmailAddress_2,
          gt_telefono                 TYPE STANDARD TABLE OF I_AddressPhoneNumber_2.

    METHODS get_data .

    METHODS infoTributaria  CHANGING inf_tribu   TYPE zts_inf_tribu.

    METHODS getClaveAcceso  IMPORTING inf_tribu   TYPE zts_inf_tribu
                                      fecha       TYPE string
                                      api         TYPE zde_type_api
                            CHANGING  estab       TYPE zts_inf_tribu-estab
                                      ptoemi      TYPE zts_inf_tribu-ptoemi
                                      secuencial  TYPE zts_inf_tribu-secuencial
                                      claveacceso TYPE zts_inf_tribu-claveacceso.

    METHODS getHeaderGuia      CHANGING  header_g  TYPE zts_guia_header.

    METHODS getDatosBP         CHANGING header_g   TYPE zts_guia_header.

    METHODS getDetalles        CHANGING detalle   TYPE zcl_create_guia_traslado=>ty_detalle_g
                                        detalle_a TYPE zcl_create_guia_traslado=>ty_det_add.

    METHODS getHeaderAdd       CHANGING header_add TYPE zcl_create_guia_traslado=>ty_head_add.

ENDCLASS.



CLASS ZCL_CREATE_GUIA_TRASLADO IMPLEMENTATION.


  METHOD calldocumenttype.

    gv_documenttype =  documenttype.

    me->get_data( ).

    me->infoTributaria( CHANGING inf_tribu = me->gs_inf_tribu ).

    CASE gv_documenttype.
      WHEN '06'.

        me->getheaderguia(     CHANGING header_g      = me->gs_header_g ).
        me->getdetalles(       CHANGING detalle       = me->gt_detalle_g
                                        detalle_a     = me->gt_det_add ).
        me->getHeaderAdd(      CHANGING header_add    = me->gt_head_add ).

        guia        = me->gs_header_g.
        inf_tribu   = me->gs_inf_tribu.
        t_head_add  = me->gt_head_add.
        t_detalle_g = me->gt_detalle_g.
        t_det_add   = me->gt_det_add.

    ENDCASE.

  ENDMETHOD.


  METHOD constructor.

    gv_companycode            = companycode.

    gv_materialdocumentyear   = materialdocumentyear.
    gv_materialdocument       = materialdocument.
    gv_goodsmovementtype      = goodsmovementtype.

  ENDMETHOD.


  METHOD getclaveacceso.

    DATA: lv_num     TYPE i,
          lv_sum     TYPE p,
          lv_sumt    TYPE i,
          lv_val     TYPE p,
          lv_six     TYPE i,
          lv_mod     TYPE i,
          lv_rep     TYPE p,
          lv_rep2(2) TYPE c.

    CASE me->gv_documenttype.
      WHEN '06'.
        IF me->gs_guia_t IS NOT INITIAL.
          estab       = me->gs_guia_t-establishment.
          ptoemi      = me->gs_guia_t-emissionpoint.
          secuencial  = me->gs_guia_t-sequential.
          claveacceso = me->gs_guia_t-accesskey.
          EXIT.
        ENDIF.
    ENDCASE.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = api fieldname = 'COD_NUM'. "Codigo Numerico
    IF sy-subrc EQ 0.
      DATA(lv_cod) = gs_ec_007-low.
    ENDIF.

    IF me->gs_ec_008 IS NOT INITIAL.
      estab  = gs_ec_008-establishment.
      ptoemi = gs_ec_008-emissionpoint.
    ENDIF.

    IF me->gs_ec_002 IS NOT INITIAL.

      TRY.
          CALL METHOD cl_numberrange_runtime=>number_get
            EXPORTING
              nr_range_nr = '01'
              object      = me->gs_ec_002-Objet
            IMPORTING
              number      = DATA(lv_number)
              returncode  = DATA(lv_rcode).

          IF lv_number IS NOT INITIAL.
            DATA(lv_len)  = strlen( lv_number ).
            DATA(lv_cant) = lv_len - 9.
            IF lv_cant LE 0.
              lv_cant = 0.
            ENDIF.
            IF lv_len GT 9.
              lv_len = 9.
            ENDIF.
            secuencial = lv_number+lv_cant(lv_len).
          ENDIF.

        CATCH cx_number_ranges INTO DATA(lr_error).

      ENDTRY.

    ENDIF.

    CONCATENATE fecha+6(2) fecha+4(2) fecha+0(4)
                inf_tribu-coddoc
                inf_tribu-ruc
                inf_tribu-ambiente
                estab
                ptoemi
                secuencial
                lv_cod
                inf_tribu-tipoemision
                INTO claveacceso.

    lv_num = strlen( claveacceso ).

    WHILE lv_num GT 0.
      lv_six = 6.
      WHILE lv_six NE 0.
        IF lv_num NE 0.
          lv_num = lv_num - 1.
          lv_val = claveacceso+lv_num(1).
          CASE lv_six.
            WHEN 6.
              lv_val = lv_val * 2.
              lv_sum = lv_sum + lv_val.
            WHEN 5.
              lv_val = lv_val * 3.
              lv_sum = lv_sum + lv_val.
            WHEN 4.
              lv_val = lv_val * 4.
              lv_sum = lv_sum + lv_val.
            WHEN 3.
              lv_val = lv_val * 5.
              lv_sum = lv_sum + lv_val.
            WHEN 2.
              lv_val = lv_val * 6.
              lv_sum = lv_sum + lv_val.
            WHEN 1.
              lv_val = lv_val * 7.
              lv_sum = lv_sum + lv_val.
          ENDCASE.
        ENDIF.
        lv_six = lv_six - 1.
      ENDWHILE.
    ENDWHILE.

    lv_mod  = lv_sum MOD 11.
    lv_rep  = 11 - lv_mod.
    lv_rep2 = lv_rep.

    IF lv_rep = 11.
      lv_rep2 = 0.
    ELSEIF lv_rep = 10.
      lv_rep2 = 1.
    ENDIF.

    CONCATENATE claveacceso lv_rep2 INTO claveacceso.

  ENDMETHOD.


  METHOD getDatosBP.

    IF me->gs_goodsmovement-Supplier IS INITIAL.
      SELECT SINGLE *
       FROM I_Plant
       WHERE Plant EQ @me->gs_goodsmovement-IssuingOrReceivingPlant
       INTO @gs_Plant.

     SELECT SINGLE *
       FROM I_OrganizationAddress
       WITH PRIVILEGED ACCESS
       WHERE AddressID EQ @me->gs_Plant-AddressID
       INTO @gs_OrganizationAddress.

     IF sy-subrc EQ 0.
       CONCATENATE me->gs_OrganizationAddress-StreetName me->gs_OrganizationAddress-HouseNumber
                   me->gs_OrganizationAddress-StreetPrefixName1 me->gs_OrganizationAddress-StreetPrefixName2 INTO header_g-dir_destinatario.
     ENDIF.

     header_g-id_destinatario = me->gs_inf_tribu-ruc.
     header_g-rz_destinatario = me->gs_inf_tribu-razonsocial.

   ELSE.

      SELECT SINGLE *
        FROM I_BusinessPartner
        WHERE BusinessPartner EQ @me->gs_goodsmovement-Supplier
        INTO @gs_BusinessPartner.

      SELECT *
       FROM I_Businesspartnertaxnumber
        WHERE BusinessPartner EQ @me->gs_goodsmovement-Supplier
        INTO TABLE @gt_Businesspartnertaxnumber.

      SELECT SINGLE *
       FROM I_BusPartAddress
        WITH PRIVILEGED ACCESS
        WHERE BusinessPartner EQ @me->gs_goodsmovement-Supplier
        INTO @me->gs_BusPartAddress.

      IF sy-subrc EQ 0.

        SELECT SINGLE *
          FROM i_address_2
          WITH PRIVILEGED ACCESS
          WHERE AddressID EQ @me->gs_BusPartAddress-AddressID
           INTO @me->gs_Address.

        IF sy-subrc EQ 0.
          header_g-dir_destinatario = |{ me->gs_Address-StreetName } { me->gs_Address-HouseNumber } { me->gs_Address-StreetPrefixName1 } { me->gs_Address-StreetPrefixName2 }|.
        ENDIF.

        SELECT *
          FROM I_AddressEmailAddress_2
          WITH PRIVILEGED ACCESS
          WHERE AddressID EQ @me->gs_BusPartAddress-AddressID
           INTO TABLE @me->gt_email.

        SELECT *
          FROM I_AddressPhoneNumber_2
          WITH PRIVILEGED ACCESS
          WHERE AddressID EQ @me->gs_BusPartAddress-AddressID
          INTO TABLE @me->gt_telefono.

      ENDIF.

      header_g-rz_destinatario = gs_BusinessPartner-BusinessPartnerFullName.

      READ TABLE gt_Businesspartnertaxnumber INTO gs_Businesspartnertaxnumber INDEX 1.
      IF sy-subrc EQ 0.
        IF gs_Businesspartnertaxnumber-BPTaxNumber IS INITIAL.
          header_g-id_destinatario = gs_Businesspartnertaxnumber-BPTaxLongNumber.
        ELSE.
          header_g-id_destinatario = gs_Businesspartnertaxnumber-BPTaxNumber.
        ENDIF.
      ENDIF.

      READ TABLE me->gt_Businesspartnertaxnumber INTO gs_Businesspartnertaxnumber INDEX 1.
      IF sy-subrc EQ 0.
        IF gs_Businesspartnertaxnumber-BPTaxNumber IS NOT INITIAL.
          header_g-id_destinatario = gs_Businesspartnertaxnumber-BPTaxNumber.
        ELSEIF gs_Businesspartnertaxnumber-BPTaxLongNumber IS NOT INITIAL .
          header_g-id_destinatario = gs_Businesspartnertaxnumber-BPTaxLongNumber.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD GetDetalles.

    DATA: lv_tarifa    TYPE i,
          lv_navnw     TYPE navnw.

    LOOP AT gt_goodsmovement INTO gs_goodsmovement WHERE GoodsMovementIsCancelled IS INITIAL
                                                     AND Material IS NOT INITIAL
                                                     AND StorageLocation IS NOT INITIAL.

      SELECT SINGLE *
      FROM I_UnitOfMeasureText
      WHERE Language EQ @sy-langu
       AND UnitOfMeasure EQ @gs_goodsmovement-MaterialBaseUnit
       INTO @DATA(ls_UnitOfMeasureText).
      IF sy-subrc EQ 0.
        gs_det_add-titulo = 'UnidadMedida'.
        gs_det_add-valor  = gs_goodsmovement-MaterialBaseUnit.
        gs_det_add-codigoprincipal  = gs_goodsmovement-Material.
        APPEND gs_det_add TO detalle_a.
      ENDIF.

      READ TABLE gt_product INTO gs_product WITH KEY Product = gs_goodsmovement-Material.
      IF sy-subrc EQ 0.
        gs_detalle_g-descripcion = gs_product-ProductDescription.
      ENDIF.
      gs_detalle_g-codigoprincipal = gs_goodsmovement-Material.

      lv_navnw =  gs_goodsmovement-MatlCnsmpnQtyInMatlBaseUnit.
      IF lv_navnw IS INITIAL.
        lv_navnw = gs_goodsmovement-GoodsIssueQtyInBaseUnit.
      ENDIF.
      gs_detalle_g-cantidad = lv_navnw.

      COLLECT gs_detalle_g INTO detalle.

    ENDLOOP.

  ENDMETHOD.


  METHOD GetHeaderAdd.

    CLEAR: gs_head_add.
    gs_head_add-valor = me->gs_goodsmovement-MaterialDocument.
    gs_head_add-nombre = 'Documento SAP'.
    APPEND gs_head_add TO header_add.

*    CLEAR: gs_head_add.
*    LOOP AT me->gt_email INTO gs_email.
*      IF sy-tabix EQ 1.
*        CONCATENATE gs_head_add-valor gs_email-EmailAddress INTO gs_head_add-valor.
*      ELSE.
*        CONCATENATE gs_head_add-valor '; ' gs_email-EmailAddress INTO gs_head_add-valor.
*      ENDIF.
*    ENDLOOP.
*    IF gs_head_add IS NOT INITIAL.
*      gs_head_add-nombre = 'Email'.
*      APPEND gs_head_add TO header_add.
*    ENDIF.
*
*    CLEAR: gs_head_add.
*    LOOP AT me->gt_telefono INTO gs_telefono.
*      IF sy-tabix EQ 1.
*        CONCATENATE gs_head_add-valor gs_telefono-PhoneAreaCodeSubscriberNumber INTO gs_head_add-valor.
*      ELSE.
*        CONCATENATE gs_head_add-valor '; ' gs_telefono-PhoneAreaCodeSubscriberNumber INTO gs_head_add-valor.
*      ENDIF.
*    ENDLOOP.
*    IF gs_head_add IS NOT INITIAL.
*      gs_head_add-nombre = 'Telefono'.
*      APPEND gs_head_add TO header_add.
*    ENDIF.

  ENDMETHOD.


  METHOD getheaderguia.

     DATA: lv_fecha TYPE string,
          lv_api   TYPE zde_type_api.

    lv_api = 'SG'.

    lv_fecha = me->gs_ec_011-startdate.
    CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO header_g-fcha_initransp SEPARATED BY '/'.
    header_g-fecha = gs_ec_011-startdate.

    lv_fecha = me->gs_ec_011-enddate.
    CONCATENATE lv_fecha+6(2) lv_fecha+4(2) lv_fecha(4) INTO header_g-fcha_fintransp SEPARATED BY '/'.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'CON_ESPECI'.
    IF sy-subrc EQ 0.
      header_g-contribuyenteespecial = gs_ec_007-low.
    ENDIF.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'OBLIG_CONT'.
    IF sy-subrc EQ 0.
      header_g-obligadocontabilidad = gs_ec_007-low.
    ENDIF.

    me->getDatosBP( CHANGING header_g = header_g ).

    header_g-raztranspor    = me->gs_ec_011-businessname.
    header_g-ruc_transpor   = me->gs_ec_011-carrierid.
    header_g-tipo_id_trans  = me->gs_ec_011-typeid.
    header_g-placa          = me->gs_ec_011-carplate.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'DIR_PARTID'.
    IF sy-subrc EQ 0.
      header_g-dirpartida = gs_ec_007-low.
    ENDIF.

    header_g-direstablecimiento = me->gs_ec_002-address.
    header_g-motivo_traslado    = me->gs_ec_001-reason.

  ENDMETHOD.


  METHOD get_data.

    SELECT client, companycode, documenttype, documentsri, sequence, export, refunds, reason
    FROM zdt_ec_001
    WHERE companycode  EQ @me->gv_companycode
      AND documenttype EQ @me->gv_goodsmovementtype
      AND documentsri  EQ @me->gv_documenttype
    INTO TABLE @gt_ec_001.

    SELECT client, companycode, documentsri, establishment, emissionpoint, objet, address
    FROM zdt_ec_002
    WHERE companycode  EQ @me->gv_companycode
      AND documentsri  EQ @me->gv_documenttype
    INTO TABLE @gt_ec_002.

    SELECT client, companycode, taxcode, notax, tax0, exempttax, tax, taxsupportid, taxsidrate, taxratepercent
    FROM zdt_ec_003
    WHERE companycode  EQ @me->gv_companycode
    INTO TABLE @gt_ec_003.

    SELECT client, companycode, bptaxtype, typedoccument, typedi
    FROM zdt_ec_004
    WHERE companycode  EQ @me->gv_companycode
    INTO TABLE @gt_ec_004.

    SELECT client, companycode, ccondition, conditionapplication, typecondition
    FROM zdt_ec_005
    WHERE companycode  EQ @me->gv_companycode
    INTO TABLE @gt_ec_005.

    SELECT client, companycode, paymentmethod, paymentsri
    FROM zdt_ec_006
    WHERE companycode  EQ @me->gv_companycode
    INTO TABLE @gt_ec_006.

    SELECT client, companycode, api, fieldname, sign, options, sequence, low, high
    FROM zdt_ec_007
    WHERE companycode  EQ @me->gv_companycode
    INTO TABLE @gt_ec_007.

    SELECT client, companycode, documentsri, establishment, emissionpoint, users, sequence, accountingdocumenttype,
           billingdocumenttype, deliverydocumenttype, goodsmovementtype, salesorganization, plant, storagelocation
    FROM zdt_ec_008
    WHERE companycode  EQ @me->gv_companycode
    INTO TABLE @gt_ec_008.

    SELECT SINGLE client, companycode, materialdocumentyear, materialdocument, goodsmovementtype,
                  carrierid, typeid, businessname, carplate, startdate, enddate
    FROM zdt_ec_011
    WHERE companycode  EQ @me->gv_companycode
      AND materialdocumentyear    EQ @me->gv_materialdocumentyear
      AND materialdocument        EQ @me->gv_materialdocument
      AND goodsmovementtype       EQ @me->gv_goodsmovementtype
    INTO @gs_ec_011.

    SELECT SINGLE client, companycode, materialdocumentyear, materialdocument, goodsmovementtype,
                  idnumber, establishment, emissionpoint, sequential, accesskey, documenttype,
                  issuedate, documentstatus, messagedocument, authorizationdate, xml, mimetype, filename, documentsupplier
      FROM zdt_mm_doc_guia
      WHERE companycode             EQ @me->gv_companycode
        AND materialdocumentyear    EQ @me->gv_materialdocumentyear
        AND materialdocument        EQ @me->gv_materialdocument
        AND goodsmovementtype       EQ @me->gv_goodsmovementtype
       INTO @gs_guia_t.

    SELECT *
      FROM I_GoodsMovementCube
      WHERE companycode             EQ @me->gv_companycode
        AND materialdocumentyear    EQ @me->gv_materialdocumentyear
        AND materialdocument        EQ @me->gv_materialdocument
        AND goodsmovementtype       EQ @me->gv_goodsmovementtype
       INTO TABLE @gt_goodsmovement.

    IF sy-subrc EQ 0.

      SELECT *
        FROM I_ProductDescription_2
        FOR ALL ENTRIES IN @me->gt_goodsmovement
       WHERE Product EQ @me->gt_goodsmovement-material
         AND Language EQ @sy-langu
        INTO TABLE @gt_product.

    ENDIF.

    SELECT SINGLE *
    FROM I_CompanyCode
    WHERE companycode = @me->gv_companycode
    INTO @me->gs_CompanyCode.

    SELECT *
    FROM I_AddlCompanyCodeInformation
    WHERE companycode = @me->gv_companycode
    INTO TABLE @me->gt_AddlInformation.

  ENDMETHOD.


  METHOD infoTributaria.

    DATA: lv_api          TYPE zde_type_api,
          lv_fecha        TYPE string,
          lv_documenttype TYPE zdt_ec_001-documenttype.

    LOOP AT me->gt_goodsmovement INTO me->gs_goodsmovement WHERE plant IS NOT INITIAL
                                                             AND MaterialDocumentItem EQ '0001'.
      EXIT.
    ENDLOOP.

    CASE me->gv_documenttype.
      WHEN '06'.
        lv_api = 'SG'.
        lv_documenttype = me->gs_goodsmovement-GoodsMovementType.
    ENDCASE.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'AMBIENTE' low = sy-sysid. "Ambiente
    IF sy-subrc EQ 0.
      inf_tribu-ambiente = gs_ec_007-high(1).
    ENDIF.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'TIPO_EMISI'. "Tipo Emision
    IF sy-subrc EQ 0.
      inf_tribu-tipoemision = gs_ec_007-low.
    ENDIF.

    IF me->gs_companycode IS NOT INITIAL."Razon Social
      inf_tribu-razonsocial = me->gs_companycode-CompanyCodeName.
    ENDIF.

    READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'NOMB_COMER'.
    IF sy-subrc EQ 0.
      inf_tribu-nombrecomercial = gs_ec_007-low.
    ELSE.
      IF me->gs_companycode IS NOT INITIAL.
        inf_tribu-nombrecomercial = me->gs_companycode-CompanyCodeName.
      ENDIF.
    ENDIF.

    IF me->gt_AddlInformation[] IS NOT INITIAL."Ruc
      READ TABLE me->gt_AddlInformation INTO gs_AddlInformation WITH KEY CompanyCode =  me->gv_companycode CompanyCodeParameterType = 'CGIID'.
      IF sy-subrc EQ 0.
        inf_tribu-ruc = gs_AddlInformation-CompanyCodeParameterValue.
      ENDIF.
    ENDIF.

    IF me->gt_ec_007[] IS NOT INITIAL."Direccion Matriz
      READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'DIR_MATRIZ'.
      IF sy-subrc EQ 0.
        inf_tribu-dirmatriz = gs_ec_007-low.
      ENDIF.
    ENDIF.

    IF me->gt_ec_007[] IS NOT INITIAL."Micro Empresa

      READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'MICROEMPRE'.
      IF sy-subrc EQ 0.
        inf_tribu-regimenmicroempresas = gs_ec_007-low.
      ENDIF.

    ENDIF.

    IF me->gt_ec_007[] IS NOT INITIAL."Agente de Retencion

      READ TABLE me->gt_ec_007 INTO gs_ec_007  WITH KEY companycode = me->gv_companycode api = lv_api fieldname = 'AG_RETENCI'.
      IF sy-subrc EQ 0.
        inf_tribu-agenteretencion = gs_ec_007-low.
      ENDIF.

    ENDIF.

    IF me->gt_ec_001[] IS NOT INITIAL."Tipo de Documento

      READ TABLE me->gt_ec_001 INTO gs_ec_001 WITH KEY CompanyCode =  me->gv_companycode documenttype = lv_documenttype.
      IF sy-subrc EQ 0.
        inf_tribu-coddoc = gs_ec_001-documentsri.
      ENDIF.

    ENDIF.

    IF gs_ec_001 IS NOT INITIAL.

      lv_fecha = me->gs_ec_011-startdate.

      READ TABLE gt_ec_008 INTO gs_ec_008 WITH KEY companycode         = me->gs_goodsmovement-CompanyCode
                                                   goodsmovementtype   = me->gs_goodsmovement-GoodsMovementType
                                                   storageLocation     = me->gs_goodsmovement-StorageLocation
                                                   plant               = me->gs_goodsmovement-plant
                                                   documentsri         = me->gs_ec_001-documentsri
                                                   users               = sy-uname.

      IF sy-subrc NE 0.

        READ TABLE gt_ec_008 INTO gs_ec_008 WITH KEY companycode         = me->gs_goodsmovement-CompanyCode
                                                     goodsmovementtype   = me->gs_goodsmovement-GoodsMovementType
                                                     plant               = me->gs_goodsmovement-plant
                                                     documentsri         = me->gs_ec_001-documentsri
                                                     users               = sy-uname.
        IF sy-subrc NE 0.

          READ TABLE gt_ec_008 INTO gs_ec_008 WITH KEY companycode         = me->gs_goodsmovement-CompanyCode
                                                       goodsmovementtype   = me->gs_goodsmovement-GoodsMovementType
                                                       storageLocation     = me->gs_goodsmovement-StorageLocation
                                                       documentsri         = me->gs_ec_001-documentsri
                                                       users               = sy-uname.

        ENDIF.

      ENDIF.

      IF gs_ec_008 IS NOT INITIAL.

        READ TABLE me->gt_ec_002 INTO gs_ec_002 WITH KEY companycode    = me->gs_goodsmovement-CompanyCode
                                                         establishment  = gs_ec_008-establishment
                                                         emissionpoint  = gs_ec_008-emissionpoint
                                                         documentsri    = me->gs_ec_001-documentsri.
        IF sy-subrc EQ 0.

          me->getclaveacceso( EXPORTING inf_tribu   = inf_tribu
                                        fecha       = lv_fecha
                                        api         = lv_api
                               CHANGING estab       = inf_tribu-estab
                                        ptoemi      = inf_tribu-ptoemi
                                        secuencial  = inf_tribu-secuencial
                                        claveacceso = inf_tribu-claveacceso ).
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
