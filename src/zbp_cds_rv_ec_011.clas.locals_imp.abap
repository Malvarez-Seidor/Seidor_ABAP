CLASS lhc_transportdata DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE TransportData.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE TransportData.

    METHODS read FOR READ
      IMPORTING keys FOR READ TransportData RESULT result.

    METHODS rba_Transferguides FOR READ
      IMPORTING keys_rba FOR READ TransportData\_Transferguides FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_transportdata IMPLEMENTATION.

  METHOD update.

    DATA : lt_inserts     TYPE STANDARD TABLE OF zdt_ec_011,
           lt_updates     TYPE STANDARD TABLE OF zdt_ec_011,
           lt_controls    TYPE STANDARD TABLE OF zdt_ec_011.

      lt_inserts  = CORRESPONDING #( entities MAPPING FROM ENTITY ).
      lt_controls = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

      SELECT * FROM zdt_ec_011
        FOR ALL ENTRIES IN @lt_inserts
        WHERE companycode             EQ @lt_inserts-CompanyCode
          AND MaterialDocumentYear    EQ @lt_inserts-MaterialDocumentYear
          AND MaterialDocument        EQ @lt_inserts-MaterialDocument
          AND GoodsMovementType       EQ @lt_inserts-GoodsMovementType
        INTO TABLE @DATA(lt_docments).

    IF sy-subrc EQ 0.

      lt_updates = VALUE #( FOR i = 1 WHILE i LE lines( lt_inserts )
        LET
          ls_control  = VALUE #( lt_controls[ i ] OPTIONAL )
          ls_insert   = VALUE #( lt_inserts[ i ] OPTIONAL )
          ls_docment  = VALUE #( lt_docments[ MaterialDocument = ls_insert-MaterialDocument ] OPTIONAL )
          IN
            ( CompanyCode            = ls_insert-CompanyCode
              MaterialDocumentYear   = ls_insert-MaterialDocumentYear
              MaterialDocument       = ls_insert-MaterialDocument
              GoodsMovementType      = ls_insert-GoodsMovementType
              carrierid              = ls_insert-carrierid


              businessname           = COND #( WHEN ls_insert-businessname IS NOT INITIAL
                                               THEN ls_insert-businessname
                                               ELSE ls_docment-businessname )

              typeid                 = COND #( WHEN ls_insert-typeid IS NOT INITIAL
                                              THEN ls_insert-typeid
                                              ELSE ls_docment-typeid )

              carplate               = COND #( WHEN ls_insert-carplate IS NOT INITIAL
                                               THEN ls_insert-carplate
                                               ELSE ls_docment-carplate )

              startdate              = COND #( WHEN ls_insert-startdate IS NOT INITIAL
                                               THEN ls_insert-startdate
                                               ELSE ls_docment-startdate )

              enddate                = COND #( WHEN ls_insert-enddate IS NOT INITIAL
                                               THEN ls_insert-enddate
                                               ELSE ls_docment-enddate ) )
            ).

    ELSE.
      lt_updates = lt_inserts.
    ENDIF.

    UPDATE zdt_ec_011 FROM TABLE @lt_updates.

  ENDMETHOD.

  METHOD delete.

    IF keys IS NOT INITIAL.

      LOOP AT keys ASSIGNING FIELD-SYMBOL(<fs_keys>).

        DELETE FROM zdt_ec_011 WHERE companycode             EQ @<fs_keys>-companycode
                                 AND MaterialDocumentYear    EQ @<fs_keys>-MaterialDocumentYear
                                 AND MaterialDocument        EQ @<fs_keys>-MaterialDocument
                                 AND GoodsMovementType       EQ @<fs_keys>-GoodsMovementType
                                 AND Carrierid               EQ @<fs_keys>-Carrierid.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD read.

    SELECT * FROM zcds_rv_ec_011
        FOR ALL ENTRIES IN @keys
        WHERE CompanyCode             EQ @keys-CompanyCode
          AND MaterialDocumentYear    EQ @keys-MaterialDocumentYear
          AND MaterialDocument        EQ @keys-MaterialDocument
          AND GoodsMovementType       EQ @keys-GoodsMovementType
          AND Carrierid               EQ @keys-Carrierid
        INTO CORRESPONDING FIELDS OF table @result.

  ENDMETHOD.

  METHOD rba_Transferguides.

    SELECT * FROM zcds_rv_ec_011
        FOR ALL ENTRIES IN @keys_rba
        WHERE CompanyCode             EQ @keys_rba-CompanyCode
          AND MaterialDocumentYear    EQ @keys_rba-MaterialDocumentYear
          AND MaterialDocument        EQ @keys_rba-MaterialDocument
          AND GoodsMovementType       EQ @keys_rba-GoodsMovementType
          AND Carrierid               EQ @keys_rba-Carrierid
        INTO CORRESPONDING FIELDS OF table @result.

  ENDMETHOD.

ENDCLASS.
