CLASS zcl_sql DEFINITION

  PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_sql IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA: lt_ec_015 TYPE TABLE OF zdt_ec_015,
          ls_ec_015 TYPE zdt_ec_015.

    DATA: ls_fact TYPE zdt_sd_doc_fac,
          ls_rete TYPE zdt_fi_doc_ret,
          ls_liqi TYPE zdt_fi_doc_liq,
          ls_ncre TYPE zdt_sd_doc_ndc,
          ls_ndeb TYPE zdt_sd_doc_ndd,
          ls_guia TYPE zdt_sd_doc_guia,
          ls_tras TYPE zdt_mm_doc_guia.


*    SELECT SINGLE *
*      FROM zdt_sd_doc_fac
*      WHERE billingdocument EQ '0090000120'
*        AND companycode     EQ 'FO01'
*        AND fiscalyear      EQ '2025'
*      INTO @ls_fact.
*    IF sy-subrc NE 0.
*
*      ls_fact-companycode            = 'FO01'.
*      ls_fact-fiscalyear             = '2025'.
*      ls_fact-billingdocument        = '0090000120'.
*      ls_fact-billingdocumenttype    = 'F2'.
*      ls_fact-accountingdocument     = '9400000113'.
*      ls_fact-accountingdocumenttype = 'RV'.
*      ls_fact-soldtoparty            = '0001000002'.
*      ls_fact-establishment          = '001'.
*      ls_fact-emissionpoint          = '010'.
*      ls_fact-sequential             = '000002847'.
*      ls_fact-accesskey              = '0104202501179248222400120010100000028471234567811'.
*      ls_fact-authorizationdate      = '20250401'.
*      ls_fact-issuedate              = '20250401'.
*      ls_fact-typeid                 = '04'.
*      ls_fact-idnumber               = '0190361799001'.
*      ls_fact-documentstatus         = 'PENDING'.
*      ls_fact-businessname           = 'IMPORTADORA Y DISTRIBUIDORA DE TABLEROS DE MADERA IMPORQUIVI CIA. LTDA'.
*      ls_fact-documenttype           = '01'.
*      ls_fact-documentsupplier       = ''.
*
*      INSERT zdt_sd_doc_fac FROM @ls_fact.
*
*    ENDIF.

  ENDMETHOD.

ENDCLASS.
