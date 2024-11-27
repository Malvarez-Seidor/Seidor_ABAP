CLASS zcls_sql DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcls_sql IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA: ls_fact TYPE zdt_sd_doc_fac.

    SELECT SINGLE *
      FROM zdt_sd_doc_fac WHERE billingdocument EQ '0090000039'
      INTO @ls_fact.
    IF sy-subrc EQ 0.
*      ls_fact-authorizationdate = |{ ls_fact-Authorizationdate DATE = ENVIRONMENT }|.
      DELETE FROM zdt_sd_doc_fac WHERE billingdocument EQ '0090000039'.
    ENDIF.

    SELECT SINGLE *
      FROM zdt_sd_doc_fac WHERE billingdocument EQ '0090000038'
      INTO @ls_fact.
    IF sy-subrc EQ 0.
*      ls_fact-authorizationdate = |{ ls_fact-Authorizationdate DATE = ENVIRONMENT }|.
      DELETE FROM zdt_sd_doc_fac WHERE billingdocument EQ '0090000038'.
    ENDIF.

*    DELETE FROM zdt_ec_012 WHERE accountingdocument EQ '1900000000'.
    DELETE FROM zdt_ec_013 WHERE accountingdocument EQ ''.

  ENDMETHOD.

ENDCLASS.
