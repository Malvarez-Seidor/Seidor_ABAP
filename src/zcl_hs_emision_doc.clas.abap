CLASS zcl_hs_emision_doc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF http_status,
        code   TYPE i,
        reason TYPE string,
      END OF http_status .


    INTERFACES if_oo_adt_classrun.

    METHODS: constructor IMPORTING documenttype TYPE zde_trsri
                                   xml          TYPE string
                              documentsupplier  TYPE sgtxt
                                   CompanyCode  TYPE bukrs,

       send_request_by_url        EXPORTING estado            TYPE zde_status
                                            messagedocument   TYPE string
                                            documentsupplier  TYPE sgtxt
                                            authorizationdate TYPE datum.

  PROTECTED SECTION.
    METHODS:
       "For local testing
*      send_request_by_url        IMPORTING out               TYPE REF TO if_oo_adt_classrun_out
*                                 EXPORTING estado            TYPE zde_status
*                                           messagedocument   TYPE string
*                                           documentsupplier  TYPE sgtxt
*                                           authorizationdate TYPE datum,

      send_request_by_arrengement IMPORTING out TYPE REF TO if_oo_adt_classrun_out,

      parse_json                  IMPORTING json_string       TYPE string
*                                            out               TYPE REF TO if_oo_adt_classrun_out " For local testing
                                            http_status       TYPE http_status
                                  EXPORTING estado            TYPE zde_status
                                            messagedocument   TYPE string
                                            documentsupplier  TYPE sgtxt
                                            authorizationdate TYPE datum,


      send_request_by_url_old_syntax IMPORTING out TYPE REF TO if_oo_adt_classrun_out.

  PRIVATE SECTION.

    DATA:
      communication_handler TYPE REF TO zcl_communication_handler,
      hostname              TYPE string,
      uri                   TYPE string,
      query                 TYPE string,
      url                   TYPE string,
      json_string           TYPE string,
      xml                   TYPE string,
      messagedocument       TYPE string,
      username              TYPE string,
      password              TYPE string,
      CompanyCode           TYPE bukrs,
      documenttype          TYPE zde_trsri,
      documentsupplier      TYPE sgtxt,
      authorizationdate     TYPE datum,
      estado                TYPE zde_status.

ENDCLASS.



CLASS ZCL_HS_EMISION_DOC IMPLEMENTATION.


  METHOD constructor.

    me->communication_handler = NEW zcl_communication_handler( ).
    me->xml                   = xml.
    me->documenttype          = documenttype.
    me->documentsupplier      = documentsupplier.
    me->companycode           = companycode.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    "For local testing
*    me->send_request_by_url( EXPORTING out = out
*                             IMPORTING estado            = me->estado
*                                       messagedocument   = me->messagedocument
*                                       documentsupplier  = me->documentsupplier
*                                       authorizationdate = me->authorizationdate  ).


  ENDMETHOD.


  METHOD parse_json.

    TYPES: BEGIN OF ty_mensajes,
             identificador         TYPE string,
             mensaje               TYPE string,
             tipo                  TYPE string,
             informacion_adicional TYPE string,
           END OF ty_mensajes,

           gtt_mensajes TYPE SORTED TABLE OF ty_mensajes WITH NON-UNIQUE KEY tipo,

           BEGIN OF ty_auto,
             fecha    TYPE string,
             estado   TYPE string,
             mensajes TYPE gtt_mensajes,
             numero   TYPE string,
           END OF ty_auto,

           BEGIN OF ty_consulta,
             estado                    TYPE string,
             numero                    TYPE string,
             url_formato_impresion     TYPE string,
             url_documento_electronico TYPE string,
             tipo                      TYPE string,
             id                        TYPE string,
             ambiente                  TYPE string,
             codigo_estado             TYPE string,
             clave_acceso              TYPE string,
             autorizacion              TYPE ty_auto,
           END OF ty_consulta,

           BEGIN OF ty_mess,
             details   TYPE string,
             message   TYPE string,
             code      TYPE string,
             parameter TYPE string,
             value     TYPE string,
           END OF ty_mess,

           gtt_mess TYPE SORTED TABLE OF ty_mess WITH NON-UNIQUE KEY code,

           BEGIN OF ty_msg,
             errors  TYPE gtt_mess,
             request TYPE string,
           END OF ty_msg,

           BEGIN OF ty_serv,
             statusCode TYPE string,
             Mensaje    TYPE string,
           END OF ty_serv,

           BEGIN OF ty_recep,
             pagos                 TYPE string,
             secuencial            TYPE string,
             issue_beta_feature    TYPE string,
             fecha_emision         TYPE string,
             emisor                TYPE string,
             numero                TYPE string,
             es_valida             TYPE string,
             moneda                TYPE string,
             id                    TYPE string,
             informacion_adicional TYPE string,
             ambiente              TYPE string,
             totales               TYPE string,
             comprador             TYPE string,
             tipo_emision          TYPE string,
             items                 TYPE string,
             credito               TYPE string,
             clave_acceso          TYPE string,
           END OF ty_recep.

    DATA: lt_mess   TYPE  gtt_mess,
          lt_return TYPE TABLE OF bapiret2.

    DATA: ls_error TYPE ty_msg,
          ls_serv  TYPE ty_serv,
          ls_recep TYPE ty_recep,
          ls_doc   TYPE ty_consulta.

    DATA: lo_root    TYPE REF TO cx_root,
          lo_writer  TYPE REF TO cl_sxml_string_writer,
          lo_reader  TYPE REF TO if_sxml_reader,
          lo_ref_exc TYPE REF TO cx_root.

    DATA: lv_xstring TYPE xstring,
          lv_xml     TYPE xstring.

    IF documentsupplier IS NOT INITIAL.

      TRY.

          CALL METHOD /ui2/cl_json=>deserialize
            EXPORTING
              json         = json_string
              pretty_name  = /ui2/cl_json=>pretty_mode-user
              assoc_arrays = abap_true
            CHANGING
              data         = ls_doc.

        CATCH cx_root INTO lo_root.

      ENDTRY.

      IF ls_doc-autorizacion-estado EQ 'AUTORIZADO'.
        estado = 'AUTHORIZED'.
        READ TABLE ls_doc-autorizacion-mensajes INTO DATA(ls_mensaje) INDEX 1.
        messagedocument = 'Autorizado'.
        REPLACE ALL OCCURRENCES OF '-' IN ls_doc-autorizacion-fecha WITH space.
        CONDENSE ls_doc-autorizacion-fecha.
        authorizationdate = ls_doc-autorizacion-fecha(8).
      ELSEIF ls_doc-autorizacion-estado EQ 'ERROR' OR ls_doc-autorizacion-estado EQ 'NO AUTORIZADO'.
        estado = 'ERROR'.
        LOOP AT ls_doc-autorizacion-mensajes INTO ls_mensaje.
          IF sy-tabix EQ 1.
            messagedocument = |{ ls_mensaje-mensaje } '; ' { ls_mensaje-informacion_adicional }|.
          ELSE.
            CONCATENATE messagedocument ls_mensaje-mensaje  ls_mensaje-informacion_adicional INTO messagedocument.
          ENDIF.
        ENDLOOP.
      ELSEIF ls_doc-autorizacion-estado IS NOT INITIAL.
        IF http_status-code EQ '200'.
          estado = 'PROCESS'.
        ELSE.
          estado = 'ERROR'.
        ENDIF.
        LOOP AT ls_doc-autorizacion-mensajes INTO ls_mensaje.
          IF sy-tabix EQ 1.
            messagedocument = |{ ls_doc-autorizacion-estado } { ls_mensaje-mensaje } '; ' { ls_mensaje-informacion_adicional }|.
          ELSE.
            CONCATENATE messagedocument ls_mensaje-mensaje  ls_mensaje-informacion_adicional INTO messagedocument.
          ENDIF.
        ENDLOOP.
      ELSEIF http_status-code EQ '200'.
        estado = 'PROCESS'.
        READ TABLE ls_doc-autorizacion-mensajes INTO ls_mensaje INDEX 1.
        messagedocument = ls_mensaje-mensaje.
      ELSE.
        estado = 'ERROR'.
        LOOP AT ls_doc-autorizacion-mensajes INTO ls_mensaje.
          IF sy-tabix EQ 1.
            messagedocument = |{ ls_mensaje-mensaje } '; ' { ls_mensaje-informacion_adicional }|.
          ELSE.
            CONCATENATE messagedocument ls_mensaje-mensaje  ls_mensaje-informacion_adicional INTO messagedocument.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ELSE.

      TRY.

          CALL METHOD /ui2/cl_json=>deserialize
            EXPORTING
              json         = json_string
              pretty_name  = /ui2/cl_json=>pretty_mode-user
              assoc_arrays = abap_true
            CHANGING
              data         = ls_serv.

        CATCH cx_root INTO lo_root.

      ENDTRY.

      TRY.

          CALL METHOD /ui2/cl_json=>deserialize
            EXPORTING
              json         = json_string
              pretty_name  = /ui2/cl_json=>pretty_mode-user
              assoc_arrays = abap_true
            CHANGING
              data         = ls_error.

        CATCH cx_root INTO lo_root.

      ENDTRY.

      TRY.

          CALL METHOD /ui2/cl_json=>deserialize
            EXPORTING
              json         = json_string
              pretty_name  = /ui2/cl_json=>pretty_mode-user
              assoc_arrays = abap_true
            CHANGING
              data         = ls_recep.

        CATCH cx_root INTO lo_root.

      ENDTRY.

      IF ls_serv IS NOT INITIAL.
        estado = 'ERROR'.
        messagedocument = ls_serv-mensaje.
      ELSEIF ls_error IS NOT INITIAL.
        estado = 'ERROR'.
        LOOP AT ls_error-errors INTO DATA(ls_errors).
          IF sy-tabix EQ 1.
            messagedocument = ls_errors-message.
          ELSE.
            CONCATENATE messagedocument '; ' ls_errors-message INTO messagedocument.
          ENDIF.
        ENDLOOP.

      ELSEIF ls_recep IS NOT INITIAL.
        estado = 'PROCESS'.
        messagedocument = 'En Proceso de Autorizacion'.
        documentsupplier = ls_recep-id.
      ELSEIF http_status-code NE '200'.
        estado = 'ERROR'.
        messagedocument = http_status-reason.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD send_request_by_arrengement.

    DATA: lv_scenario_id   TYPE        if_com_management=>ty_cscn_id          VALUE 'ZCS_EMI_DOCUMENTO',
          lv_service_id    TYPE        if_com_management=>ty_cscn_outb_srv_id VALUE 'ZOS_EMI_DOCUMENT_REST',
          lo_http_response TYPE REF TO if_web_http_response.

    TRY.
        "Send request
        lo_http_response = me->communication_handler->send_request_by_arrengement( scenario_id = lv_scenario_id
                                                                                   service_id  = lv_service_id
                                                                                   url_path    = me->uri
                                                                                   query       = me->query
                                                                                   username    = me->username
                                                                                   password    = me->password ).
        "Get status from response object
        DATA(ls_status) = lo_http_response->get_status(  ).

        "Get json string from response object
        me->json_string = lo_http_response->get_text(  ).

        "Output json string
        out->write( |Response from "send_request_by_url" : {  ls_status-code } {  ls_status-reason } \n { me->json_string } \n| ).

      CATCH cx_http_dest_provider_error cx_web_http_client_error INTO DATA(lx_error).
        "Display Error details
        out->write( lx_error->get_text(  ) ).

    ENDTRY.

  ENDMETHOD.


  METHOD send_request_by_url.

    DATA: lt_ec_007   TYPE TABLE OF zdt_ec_007,
          ls_ec_007   TYPE zdt_ec_007,
          lv_username TYPE string,
          lv_password TYPE string,
          lv_uri      TYPE string,
          lv_hostname TYPE string,
          lv_url      TYPE string.

    SELECT client, companycode, api, fieldname, sign, options, sequence, low, high
    FROM zdt_ec_007
    WHERE ( api EQ 'CS' "Consult
       OR api EQ 'EM'   "Emission
       OR api EQ 'GE'   "Get Inactive
       OR api EQ 'DW' ) "Download Inactive
      AND companycode EQ @me->CompanyCode
      INTO TABLE @lt_ec_007.

    IF me->documentsupplier IS NOT INITIAL.

      READ TABLE lt_ec_007 INTO ls_ec_007 WITH KEY api = 'CS' fieldname = 'URL'.
      IF sy-subrc EQ 0.
        lv_url = ls_ec_007-low.
      ENDIF.

      READ TABLE lt_ec_007 INTO ls_ec_007 WITH KEY api = 'CS' fieldname = 'URI'.
      IF sy-subrc EQ 0.
        lv_uri = ls_ec_007-low.
      ENDIF.

      READ TABLE lt_ec_007 INTO ls_ec_007 WITH KEY api = 'CS' fieldname = 'HOSTNAME'.
      IF sy-subrc EQ 0.
        lv_hostname = ls_ec_007-low.
      ENDIF.

      READ TABLE lt_ec_007 INTO ls_ec_007 WITH KEY api = 'CS' fieldname = 'USERNAME'.
      IF sy-subrc EQ 0.
        lv_username = ls_ec_007-low.
      ENDIF.

      READ TABLE lt_ec_007 INTO ls_ec_007 WITH KEY api = 'CS' fieldname = 'PASSWORD'.
      IF sy-subrc EQ 0.
        lv_password = ls_ec_007-low.
      ENDIF.

    ELSE.

      READ TABLE lt_ec_007 INTO ls_ec_007 WITH KEY api = 'EM' fieldname = 'URL'.
      IF sy-subrc EQ 0.
        lv_url = ls_ec_007-low.
      ENDIF.

      READ TABLE lt_ec_007 INTO ls_ec_007 WITH KEY api = 'EM' fieldname = 'URI'.
      IF sy-subrc EQ 0.
        lv_uri = ls_ec_007-low.
      ENDIF.

      READ TABLE lt_ec_007 INTO ls_ec_007 WITH KEY api = 'EM' fieldname = 'HOSTNAME'.
      IF sy-subrc EQ 0.
        lv_hostname = ls_ec_007-low.
      ENDIF.

      READ TABLE lt_ec_007 INTO ls_ec_007 WITH KEY api = 'CS' fieldname = 'USERNAME'.
      IF sy-subrc EQ 0.
        lv_username = ls_ec_007-low.
      ENDIF.

      READ TABLE lt_ec_007 INTO ls_ec_007 WITH KEY api = 'CS' fieldname = 'PASSWORD'.
      IF sy-subrc EQ 0.
        lv_password = ls_ec_007-low.
      ENDIF.

    ENDIF.

    me->url                   = lv_url.
    me->hostname              = lv_hostname.
    me->uri                   = lv_uri.
    me->username              = lv_username.
    me->password              = lv_password.
    me->query                 = |url={ url }|.

    "HTTP Communication via URL"
    DATA: lv_request_string TYPE string,
          lo_http_response  TYPE REF TO if_web_http_response.

    "Set the request string
    lv_request_string = |https://{ me->hostname }{ me->uri }|.
    TRY.

        "Send request
        lo_http_response = me->communication_handler->send_request_by_url( url = lv_request_string
                                                                           xml = me->xml
                                                              documentsupplier = me->documentsupplier
                                                                  documenttype = me->documenttype
                                                                   username    = me->username
                                                                   password    = me->password ).
        "Get status from response object
        DATA(ls_status) = lo_http_response->get_status(  ).

        "Get json string from response object
        me->json_string = lo_http_response->get_text(  ).

        "Get Responce  of json
        me->parse_json( EXPORTING json_string = me->json_string
*                                  out         = out   "For local testing
                                  http_status = ls_status
                        IMPORTING estado            = estado
                                  messagedocument   = me->messagedocument
                                  documentsupplier  = me->documentsupplier
                                  authorizationdate = me->authorizationdate ).

        messagedocument   = me->messagedocument.
        documentsupplier  = me->documentsupplier.
        authorizationdate = me->authorizationdate.
        "Output json string For local testing
*        out->write( |Response from "send_request_by_url" : {  ls_status-code } {  ls_status-reason } \n { me->json_string } \n| ).

      CATCH cx_http_dest_provider_error cx_web_http_client_error INTO DATA(lx_error).
        "Display Error details
*        out->write( lx_error->get_text(  ) ). "For local testing
    ENDTRY.

  ENDMETHOD.


  METHOD send_request_by_url_old_syntax.

  ENDMETHOD.
ENDCLASS.
