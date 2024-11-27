CLASS zcl_communication_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      send_request_by_url "*    HTTP Comunicacion via URL  *
        IMPORTING url                TYPE string
                  documenttype       TYPE zde_trsri
                  xml                TYPE string
                  documentsupplier   TYPE sgtxt
                  username           TYPE string
                  password           TYPE string
        RETURNING VALUE(lo_responce) TYPE REF TO if_web_http_response
        RAISING   cx_web_http_client_error,

      send_request_by_arrengement "*  HTTP Comunicacion via acuerdo de comunicacion  *
        IMPORTING
                  scenario_id        TYPE if_com_management=>ty_cscn_id
                  service_id         TYPE if_com_management=>ty_cscn_outb_srv_id OPTIONAL
                  url_path           TYPE string
                  query              TYPE string
                  username           TYPE string
                  password           TYPE string
        RETURNING VALUE(lo_responce) TYPE REF TO if_web_http_response
        RAISING   cx_http_dest_provider_error cx_web_http_client_error,

      send_request_by_dest_service "*    HTTP Comunicacion via Destination Services  *
        IMPORTING
                  destination_name   TYPE string
        RETURNING VALUE(lo_responce) TYPE REF TO if_web_http_response
        RAISING   cx_web_http_client_error.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_COMMUNICATION_HANDLER IMPLEMENTATION.


  METHOD send_request_by_arrengement.

    DATA: lr_cscn TYPE if_com_scenario_factory=>ty_query-cscn_id_range.

    "Find Communication Scenario by scenario
    lr_cscn = VALUE #( ( sign = 'I' option = 'EQ' low = scenario_id ) ).

    DATA(lo_factory) =  cl_com_arrangement_factory=>create_instance( ).

    lo_factory->query_ca(
                  EXPORTING
                    is_query =  VALUE #( cscn_id_range = lr_cscn )
                  IMPORTING
                    et_com_arrangement = DATA(lt_ca) ).
    IF lt_ca IS INITIAL.
      EXIT.
    ENDIF.

    READ TABLE lt_ca INTO DATA(lo_ca) INDEX 1.
    TRY.

        "Create HTTP Destination via Communication Scenario
        DATA(lo_http_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                                         comm_scenario  = scenario_id
                                                         service_id     = service_id
                                                         comm_system_id = lo_ca->get_comm_system_id(  ) ).

        "Create HTTP client via by HTTP Destination
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ).

        "Execute request
        DATA(lo_request) = lo_http_client->get_http_request( ).

        lo_request->set_authorization_basic(
          i_username = username
          i_password = password ).

        lo_request->set_uri_path( i_uri_path = url_path ).
        lo_request->set_query( query = query ).



        "adding HTTP GET-request and store response
        lo_responce = lo_http_client->execute( if_web_http_client=>post ).
        "lo_responce = lo_http_client->execute( if_web_http_client=>get ). "Obtain Data Only

      CATCH cx_http_dest_provider_error.
        RAISE EXCEPTION TYPE cx_http_dest_provider_error.

      CATCH cx_web_http_client_error.
        RAISE EXCEPTION TYPE cx_web_http_client_error.

    ENDTRY.


  ENDMETHOD.


  METHOD send_request_by_dest_service.

  ENDMETHOD.


  METHOD send_request_by_url.

    DATA: lo_http_destination TYPE REF TO if_http_destination,
          lo_http_client      TYPE REF TO if_web_http_client,
          lv_responce         TYPE string,
          lv_data             TYPE string,
          lv_body             TYPE string.

    TRY.

        "Create HTTP Destination via URL
        lo_http_destination = cl_http_destination_provider=>create_by_url( url ).

        "Create HTTP client via by HTTP Destination
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ).

        DATA(lo_request) = lo_http_client->get_http_request( ).

        IF documenttype IS NOT INITIAL AND xml IS NOT INITIAL.
          CONCATENATE '{ "TipoDocumento": "'  documenttype  '", "xmlBase64": "'  xml  '" }' INTO lv_body.
        ELSEIF documentsupplier IS NOT INITIAL.
          CONCATENATE '{ "id": "'  documentsupplier  '" }' INTO lv_body.
        ENDIF.

        "lo_request->set_header_fields( VALUE #(
        "( name = 'Content-Type'
        "  value = 'application/json' )
        "( name = 'Accept'
        "  value = 'application/json' )
        "( name = 'Content-Length'
        "  value = strlen( lv_body ) ) ) ).

        lo_request->set_authorization_basic(
          i_username = username
          i_password = password ).

        lo_request->append_text( lv_body ).

        "adding HTTP GET-request and store response
        lo_responce = lo_http_client->execute( if_web_http_client=>put ).

        "Print response text in console
        DATA(ls_status) = lo_responce->get_status( ).

      CATCH cx_http_dest_provider_error cx_web_http_client_error INTO DATA(lx_error).
        RAISE EXCEPTION TYPE cx_web_http_client_error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
