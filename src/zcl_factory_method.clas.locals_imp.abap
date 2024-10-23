CLASS lcl_connection DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    CLASS-METHODS get_connection IMPORTING airlineid            TYPE zcarrier-carrier_id
                                           connectionnumber     TYPE zconnection-connection_id
                                 RETURNING VALUE(ro_connection) TYPE REF TO lcl_connection.

    METHODS constructor IMPORTING   airlineid           TYPE zcarrier-carrier_id
                                    connectionnumber    TYPE zconnection-connection_id
                                    fromairport         TYPE zconnection-airport_from_id
                                    toairport           TYPE zconnection-airport_to_id.

  PRIVATE SECTION.

    DATA airlineid          TYPE zcarrier-carrier_id.
    DATA connectionnumber   TYPE zconnection-connection_id.
    DATA fromairport        TYPE zconnection-airport_from_id.
    DATA toairport          TYPE zconnection-airport_to_id.

ENDCLASS. " lcl_connection DEFINITION

CLASS lcl_connection IMPLEMENTATION.

  METHOD constructor.

    me->airlineid = airlineid.
    me->connectionnumber = connectionnumber.
    me->fromairport = fromairport.
    me->toairport = toairport.

  ENDMETHOD. " constructor

  METHOD get_connection.

    DATA fromairport TYPE zconnection-airport_from_id.
    DATA toairport TYPE zconnection-airport_to_id.

    SELECT SINGLE FROM zconnection FIELDS airport_from_id, airport_to_id
    WHERE carrier_id = @airlineid
    AND connection_id = @connectionnumber
    INTO ( @fromairport, @toairport ).

    ro_connection = NEW #( airlineid = airlineid connectionnumber = connectionnumber fromairport = fromairport toairport = toairport ).

  ENDMETHOD. " get_connection

ENDCLASS. " lcl_connection IMPLEMENTATION
