CLASS zcl_factory_method DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
* PROTECTED SECTION.
* PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_FACTORY_METHOD IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA connection TYPE REF TO lcl_connection.

* Debug the method to show that the class returns objects, but that there are different
* objects for the same combination of airline and flight number

    connection = lcl_connection=>get_connection( airlineid = 'LH' connectionnumber = '0400' ).
    connection = lcl_connection=>get_connection( airlineid = 'LH' connectionnumber = '0400' ).

  ENDMETHOD. " if_oo_adt_classrun~main
ENDCLASS.
