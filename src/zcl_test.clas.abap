CLASS zcl_test DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
    METHODS get_eurofxref IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS populate_tables IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS joins  IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS numerics IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS text_translations IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS string_functions IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS string_functions2 IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS predicate_functions IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS cast_in_sql IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS calculation_in_sql IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS internal_tables  IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS internal_tables2  IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS internal_tables3  IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS internal_tables4  IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS internal_tables5  IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS string_functions3 IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS exact_calculations IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS modify_sales_order IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS c1_contract IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS material_document_api IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS material_document_eml IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS use_eml_crud_operations IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS use_eml_create IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS use_eml_read IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS use_eml_update IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
    METHODS use_eml_delete IMPORTING out TYPE REF TO if_oo_adt_classrun_out.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TEST IMPLEMENTATION.


  METHOD c1_contract.
  ENDMETHOD. " c1_contract


  METHOD calculation_in_sql.

    SELECT
        FROM
           zflight
        FIELDS
            seats_max,
            seats_occupied,
            seats_max - seats_occupied AS seats_avaliable,
            (   CAST( seats_occupied AS FLTP )
              * CAST( 100 AS FLTP )
            ) / CAST(  seats_max AS FLTP ) AS percentage_fltp
        WHERE
            ( carrier_id      = 'LH' ) AND
            ( connection_id   = '0400' )
        INTO TABLE
            @DATA(result).

    out->write(
        EXPORTING
            data   = result
            name   = 'RESULT'
    ).

  ENDMETHOD. " calculation_in_sql


  METHOD cast_in_sql.

    SELECT FROM ztest001
         FIELDS '19891109'                           AS char_8,
                CAST( '19891109' AS CHAR( 4 ) )      AS char_4,
                CAST( '19891109' AS NUMC( 8  ) )     AS numc_8,

                CAST( '19891109' AS INT4 )          AS integer,
                CAST( '19891109' AS DEC( 10, 2 ) )  AS dec_10_2,
                CAST( '19891109' AS FLTP )          AS fltp,

                CAST( '19891109' AS DATS )          AS date

           INTO TABLE @DATA(result).

    out->write(
      EXPORTING
        data   = result
        name   = 'RESULT'
    ).

    SELECT FROM
            ztest001
        FIELDS
            id,
            data,
            CASE id
                WHEN '1'  THEN 'One'
                WHEN '2'  THEN 'Two'
                WHEN '3'  THEN 'Three'
                ELSE 'More'
            END AS calc_col
         INTO TABLE
            @DATA(result_simple).

    out->write(
        EXPORTING
            data   = result_simple
            name   = 'RESULT_SIMPLE'
    ).

**********************************************************************

    SELECT
        FROM
            ztest001
        FIELDS
            id,
            data,
            CASE
                WHEN id < '2' THEN 'Less'
                WHEN id = '2' THEN 'Equal'
                WHEN id > '2' THEN 'More'
                ELSE 'This is impossible'
            END AS calc_col
        INTO TABLE
            @DATA(result_complex).

    out->write(
        EXPORTING
            data   = result_complex
            name   = 'RESULT_COMPLEX'
    ).

  ENDMETHOD. " cast_in_sql


  METHOD exact_calculations.

*   Which patterns raise an exception?
*   Note: There are 3 correct answers to this question

*   A.
    TRY.
        DATA: gv_target1 TYPE D.
        CONSTANTS: gco_date TYPE C LENGTH 8 VALUE '20331233'.
        gv_target1 = EXACT #( gco_date ).
    CATCH cx_sy_conversion_error.
       out->write( 'A - cx_sy_conversion_error.' ).
    ENDTRY.

*   B.
    TRY.
        DATA: gv_target2 TYPE c LENGTH 5.
        CONSTANTS: gco_string2 TYPE string VALUE '0123456789ABCDEF'.
        gv_target2 = EXACT #( gco_string2+5(6) ).
    CATCH cx_sy_conversion_error.
       out->write( 'B - cx_sy_conversion_error.' ).
    ENDTRY.

*   C.
    TRY.
        DATA: gv_target3 TYPE p DECIMALS 3.
        CONSTANTS: gco_int3 TYPE i VALUE 2.
        gv_target3 = EXACT #( 2 / gco_int3 ).
    CATCH cx_sy_conversion_error.
       out->write( 'C - cx_sy_conversion_error.' ).
    ENDTRY.

*   D.
    TRY.
        DATA: gv_target4 TYPE string.
        CONSTANTS: gco_string4 TYPE C LENGTH 16 VALUE '0123456789ABCDEF'.
        gv_target4 = EXACT #( gco_string4+5(5) ).
    CATCH cx_sy_conversion_error.
       out->write( 'D - cx_sy_conversion_error.' ).
    ENDTRY.

*   E.
    TRY.
        DATA: gv_target5 TYPE p DECIMALS 2.
        CONSTANTS: gco_int5 TYPE i VALUE 3.
        gv_target5 = EXACT #( 2 * gco_int5 ).
    CATCH cx_root.
       out->write( 'E - cx_root.' ).
    ENDTRY.

*   From ABAPD Course
    DATA var_date TYPE d.
    DATA var_pack TYPE p LENGTH 3 DECIMALS 2.
    DATA var_string TYPE string.
    DATA var_char TYPE c LENGTH 3.

    var_pack = 1 / 8.
    out->write( |1/8 = { var_pack NUMBER = USER }| ).

    TRY.
    var_pack = EXACT #( 1 / 8 ).
    CATCH cx_sy_conversion_error.
    out->write( |1/8 has to be rounded. EXACT triggered an exception| ).
    ENDTRY.

    var_string = 'ABCDE'.
    var_Char = var_string.
    out->write( var_char ).

    TRY.
    var_char = EXACT #( var_string ).
    CATCH cx_sy_conversion_error.
    out->write( 'String has to be truncated. EXACT triggered an exception' ).
    ENDTRY.

    var_date = 'ABCDEFGH'.
    out->write( var_Date ).

    TRY.
    var_date = EXACT #( 'ABCDEFGH' ).
    CATCH cx_sy_conversion_error.
    out->write( |ABCDEFGH is not a valid date. EXACT triggered an exception| ).
    ENDTRY.

    var_date = '20221232'.
    out->write( var_date ).

    TRY.
    var_date = EXACT #( '20221232' ).
    CATCH cx_sy_conversion_error.
    out->write( |2022-12-32 is not a valid date. EXACT triggered an exception| ).
    ENDTRY.

  ENDMETHOD. " exact_calculations


  METHOD get_eurofxref. "  Get https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml

*   Do Free Style HTTP Request
    TRY.

        DATA i_url         TYPE string VALUE 'https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml'.
        DATA i_username    TYPE string VALUE ''.
        DATA i_password    TYPE string VALUE ''.

        DATA(http_destination) = cl_http_destination_provider=>create_by_url(
            i_url = i_url
        ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination(
            i_destination = http_destination
        ).

        DATA(lo_http_response) = lo_http_client->execute(
            i_method   = if_web_http_client=>get
*            i_timeout  = 0
        ).

        DATA(response_body) = lo_http_response->get_text( ).
        DATA(status)        = lo_http_response->get_status( ).
        DATA(header_fields) = lo_http_response->get_header_fields( ).
        DATA(header_status) = lo_http_response->get_header_field( '~status_code' ).

        out->write( cl_abap_char_utilities=>cr_lf && status-code && cl_abap_char_utilities=>cr_lf ).
        out->write( cl_abap_char_utilities=>cr_lf && response_body && cl_abap_char_utilities=>cr_lf ).

*    SELECT * FROM I_ExchangeRateRawData INTO TABLE @DATA(exchangeRateRawData).

    CATCH cx_web_http_client_error      INTO DATA(lx_web_http_client_error).

    CATCH cx_http_dest_provider_error   INTO DATA(lx_http_dest_provider_error).

    ENDTRY.

  ENDMETHOD. " get_eurofxref


  METHOD if_oo_adt_classrun~main.

*   Get https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml
*    get_eurofxref( out ).
*    populate_tables( out ).
*    joins( out ).
*    string_functions( out ).
*    string_functions2( out ).
*    predicate_functions( out ).
*    cast_in_sql( out ).
*    calculation_in_sql( out ).
*    internal_tables( out ).
*    internal_tables2( out ).
*    internal_tables3( out ).
*    internal_tables4( out ).
*    internal_tables5( out ).
*    string_functions3( out ).
*    exact_calculations( out ).
*    material_document_api( out ).
*    material_document_eml( out ).
    use_eml_crud_operations( out ).
  ENDMETHOD. " main


  METHOD internal_tables.

    TYPES t_flights TYPE STANDARD TABLE OF zflight WITH NON-UNIQUE KEY carrier_id connection_id flight_date.
    DATA flights TYPE t_flights.

    flights = VALUE #(
        ( client = sy-mandt carrier_id = 'LH' connection_id = '0400' flight_date = '20230201' plane_type_id = '747-400' price = '600' currency_code = 'EUR' )
        ( client = sy-mandt carrier_id = 'LH' connection_id = '0400' flight_date = '20230115' plane_type_id = '747-400' price = '600' currency_code = 'EUR' )
        ( client = sy-mandt carrier_id = 'QF' connection_id = '0006' flight_date = '20230112' plane_type_id = 'A380' price = '1600' currency_code = 'AUD' )
        ( client = sy-mandt carrier_id = 'AA' connection_id = '0017' flight_date = '20230110' plane_type_id = '747-400' price = '600' currency_code = 'USD' )
        ( client = sy-mandt carrier_id = 'UA' connection_id = '0900' flight_date = '20230201' plane_type_id = '777-200' price = '600' currency_code = 'USD' )
    ).

    out->write( 'Contents Before Sort' ).
    out->write( '____________________' ).
    out->write( flights ).
    out->write( ` ` ).

*   Sort with no additions - sort by primary table key carrier_id connection_id flight_date
    SORT flights.

    out->write( 'Effect of SORT with no additions - sort by primary table key' ).
    out->write( '____________________________________________________________' ).
    out->write( flights ).
    out->write( ` ` ).

*     Sort with field list - default sort direction is ascending
    SORT flights BY currency_code plane_type_id.
    out->write( 'Effect of SORT with field list - ascending is default direction' ).
    out->write( '________________________________________________________________' ).
    out->write( flights ).
    out->write( ` ` ).

*     Sort with field list and sort directions.
    SORT flights BY carrier_Id ASCENDING flight_Date DESCENDING.
    out->write( 'Effect of SORT with field list and sort direction' ).
    out->write( '_________________________________________________' ).
    out->write( flights ).
    out->write( ` ` ).

  ENDMETHOD. " internal_tables


  METHOD internal_tables2.

    TYPES t_flights TYPE STANDARD TABLE OF zflight WITH NON-UNIQUE KEY carrier_id connection_id flight_date.
    DATA: flights TYPE t_flights.

    flights = VALUE #(
        ( client = sy-mandt carrier_id = 'LH' connection_id = '0400' flight_date = '20230201' plane_type_id = '747-400' price = '600' currency_code = 'EUR' )
        ( client = sy-mandt carrier_id = 'QF' connection_id = '0006' flight_date = '20230112' plane_type_id = 'A380' price = '1600' currency_code = 'AUD' )
        ( client = sy-mandt carrier_id = 'AA' connection_id = '0017' flight_date = '20230110' plane_type_id = '747-400' price = '600' currency_code = 'USD' )
        ( client = sy-mandt carrier_id = 'LH' connection_id = '0400' flight_date = '20230301' plane_type_id = '747-400' price = '600' currency_code = 'EUR' )
        ( client = sy-mandt carrier_id = 'UA' connection_id = '0900' flight_date = '20230201' plane_type_id = '777-200' price = '600' currency_code = 'USD' )
        ( client = sy-mandt carrier_id = 'QF' connection_id = '0006' flight_date = '20230210' plane_type_id = 'A380' price = '1600' currency_code = 'AUD' )
    ).

    out->write( 'Contents Before DELETE ADJACENT DUPLICATES' ).
    out->write( '____________________' ).
    out->write( flights ).
    out->write( ` ` ).

    DELETE ADJACENT DUPLICATES FROM flights.
    out->write( 'Contents after DELETE ADJACENT DUPLICATES' ).
    out->write( 'Nothing deleted - key values are not adjacent' ).
    out->write( 'Sort the table before DELETE ADJACENT DUPLICATES' ).
    out->write( flights ).
    out->write( ` ` ).


    SORT flights BY carrier_id connection_id flight_date.
    DELETE ADJACENT DUPLICATES FROM flights.
    out->write( 'Contents after DELETE ADJACENT DUPLICATES' ).
    out->write( 'Nothing deleted - ABAP compares all key values including flight_date, which is different for every entry' ).
    out->write( flights ).
    out->write( ` ` ).

    DELETE ADJACENT DUPLICATES FROM flights COMPARING carrier_id connection_id.
    out->write( 'Contents after DELETE ADJACENT DUPLICATES with COMPARING and field list' ).
    out->write( 'Entries with identical values of carrier_id and connection_id have been deleted' ).
    out->write( flights ).

  ENDMETHOD. " internal_tables2


  METHOD internal_tables3.

    TYPES: BEGIN OF t_connection,
    carrier_id TYPE zconnection-carrier_id,
    connection_id TYPE zconnection-connection_id,
    departure_airport TYPE zconnection-airport_from_id,
    departure_airport_Name TYPE zairport-name,
    END OF t_connection.

    TYPES t_connections TYPE STANDARD TABLE OF t_connection WITH NON-UNIQUE KEY carrier_id connection_id.

    DATA connections TYPE TABLE OF zconnection.
    DATA airports TYPE TABLE OF zairport.
    DATA result_table TYPE t_connections.

*     Aim of the method:
*     Read a list of connections from the database and use them to fill an internal table result_table.
*     This contains some data from the table connections and adds the name of the departure airport.

    SELECT FROM zairport FIELDS * INTO TABLE @airports.
    SELECT FROM zconnection FIELDS * INTO TABLE @connections.

    out->write( 'Connection Table' ).
    out->write( '________________' ).
    out->write( connections ).
    out->write( ` ` ).

*     The VALUE expression iterates over the table connections. In each iteration, the variable line
*     accesses the current line. Inside the parentheses, we build the next line of result_table by
*     copying the values of line-carrier_Id, line-connection_Id and line-airport_from_id, then
*     loooking up the airport name in the internal table airports using a table expression


    result_table = VALUE #( FOR line IN connections ( carrier_Id = line-carrier_id
    connection_id = line-connection_id
    departure_airport = line-airport_from_id
    departure_airport_name = airports[ airport_id = line-airport_from_id ]-name ) ).

    out->write( 'Results' ).
    out->write( '_______' ).
    out->write( result_table ).

  ENDMETHOD. " internal_tables3


  METHOD internal_tables4.

    TYPES: BEGIN OF t_results,
        occupied TYPE int4, " /dmo/plane_seats_occupied,
        maximum TYPE int4, "/dmo/plane_seats_max,
    END OF t_results.


    TYPES: BEGIN OF t_results_with_Avg,
    occupied TYPE int4, " /dmo/plane_seats_occupied,
    maximum TYPE int4, " /dmo/plane_seats_max,
    average TYPE p LENGTH 16 DECIMALS 2,
    END OF t_results_with_avg.

    DATA flights TYPE TABLE OF zflight.
    SELECT FROM zflight FIELDS * INTO TABLE @flights.

*     Result is a scalar data type
    DATA(sum) = REDUCE i(
        INIT total = 0
        FOR line IN flights
        NEXT total += line-seats_occupied
    ).
    out->write( 'Result is a scalar data type' ).
    out->write( '_____________ ______________' ).
    out->write( sum ).
    out->write( ` ` ).


*     Result is a structured data type
    DATA(results) = REDUCE t_results(
        INIT totals TYPE t_results
        FOR line IN flights
        NEXT totals-occupied += line-seats_occupied
             totals-maximum += line-seats_max
    ).
    out->write( 'Result is a structure' ).
    out->write( '_____________________' ).


    out->write( results ).
    out->write( ` ` ).

*     Result is a structured data type
*     Reduction uses a local helper variable
*     Result of the reduction is always the *first* variable declared after init
    out->write( 'Result is a structure. The average is calculated using a local helper variable' ).
    out->write( '______________________________________________________________________________' ).

    DATA(result_with_Average) = REDUCE t_results_with_avg(
        INIT totals_avg TYPE t_results_with_avg
             count = 1
        FOR line IN flights
        NEXT totals_Avg-occupied += line-seats_occupied
             totals_avg-maximum += line-seats_max
             totals_avg-average = totals_avg-occupied / count
             count += 1
    ).

    out->write( result_with_average ).

  ENDMETHOD. " internal_tables4


  METHOD internal_tables5.

    TYPES it_type   TYPE TABLE OF ztest001. " Table Type
    TYPES wa_type   TYPE ztest001.          " Structure Type

    DATA it_tab2    TYPE it_type.           " Table
    APPEND VALUE #( id = '1' data = '11111' ) TO it_tab2.
    APPEND VALUE #( id = '2' data = '22222' ) TO it_tab2.

    DATA it_tab3    LIKE it_tab2.           " Structure
    APPEND VALUE #( id = '1' data = '11111' ) TO it_tab3.
    APPEND VALUE #( id = '2' data = '22222' ) TO it_tab3.

    DATA it_tab4    LIKE TABLE OF it_tab2.  " Table
    APPEND VALUE #( ( id = '1' data = '11111' ) ) TO it_tab4.
    APPEND VALUE #( ( id = '2' data = '22222' ) ) TO it_tab4.

    DATA it_tab5    LIKE it_tab2[].         " Table
    APPEND VALUE #( id = '1' data = '11111' ) TO it_tab5.
    APPEND VALUE #( id = '2' data = '22222' ) TO it_tab5.

    DATA it_tab6 TYPE STANDARD TABLE OF wa_type WITH NON-UNIQUE KEY id. " Standard Table With Key
*    DATA it_tab6 TYPE STANDARD TABLE OF wa_type WITH UNIQUE KEY id.    " UNIQUE can only be specified with the table categories HASHED and SORTED.

    DATA it_tab7 TYPE SORTED TABLE OF wa_type WITH NON-UNIQUE KEY id.   " Sorted Table With Non Unique Key
    DATA it_tab8 TYPE SORTED TABLE OF wa_type WITH UNIQUE KEY id.       " Sorted Table With Unique Key

*    DATA it_tab9 TYPE HASHED TABLE OF wa_type WITH NON-UNIQUE KEY id.  " HASHED keys must be unique. The addition UNIQUE must be specified.
    DATA it_tab10 TYPE HASHED TABLE OF wa_type WITH UNIQUE KEY id.      " Sorted Table With Key

    DATA it_tab11 TYPE ztest001. " Structure
*    APPEND VALUE #( id = '1' data = '11111' ) TO it_tab11. " IT_TAB11 is not an internal table.
*    APPEND VALUE #( id = '2' data = '22222' ) TO it_tab11.

    DATA(descr_ref1) = cl_abap_typedescr=>describe_by_data( it_tab11 ).
    out->write( 'Typename:' ).
    out->write( descr_ref1->absolute_name ).
    out->write( 'Kind:' ).
    out->write( descr_ref1->type_kind ). " Internal type u (flat structure)
     out->write( 'Length:' ).
    out->write( descr_ref1->length ).
    out->write( 'Decimals:' ).
    out->write( descr_ref1->decimals ).


* "IT_TYPE" is a generic type. Use this type only for typing field symbols and formal parameters.
*    DATA it_tab12    TYPE TABLE OF it_type.  " None
*    APPEND VALUE #( id = '1' data = '11111' ) TO it_tab12.
*    APPEND VALUE #( id = '2' data = '22222' ) TO it_tab12.

    DATA it_tab13 LIKE it_tab2. " Table
    APPEND VALUE #( id = '1' data = '11111' ) TO it_tab13.
    APPEND VALUE #( id = '2' data = '22222' ) TO it_tab13.

    DATA(descr_ref2) = cl_abap_typedescr=>describe_by_data( it_tab13 ).
    out->write( 'Typename:' ).
    out->write( descr_ref2->absolute_name ).
    out->write( 'Kind:' ).
    out->write( descr_ref2->type_kind ). " Internal type h (internal table)
    out->write( 'Length:' ).
    out->write( descr_ref2->length ).
    out->write( 'Decimals:' ).
    out->write( descr_ref2->decimals ).

    DATA(a) = 1.

  ENDMETHOD. " internal_tables5


  METHOD joins.

*     Joins:
    DATA BEGIN OF wa_tab.
      DATA t1(10) TYPE C.
      DATA t2(10) TYPE C.
      DATA t3(10) TYPE C.
    DATA END OF wa_tab.
    DATA it_tab LIKE TABLE OF wa_tab.

*     T1
    SELECT
      FROM
        ztest001 as zt001
      FIELDS
        zt001~id AS t1
      ORDER BY
        t1
      INTO CORRESPONDING FIELDS OF TABLE
        @it_tab.
    out->write( 'T1:' ).
    LOOP AT it_tab INTO wa_tab.
      out->write( wa_tab-t1 ).
    ENDLOOP.

*     T2
    SELECT
      FROM
        ztest002 as zt002
      FIELDS
        zt002~id AS t2
      ORDER BY
        t2
      INTO CORRESPONDING FIELDS OF TABLE
        @it_tab.
    out->write( 'T2:' ).
    LOOP AT it_tab INTO wa_tab.
      out->write( wa_tab-t2 ).
    ENDLOOP.

*     T3
    SELECT
      FROM
        ztest003 as zt003
      FIELDS
        zt003~id AS t3
      ORDER BY
        t3
      INTO CORRESPONDING FIELDS OF TABLE
        @it_tab.
    out->write( 'T3:' ).
    LOOP AT it_tab INTO wa_tab.
      out->write( wa_tab-t3 ).
    ENDLOOP.

*     [Inner] Join
    SELECT
      FROM
        ztest001 as zt001
        JOIN ztest002 as zt002 ON zt001~id = zt002~id
        JOIN ztest003 as zt003 ON zt002~id = zt003~id
      FIELDS
        zt001~id AS t1,
        zt002~id AS t2,
        zt003~id AS t3
      ORDER BY
        t1, t2, t3
      INTO CORRESPONDING FIELDS OF TABLE
        @it_tab.
    out->write( '[Inner] Join:' ).
    LOOP AT it_tab INTO wa_tab.
      out->write( |{ wa_tab-t1 }   { wa_tab-t2 }   { wa_tab-t3 }| ).
    ENDLOOP.

*     Left [Outer] Join (2)
    SELECT
        ztest001~id AS t1,
        ztest002~id AS t2
      FROM
        ( ztest001
        LEFT JOIN ztest002 ON ztest001~id = ztest002~id )
      ORDER BY
        t1, t2
      INTO CORRESPONDING FIELDS OF TABLE
        @it_tab.
    out->write( 'Left [Outer] Join (2):' ).
    LOOP AT it_tab INTO wa_tab.
      out->write( |{ wa_tab-t1 }   { wa_tab-t2 }| ).
    ENDLOOP.

*     Left [Outer] Join (3A)
    SELECT
        ztest001~id AS t1,
        ztest002~id AS t2,
        ztest003~id AS t3
      FROM
        (
          (
            ( ztest001 ) LEFT JOIN
            ( ztest002 )
            ON ( ztest001~id = ztest002~id )
          ) LEFT JOIN
          ( ztest003 )
          ON ( ztest002~id = ztest003~id )
        )
      ORDER BY
        t1, t2, t3
      INTO CORRESPONDING FIELDS OF TABLE
        @it_tab.
    out->write( 'Left [Outer] Join (3A):' ).
    LOOP AT it_tab INTO wa_tab.
      out->write( |{ wa_tab-t1 }   { wa_tab-t2 }   { wa_tab-t3 }| ).
    ENDLOOP.

*     Left [Outer] Join (3B)
    SELECT
        ztest001~id AS t1,
        ztest002~id AS t2,
        ztest003~id AS t3
      FROM
        ztest001 LEFT JOIN
        (
          ztest002 LEFT JOIN
          ztest003
          ON ztest003~id = ztest002~id " 1. step
        )
        ON ztest001~id = ztest002~id " 2. step
      ORDER BY
        t1, t2, t3
      INTO CORRESPONDING FIELDS OF TABLE
        @it_tab.
    out->write( 'Left [Outer] Join (3B):' ).
    LOOP AT it_tab INTO wa_tab.
      out->write( |{ wa_tab-t1 }   { wa_tab-t2 }   { wa_tab-t3 }| ).
    ENDLOOP.

*     Right [Outer] Join (2)
    SELECT
        ztest001~id AS t1,
        ztest002~id AS t2
      FROM
        ( ztest001
        RIGHT JOIN ztest002 ON ztest001~id = ztest002~id )
      ORDER BY
        t2, t1
      INTO CORRESPONDING FIELDS OF TABLE
        @it_tab.
    out->write( 'Right [Outer] Join (2):' ).
    LOOP AT it_tab INTO wa_tab.
      IF ( wa_tab-t1 IS INITIAL ).
      ENDIF.
      out->write( |{ COND string( WHEN ( wa_tab-t1 IS INITIAL ) THEN '-' ELSE wa_tab-t1 ) }   { COND string( WHEN ( wa_tab-t2 IS INITIAL ) THEN '-' ELSE wa_tab-t2 ) }| ).
    ENDLOOP.

*     Right [Outer] Join (3)
    SELECT
        ztest001~id AS t1,
        ztest002~id AS t2,
        ztest003~id AS t3
      FROM
        ztest001
        RIGHT JOIN ztest002 ON ztest001~id = ztest002~id
        RIGHT JOIN ztest003 ON ztest002~id = ztest003~id
      ORDER BY
        t3, t2, t1
      INTO CORRESPONDING FIELDS OF TABLE
        @it_tab.
    out->write( 'Right [Outer] Join (3):' ).
    LOOP AT it_tab INTO wa_tab.
      out->write( |{ COND string( WHEN ( wa_tab-t1 IS INITIAL ) THEN '-' ELSE wa_tab-t1 ) }   { COND string( WHEN ( wa_tab-t2 IS INITIAL ) THEN '-' ELSE wa_tab-t2 ) }   { COND string( WHEN ( wa_tab-t3 IS INITIAL ) THEN '-' ELSE wa_tab-t3 ) }| ).
    ENDLOOP.

*     Cross Join (with Where = Inner Join)
    SELECT
      FROM
        ztest001
        CROSS JOIN ztest002
        CROSS JOIN ztest003
      FIELDS
        ztest001~id AS t1,
        ztest002~id AS t2,
        ztest003~id AS t3
      WHERE
        ( ztest001~id = ztest002~id ) AND
        ( ztest002~id = ztest003~id )
      ORDER BY
        t1, t2, t3
      INTO CORRESPONDING FIELDS OF TABLE
        @it_tab.
    out->write( 'Cross Join:' ).
    LOOP AT it_tab INTO wa_tab.
      out->write( |{ wa_tab-t1 }   { wa_tab-t2 }   { wa_tab-t3 }| ).
    ENDLOOP.

*     Left Outer Many To One Join
    SELECT
        ztest001~id AS t1,
        ztest002~id AS t2
      FROM
        ( ztest001
        LEFT OUTER MANY TO ONE JOIN ztest002 ON ztest001~id = ztest002~id )
      ORDER BY
        t1, t2
      INTO CORRESPONDING FIELDS OF TABLE
        @it_tab.
    out->write( 'Left Outer Many To One Join:' ).
    LOOP AT it_tab INTO wa_tab.
      out->write( |{ wa_tab-t1 }   { wa_tab-t2 }| ).
    ENDLOOP.

*     Left Outer Many To Many Join
    SELECT
        ztest001~id AS t1,
        ztest002~id AS t2
      FROM
        ( ztest001
        LEFT OUTER MANY TO MANY JOIN ztest002 ON ztest001~id = ztest002~id )
      ORDER BY
        t1, t2
      INTO CORRESPONDING FIELDS OF TABLE
        @it_tab.
    out->write( 'Left Outer Many To Many Join:' ).
    LOOP AT it_tab INTO wa_tab.
      out->write( |{ wa_tab-t1 }   { wa_tab-t2 }| ).
    ENDLOOP.

  ENDMETHOD. " joins


  METHOD material_document_api.

*    SELECT * FROM i_product INTO TABLE @DATA(it_product).

    TRY.

*       DATA(i_url) = 'https://my404930-api.s4hana.cloud.sap:443/sap/opu/odata/sap/API_PRODUCT_SRV/A_Product'.

        DATA i_url      TYPE string. " VALUE 'https://my404907-api.s4hana.cloud.sap/sap/opu/odata/sap//sap/opu/odata/sap/API_PRODUCT_SRV/A_Product'.
        DATA i_username TYPE string. " VALUE 'INBOUND_USER'.
        DATA i_password TYPE string. " VALUE 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.

        DATA(system_url) = cl_abap_context_info=>get_system_url( ).

        IF ( system_url(8) = 'my404930' ). " dev dev
            i_url       = 'https://my404930-api.s4hana.cloud.sap/sap/opu/odata/sap/API_MATERIAL_DOCUMENT_SRV/A_MaterialDocumentHeader'.
            i_username  = 'INBOUND_USER'.
            i_password  = 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.
        ENDIF.
        IF ( system_url(8) = 'my404898' ). " dev cust
            i_url       = 'https://my404898-api.s4hana.cloud.sap/sap/opu/odata/sap/API_MATERIAL_DOCUMENT_SRV/A_MaterialDocumentHeader'.
            i_username  = 'INBOUND_USER'.
            i_password  = 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.
        ENDIF.
        IF ( system_url(8) = 'my404907' ). " test
            i_url       = 'https://my404907-api.s4hana.cloud.sap/sap/opu/odata/sap/API_MATERIAL_DOCUMENT_SRV/A_MaterialDocumentHeader'.
            i_username  = 'INBOUND_USER'.
            i_password  = 'rtrVDDgelabtTjUiybRX}tVD3JksqqfvPpBdJRaL'.
        ENDIF.
        IF ( system_url(8) = 'my410080' ). " prod
            i_url       = 'https://my410080-api.s4hana.cloud.sap/sap/opu/odata/sap/API_MATERIAL_DOCUMENT_SRV/A_MaterialDocumentHeader'.
            i_username  = 'INBOUND_USER'.
            i_password  = 'YKXMYdjNnGgqko&aEueVx5mHTFPRGcDGAVgQgnFh'.
        ENDIF.

*       Get Source Material Document:

*       https://my404898-api.s4hana.cloud.sap/sap/opu/odata/sap/API_MATERIAL_DOCUMENT_SRV/A_MaterialDocumentHeader(MaterialDocumentYear='2024',MaterialDocument='4900001301')?$expand=to_MaterialDocumentItem&$format=json
        CONCATENATE
                i_url
                '('
                'MaterialDocumentYear=''2024'',MaterialDocument=''4900001301'''
                ')'
                '?$expand='
                'to_MaterialDocumentItem'
                '&$format=json'
            INTO
                DATA(i_url1).

        DATA(http_destination1) = cl_http_destination_provider=>create_by_url( i_url1 ).

        DATA(lo_http_client1) = cl_web_http_client_manager=>create_by_http_destination( http_destination1 ).

        lo_http_client1->get_http_request( )->set_authorization_basic(
            i_username = i_username
            i_password = i_password
        ).

        DATA(lo_http_request1) = lo_http_client1->get_http_request( ).

        DATA(lo_http_response1) = lo_http_client1->execute(
            i_method   = if_web_http_client=>get
        ).

        DATA(text1)                   = lo_http_response1->get_text( ).
        DATA(status1)                 = lo_http_response1->get_status( ).
        DATA(response_header_fields1) = lo_http_response1->get_header_fields( ).

*        RETURN.

*       Get Token:

        DATA(http_destination) = cl_http_destination_provider=>create_by_url( i_url ).

        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( http_destination ).

        lo_http_client->get_http_request( )->set_authorization_basic(
            i_username = i_username
            i_password = i_password
        ).

        DATA(lo_http_request) = lo_http_client->get_http_request( ).

        lo_http_request->set_header_field(
            i_name  = 'x-csrf-token'
            i_value = 'fetch'
        ).

        DATA(lo_http_response) = lo_http_client->execute(
            i_method   = if_web_http_client=>get
        ).

        DATA(text)                   = lo_http_response->get_text( ).
        DATA(status)                 = lo_http_response->get_status( ).
        DATA(response_header_fields) = lo_http_response->get_header_fields( ).

        READ TABLE response_header_fields WITH KEY name = 'x-csrf-token' INTO DATA(field).
        IF ( sy-subrc = 0 ).
            DATA(token) = field-value.
        ENDIF.

*       Create New Material Document:

        DATA i_fields TYPE if_web_http_request=>name_value_pairs.
        APPEND VALUE #(
            name  = 'x-csrf-token'
            value = token " '5iGZK1qT45Vi4UfHYazbPQ=='
        )
        TO i_fields.
        APPEND VALUE #(
            name  = 'Content-Type'
            value = 'application/json'
        )
        TO i_fields.

        lo_http_request->set_header_fields(
          EXPORTING
            i_fields = i_fields
*          RECEIVING
*            r_value  =
        ).

        DATA i_text TYPE string.

        i_text = text1.


        CONCATENATE '"MaterialDocumentYear":"' '2024' '","MaterialDocument":"' '4900001301' '",' INTO DATA(sourceTag).
        CONCATENATE '"MaterialDocumentYear":"' '' '","MaterialDocument":"' '' '",' INTO DATA(targetTag).
        "GoodsMovementCode":"",

        REPLACE ALL OCCURRENCES OF sourceTag in i_text WITH targetTag.
        REPLACE ALL OCCURRENCES OF '"GoodsMovementCode":"",' in i_text WITH '"GoodsMovementCode":"01",'.

        lo_http_request->set_text(
            i_text  = i_text
        ).

        lo_http_response = lo_http_client->execute(
            i_method   = if_web_http_client=>post
        ).

        text                      = lo_http_response->get_text( ).
        status                    = lo_http_response->get_status( ).
        response_header_fields    = lo_http_response->get_header_fields( ).

        IF ( status-code = '201' ).
*            APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-success text = 'Product "' && item-Product && '" created.' ) ) TO reported-product.
        ELSE.
*            APPEND VALUE #( %key = <entity>-%key %msg = new_message_with_text( severity = if_abap_behv_message=>severity-error text = 'Product "' && item-Product && '" not created.' ) ) TO reported-product.
        ENDIF.

    CATCH cx_web_http_client_error.
        "handle exception

    CATCH cx_abap_context_info_error.
        "handle exception

    CATCH cx_http_dest_provider_error.
        "handle exception

    ENDTRY.

  ENDMETHOD. " material_document_api


  METHOD material_document_eml.

    DATA it_materialdocument_create TYPE TABLE FOR CREATE I_MaterialDocumentTP\\MaterialDocument.
    DATA it_materialdocumentitem_create TYPE TABLE FOR CREATE I_MaterialDocumentTP\\MaterialDocument\_MaterialDocumentItem.

    MODIFY ENTITIES OF I_MaterialDocumentTP
        ENTITY MaterialDocument
        CREATE FROM VALUE #( ( %cid = 'CID_001'
             goodsmovementcode = '05' " '03' " '01'
*             postingdate = '20240807'
*             documentdate = '20240807'
             %control-goodsmovementcode = cl_abap_behv=>flag_changed
*             %control-postingdate = cl_abap_behv=>flag_changed
*             %control-documentdate = cl_abap_behv=>flag_changed
        ) )
        ENTITY MaterialDocument
        CREATE BY \_MaterialDocumentItem
        FROM VALUE #( (
             %cid_ref = 'CID_001'
             %target = VALUE #( ( %cid = 'CID_ITM_001'
                 plant = '1000'
                 material = '0803820-004-E-095' " '0000319-595-B-090'
                 goodsmovementtype = '711' " '701' " '552'
*                 storagelocation = ''
                 quantityinentryunit = 1
                 entryunit = 'EA'
                 inventoryspecialstocktype = 'W'
                 customer = '0010004935'
*                 costcenter = '0000010000'
                 %control-plant = cl_abap_behv=>flag_changed
                 %control-material = cl_abap_behv=>flag_changed
                 %control-goodsmovementtype = cl_abap_behv=>flag_changed
*                 %control-storagelocation = cl_abap_behv=>flag_changed
                 %control-quantityinentryunit = cl_abap_behv=>flag_changed
                 %control-entryunit = cl_abap_behv=>flag_changed
                 %control-inventoryspecialstocktype = cl_abap_behv=>flag_changed
                 %control-customer = cl_abap_behv=>flag_changed
*                 %control-costcenter = cl_abap_behv=>flag_changed
             ) )
         ) )
    MAPPED DATA(mapped)
    FAILED DATA(failed)
    REPORTED DATA(reported).

    COMMIT ENTITIES BEGIN

        RESPONSE OF i_materialdocumenttp
        FAILED DATA(commit_failed)
        REPORTED DATA(commit_reported).

*        LOOP AT mapped-materialdocument ASSIGNING FIELD-SYMBOL(<keys_header>).
*         CONVERT KEY OF i_materialdocumentTp
*         FROM <keys_header>-%pid
*         TO <keys_header>-%key.
*        ENDLOOP.

*        LOOP AT mapped-materialdocumentitem ASSIGNING FIELD-SYMBOL(<keys_item>).
*         CONVERT KEY OF i_materialdocumentitemtp
*         FROM <keys_item>-%pid
*         TO <keys_item>-%key.
*        ENDLOOP.

*       Message
        DATA severity           TYPE if_abap_behv_message=>t_severity VALUE if_abap_behv_message=>severity-error.
        DATA msgno              LIKE if_abap_behv_message=>if_t100_message~t100key-msgno VALUE '001'.
        DATA msgid              LIKE if_abap_behv_message=>if_t100_message~t100key-msgid VALUE 'Z_PRODUCT_001'.
        DATA msgty              LIKE if_t100_dyn_msg=>msgty.
        DATA msgv1              LIKE if_abap_behv_message=>if_t100_message~t100key-attr1.
        DATA msgv2              LIKE if_abap_behv_message=>if_t100_message~t100key-attr2.
        DATA msgv3              LIKE if_abap_behv_message=>if_t100_message~t100key-attr3.
        DATA msgv4              LIKE if_abap_behv_message=>if_t100_message~t100key-attr4.
        DATA text               TYPE string.
        DATA longtext           TYPE string.

        LOOP AT commit_reported-materialdocument INTO DATA(materialdocument).
            severity  = materialdocument-%msg->m_severity.
            msgno     = materialdocument-%msg->if_t100_message~t100key-msgno.
            msgid     = materialdocument-%msg->if_t100_message~t100key-msgid.
            msgty     = materialdocument-%msg->if_t100_dyn_msg~msgty.
            msgv1     = CONV scx_attrname( materialdocument-%msg->if_t100_dyn_msg~msgv1 ).
            msgv2     = CONV scx_attrname( materialdocument-%msg->if_t100_dyn_msg~msgv2 ).
            msgv3     = CONV scx_attrname( materialdocument-%msg->if_t100_dyn_msg~msgv3 ).
            msgv4     = CONV scx_attrname( materialdocument-%msg->if_t100_dyn_msg~msgv4 ).
            text      = materialdocument-%msg->if_message~get_text(  ).
            longtext  = materialdocument-%msg->if_message~get_longtext(  ).
*           APPEND VALUE #( %key = key-%key %msg = new_message( severity = severity id = msgid number = msgno v1 = msgv1 v2 = msgv2 v3 = msgv3 v4 = msgv4 ) ) TO reported-materialdocumentitem.
            out->write( 'materialdocument-text :' && text ).
            out->write( 'materialdocument-longtext :' && longtext ).
        ENDLOOP.
        LOOP AT commit_reported-materialdocumentitem INTO DATA(materialdocumentitem).
            severity    = materialdocumentitem-%msg->m_severity.
            msgno       = materialdocumentitem-%msg->if_t100_message~t100key-msgno.
            msgid       = materialdocumentitem-%msg->if_t100_message~t100key-msgid.
            msgty       = materialdocumentitem-%msg->if_t100_dyn_msg~msgty.
            msgv1       = CONV scx_attrname( materialdocumentitem-%msg->if_t100_dyn_msg~msgv1 ).
            msgv2       = CONV scx_attrname( materialdocumentitem-%msg->if_t100_dyn_msg~msgv2 ).
            msgv3       = CONV scx_attrname( materialdocumentitem-%msg->if_t100_dyn_msg~msgv3 ).
            msgv4       = CONV scx_attrname( materialdocumentitem-%msg->if_t100_dyn_msg~msgv4 ).
            text        = materialdocumentitem-%msg->if_message~get_text(  ).
            longtext    = materialdocumentitem-%msg->if_message~get_longtext(  ).
*            APPEND VALUE #( %key = key-%key %msg = new_message( severity = severity id = msgid number = msgno v1 = msgv1 v2 = msgv2 v3 = msgv3 v4 = msgv4 ) ) TO reported-materialdocumentitem.
            out->write( 'materialdocumentitem-text :' && text ).
            out->write( 'materialdocumentitem-longtext :' && longtext ).

        ENDLOOP.

    COMMIT ENTITIES END.

  ENDMETHOD. " material_document_eml


  METHOD modify_sales_order.

*    DATA ls_key_data        TYPE zrap_zc_matrix_005.
*    DATA ls_business_data   TYPE zrap_zc_matrix_005.
*    DATA lo_client_proxy    TYPE REF TO /iwbep/if_cp_client_proxy.
*    DATA lo_request1        TYPE REF TO /iwbep/if_cp_request_read.
*    DATA lo_response1       TYPE REF TO /iwbep/if_cp_response_read.
*    DATA lo_request2        TYPE REF TO /iwbep/if_cp_request_update.
*    DATA lo_response2       TYPE REF TO /iwbep/if_cp_response_update.
*    DATA lo_http_client     TYPE REF TO if_web_http_client.
*
*    TRY.
*
*        DATA(http_destination) = cl_http_destination_provider=>create_by_url( i_url = i_url ).
*
*        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( i_destination = http_destination ).
*
*        lo_http_client->accept_cookies( i_allow = abap_true ).
*
*        lo_http_client->get_http_request( )->set_authorization_basic( i_username = i_username i_password = i_password ).
*
*        lo_client_proxy = cl_web_odata_client_factory=>create_v2_remote_proxy(
**                iv_do_fetch_csrf_token      = abap_true
*                iv_service_definition_name  = 'ZSC_MATRIX_005'
*                io_http_client              = lo_http_client
*                iv_relative_service_root    = '/sap/opu/odata/sap/API_SALES_ORDER_SRV/'
*        ).
*
**       Prepare key data (matrix)
*        ls_key_data = VALUE #(
*            MatrixUUID                      = i_matrix-MatrixUUID " '87ED5F17FA6A1EEE94807C16CC430947'
*            IsActiveEntity                  = abap_true
*        ).
*
*        " Navigate to the resource
*        lo_request1 = lo_client_proxy->create_resource_for_entity_set( 'ZC_MATRIX_005' )->navigate_with_key( ls_key_data )->create_request_for_read( ).
*
*        " Execute the request and retrieve the business data
*        lo_response1 = lo_request1->execute( ).
*
*        " Get business data
*        lo_response1->get_business_data( IMPORTING es_business_data = ls_business_data ).
*
*        ls_business_data-PurchaseOrderByCustomer          = i_matrix-PurchaseOrderByCustomer .
*        ls_business_data-RequestedDeliveryDate            = i_matrix-RequestedDeliveryDate.
*
*        " Navigate to the resource
*        lo_request2 = lo_client_proxy->create_resource_for_entity_set( 'ZC_MATRIX_005' )->navigate_with_key( ls_key_data )->create_request_for_update( /iwbep/if_cp_request_update=>gcs_update_semantic-put ).
*
*        " Set the business data for the created entity
*        lo_request2->set_business_data( ls_business_data ).
*
*        " Execute the request (Update)
*        lo_response2 = lo_request2->execute( ).
*
*        " Get the after image
**        lo_response2->get_business_data( IMPORTING es_business_data = ls_business_data ).
*
*    CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
*      " Handle remote Exception
**      RAISE SHORTDUMP lx_remote.
*
*    CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
*      " Handle Exception
**      RAISE SHORTDUMP lx_gateway.
*
*    CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
*      " Handle Exception
**      RAISE SHORTDUMP lx_web_http_client_error.
*
*    CATCH cx_http_dest_provider_error INTO DATA(lx_http_dest_provider_error).
*        "handle exception
**      RAISE SHORTDUMP lx_http_dest_provider_error.
*
*    ENDTRY.

  ENDMETHOD. " modify_sales_order


  METHOD numerics.

    DATA p1 TYPE P.
    DATA p2 TYPE P DECIMALS 3.
    DATA p3 TYPE P DECIMALS 2.
    p1 = '10.336'.
    p2 = '10.336'.
    p3 = '10.336'.
    out->write( p1 ).
    out->write( p2 ).
    out->write( p3 ).

    out->write( |{ 2 * '2.31' }| ).
    out->write( |{ '2.31' * 2 }| ).
    out->write( |{ 1 / 8 }| ).
    out->write( |{ EXACT #(  1 / 8 ) }| ).

  ENDMETHOD. " numerics


  METHOD populate_tables.

    DATA it_tab1 TYPE TABLE OF ztest001.
    DELETE FROM ztest001.
    APPEND VALUE #( id = '1' data = '111' ) TO it_tab1.
    APPEND VALUE #( id = '4' data = '444' ) TO it_tab1.
    APPEND VALUE #( id = '6' data = '666' ) TO it_tab1.
    APPEND VALUE #( id = '7' data = '777' ) TO it_tab1.
    INSERT ztest001 FROM TABLE @it_tab1.

    DATA it_tab2 TYPE TABLE OF ztest002.
    DELETE FROM ztest002.
    APPEND VALUE #( id = '2' data = '222' ) TO it_tab2.
    APPEND VALUE #( id = '4' data = '444' ) TO it_tab2.
    APPEND VALUE #( id = '5' data = '555' ) TO it_tab2.
    APPEND VALUE #( id = '7' data = '777' ) TO it_tab2.
    INSERT ztest002 FROM TABLE @it_tab2.

    DATA it_tab3 TYPE TABLE OF ztest003.
    DELETE FROM ztest003.
    APPEND VALUE #( id = '3' data = '333' ) TO it_tab3.
    APPEND VALUE #( id = '5' data = '555' ) TO it_tab3.
    APPEND VALUE #( id = '6' data = '666' ) TO it_tab3.
    APPEND VALUE #( id = '7' data = '777' ) TO it_tab3.
    INSERT ztest003 FROM TABLE @it_tab3.

    out->write( 'Generate Data: Airport      /DMO/AIRPORT' ) ##NO_TEXT.
**    lcl_airport_data_generator=>lif_data_generator~create( out ).
    out->write( 'Generate Data: Carrier      /DMO/CARRIER' ) ##NO_TEXT.
**    lcl_carrier_data_generator=>lif_data_generator~create( out ).
    out->write( 'Generate Data: Connection   /DMO/CONNECTION' ) ##NO_TEXT.
**    lcl_connection_data_generator=>lif_data_generator~create( out ).
    out->write( 'Generate Data: Flight       /DMO/FLIGHT' ) ##NO_TEXT.
**    lcl_flight_data_generator=>lif_data_generator~create( out ).
    out->write( 'Generate Data: Agency       /DMO/AGENCY' ) ##NO_TEXT.
**    lcl_agency_data_generator=>lif_data_generator~create( out ).
    out->write( 'Generate Data: Customer      /DMO/CUSTOMER' ) ##NO_TEXT.
*    lcl_customer_data_generator=>lif_data_generator~create( out ).
    out->write( 'Generate Data: Supplement      /DMO/SUPPLEMENT' ) ##NO_TEXT.
*    lcl_supplement_data_generator=>lif_data_generator~create( out ).
    out->write( 'Generate Data: Travel      /DMO/TRAVEL' ) ##NO_TEXT.
    out->write( 'Generate Data: Booking      /DMO/BOOKING' ) ##NO_TEXT.
    out->write( 'Generate Data: Booking Supplement      /DMO/BOOK_SUPPL' ) ##NO_TEXT.
*    lcl_travel_data_generator=>lif_data_generator~create( out ).
    out->write( 'Generate Data: Status ValueHelps' ) ##NO_TEXT.
*    lcl_status_vh_data_generator=>lif_data_generator~create( out ).

*    out->write(  'Calling BAdIs' ) ##NO_TEXT.

*    DATA lo_badi TYPE REF TO /dmo/data_generation_badi.
*    GET BADI lo_badi.
*    CALL BADI lo_badi->data_generation
*      EXPORTING
*        out = out.
*    out->write( 'Finished Calling BAdIs' ) ##NO_TEXT.
*    out->write( 'Finished Data Generation' ) ##NO_TEXT.

    GET TIME STAMP FIELD DATA(current_timestamp).

*   Agency
    DATA it_agency TYPE TABLE OF zagency.
    DELETE FROM zagency.
    it_agency = VALUE #( ##NO_TEXT
        ( CLIENT = '080' AGENCY_ID = '070001' NAME = 'Sunshine Travel' STREET = '134 West Street' POSTAL_CODE = '54323' CITY = 'Rochester' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 901-632-5620' EMAIL_ADDRESS = 'info@sunshine-travel.sap'
        WEB_ADDRESS = 'http://www.sunshine-travel.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Fast and smooth!'  )
        ( CLIENT = '080' AGENCY_ID = '070002' NAME = 'Fly High' STREET = 'Berliner Allee 11' POSTAL_CODE = '40880' CITY = 'Duesseldorf' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 2102 69555' EMAIL_ADDRESS = 'info@flyhigh.sap' WEB_ADDRESS =
'http://www.flyhigh.sap'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Cool and smooth! Fly with us! We are the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070003' NAME = 'Happy Hopping' STREET = 'Calvinstr. 36' POSTAL_CODE = '13467' CITY = 'Berlin' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 30-8853-0' EMAIL_ADDRESS = 'info@haphop.sap' WEB_ADDRESS = 'http://www.haphop.sap'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Happy Hopping is the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070004' NAME = 'Pink Panther' STREET = 'Auf der Schanz 54' POSTAL_CODE = '65936' CITY = 'Frankfurt' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 69-467653-0' EMAIL_ADDRESS = 'info@pinkpanther.sap'
        WEB_ADDRESS = 'http://www.pinkpanther.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
DUMMY_FIELD = ''
        ZZSLOGANZAG = 'We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070005' NAME = 'Your Choice' STREET = 'Gustav-Jung-Str. 425' POSTAL_CODE = '90455' CITY = 'Nuernberg' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 9256-4548-0' EMAIL_ADDRESS = 'info@yc.sap' WEB_ADDRESS =
'http://www.yc.sap'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = '' ZZSLOGANZAG = 'Better with us!'  )
        ( CLIENT = '080' AGENCY_ID = '070006' NAME = 'Bella Italia' STREET = 'Via Marconi 123' POSTAL_CODE = '00139' CITY = 'Roma' COUNTRY_CODE = 'IT' PHONE_NUMBER = '+39 6 893546721' EMAIL_ADDRESS = 'info@tours.it/Adventure/'
        WEB_ADDRESS = 'http://www.tours.it/Adventure/' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Cool and save!'  )
        ( CLIENT = '080' AGENCY_ID = '070007' NAME = 'Hot Socks Travel' STREET = '224 Balnagask Rd' POSTAL_CODE = '8053' CITY = 'Sydney' COUNTRY_CODE = 'AU' PHONE_NUMBER = '+61 2 2004 5000' EMAIL_ADDRESS = 'info@hst.co.au' WEB_ADDRESS =
'http://www.hst.co.au'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Fast and save! Hot Socks Travel is the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070008' NAME = 'Burns Nuclear' STREET = '14 Science Park Drive' POSTAL_CODE = '118228' CITY = 'Singapore' COUNTRY_CODE = 'SG' PHONE_NUMBER = '+65 777-5566' EMAIL_ADDRESS = 'info@burns-burns-burns.sg'
        WEB_ADDRESS = 'http://www.burns-burns-burns.sg' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Fast and smooth! We are the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070009' NAME = 'Honauer Reisen GmbH' STREET = 'Baumgarten 8' POSTAL_CODE = '4212' CITY = 'Neumarkt' COUNTRY_CODE = 'AT' PHONE_NUMBER = '+43 7941 8903' EMAIL_ADDRESS = 'info@honauer.at' WEB_ADDRESS =
'http://www.honauer.at'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Fast and save! Honauer Reisen GmbH is the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070010' NAME = 'Travel from Walldorf' STREET = 'Altonaer Str. 24' POSTAL_CODE = '10557' CITY = 'Berlin' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 30-622860' EMAIL_ADDRESS = 'info@travel-from-walldorf'
        WEB_ADDRESS = 'http://www.travel-from-walldorf' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Fast and smooth! Travel from Walldorf is the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070011' NAME = 'Voyager Enterprises' STREET = 'Gustavslundsvaegen 151' POSTAL_CODE = '70563' CITY = 'Stockholm' COUNTRY_CODE = 'SE' PHONE_NUMBER = '+46 8/ 587 70000' EMAIL_ADDRESS = 'info@starfleet.ufp'
        WEB_ADDRESS = 'http://www.starfleet.ufp' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD
= ''
        ZZSLOGANZAG = 'Fast and smooth!'  )
        ( CLIENT = '080' AGENCY_ID = '070012' NAME = 'Ben McCloskey Ltd.' STREET = '74 Court Oak Rd' POSTAL_CODE = 'B17 9TN' CITY = 'Birmingham' COUNTRY_CODE = 'GB' PHONE_NUMBER = '+44 121 365-2251' EMAIL_ADDRESS = 'info@ben-mcCloskey.co.uk'
        WEB_ADDRESS = 'http://www.ben-mcCloskey.co.uk' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Cool and smooth! Fly with the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070013' NAME = 'Pillepalle Trips' STREET = 'Gorki Park 4' POSTAL_CODE = '8008' CITY = 'Zuerich' COUNTRY_CODE = 'CH' PHONE_NUMBER = '+41 1 345-5321' EMAIL_ADDRESS = 'info@pi-pa-tri.sap'
        WEB_ADDRESS = 'http://www.pi-pa-tri.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD
= ''
        ZZSLOGANZAG = 'Cool and save! Pillepalle Trips is the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070014' NAME = 'Kangeroos' STREET = 'Lancaster drive 435' POSTAL_CODE = '20001' CITY = 'London' COUNTRY_CODE = 'GB' PHONE_NUMBER = '+44 171-2937638' EMAIL_ADDRESS = 'info@hopp.sap' WEB_ADDRESS = 'http://www.hopp.sap'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Better with us! We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070015' NAME = 'Bavarian Castle' STREET = 'Pilnizerstr. 241' POSTAL_CODE = '01069' CITY = 'Dresden' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 98-32832732' EMAIL_ADDRESS = 'info@neu.schwanstein.sap'
        WEB_ADDRESS = 'http://www.neu.schwanstein.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Fly with us! We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070016' NAME = 'Ali''s Bazar' STREET = '45, Mac Arthur Boulevard' POSTAL_CODE = '19113' CITY = 'Boston' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 508-692-5200' EMAIL_ADDRESS = 'info@ali.sap' WEB_ADDRESS =
'http://www.ali.sap'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Better with Ali''s Bazar! We are the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070017' NAME = 'Super Agency' STREET = '50 Cranworth St' POSTAL_CODE = 'G12 8AG' CITY = 'Glasgow' COUNTRY_CODE = 'GB' PHONE_NUMBER = '+44 141 711-5643' EMAIL_ADDRESS = 'info@super.sap' WEB_ADDRESS =
'http://www.super.sap'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Super Agency is the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070018' NAME = 'Wang Chong' STREET = 'Gagarine Park' POSTAL_CODE = '150021' CITY = 'Moscow' COUNTRY_CODE = 'RU' PHONE_NUMBER = '+7 3287-213321' EMAIL_ADDRESS = 'info@wang.chong.sap' WEB_ADDRESS =
'http://www.wang.chong.sap'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = '' ZZSLOGANZAG = 'Fast and save!'  )
        ( CLIENT = '080' AGENCY_ID = '070019' NAME = 'Around the World' STREET = 'An der Breiten Wiese 122' POSTAL_CODE = '30625' CITY = 'Hannover' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 511-347589-0' EMAIL_ADDRESS = 'info@atw.sap'
        WEB_ADDRESS = 'http://www.atw.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Fly with us!'  )
        ( CLIENT = '080' AGENCY_ID = '070020' NAME = 'No Return' STREET = 'Wahnheider Str. 57' POSTAL_CODE = '51105' CITY = 'Koeln' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 221-5689-100' EMAIL_ADDRESS = 'info@bye-bye.sap' WEB_ADDRESS =
'http://www.bye-bye.sap'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'No Return is the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070021' NAME = 'Special Agency Peru' STREET = 'Triberger Str. 42' POSTAL_CODE = '70569' CITY = 'Stuttgart' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 711-7100' EMAIL_ADDRESS = 'info@sap.com' WEB_ADDRESS =
'http://www.sap.com'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = '' ZZSLOGANZAG = 'We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070022' NAME = 'Caribian Dreams' STREET = 'Deichstrasse 45' POSTAL_CODE = '26721' CITY = 'Emden' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 2670-8560-0' EMAIL_ADDRESS = 'info@cuba-libre.sap'
        WEB_ADDRESS = 'http://www.cuba-libre.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Cool and save! Caribian Dreams is the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070023' NAME = 'Asia By Plane' STREET = '6-9 Iidabashi 7-chome' POSTAL_CODE = '102-0072' CITY = 'Tokyo' COUNTRY_CODE = 'JP' PHONE_NUMBER = '+81 3-3239-3501' EMAIL_ADDRESS = 'info@asia-by-plane.co.jp'
        WEB_ADDRESS = 'http://www.asia-by-plane.co.jp' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070024' NAME = 'Everywhere' STREET = 'Regensburger Platz 23' POSTAL_CODE = '81679' CITY = 'Muenchen' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 89-2499239' EMAIL_ADDRESS = 'info@everywhere.sap'
        WEB_ADDRESS = 'http://www.everywhere.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'We are the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070025' NAME = 'Happy Holiday' STREET = 'Rastenburger Str. 12' POSTAL_CODE = '28779' CITY = 'Bremen' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 3266-288817' EMAIL_ADDRESS = 'info@haphol.sap'
        WEB_ADDRESS = 'http://www.haphol.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Fly with Happy Holiday! We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070026' NAME = 'No Name' STREET = 'Schwalbenweg 43' POSTAL_CODE = '52078' CITY = 'Aachen' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 241-77729' EMAIL_ADDRESS = 'info@nn.sap' WEB_ADDRESS = 'http://www.nn.sap' ATTACHMENT
= ''
        MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = '' ZZSLOGANZAG = 'Cool and save! We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070027' NAME = 'Fly Low' STREET = 'Chemnitzer Str. 42' POSTAL_CODE = '01187' CITY = 'Dresden' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 351-5423-00' EMAIL_ADDRESS = 'info@fly-low.sap' WEB_ADDRESS =
'http://www.fly-low.sap'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Better with the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070028' NAME = 'Aussie Travel' STREET = 'Queens Road' POSTAL_CODE = 'M8 7RYP' CITY = 'Manchester' COUNTRY_CODE = 'GB' PHONE_NUMBER = '+44 161 2052000' EMAIL_ADDRESS = 'info@down-under.sap'
        WEB_ADDRESS = 'http://www.down-under.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Fast and save! Aussie Travel is the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070029' NAME = 'Up ''n'' Away' STREET = 'Nackenbergerstr. 92' POSTAL_CODE = '30625' CITY = 'Hannover' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 511 403266-0' EMAIL_ADDRESS = 'info@una.sap' WEB_ADDRESS =
'http://www.una.sap'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Better with the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070030' NAME = 'Trans World Travel' STREET = '100 Industrial Drive' POSTAL_CODE = '60804' CITY = 'Chicago' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 708-454-8723' EMAIL_ADDRESS = 'info@twt.sap'
        WEB_ADDRESS = 'http://www.twt.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Fast and save! Trans World Travel is the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070031' NAME = 'Bright Side of Life' STREET = '340 State Street' POSTAL_CODE = '30432' CITY = 'San Francisco' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 415-454-9877' EMAIL_ADDRESS = 'info@ruebennase.sap'
        WEB_ADDRESS = 'http://www.ruebennase.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Fast and save! We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070032' NAME = 'Sunny, Sunny, Sunny' STREET = '1300 State Street' POSTAL_CODE = '19003' CITY = 'Philadelphia' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 215-090-7659' EMAIL_ADDRESS = 'info@s3.sap'
        WEB_ADDRESS = 'http://www.s3.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'We are the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070033' NAME = 'Fly & Smile' STREET = 'Zeppelinstr. 17' POSTAL_CODE = '60318' CITY = 'Frankfurt' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 69-99-0' EMAIL_ADDRESS = 'info@fly-and-smile.sap'
        WEB_ADDRESS = 'http://www.fly-and-smile.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Cool and smooth!'  )
        ( CLIENT = '080' AGENCY_ID = '070034' NAME = 'Supercheap' STREET = '1400, Washington Circle' POSTAL_CODE = '30439' CITY = 'Los Angeles' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 251-369-2510' EMAIL_ADDRESS = 'info@supercheap.sap'
        WEB_ADDRESS = 'http://www.supercheap.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Cool and save! We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070035' NAME = 'Hitchhiker' STREET = '21 Rue de Moselle' POSTAL_CODE = '92132' CITY = 'Issy-les-Moulineaux' COUNTRY_CODE = 'FR' PHONE_NUMBER = '+33 1-405-555-888' EMAIL_ADDRESS = 'info@42.sap'
        WEB_ADDRESS = 'http://www.42.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070036' NAME = 'Fly Now, Pay Later' STREET = '100 Madison' POSTAL_CODE = '11012' CITY = 'New York' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 512 343-8543' EMAIL_ADDRESS = 'info@fn-pl.sap'
        WEB_ADDRESS = 'http://www.fn-pl.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Fly with the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070037' NAME = 'Real Weird Vacation' STREET = '949 5th Street' POSTAL_CODE = 'V6T 1Z4' CITY = 'Vancouver' COUNTRY_CODE = 'CA' PHONE_NUMBER = '+1 604 827-8024' EMAIL_ADDRESS = 'info@reweva.sap'
        WEB_ADDRESS = 'http://www.reweva.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'We are the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070038' NAME = 'Cap Travels Ltd.' STREET = '10 Mandela St' POSTAL_CODE = '2128' CITY = 'Johannesburg' COUNTRY_CODE = 'ZA' PHONE_NUMBER = '+27 11 886-8981' EMAIL_ADDRESS = 'info@cap-travels.co.za'
        WEB_ADDRESS = 'http://www.cap-travels.co.za' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Better with Cap Travels Ltd.! We are the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070039' NAME = 'Rainy, Stormy, Cloudy' STREET = 'Lindenstr. 462' POSTAL_CODE = '70563' CITY = 'Stuttgart' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 711-7992-00' EMAIL_ADDRESS = 'info@windy.sap/rsc/'
        WEB_ADDRESS = 'http://www.windy.sap/rsc/' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Rainy, Stormy, Cloudy is the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070040' NAME = 'Women only' STREET = 'Kirchstr. 53' POSTAL_CODE = '55124' CITY = 'Mainz' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 6131-543-00' EMAIL_ADDRESS = 'info@women-only.sap'
        WEB_ADDRESS = 'http://www.women-only.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Fast and smooth! We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070041' NAME = 'Maxitrip' STREET = 'Flugfeld 17' POSTAL_CODE = '65128' CITY = 'Wiesbaden' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 611-55 66 77' EMAIL_ADDRESS = 'info@maxitrip.sap'
        WEB_ADDRESS = 'http://www.maxitrip.sap'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Better with us! We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070042' NAME = 'The Ultimate Answer' STREET = 'Manchester Rd 20' POSTAL_CODE = 'AB1 1SA' CITY = 'Avon' COUNTRY_CODE = 'GB' PHONE_NUMBER = '+44 934-66799' EMAIL_ADDRESS = 'info@thulan.sap'
        WEB_ADDRESS = 'http://www.thulan.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'The Ultimate Answer is the leader in business!'  )
        ( CLIENT = '080' AGENCY_ID = '070043' NAME = 'Intertravel' STREET = 'Michigan Ave' POSTAL_CODE = '60154' CITY = 'Chicago' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 788 798-6555' EMAIL_ADDRESS = 'info@intertravel.sap'
        WEB_ADDRESS = 'http://www.intertravel.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'We are the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070044' NAME = 'Ultimate Goal' STREET = '300 Peach tree street Sou' POSTAL_CODE = '01069' CITY = 'Atlanta' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 874-654-6686' EMAIL_ADDRESS = 'info@ultimate-goal.sap'
        WEB_ADDRESS = 'http://www.ultimate-goal.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Fly with Ultimate Goal!'  )
        ( CLIENT = '080' AGENCY_ID = '070045' NAME = '' STREET = '20890 East Central Ave' POSTAL_CODE = '30987' CITY = 'Palo Alto' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 652 645-5236' EMAIL_ADDRESS = 'info@sar.sap' WEB_ADDRESS = 'http://www.sar.sap'
        ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Cool and save! Fly with the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070046' NAME = 'Hendrik''s' STREET = '1200 Industrial Drive' POSTAL_CODE = '60153' CITY = 'Chicago' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 08-924-9884' EMAIL_ADDRESS = 'info@essen.sap/150596'
        WEB_ADDRESS = 'http://www.essen.sap/150596' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Fast and smooth!'  )
        ( CLIENT = '080' AGENCY_ID = '070047' NAME = 'All British Air Planes' STREET = '224 Tomato Lane' POSTAL_CODE = '08965' CITY = 'Vineland' COUNTRY_CODE = 'US' PHONE_NUMBER = '+44 609-896-Moore' EMAIL_ADDRESS = 'info@abap.sap'
        WEB_ADDRESS = 'http://www.abap.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 ' DUMMY_FIELD = ''
        ZZSLOGANZAG = 'Fast and save!'  )
        ( CLIENT = '080' AGENCY_ID = '070048' NAME = 'Rocky Horror Tours' STREET = '789 Santa Monica Blvd.' POSTAL_CODE = '08934' CITY = 'Santa Monica' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 64351-6455-654' EMAIL_ADDRESS = 'info@frank.furter.sap'
        WEB_ADDRESS = 'http://www.frank.furter.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Rocky Horror Tours is the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070049' NAME = 'Flights and More' STREET = '213 Park Blvd.' POSTAL_CODE = '35515' CITY = 'Los Walldos' COUNTRY_CODE = 'US' PHONE_NUMBER = '+1 646 555-6876' EMAIL_ADDRESS = 'info@fam.sap'
        WEB_ADDRESS = 'http://www.fam.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Flights and More is the best!'  )
        ( CLIENT = '080' AGENCY_ID = '070050' NAME = 'Not Only By Bike' STREET = 'Saalburgstr. 765' POSTAL_CODE = '60385' CITY = 'Frankfurt' COUNTRY_CODE = 'DE' PHONE_NUMBER = '+49 69 465789-0' EMAIL_ADDRESS = 'info@nobb.sap'
        WEB_ADDRESS = 'http://www.nobb.sap' ATTACHMENT = '' MIME_TYPE = '' FILENAME = '' LOCAL_CREATED_BY = '' LOCAL_CREATED_AT = '0.0000000 ' LOCAL_LAST_CHANGED_BY = '' LOCAL_LAST_CHANGED_AT = '0.0000000 ' LAST_CHANGED_AT = '0.0000000 '
        DUMMY_FIELD = '' ZZSLOGANZAG = 'Cool and save!'  )
     ).
    INSERT zagency FROM TABLE @it_agency.


*   Airport
    DATA it_airport TYPE TABLE OF zairport.
    DELETE FROM zairport.
    it_airport = VALUE #( ##NO_TEXT
        ( CLIENT = '080' AIRPORT_ID = 'FRA' NAME = 'Frankfurt Airport' CITY = 'Frankfurt/Main' COUNTRY = 'DE'  )
        ( CLIENT = '080' AIRPORT_ID = 'HAM' NAME = 'Hamburg Airport' CITY = 'Hamburg' COUNTRY = 'DE'  )
        ( CLIENT = '080' AIRPORT_ID = 'MUC' NAME = 'Munich Airport' CITY = 'Munich' COUNTRY = 'DE'  )
        ( CLIENT = '080' AIRPORT_ID = 'SXF' NAME = 'Berlin Schönefeld Airport' CITY = 'Berlin' COUNTRY = 'DE'  )
        ( CLIENT = '080' AIRPORT_ID = 'THF' NAME = 'Berlin Tempelhof Airport' CITY = 'Berlin' COUNTRY = 'DE'  )
        ( CLIENT = '080' AIRPORT_ID = 'TXL' NAME = 'Berlin Tegel Airport' CITY = 'Berlin' COUNTRY = 'DE'  )
        ( CLIENT = '080' AIRPORT_ID = 'CDG' NAME = 'Charles de Gaulle Airport' CITY = 'Paris' COUNTRY = 'FR'  )
        ( CLIENT = '080' AIRPORT_ID = 'ORY' NAME = 'Orly Airport' CITY = 'Paris' COUNTRY = 'FR'  )
        ( CLIENT = '080' AIRPORT_ID = 'VIE' NAME = 'Vienna International Airport' CITY = 'Vienna' COUNTRY = 'AT'  )
        ( CLIENT = '080' AIRPORT_ID = 'ZRH' NAME = 'Zürich Airport' CITY = 'Zurich' COUNTRY = 'CH'  )
        ( CLIENT = '080' AIRPORT_ID = 'RTM' NAME = 'Rotterdam The Hague Airport' CITY = 'Rotterdam' COUNTRY = 'NL'  )
        ( CLIENT = '080' AIRPORT_ID = 'FCO' NAME = 'Leonardo da Vinci–Fiumicino Airport' CITY = 'Rome' COUNTRY = 'IT'  )
        ( CLIENT = '080' AIRPORT_ID = 'VCE' NAME = 'Venice Marco Polo Airport' CITY = 'Venice' COUNTRY = 'IT'  )
        ( CLIENT = '080' AIRPORT_ID = 'LCY' NAME = 'London City Airport' CITY = 'London' COUNTRY = 'UK'  )
        ( CLIENT = '080' AIRPORT_ID = 'LGW' NAME = 'Gatwick Airport' CITY = 'London' COUNTRY = 'UK'  )
        ( CLIENT = '080' AIRPORT_ID = 'LHR' NAME = 'Heathrow Airport' CITY = 'London' COUNTRY = 'UK'  )
        ( CLIENT = '080' AIRPORT_ID = 'MAD' NAME = 'Adolfo Suárez Madrid–Barajas Airport' CITY = 'Madrid' COUNTRY = 'ES'  )
        ( CLIENT = '080' AIRPORT_ID = 'VKO' NAME = 'Vnukovo International Airport' CITY = 'Moscow' COUNTRY = 'RU'  )
        ( CLIENT = '080' AIRPORT_ID = 'SVO' NAME = 'Sheremetyevo International Airport' CITY = 'Moscow' COUNTRY = 'RU'  )
        ( CLIENT = '080' AIRPORT_ID = 'JFK' NAME = 'John F. Kennedy International Airport' CITY = 'New York City, New York' COUNTRY = 'US'  )
        ( CLIENT = '080' AIRPORT_ID = 'BNA' NAME = 'Nashville International Airport' CITY = 'Nashville, Tennessee' COUNTRY = 'US'  )
        ( CLIENT = '080' AIRPORT_ID = 'BOS' NAME = 'Logan International Airport' CITY = 'Boston, Massachusetts' COUNTRY = 'US'  )
        ( CLIENT = '080' AIRPORT_ID = 'ELP' NAME = 'El Paso International Airport' CITY = 'El Paso, Texas' COUNTRY = 'US'  )
        ( CLIENT = '080' AIRPORT_ID = 'DEN' NAME = 'Denver International Airport' CITY = 'Denver, Colorado' COUNTRY = 'US'  )
        ( CLIENT = '080' AIRPORT_ID = 'HOU' NAME = 'William P. Hobby Airport' CITY = 'Houston, Texas' COUNTRY = 'US'  )
        ( CLIENT = '080' AIRPORT_ID = 'LAS' NAME = 'McCarran International Airport' CITY = 'Las Vegas, Nevada' COUNTRY = 'US'  )
        ( CLIENT = '080' AIRPORT_ID = 'LAX' NAME = 'Los Angeles International Airport' CITY = 'Los Angeles, California' COUNTRY = 'US'  )
        ( CLIENT = '080' AIRPORT_ID = 'MCI' NAME = 'Kansas City International Airport' CITY = 'Kansas City, Missouri' COUNTRY = 'US'  )
        ( CLIENT = '080' AIRPORT_ID = 'MIA' NAME = 'Miami International Airport' CITY = 'Miami, Florida' COUNTRY = 'US'  )
        ( CLIENT = '080' AIRPORT_ID = 'SFO' NAME = 'San Francisco International Airport' CITY = 'San Francisco, California' COUNTRY = 'US'  )
        ( CLIENT = '080' AIRPORT_ID = 'EWR' NAME = 'Newark Liberty International Airport' CITY = 'Newark, New Jersey' COUNTRY = 'US'  )
        ( CLIENT = '080' AIRPORT_ID = 'YOW' NAME = 'Ottawa Macdonald–Cartier Int. Airport' CITY = 'Ottawa, Ontario' COUNTRY = 'CA'  )
        ( CLIENT = '080' AIRPORT_ID = 'ACA' NAME = 'General Juan N. Álvarez Int. Airport' CITY = 'Acapulco, Guerrero' COUNTRY = 'MX'  )
        ( CLIENT = '080' AIRPORT_ID = 'GIG' NAME = 'Rio de Janeiro–Galeão Int. Airport' CITY = 'Rio de Janeiro' COUNTRY = 'BR'  )
        ( CLIENT = '080' AIRPORT_ID = 'HAV' NAME = 'José Martí International Airport' CITY = 'Havana' COUNTRY = 'CU'  )
        ( CLIENT = '080' AIRPORT_ID = 'ASP' NAME = 'Alice Springs Airport' CITY = 'Alice Springs, Northern Territory' COUNTRY = 'AU'  )
        ( CLIENT = '080' AIRPORT_ID = 'ACE' NAME = 'Lanzarote Airport' CITY = 'Lanzarote, Canary Islands' COUNTRY = 'ES'  )
        ( CLIENT = '080' AIRPORT_ID = 'HRE' NAME = 'Harare International Airport' CITY = 'Harare' COUNTRY = 'ZW'  )
        ( CLIENT = '080' AIRPORT_ID = 'GCJ' NAME = 'Grand Central Airport' CITY = 'Johannesburg' COUNTRY = 'SA'  )
        ( CLIENT = '080' AIRPORT_ID = 'NRT' NAME = 'Narita International Airport' CITY = 'Tokyo, Honshu' COUNTRY = 'JP'  )
        ( CLIENT = '080' AIRPORT_ID = 'ITM' NAME = 'Osaka International Airport' CITY = 'Osaka, Honshu' COUNTRY = 'JP'  )
        ( CLIENT = '080' AIRPORT_ID = 'KIX' NAME = 'Kansai International Airport' CITY = 'Osaka, Honshu' COUNTRY = 'JP'  )
        ( CLIENT = '080' AIRPORT_ID = 'HIJ' NAME = 'Hiroshima Airport' CITY = 'Hiroshima, Honshu' COUNTRY = 'JP'  )
        ( CLIENT = '080' AIRPORT_ID = 'SIN' NAME = 'Singapore Changi Airport' CITY = 'Singapore' COUNTRY = 'SG'  )
        ( CLIENT = '080' AIRPORT_ID = 'KUL' NAME = 'Kuala Lumpur International Airport' CITY = 'Kuala Lumpur' COUNTRY = 'MY'  )
        ( CLIENT = '080' AIRPORT_ID = 'HKG' NAME = 'Hong Kong International Airport' CITY = 'Hongkong' COUNTRY = 'CN'  )
        ( CLIENT = '080' AIRPORT_ID = 'BKK' NAME = 'Suvarnabhumi Airport' CITY = 'Bangkok' COUNTRY = 'TH'  )
    ).
    INSERT zairport FROM TABLE @it_airport.

*   Carriers
    DATA it_carrier TYPE TABLE OF zcarrier.
    DELETE FROM zcarrier.
    it_carrier = VALUE #( ##NO_TEXT
        local_created_by      = 'GENERATOR'
        local_last_changed_by = 'GENERATOR'
        local_created_at      = current_timestamp
        local_last_changed_at = current_timestamp
        last_changed_at       = current_timestamp
        (   carrier_id = 'AA'  name = 'American Airlines Inc.'                  currency_code = 'USD'  )
        (   carrier_id = 'AC'  name = 'Air Canada'                              currency_code = 'CAD'  )
        (   carrier_id = 'AF'  name = 'Air France'                              currency_code = 'EUR'  )
        (   carrier_id = 'AZ'  name = 'Alitalia Societa Aerea Italiana S.p.A.'  currency_code = 'EUR'  )
        (   carrier_id = 'BA'  name = 'British Airways p.l.c.'                  currency_code = 'GBP'  )
        (   carrier_id = 'FJ'  name = 'Air Pacific Limited t/a Fiji Airway'     currency_code = 'USD'  )
        (   carrier_id = 'CO'  name = 'Cobaltair Ltd. dba Cobalt'               currency_code = 'USD'  )
        (   carrier_id = 'DL'  name = 'Delta Air Lines, Inc.'                   currency_code = 'USD'  )
        (   carrier_id = 'LH'  name = 'Deutsche Lufthansa AG'                   currency_code = 'EUR'  )
        (   carrier_id = 'NG'  name = 'AL-Naser Wings'                          currency_code = 'EUR'  )
        (   carrier_id = 'JL'  name = 'Japan Airlines Co., Ltd.'                currency_code = 'JPY'  )
        (   carrier_id = 'QF'  name = 'Qantas Airways Ltd.'                     currency_code = 'AUD'  )
        (   carrier_id = 'SA'  name = 'South African Airways'                   currency_code = 'ZAR'  )
        (   carrier_id = 'SQ'  name = 'Singapore Airlines Limited'              currency_code = 'SGD'  )
        (   carrier_id = 'SR'  name = 'Sundair GmbH'                            currency_code = 'CHF'  )
        (   carrier_id = 'UA'  name = 'United Airlines, Inc.'                   currency_code = 'USD'  )
    ).
    INSERT zcarrier FROM TABLE @it_carrier.

*   Connection
    DATA it_connection TYPE TABLE OF zconnection.
    DELETE FROM zconnection.
    it_connection = VALUE #( ##NO_TEXT
        ( CLIENT = '080' CARRIER_ID = 'SQ' CONNECTION_ID = '0001' AIRPORT_FROM_ID = 'SFO' AIRPORT_TO_ID = 'SIN' DEPARTURE_TIME = '011500' ARRIVAL_TIME = '115000' DISTANCE = '13523' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'SQ' CONNECTION_ID = '0002' AIRPORT_FROM_ID = 'SIN' AIRPORT_TO_ID = 'SFO' DEPARTURE_TIME = '063000' ARRIVAL_TIME = '091500' DISTANCE = '13523' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'SQ' CONNECTION_ID = '0011' AIRPORT_FROM_ID = 'NRT' AIRPORT_TO_ID = 'SIN' DEPARTURE_TIME = '145500' ARRIVAL_TIME = '205000' DISTANCE = '5363' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'SQ' CONNECTION_ID = '0012' AIRPORT_FROM_ID = 'SIN' AIRPORT_TO_ID = 'NRT' DEPARTURE_TIME = '095300' ARRIVAL_TIME = '175400' DISTANCE = '5363' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'UA' CONNECTION_ID = '0058' AIRPORT_FROM_ID = 'SFO' AIRPORT_TO_ID = 'FRA' DEPARTURE_TIME = '134500' ARRIVAL_TIME = '095500' DISTANCE = '9608' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'UA' CONNECTION_ID = '0059' AIRPORT_FROM_ID = 'FRA' AIRPORT_TO_ID = 'SFO' DEPARTURE_TIME = '135500' ARRIVAL_TIME = '163000' DISTANCE = '9608' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'UA' CONNECTION_ID = '1537' AIRPORT_FROM_ID = 'EWR' AIRPORT_TO_ID = 'MIA' DEPARTURE_TIME = '215600' ARRIVAL_TIME = '004700' DISTANCE = '1752' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '0322' AIRPORT_FROM_ID = 'MIA' AIRPORT_TO_ID = 'EWR' DEPARTURE_TIME = '201700' ARRIVAL_TIME = '231900' DISTANCE = '1752' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '0017' AIRPORT_FROM_ID = 'MIA' AIRPORT_TO_ID = 'HAV' DEPARTURE_TIME = '071900' ARRIVAL_TIME = '080300' DISTANCE = '520' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '2678' AIRPORT_FROM_ID = 'HAV' AIRPORT_TO_ID = 'MIA' DEPARTURE_TIME = '061500' ARRIVAL_TIME = '103000' DISTANCE = '520' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '0015' AIRPORT_FROM_ID = 'JFK' AIRPORT_TO_ID = 'SFO' DEPARTURE_TIME = '071300' ARRIVAL_TIME = '100400' DISTANCE = '4156' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '0018' AIRPORT_FROM_ID = 'SFO' AIRPORT_TO_ID = 'JFK' DEPARTURE_TIME = '064000' ARRIVAL_TIME = '150600' DISTANCE = '4156' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'LH' CONNECTION_ID = '0400' AIRPORT_FROM_ID = 'FRA' AIRPORT_TO_ID = 'JFK' DEPARTURE_TIME = '101000' ARRIVAL_TIME = '113400' DISTANCE = '6162' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'LH' CONNECTION_ID = '0401' AIRPORT_FROM_ID = 'JFK' AIRPORT_TO_ID = 'FRA' DEPARTURE_TIME = '183000' ARRIVAL_TIME = '074500' DISTANCE = '6162' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'LH' CONNECTION_ID = '0402' AIRPORT_FROM_ID = 'FRA' AIRPORT_TO_ID = 'EWR' DEPARTURE_TIME = '133000' ARRIVAL_TIME = '153500' DISTANCE = '6217' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'LH' CONNECTION_ID = '0403' AIRPORT_FROM_ID = 'EWR' AIRPORT_TO_ID = 'FRA' DEPARTURE_TIME = '180900' ARRIVAL_TIME = '073000' DISTANCE = '6217' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'JL' CONNECTION_ID = '0407' AIRPORT_FROM_ID = 'NRT' AIRPORT_TO_ID = 'FRA' DEPARTURE_TIME = '132300' ARRIVAL_TIME = '155600' DISTANCE = '9379' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'JL' CONNECTION_ID = '0408' AIRPORT_FROM_ID = 'FRA' AIRPORT_TO_ID = 'NRT' DEPARTURE_TIME = '202500' ARRIVAL_TIME = '154000' DISTANCE = '9379' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'AZ' CONNECTION_ID = '0788' AIRPORT_FROM_ID = 'VCE' AIRPORT_TO_ID = 'NRT' DEPARTURE_TIME = '132500' ARRIVAL_TIME = '101300' DISTANCE = '9595' DISTANCE_UNIT = 'KM'  )
        ( CLIENT = '080' CARRIER_ID = 'AZ' CONNECTION_ID = '0789' AIRPORT_FROM_ID = 'NRT' AIRPORT_TO_ID = 'VCE' DEPARTURE_TIME = '142600' ARRIVAL_TIME = '213100' DISTANCE = '9595' DISTANCE_UNIT = 'KM'  )
    ).
    INSERT zconnection FROM TABLE @it_connection.

*   Flight
    DATA it_flight TYPE TABLE OF zflight.
    DELETE FROM zflight.
    it_flight = VALUE #( ##NO_TEXT
        ( CLIENT = '080' CARRIER_ID = 'SQ' CONNECTION_ID = '0001' FLIGHT_DATE = '20241120' PRICE = '10818.00 ' CURRENCY_CODE = 'SGD' PLANE_TYPE_ID = '767-200' SEATS_MAX = '260' SEATS_OCCUPIED = '223'  )
        ( CLIENT = '080' CARRIER_ID = 'SQ' CONNECTION_ID = '0001' FLIGHT_DATE = '20240125' PRICE = '5950.00 ' CURRENCY_CODE = 'SGD' PLANE_TYPE_ID = 'A340-600' SEATS_MAX = '330' SEATS_OCCUPIED = '168'  )
        ( CLIENT = '080' CARRIER_ID = 'SQ' CONNECTION_ID = '0002' FLIGHT_DATE = '20241121' PRICE = '11765.00 ' CURRENCY_CODE = 'SGD' PLANE_TYPE_ID = '747-400' SEATS_MAX = '385' SEATS_OCCUPIED = '350'  )
        ( CLIENT = '080' CARRIER_ID = 'SQ' CONNECTION_ID = '0002' FLIGHT_DATE = '20240126' PRICE = '10953.00 ' CURRENCY_CODE = 'SGD' PLANE_TYPE_ID = '747-400' SEATS_MAX = '385' SEATS_OCCUPIED = '334'  )
        ( CLIENT = '080' CARRIER_ID = 'SQ' CONNECTION_ID = '0011' FLIGHT_DATE = '20241121' PRICE = '2359.00 ' CURRENCY_CODE = 'SGD' PLANE_TYPE_ID = '767-200' SEATS_MAX = '260' SEATS_OCCUPIED = '132'  )
        ( CLIENT = '080' CARRIER_ID = 'SQ' CONNECTION_ID = '0011' FLIGHT_DATE = '20240126' PRICE = '4880.00 ' CURRENCY_CODE = 'SGD' PLANE_TYPE_ID = 'A340-600' SEATS_MAX = '330' SEATS_OCCUPIED = '310'  )
        ( CLIENT = '080' CARRIER_ID = 'SQ' CONNECTION_ID = '0012' FLIGHT_DATE = '20241123' PRICE = '4665.00 ' CURRENCY_CODE = 'SGD' PLANE_TYPE_ID = '767-200' SEATS_MAX = '260' SEATS_OCCUPIED = '236'  )
        ( CLIENT = '080' CARRIER_ID = 'SQ' CONNECTION_ID = '0012' FLIGHT_DATE = '20240128' PRICE = '2574.00 ' CURRENCY_CODE = 'SGD' PLANE_TYPE_ID = '747-400' SEATS_MAX = '385' SEATS_OCCUPIED = '215'  )
        ( CLIENT = '080' CARRIER_ID = 'UA' CONNECTION_ID = '0058' FLIGHT_DATE = '20241118' PRICE = '6629.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = '767-200' SEATS_MAX = '260' SEATS_OCCUPIED = '200'  )
        ( CLIENT = '080' CARRIER_ID = 'UA' CONNECTION_ID = '0058' FLIGHT_DATE = '20240123' PRICE = '4996.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = '747-400' SEATS_MAX = '385' SEATS_OCCUPIED = '231'  )
        ( CLIENT = '080' CARRIER_ID = 'UA' CONNECTION_ID = '0059' FLIGHT_DATE = '20241119' PRICE = '4131.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = 'A340-600' SEATS_MAX = '330' SEATS_OCCUPIED = '161'  )
        ( CLIENT = '080' CARRIER_ID = 'UA' CONNECTION_ID = '0059' FLIGHT_DATE = '20240124' PRICE = '6053.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = 'A340-600' SEATS_MAX = '330' SEATS_OCCUPIED = '237'  )
        ( CLIENT = '080' CARRIER_ID = 'UA' CONNECTION_ID = '1537' FLIGHT_DATE = '20241122' PRICE = '893.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = 'A321-200' SEATS_MAX = '150' SEATS_OCCUPIED = '88'  )
        ( CLIENT = '080' CARRIER_ID = 'UA' CONNECTION_ID = '1537' FLIGHT_DATE = '20240127' PRICE = '805.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = '737-800' SEATS_MAX = '140' SEATS_OCCUPIED = '75'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '0322' FLIGHT_DATE = '20241124' PRICE = '1103.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = 'A320-200' SEATS_MAX = '130' SEATS_OCCUPIED = '93'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '0322' FLIGHT_DATE = '20240129' PRICE = '1611.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = 'A320-200' SEATS_MAX = '130' SEATS_OCCUPIED = '123'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '0017' FLIGHT_DATE = '20241120' PRICE = '462.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = 'A321-200' SEATS_MAX = '150' SEATS_OCCUPIED = '139'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '0017' FLIGHT_DATE = '20240125' PRICE = '478.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = '737-800' SEATS_MAX = '140' SEATS_OCCUPIED = '133'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '2678' FLIGHT_DATE = '20241123' PRICE = '473.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = 'A321-200' SEATS_MAX = '150' SEATS_OCCUPIED = '141'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '2678' FLIGHT_DATE = '20240128' PRICE = '473.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = 'A320-200' SEATS_MAX = '130' SEATS_OCCUPIED = '122'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '0015' FLIGHT_DATE = '20241122' PRICE = '1911.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = '767-200' SEATS_MAX = '260' SEATS_OCCUPIED = '137'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '0015' FLIGHT_DATE = '20240127' PRICE = '3117.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = '767-200' SEATS_MAX = '260' SEATS_OCCUPIED = '213'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '0018' FLIGHT_DATE = '20241121' PRICE = '3781.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = 'A380-800' SEATS_MAX = '475' SEATS_OCCUPIED = '446'  )
        ( CLIENT = '080' CARRIER_ID = 'AA' CONNECTION_ID = '0018' FLIGHT_DATE = '20240126' PRICE = '3823.00 ' CURRENCY_CODE = 'USD' PLANE_TYPE_ID = '767-200' SEATS_MAX = '260' SEATS_OCCUPIED = '247'  )
        ( CLIENT = '080' CARRIER_ID = 'LH' CONNECTION_ID = '0400' FLIGHT_DATE = '20241123' PRICE = '5484.00 ' CURRENCY_CODE = 'EUR' PLANE_TYPE_ID = 'A340-600' SEATS_MAX = '330' SEATS_OCCUPIED = '306'  )
        ( CLIENT = '080' CARRIER_ID = 'LH' CONNECTION_ID = '0400' FLIGHT_DATE = '20240128' PRICE = '2649.00 ' CURRENCY_CODE = 'EUR' PLANE_TYPE_ID = '767-200' SEATS_MAX = '260' SEATS_OCCUPIED = '130'  )
        ( CLIENT = '080' CARRIER_ID = 'LH' CONNECTION_ID = '0401' FLIGHT_DATE = '20241122' PRICE = '3697.00 ' CURRENCY_CODE = 'EUR' PLANE_TYPE_ID = '747-400' SEATS_MAX = '385' SEATS_OCCUPIED = '265'  )
        ( CLIENT = '080' CARRIER_ID = 'LH' CONNECTION_ID = '0401' FLIGHT_DATE = '20240127' PRICE = '4867.00 ' CURRENCY_CODE = 'EUR' PLANE_TYPE_ID = 'A380-800' SEATS_MAX = '475' SEATS_OCCUPIED = '403'  )
        ( CLIENT = '080' CARRIER_ID = 'LH' CONNECTION_ID = '0402' FLIGHT_DATE = '20241118' PRICE = '4911.00 ' CURRENCY_CODE = 'EUR' PLANE_TYPE_ID = '767-200' SEATS_MAX = '260' SEATS_OCCUPIED = '221'  )
        ( CLIENT = '080' CARRIER_ID = 'LH' CONNECTION_ID = '0402' FLIGHT_DATE = '20240123' PRICE = '3232.00 ' CURRENCY_CODE = 'EUR' PLANE_TYPE_ID = '747-400' SEATS_MAX = '385' SEATS_OCCUPIED = '231'  )
        ( CLIENT = '080' CARRIER_ID = 'LH' CONNECTION_ID = '0403' FLIGHT_DATE = '20241118' PRICE = '2797.00 ' CURRENCY_CODE = 'EUR' PLANE_TYPE_ID = 'A340-600' SEATS_MAX = '330' SEATS_OCCUPIED = '171'  )
        ( CLIENT = '080' CARRIER_ID = 'LH' CONNECTION_ID = '0403' FLIGHT_DATE = '20240123' PRICE = '2486.00 ' CURRENCY_CODE = 'EUR' PLANE_TYPE_ID = '767-200' SEATS_MAX = '260' SEATS_OCCUPIED = '117'  )
        ( CLIENT = '080' CARRIER_ID = 'JL' CONNECTION_ID = '0407' FLIGHT_DATE = '20241122' PRICE = '5346.00 ' CURRENCY_CODE = 'JPY' PLANE_TYPE_ID = '747-400' SEATS_MAX = '385' SEATS_OCCUPIED = '254'  )
        ( CLIENT = '080' CARRIER_ID = 'JL' CONNECTION_ID = '0407' FLIGHT_DATE = '20240127' PRICE = '4032.00 ' CURRENCY_CODE = 'JPY' PLANE_TYPE_ID = 'A340-600' SEATS_MAX = '330' SEATS_OCCUPIED = '165'  )
        ( CLIENT = '080' CARRIER_ID = 'JL' CONNECTION_ID = '0408' FLIGHT_DATE = '20241123' PRICE = '8159.00 ' CURRENCY_CODE = 'JPY' PLANE_TYPE_ID = 'A380-800' SEATS_MAX = '475' SEATS_OCCUPIED = '432'  )
        ( CLIENT = '080' CARRIER_ID = 'JL' CONNECTION_ID = '0408' FLIGHT_DATE = '20240128' PRICE = '6471.00 ' CURRENCY_CODE = 'JPY' PLANE_TYPE_ID = '747-400' SEATS_MAX = '385' SEATS_OCCUPIED = '296'  )
        ( CLIENT = '080' CARRIER_ID = 'AZ' CONNECTION_ID = '0788' FLIGHT_DATE = '20241123' PRICE = '7580.00 ' CURRENCY_CODE = 'EUR' PLANE_TYPE_ID = '767-200' SEATS_MAX = '260' SEATS_OCCUPIED = '221'  )
        ( CLIENT = '080' CARRIER_ID = 'AZ' CONNECTION_ID = '0788' FLIGHT_DATE = '20240128' PRICE = '8059.00 ' CURRENCY_CODE = 'EUR' PLANE_TYPE_ID = 'A380-800' SEATS_MAX = '475' SEATS_OCCUPIED = '422'  )
        ( CLIENT = '080' CARRIER_ID = 'AZ' CONNECTION_ID = '0789' FLIGHT_DATE = '20241122' PRICE = '8539.00 ' CURRENCY_CODE = 'EUR' PLANE_TYPE_ID = 'A380-800' SEATS_MAX = '475' SEATS_OCCUPIED = '441'  )
        ( CLIENT = '080' CARRIER_ID = 'AZ' CONNECTION_ID = '0789' FLIGHT_DATE = '20240127' PRICE = '5852.00 ' CURRENCY_CODE = 'EUR' PLANE_TYPE_ID = 'A380-800' SEATS_MAX = '475' SEATS_OCCUPIED = '332'  )
    ).
    INSERT zflight FROM TABLE @it_flight.


*   Flight
    DATA it_travel TYPE TABLE OF ztravel.
    DELETE FROM ztravel.
    it_travel = VALUE #( ##NO_TEXT
        ( CLIENT = '080' TRAVEL_ID = '00000001' AGENCY_ID = '070041' CUSTOMER_ID = '000594' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '1889.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Pan' CREATEDAT = '20231226035450.0000000 ' LASTCHANGEDBY = 'Barth' LASTCHANGEDAT = '20240112194808.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000002' AGENCY_ID = '070007' CUSTOMER_ID = '000608' BEGIN_DATE = '20240127' END_DATE = '20240127' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '900.00 ' CURRENCY_CODE = 'USD' DESCRIPTION =
'Business Trip for Christine, Pierre'
        STATUS = 'N' CREATEDBY = 'Meier' CREATEDAT = '20240110205413.0000000 ' LASTCHANGEDBY = 'Lindwurm' LASTCHANGEDAT = '20240112210758.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000003' AGENCY_ID = '070046' CUSTOMER_ID = '000093' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '80.00 ' TOTAL_PRICE = '4164.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Detemple' CREATEDAT = '20231223230532.0000000 ' LASTCHANGEDBY = 'Mustermann' LASTCHANGEDAT = '20240111033718.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000004' AGENCY_ID = '070042' CUSTOMER_ID = '000665' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '1871.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Rahn' CREATEDAT = '20240106053259.0000000 ' LASTCHANGEDBY = 'Hoffen' LASTCHANGEDAT = '20240123211611.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000005' AGENCY_ID = '070007' CUSTOMER_ID = '000225' BEGIN_DATE = '20240127' END_DATE = '20240127' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '992.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip for Kurt, Ida'
        STATUS = 'N' CREATEDBY = 'Sessler' CREATEDAT = '20240109075758.0000000 ' LASTCHANGEDBY = 'Heller' LASTCHANGEDAT = '20240111155759.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000006' AGENCY_ID = '070049' CUSTOMER_ID = '000072' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '120.00 ' TOTAL_PRICE = '5586.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Neubasler' CREATEDAT = '20240101135858.0000000 ' LASTCHANGEDBY = 'Illner' LASTCHANGEDAT = '20240121020943.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000007' AGENCY_ID = '070046' CUSTOMER_ID = '000138' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '120.00 ' TOTAL_PRICE = '5691.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Mechler' CREATEDAT = '20231223042947.0000000 ' LASTCHANGEDBY = 'Koslowski' LASTCHANGEDAT = '20240111065706.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000008' AGENCY_ID = '070012' CUSTOMER_ID = '000705' BEGIN_DATE = '20240127' END_DATE = '20240129' BOOKING_FEE = '60.00 ' TOTAL_PRICE = '2777.00 ' CURRENCY_CODE = 'USD' DESCRIPTION =
'Vacation for Lothar, Ulla, Thilo,'
        STATUS = 'N' CREATEDBY = 'Illner' CREATEDAT = '20231223230800.0000000 ' LASTCHANGEDBY = 'Fischer' LASTCHANGEDAT = '20231227134817.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000009' AGENCY_ID = '070032' CUSTOMER_ID = '000115' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '120.00 ' TOTAL_PRICE = '5792.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Visiting Anna' STATUS = 'B'
        CREATEDBY = 'Mustermann' CREATEDAT = '20231223235438.0000000 ' LASTCHANGEDBY = 'Fischer' LASTCHANGEDAT = '20240105222506.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000010' AGENCY_ID = '070016' CUSTOMER_ID = '000697' BEGIN_DATE = '20240127' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '959.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip to USA' STATUS =
'N'
        CREATEDBY = 'Mustermann' CREATEDAT = '20231227235730.0000000 ' LASTCHANGEDBY = 'Mustermann' LASTCHANGEDAT = '20240102234752.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000011' AGENCY_ID = '070005' CUSTOMER_ID = '000582' BEGIN_DATE = '20240127' END_DATE = '20240127' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '959.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'N'
        CREATEDBY = 'Benz' CREATEDAT = '20240110170221.0000000 ' LASTCHANGEDBY = 'Gueldenpfenn' LASTCHANGEDAT = '20240112033947.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000012' AGENCY_ID = '070028' CUSTOMER_ID = '000583' BEGIN_DATE = '20240127' END_DATE = '20241122' BOOKING_FEE = '90.00 ' TOTAL_PRICE = '4304.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Sightseeing in Miami, Florida'
        STATUS = 'B' CREATEDBY = 'Mustermann' CREATEDAT = '20240112235640.0000000 ' LASTCHANGEDBY = 'Becker' LASTCHANGEDAT = '20240123235012.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000013' AGENCY_ID = '070014' CUSTOMER_ID = '000004' BEGIN_DATE = '20240127' END_DATE = '20240129' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '2014.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation for Thomas, Jasmin,'
        STATUS = 'N' CREATEDBY = 'Mustermann' CREATEDAT = '20231224024544.0000000 ' LASTCHANGEDBY = 'Martin' LASTCHANGEDAT = '20231229224023.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000014' AGENCY_ID = '070010' CUSTOMER_ID = '000667' BEGIN_DATE = '20240127' END_DATE = '20240127' BOOKING_FEE = '10.00 ' TOTAL_PRICE = '544.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip for Anna' STATUS =
'N'
        CREATEDBY = 'Matthaeus' CREATEDAT = '20231227192325.0000000 ' LASTCHANGEDBY = 'Deichgraeber' LASTCHANGEDAT = '20231230114428.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000015' AGENCY_ID = '070028' CUSTOMER_ID = '000466' BEGIN_DATE = '20240127' END_DATE = '20241122' BOOKING_FEE = '60.00 ' TOTAL_PRICE = '3001.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Sightseeing in Miami, Florida'
        STATUS = 'B' CREATEDBY = 'Deichgraeber' CREATEDAT = '20231225071018.0000000 ' LASTCHANGEDBY = 'Buehler' LASTCHANGEDAT = '20240105193910.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000016' AGENCY_ID = '070050' CUSTOMER_ID = '000640' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '120.00 ' TOTAL_PRICE = '5937.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Barth' CREATEDAT = '20231230000118.0000000 ' LASTCHANGEDBY = 'Buchholm' LASTCHANGEDAT = '20240119034247.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000017' AGENCY_ID = '070048' CUSTOMER_ID = '000478' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '120.00 ' TOTAL_PRICE = '6087.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Delon' CREATEDAT = '20231226020208.0000000 ' LASTCHANGEDBY = 'Martin' LASTCHANGEDAT = '20240115101652.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000018' AGENCY_ID = '070050' CUSTOMER_ID = '000264' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '80.00 ' TOTAL_PRICE = '4040.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Rahn' CREATEDAT = '20231229154949.0000000 ' LASTCHANGEDBY = 'Montero' LASTCHANGEDAT = '20240118210559.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000019' AGENCY_ID = '070049' CUSTOMER_ID = '000155' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '2042.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Prinz' CREATEDAT = '20231228211945.0000000 ' LASTCHANGEDBY = 'Sisko' LASTCHANGEDAT = '20240117194107.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000020' AGENCY_ID = '070049' CUSTOMER_ID = '000680' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '120.00 ' TOTAL_PRICE = '6272.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Mustermann' CREATEDAT = '20231230120239.0000000 ' LASTCHANGEDBY = 'Babilon' LASTCHANGEDAT = '20240119235729.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000021' AGENCY_ID = '070008' CUSTOMER_ID = '000552' BEGIN_DATE = '20240127' END_DATE = '20240127' BOOKING_FEE = '10.00 ' TOTAL_PRICE = '586.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip for Kristina'
STATUS = 'N'
        CREATEDBY = 'Domenech' CREATEDAT = '20231229192212.0000000 ' LASTCHANGEDBY = 'Deichgraeber' LASTCHANGEDAT = '20240101152607.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000022' AGENCY_ID = '070037' CUSTOMER_ID = '000540' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '2255.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'B'
        CREATEDBY = 'Jeremias' CREATEDAT = '20231227084702.0000000 ' LASTCHANGEDBY = 'Marshall' LASTCHANGEDAT = '20240111004503.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000023' AGENCY_ID = '070049' CUSTOMER_ID = '000542' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '2365.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Kirk' CREATEDAT = '20231225052927.0000000 ' LASTCHANGEDBY = 'Koller' LASTCHANGEDAT = '20240114201124.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000024' AGENCY_ID = '070050' CUSTOMER_ID = '000346' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '120.00 ' TOTAL_PRICE = '6610.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Leisert' CREATEDAT = '20231223163847.0000000 ' LASTCHANGEDBY = 'Sommer' LASTCHANGEDAT = '20240112222629.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000025' AGENCY_ID = '070048' CUSTOMER_ID = '000478' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '2213.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Moyano' CREATEDAT = '20240110094548.0000000 ' LASTCHANGEDBY = 'D´Oultrement' LASTCHANGEDAT = '20240130164255.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000026' AGENCY_ID = '070006' CUSTOMER_ID = '000220' BEGIN_DATE = '20240127' END_DATE = '20240127' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1245.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip for Cindy, Ulla'
        STATUS = 'N' CREATEDBY = 'Lautenbach' CREATEDAT = '20240102174000.0000000 ' LASTCHANGEDBY = 'Sudhoff' LASTCHANGEDAT = '20240104181642.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000027' AGENCY_ID = '070025' CUSTOMER_ID = '000515' BEGIN_DATE = '20240127' END_DATE = '20241122' BOOKING_FEE = '60.00 ' TOTAL_PRICE = '3544.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation to USA' STATUS = 'N'
        CREATEDBY = 'Sudhoff' CREATEDAT = '20240112110938.0000000 ' LASTCHANGEDBY = 'Cesari' LASTCHANGEDAT = '20240122173942.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000028' AGENCY_ID = '070040' CUSTOMER_ID = '000024' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '80.00 ' TOTAL_PRICE = '4524.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'P'
        CREATEDBY = 'D´Oultrement' CREATEDAT = '20240110102946.0000000 ' LASTCHANGEDBY = 'Delon' LASTCHANGEDAT = '20240126090727.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000029' AGENCY_ID = '070041' CUSTOMER_ID = '000202' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '2419.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Domenech' CREATEDAT = '20240111042052.0000000 ' LASTCHANGEDBY = 'Hansmann' LASTCHANGEDAT = '20240127155734.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000030' AGENCY_ID = '070015' CUSTOMER_ID = '000636' BEGIN_DATE = '20240127' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1285.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation for Cindy,' STATUS =
'N'
        CREATEDBY = 'Hansmann' CREATEDAT = '20240104071301.0000000 ' LASTCHANGEDBY = 'Buehler' LASTCHANGEDAT = '20240110043548.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000031' AGENCY_ID = '070008' CUSTOMER_ID = '000071' BEGIN_DATE = '20240127' END_DATE = '20240127' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1348.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip for Andrej, Anna'
        STATUS = 'N' CREATEDBY = 'Schneider' CREATEDAT = '20240103191549.0000000 ' LASTCHANGEDBY = 'Deichgraeber' LASTCHANGEDAT = '20240105164549.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000032' AGENCY_ID = '070001' CUSTOMER_ID = '000599' BEGIN_DATE = '20240127' END_DATE = '20240127' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1406.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'N'
        CREATEDBY = 'Jeremias' CREATEDAT = '20240103074124.0000000 ' LASTCHANGEDBY = 'Heller' LASTCHANGEDAT = '20240103003056.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000033' AGENCY_ID = '070022' CUSTOMER_ID = '000506' BEGIN_DATE = '20240127' END_DATE = '20241122' BOOKING_FEE = '90.00 ' TOTAL_PRICE = '5624.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation to USA' STATUS = 'N'
        CREATEDBY = 'Mechler' CREATEDAT = '20240102205734.0000000 ' LASTCHANGEDBY = 'Sisko' LASTCHANGEDAT = '20240110065325.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000034' AGENCY_ID = '070006' CUSTOMER_ID = '000358' BEGIN_DATE = '20240127' END_DATE = '20240127' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1465.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip for Ulla, Fabio'
        STATUS = 'N' CREATEDBY = 'Gutenberg' CREATEDAT = '20231229030845.0000000 ' LASTCHANGEDBY = 'Benz' LASTCHANGEDAT = '20231231013209.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000035' AGENCY_ID = '070046' CUSTOMER_ID = '000026' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '80.00 ' TOTAL_PRICE = '5025.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Neubasler' CREATEDAT = '20231224231548.0000000 ' LASTCHANGEDBY = 'Fischer' LASTCHANGEDAT = '20240112020813.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000036' AGENCY_ID = '070032' CUSTOMER_ID = '000320' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '80.00 ' TOTAL_PRICE = '5173.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Visiting Simon' STATUS = 'B'
        CREATEDBY = 'Pan' CREATEDAT = '20231229191519.0000000 ' LASTCHANGEDBY = 'Pan' LASTCHANGEDAT = '20240111195059.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000037' AGENCY_ID = '070040' CUSTOMER_ID = '000154' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '2654.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'P'
        CREATEDBY = 'Sommer' CREATEDAT = '20231229031400.0000000 ' LASTCHANGEDBY = 'Detemple' LASTCHANGEDAT = '20240114164817.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000038' AGENCY_ID = '070044' CUSTOMER_ID = '000558' BEGIN_DATE = '20240127' END_DATE = '20241124' BOOKING_FEE = '120.00 ' TOTAL_PRICE = '8008.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Heller' CREATEDAT = '20240103182054.0000000 ' LASTCHANGEDBY = 'Lautenbach' LASTCHANGEDAT = '20240121075513.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000039' AGENCY_ID = '070048' CUSTOMER_ID = '000027' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '60.00 ' TOTAL_PRICE = '3601.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Müller' CREATEDAT = '20240325111557.0000000 ' LASTCHANGEDBY = 'Matthaeus' LASTCHANGEDAT = '20240414221150.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000040' AGENCY_ID = '070026' CUSTOMER_ID = '000357' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1324.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Sightseeing in Miami, Florida'
        STATUS = 'B' CREATEDBY = 'Jeremias' CREATEDAT = '20240325152859.0000000 ' LASTCHANGEDBY = 'Domenech' LASTCHANGEDAT = '20240404003646.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000041' AGENCY_ID = '070033' CUSTOMER_ID = '000334' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '60.00 ' TOTAL_PRICE = '3800.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Visiting Anna' STATUS = 'B'
        CREATEDBY = 'Kreiss' CREATEDAT = '20240325034947.0000000 ' LASTCHANGEDBY = 'Detemple' LASTCHANGEDAT = '20240407102104.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000042' AGENCY_ID = '070040' CUSTOMER_ID = '000579' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '60.00 ' TOTAL_PRICE = '3854.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'P'
        CREATEDBY = 'Jacqmain' CREATEDAT = '20240325053243.0000000 ' LASTCHANGEDBY = 'Koller' LASTCHANGEDAT = '20240410093535.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000043' AGENCY_ID = '070002' CUSTOMER_ID = '000201' BEGIN_DATE = '20241122' END_DATE = '20241122' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1323.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'N'
        CREATEDBY = 'Lautenbach' CREATEDAT = '20240325011740.0000000 ' LASTCHANGEDBY = 'Gutenberg' LASTCHANGEDAT = '20240325185541.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000044' AGENCY_ID = '070019' CUSTOMER_ID = '000514' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '2826.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip to USA' STATUS =
'N'
        CREATEDBY = 'Prinz' CREATEDAT = '20240325004533.0000000 ' LASTCHANGEDBY = 'Jeremias' LASTCHANGEDAT = '20240401194523.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000045' AGENCY_ID = '070043' CUSTOMER_ID = '000003' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1454.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Koslowski' CREATEDAT = '20240325093431.0000000 ' LASTCHANGEDBY = 'Hunter' LASTCHANGEDAT = '20240411042212.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000046' AGENCY_ID = '070011' CUSTOMER_ID = '000518' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1418.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation for Walter,' STATUS =
'N'
        CREATEDBY = 'Matthaeus' CREATEDAT = '20240325100321.0000000 ' LASTCHANGEDBY = 'Trensch' LASTCHANGEDAT = '20240329110424.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000047' AGENCY_ID = '070047' CUSTOMER_ID = '000119' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1398.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Kreiss' CREATEDAT = '20240325114621.0000000 ' LASTCHANGEDBY = 'Matthaeus' LASTCHANGEDAT = '20240413103135.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000048' AGENCY_ID = '070035' CUSTOMER_ID = '000469' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '60.00 ' TOTAL_PRICE = '4210.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Visiting Holm' STATUS = 'B'
        CREATEDBY = 'Montero' CREATEDAT = '20240325190709.0000000 ' LASTCHANGEDBY = 'Prinz' LASTCHANGEDAT = '20240408155657.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000049' AGENCY_ID = '070034' CUSTOMER_ID = '000564' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '2947.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Visiting Dominik' STATUS = 'B'
        CREATEDBY = 'Schneider' CREATEDAT = '20240325222823.0000000 ' LASTCHANGEDBY = 'Rahn' LASTCHANGEDAT = '20240408173423.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000050' AGENCY_ID = '070020' CUSTOMER_ID = '000248' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1532.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip to USA' STATUS =
'N'
        CREATEDBY = 'Lautenbach' CREATEDAT = '20240325192023.0000000 ' LASTCHANGEDBY = 'Deichgraeber' LASTCHANGEDAT = '20240402180043.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000051' AGENCY_ID = '070038' CUSTOMER_ID = '000639' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1561.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'P'
        CREATEDBY = 'Mechler' CREATEDAT = '20240325164726.0000000 ' LASTCHANGEDBY = 'Schneider' LASTCHANGEDAT = '20240409060955.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000052' AGENCY_ID = '070038' CUSTOMER_ID = '000427' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '3146.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'B'
        CREATEDBY = 'Moyano' CREATEDAT = '20240325003137.0000000 ' LASTCHANGEDBY = 'Buchholm' LASTCHANGEDAT = '20240409161851.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000053' AGENCY_ID = '070038' CUSTOMER_ID = '000163' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1584.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'B'
        CREATEDBY = 'Domenech' CREATEDAT = '20240325173628.0000000 ' LASTCHANGEDBY = 'Eichbaum' LASTCHANGEDAT = '20240409150907.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000054' AGENCY_ID = '070024' CUSTOMER_ID = '000013' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '60.00 ' TOTAL_PRICE = '5061.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation to USA' STATUS = 'N'
        CREATEDBY = 'Hansmann' CREATEDAT = '20240325153931.0000000 ' LASTCHANGEDBY = 'Montero' LASTCHANGEDAT = '20240403031520.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000055' AGENCY_ID = '070020' CUSTOMER_ID = '000186' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '3276.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip to USA' STATUS =
'N'
        CREATEDBY = 'Detemple' CREATEDAT = '20240325104459.0000000 ' LASTCHANGEDBY = 'Trensch' LASTCHANGEDAT = '20240402024142.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000056' AGENCY_ID = '070022' CUSTOMER_ID = '000369' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '3437.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation to USA' STATUS = 'N'
        CREATEDBY = 'Babilon' CREATEDAT = '20240325180554.0000000 ' LASTCHANGEDBY = 'Ryan' LASTCHANGEDAT = '20240402112743.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000057' AGENCY_ID = '070033' CUSTOMER_ID = '000509' BEGIN_DATE = '20241124' END_DATE = '20241124' BOOKING_FEE = '30.00 ' TOTAL_PRICE = '2697.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Visiting Jean' STATUS = 'B'
        CREATEDBY = 'Mustermann' CREATEDAT = '20240325135433.0000000 ' LASTCHANGEDBY = 'Gahl' LASTCHANGEDAT = '20240407231330.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000058' AGENCY_ID = '070009' CUSTOMER_ID = '000423' BEGIN_DATE = '20241122' END_DATE = '20241122' BOOKING_FEE = '10.00 ' TOTAL_PRICE = '876.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip for Siegfried'
        STATUS = 'N' CREATEDBY = 'Henry' CREATEDAT = '20240325214927.0000000 ' LASTCHANGEDBY = 'Sisko' LASTCHANGEDAT = '20240328080930.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000059' AGENCY_ID = '070009' CUSTOMER_ID = '000101' BEGIN_DATE = '20241124' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1913.00 ' CURRENCY_CODE = 'USD' DESCRIPTION =
'Business Trip for Peter, Astrid'
        STATUS = 'N' CREATEDBY = 'Benjamin' CREATEDAT = '20240325195926.0000000 ' LASTCHANGEDBY = 'Deichgraeber' LASTCHANGEDAT = '20240328143232.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000060' AGENCY_ID = '070016' CUSTOMER_ID = '000555' BEGIN_DATE = '20241124' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1861.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip to USA' STATUS =
'N'
        CREATEDBY = 'Hoffen' CREATEDAT = '20240325002636.0000000 ' LASTCHANGEDBY = 'Vrsic' LASTCHANGEDAT = '20240331050451.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000061' AGENCY_ID = '070036' CUSTOMER_ID = '000168' BEGIN_DATE = '20241124' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1915.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'B'
        CREATEDBY = 'Sudhoff' CREATEDAT = '20240325195758.0000000 ' LASTCHANGEDBY = 'Pan' LASTCHANGEDAT = '20240408180629.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000062' AGENCY_ID = '070040' CUSTOMER_ID = '000590' BEGIN_DATE = '20241124' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1946.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'P'
        CREATEDBY = 'Goelke' CREATEDAT = '20240325194658.0000000 ' LASTCHANGEDBY = 'Pan' LASTCHANGEDAT = '20240410060002.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000063' AGENCY_ID = '070002' CUSTOMER_ID = '000243' BEGIN_DATE = '20241124' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1987.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'N'
        CREATEDBY = 'Babilon' CREATEDAT = '20240325225240.0000000 ' LASTCHANGEDBY = 'Leisert' LASTCHANGEDAT = '20240325124921.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000064' AGENCY_ID = '070016' CUSTOMER_ID = '000593' BEGIN_DATE = '20241124' END_DATE = '20241124' BOOKING_FEE = '30.00 ' TOTAL_PRICE = '3032.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip to USA' STATUS =
'N'
        CREATEDBY = 'Schneider' CREATEDAT = '20240325092351.0000000 ' LASTCHANGEDBY = 'Hunter' LASTCHANGEDAT = '20240331160520.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000065' AGENCY_ID = '070014' CUSTOMER_ID = '000010' BEGIN_DATE = '20241122' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1985.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation for Irmtraut,' STATUS
= 'N'
        CREATEDBY = 'Madeira' CREATEDAT = '20240325104839.0000000 ' LASTCHANGEDBY = 'Delon' LASTCHANGEDAT = '20240330211502.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000066' AGENCY_ID = '070044' CUSTOMER_ID = '000677' BEGIN_DATE = '20241124' END_DATE = '20241124' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '2171.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Mustermann' CREATEDAT = '20240325135407.0000000 ' LASTCHANGEDBY = 'Benjamin' LASTCHANGEDAT = '20240412232544.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000067' AGENCY_ID = '070003' CUSTOMER_ID = '000661' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '10.00 ' TOTAL_PRICE = '729.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'N'
        CREATEDBY = 'Illner' CREATEDAT = '20231225031505.0000000 ' LASTCHANGEDBY = 'Benz' LASTCHANGEDAT = '20231225131058.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000068' AGENCY_ID = '070008' CUSTOMER_ID = '000183' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1450.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip for Ludwig, Ulla'
        STATUS = 'N' CREATEDBY = 'Barth' CREATEDAT = '20240107180848.0000000 ' LASTCHANGEDBY = 'Lautenbach' LASTCHANGEDAT = '20240110030816.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000069' AGENCY_ID = '070005' CUSTOMER_ID = '000345' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '30.00 ' TOTAL_PRICE = '2231.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'N'
        CREATEDBY = 'Benz' CREATEDAT = '20240113053437.0000000 ' LASTCHANGEDBY = 'Hoffen' LASTCHANGEDAT = '20240115035204.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000070' AGENCY_ID = '070050' CUSTOMER_ID = '000719' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1543.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Goelke' CREATEDAT = '20231228190840.0000000 ' LASTCHANGEDBY = 'Deichgraeber' LASTCHANGEDAT = '20240117063200.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000071' AGENCY_ID = '070042' CUSTOMER_ID = '000050' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '10.00 ' TOTAL_PRICE = '804.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Kirk' CREATEDAT = '20231229233726.0000000 ' LASTCHANGEDBY = 'Fischer' LASTCHANGEDAT = '20240115205132.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000072' AGENCY_ID = '070042' CUSTOMER_ID = '000605' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1638.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Goelke' CREATEDAT = '20240102073008.0000000 ' LASTCHANGEDBY = 'Simonen' LASTCHANGEDAT = '20240119062111.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000073' AGENCY_ID = '070035' CUSTOMER_ID = '000427' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1678.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Visiting Allen' STATUS = 'B'
        CREATEDBY = 'Pan' CREATEDAT = '20240102070445.0000000 ' LASTCHANGEDBY = 'Buehler' LASTCHANGEDAT = '20240116192422.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000074' AGENCY_ID = '070007' CUSTOMER_ID = '000209' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '30.00 ' TOTAL_PRICE = '2604.00 ' CURRENCY_CODE = 'USD' DESCRIPTION =
'Business Trip for August, Anna, Holm'
        STATUS = 'N' CREATEDBY = 'Dumbach' CREATEDAT = '20240103000715.0000000 ' LASTCHANGEDBY = 'Buchholm' LASTCHANGEDAT = '20240105051402.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000075' AGENCY_ID = '070016' CUSTOMER_ID = '000495' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '30.00 ' TOTAL_PRICE = '2667.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip to USA' STATUS =
'N'
        CREATEDBY = 'Leisert' CREATEDAT = '20231230051449.0000000 ' LASTCHANGEDBY = 'Koller' LASTCHANGEDAT = '20240105221130.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000076' AGENCY_ID = '070039' CUSTOMER_ID = '000549' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '10.00 ' TOTAL_PRICE = '921.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'P'
        CREATEDBY = 'Marshall' CREATEDAT = '20231226210249.0000000 ' LASTCHANGEDBY = 'Meier' LASTCHANGEDAT = '20240111082558.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000077' AGENCY_ID = '070048' CUSTOMER_ID = '000409' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '1868.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Wohl' CREATEDAT = '20231231205602.0000000 ' LASTCHANGEDBY = 'Kirk' LASTCHANGEDAT = '20240119045418.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000078' AGENCY_ID = '070025' CUSTOMER_ID = '000152' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '30.00 ' TOTAL_PRICE = '2855.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation to USA' STATUS = 'N'
        CREATEDBY = 'Goelke' CREATEDAT = '20240108222932.0000000 ' LASTCHANGEDBY = 'Müller' LASTCHANGEDAT = '20240118062914.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000079' AGENCY_ID = '070002' CUSTOMER_ID = '000257' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '30.00 ' TOTAL_PRICE = '3117.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'N'
        CREATEDBY = 'Weiss' CREATEDAT = '20231231185420.0000000 ' LASTCHANGEDBY = 'Ryan' LASTCHANGEDAT = '20231231142053.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000080' AGENCY_ID = '070034' CUSTOMER_ID = '000387' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '2061.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Visiting Matthias' STATUS = 'B'
        CREATEDBY = 'Cesari' CREATEDAT = '20231230230609.0000000 ' LASTCHANGEDBY = 'Fischer' LASTCHANGEDAT = '20240112112201.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000081' AGENCY_ID = '070022' CUSTOMER_ID = '000260' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '10.00 ' TOTAL_PRICE = '1060.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation to USA' STATUS = 'N'
        CREATEDBY = 'Marshall' CREATEDAT = '20240106012312.0000000 ' LASTCHANGEDBY = 'Fischmann' LASTCHANGEDAT = '20240115084652.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000082' AGENCY_ID = '070007' CUSTOMER_ID = '000113' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '30.00 ' TOTAL_PRICE = '3274.00 ' CURRENCY_CODE = 'USD' DESCRIPTION =
'Business Trip for Allen, Roland, Ulla'
        STATUS = 'N' CREATEDBY = 'Lindwurm' CREATEDAT = '20231230192429.0000000 ' LASTCHANGEDBY = 'Prinz' LASTCHANGEDAT = '20240101191416.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000083' AGENCY_ID = '070020' CUSTOMER_ID = '000415' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '30.00 ' TOTAL_PRICE = '3341.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip to USA' STATUS =
'N'
        CREATEDBY = 'Benjamin' CREATEDAT = '20240103140729.0000000 ' LASTCHANGEDBY = 'Miguel' LASTCHANGEDAT = '20240111140513.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000084' AGENCY_ID = '070011' CUSTOMER_ID = '000246' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '30.00 ' TOTAL_PRICE = '3413.00 ' CURRENCY_CODE = 'USD' DESCRIPTION =
'Vacation for Achim, Irene, Christine'
        STATUS = 'N' CREATEDBY = 'Illner' CREATEDAT = '20240107175739.0000000 ' LASTCHANGEDBY = 'Gueldenpfenn' LASTCHANGEDAT = '20240111135657.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000085' AGENCY_ID = '070039' CUSTOMER_ID = '000283' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '30.00 ' TOTAL_PRICE = '3793.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'P'
        CREATEDBY = 'Rahn' CREATEDAT = '20240110160326.0000000 ' LASTCHANGEDBY = 'Sommer' LASTCHANGEDAT = '20240126223445.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000086' AGENCY_ID = '070003' CUSTOMER_ID = '000227' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '2489.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'N'
        CREATEDBY = 'Mechler' CREATEDAT = '20240103162103.0000000 ' LASTCHANGEDBY = 'Moyano' LASTCHANGEDAT = '20240104061433.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000087' AGENCY_ID = '070040' CUSTOMER_ID = '000629' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '30.00 ' TOTAL_PRICE = '3938.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'P'
        CREATEDBY = 'Lautenbach' CREATEDAT = '20240103090024.0000000 ' LASTCHANGEDBY = 'D´Oultrement' LASTCHANGEDAT = '20240119184919.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000088' AGENCY_ID = '070002' CUSTOMER_ID = '000439' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '2671.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'N'
        CREATEDBY = 'Lautenbach' CREATEDAT = '20240113131048.0000000 ' LASTCHANGEDBY = 'Pratt' LASTCHANGEDAT = '20240113180254.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000089' AGENCY_ID = '070010' CUSTOMER_ID = '000192' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '10.00 ' TOTAL_PRICE = '1346.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip for Jasmin'
STATUS = 'N'
        CREATEDBY = 'Jacqmain' CREATEDAT = '20240106044307.0000000 ' LASTCHANGEDBY = 'Koller' LASTCHANGEDAT = '20240109093815.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000090' AGENCY_ID = '070021' CUSTOMER_ID = '000248' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '10.00 ' TOTAL_PRICE = '1381.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation to USA' STATUS = 'N'
        CREATEDBY = 'Gahl' CREATEDAT = '20240113122722.0000000 ' LASTCHANGEDBY = 'Kramer' LASTCHANGEDAT = '20240121133829.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000091' AGENCY_ID = '070025' CUSTOMER_ID = '000477' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '2806.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation to USA' STATUS = 'N'
        CREATEDBY = 'Columbo' CREATEDAT = '20240107091717.0000000 ' LASTCHANGEDBY = 'D´Oultrement' LASTCHANGEDAT = '20240117013243.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000092' AGENCY_ID = '070023' CUSTOMER_ID = '000502' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '2856.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation to USA' STATUS = 'N'
        CREATEDBY = 'Gutenberg' CREATEDAT = '20240103081148.0000000 ' LASTCHANGEDBY = 'Marshall' LASTCHANGEDAT = '20240112014437.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000093' AGENCY_ID = '070023' CUSTOMER_ID = '000545' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '10.00 ' TOTAL_PRICE = '1506.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation to USA' STATUS = 'N'
        CREATEDBY = 'Kramer' CREATEDAT = '20240102141123.0000000 ' LASTCHANGEDBY = 'Benjamin' LASTCHANGEDAT = '20240111124118.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000094' AGENCY_ID = '070025' CUSTOMER_ID = '000720' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '3031.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation to USA' STATUS = 'N'
        CREATEDBY = 'Ryan' CREATEDAT = '20231229083748.0000000 ' LASTCHANGEDBY = 'Marshall' LASTCHANGEDAT = '20240108182220.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000095' AGENCY_ID = '070033' CUSTOMER_ID = '000525' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '3019.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Visiting Harish' STATUS = 'B'
        CREATEDBY = 'Leisert' CREATEDAT = '20240107230422.0000000 ' LASTCHANGEDBY = 'Mustermann' LASTCHANGEDAT = '20240120224640.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000096' AGENCY_ID = '070040' CUSTOMER_ID = '000167' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '20.00 ' TOTAL_PRICE = '3211.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Business Trip' STATUS = 'P'
        CREATEDBY = 'Detemple' CREATEDAT = '20231226034618.0000000 ' LASTCHANGEDBY = 'Benz' LASTCHANGEDAT = '20240111035527.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000097' AGENCY_ID = '070036' CUSTOMER_ID = '000665' BEGIN_DATE = '20240123' END_DATE = '20240126' BOOKING_FEE = '80.00 ' TOTAL_PRICE = '21693.00 ' CURRENCY_CODE = 'EUR' DESCRIPTION = 'Business Trip' STATUS = 'B'
        CREATEDBY = 'Gahl' CREATEDAT = '20231219092629.0000000 ' LASTCHANGEDBY = 'D´Oultrement' LASTCHANGEDAT = '20240102132954.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000098' AGENCY_ID = '070047' CUSTOMER_ID = '000111' BEGIN_DATE = '20240129' END_DATE = '20240129' BOOKING_FEE = '10.00 ' TOTAL_PRICE = '1605.00 ' CURRENCY_CODE = 'USD' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Cesari' CREATEDAT = '20240106091806.0000000 ' LASTCHANGEDBY = 'Jacqmain' LASTCHANGEDAT = '20240125112645.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000099' AGENCY_ID = '070038' CUSTOMER_ID = '000367' BEGIN_DATE = '20240123' END_DATE = '20240126' BOOKING_FEE = '40.00 ' TOTAL_PRICE = '10907.00 ' CURRENCY_CODE = 'EUR' DESCRIPTION = 'Business Trip' STATUS = 'P'
        CREATEDBY = 'Buchholm' CREATEDAT = '20231230173052.0000000 ' LASTCHANGEDBY = 'Gueldenpfenn' LASTCHANGEDAT = '20240114003023.0000000 '  )
        ( CLIENT = '080' TRAVEL_ID = '00000100' AGENCY_ID = '070041' CUSTOMER_ID = '000602' BEGIN_DATE = '20240123' END_DATE = '20241120' BOOKING_FEE = '50.00 ' TOTAL_PRICE = '14299.00 ' CURRENCY_CODE = 'EUR' DESCRIPTION = 'Vacation' STATUS = 'P'
        CREATEDBY = 'Henry' CREATEDAT = '20240101094755.0000000 ' LASTCHANGEDBY = 'Jacqmain' LASTCHANGEDAT = '20240117084921.0000000 '  )
    ).
    INSERT ztravel FROM TABLE @it_travel.

    out->write( 'Tables populated.' ).

  ENDMETHOD. " populate_tables


  METHOD predicate_functions.

    IF contains( val = 'ABAP' pcre = 'AC' ).
        out->write( 'contains' ).
    ELSE.
        out->write( 'not contains' ).
    ENDIF.

    IF matches( val = 'ABAP' pcre = '....' ). " '....'
        out->write( '1 matches' ).
    ELSE.
        out->write( '1 not matches' ).
    ENDIF.

    IF matches( val = 'ABAP' pcre = '(.*)' ). " '*'
        out->write( '2 matches' ).
    ELSE.
        out->write( '2 not matches' ).
    ENDIF.

    IF matches( val = 'Well, ABAP  is  definitely  cool.' pcre = '(.*)ABAP(.*)is(.*)c..l(.*)' ). " = '*ABAP*is*c..l*'
        out->write( '3 matches' ).
    ELSE.
        out->write( '3 not matches' ).
    ENDIF.

    IF matches( val = '31-12-2024' pcre = '([0][1-9]|[1-2][0-9]|[3][0-1])-([0][1-9]|[1][1-2])-([0-9]{4})' ). " check date format dd-mm-yyyy)
        out->write( '4 matches' ).
    ELSE.
        out->write( '4 not matches' ).
    ENDIF.

  ENDMETHOD. " predicate_functions


  METHOD string_functions.

    DATA result TYPE i.

    DATA text       TYPE string VALUE `  ABAP  `.
    DATA substring  TYPE string VALUE `AB`.
    DATA offset     TYPE i      VALUE 1.

* Call different description functions
******************************************************************************
*    result = strlen(     string ).
*    result = numofchar(  string ).

    result = count(             val = text sub = substring off = offset ).
*    result = find(             val = string sub = substring off = offset ).

*    result = count_any_of(     val = string sub = substring off = offset ).
*    result = find_any_of(      val = string sub = substring off = offset ).

*    result = count_any_not_of( val = string sub = substring off = offset ).
*    result = find_any_not_of(  val = string sub = substring off = offset ).

    out->write( |Text      = `{ text }`| ).
    out->write( |Substring = `{ substring }` | ).
    out->write( |Offset    = { offset } | ).
    out->write( |Result    = { result } | ).

  ENDMETHOD. " string_functions


  METHOD string_functions2.

    DATA text TYPE string      VALUE ` SAP BTP,   ABAP Environment  `.

* Change Case of characters
**********************************************************************
    out->write( |TO_UPPER         = {   to_upper(  text ) } | ).
    out->write( |TO_LOWER         = {   to_lower(  text ) } | ).
    out->write( |TO_MIXED         = {   to_mixed(  text ) } | ).
    out->write( |FROM_MIXED       = { from_mixed(  text ) } | ).

* Change order of characters
**********************************************************************
    out->write( |REVERSE             = {  reverse( text ) } | ).
    out->write( |SHIFT_LEFT  (places)= {  shift_left(  val = text places   = 3  ) } | ).
    out->write( |SHIFT_RIGHT (places)= {  shift_right( val = text places   = 3  ) } | ).
    out->write( |SHIFT_LEFT  (circ)  = {  shift_left(  val = text circular = 3  ) } | ).
    out->write( |SHIFT_RIGHT (circ)  = {  shift_right( val = text circular = 3  ) } | ).

* Extract a Substring
**********************************************************************
    out->write( |SUBSTRING       = {  substring(        val = text off = 4 len = 10 ) } | ).
    out->write( |SUBSTRING_FROM  = {  substring_from(   val = text sub = 'ABAP'     ) } | ).
    out->write( |SUBSTRING_AFTER = {  substring_after(  val = text sub = 'ABAP'     ) } | ).
    out->write( |SUBSTRING_TO    = {  substring_to(     val = text sub = 'ABAP'     ) } | ).
    out->write( |SUBSTRING_BEFORE= {  substring_before( val = text sub = 'ABAP'     ) } | ).

* Condense, REPEAT and Segment
**********************************************************************
    out->write( |CONDENSE         = {   condense( val = text ) } | ).
    out->write( |REPEAT           = {   repeat(   val = text occ = 2 ) } | ).

    out->write( |SEGMENT1         = {   segment(  val = text sep = ',' index = 1 ) } |  ).
    out->write( |SEGMENT2         = {   segment(  val = text sep = ',' index = 2 ) } |  ).


  ENDMETHOD. " string_functions2


  METHOD string_functions3.

    SELECT SINGLE FROM ztest004 FIELDS ID, LEFT( Data, 3 ) as str WHERE ( id = '1' ) INTO @DATA(result1).
    SELECT SINGLE FROM ztest004 FIELDS ID, SUBSTRING( Data, 1, 3 ) as str WHERE ( id = '1' ) INTO @DATA(result2).

    out->write( result1-str ).
    out->write( result2-str ).

  ENDMETHOD. " string_functions3


  METHOD text_translations.

    out->write( 'Hello, World'(001) ).
    out->write( text-hau ).

  ENDMETHOD. " text_translations

  METHOD use_eml_crud_operations.

*    use_eml_create( out ).
*    use_eml_read( out ).
*    use_eml_update( out ).
    use_eml_delete( out ).

  ENDMETHOD. " use_eml_crud_operations

  METHOD use_eml_create. " Create

*   Create two (root) rows:

    DATA it_simple_create TYPE TABLE FOR CREATE zi_simple_008\\Simple.

    APPEND VALUE #(
            %cid            = '1'
            %is_draft       = '00'      " Saved (not draft)
            Field1          = 'Value1'
            Field2          = 'Value2'
    ) TO it_simple_create.
    APPEND VALUE #(
            %cid            = '2'
            %is_draft       = '00'      " Saved (not draft)
            Field1          = 'Value3'
            Field2          = 'Value4'
    ) TO it_simple_create.

*   Create rows
    MODIFY ENTITIES OF zi_simple_008 " IN LOCAL MODE
        ENTITY Simple
        CREATE
        FIELDS ( Field1 Field2 )
        WITH it_simple_create
        MAPPED DATA(mapped1)
        FAILED DATA(failed1)
        REPORTED DATA(reported1).

    COMMIT ENTITIES.

    IF ( sy-subrc = 0 ).
      out->write( `Root created successfully.` ).
    ELSE.
      out->write( `An issue occurred.` ).
    ENDIF.

*   Get 1st (created) row
    SELECT SINGLE * FROM zi_simple_008 WHERE ( SimpleID = '1' ) INTO @DATA(simple).

*   Create two items in the 1st row:

    DATA it_item_create TYPE TABLE FOR CREATE zi_simple_008\\Simple\_Item.

    APPEND VALUE #(
        %is_draft  = '00'               " Saved
        SimpleUUID = simple-SimpleUUID  " Key
        %target = VALUE #( (
            %cid    = '1'
            Field3  = 'Value5'
            Field4  = 'Value6'
        ) )
    ) TO it_item_create.
    APPEND VALUE #(
        %is_draft  = '00'               " Saved
        SimpleUUID = simple-SimpleUUID  " Key
        %target = VALUE #( (
            %cid    = '2'
            Field3  = 'Value7'
            Field4  = 'Value8'
        ) )
    ) TO it_item_create.

*   Create items
    MODIFY ENTITIES OF zi_simple_008 " IN LOCAL MODE
        ENTITY Simple
        CREATE BY \_Item
        FIELDS ( Field3 Field4 )
        WITH it_item_create
        MAPPED DATA(mapped2)
        FAILED DATA(failed2)
        REPORTED DATA(reported2).

    COMMIT ENTITIES.

    IF ( sy-subrc = 0 ).
      out->write( `Items created successfully.` ).
    ELSE.
      out->write( `An issue occurred.` ).
    ENDIF.

  ENDMETHOD. " use_eml_create

  METHOD use_eml_read. " Read

*   Get 1st row
    SELECT SINGLE * FROM zi_simple_008 WHERE ( SimpleID = '1' ) INTO @DATA(simple).

*   Read the 1st (root) row (with EML):
    READ ENTITIES OF zi_simple_008 " IN LOCAL MODE
        ENTITY Simple
        ALL FIELDS WITH VALUE #( (
            %is_draft   = '00'              " Saved
            SimpleUUID  = simple-simpleUUID " Key
        ) )
        RESULT DATA(simples)
        FAILED DATA(failed1)
        REPORTED DATA(reported1).

    IF ( sy-subrc = 0 ).
      out->write( CONV string( LINES( simples ) ) && ` root read successfully.`  ).
    ELSE.
      out->write( `An issue occurred.` ).
    ENDIF.

*   Read all items from the 1st row:
    READ ENTITIES OF zi_simple_008 " IN LOCAL MODE
        ENTITY Simple BY \_Item
        ALL FIELDS WITH VALUE #( (
            %is_draft   = '00'              " Saved
            SimpleUUID  = simple-SimpleUUID " Key
        ) )
        RESULT DATA(items)
        FAILED DATA(failed2)
        REPORTED DATA(reported2).

    IF ( sy-subrc = 0 ).
      out->write( CONV string( LINES( items ) ) && ` items read successfully.`  ).
    ELSE.
      out->write( `An issue occurred.` ).
    ENDIF.

  ENDMETHOD. " use_eml_read

  METHOD use_eml_update. " Update

*   Get the 1st row:
    SELECT SINGLE * FROM zi_simple_008 WHERE ( SimpleID = '1' ) INTO @DATA(simple).

*   Get the 1st item of the 1st row:
    SELECT SINGLE * FROM zi_item_008 WHERE ( SimpleUUID = @simple-SimpleUUID ) AND ( ItemID = '1' ) INTO @DATA(item).

*   Update the 1st (root) row:

    DATA it_simple_update TYPE TABLE FOR UPDATE zi_simple_008\\Simple.

    APPEND VALUE #(
        %is_draft   = '00'              " Saved
        SimpleUUID = simple-SimpleUUID  " Key
        Field1 = 'Value8'
        Field2 = 'Value9'
    ) TO it_simple_update.

    MODIFY ENTITIES OF zi_simple_008 " IN LOCAL MODE
        ENTITY Simple
        UPDATE FIELDS ( Field1 Field2 )
        WITH it_simple_update
        MAPPED DATA(mapped1)
        FAILED DATA(failed1)
        REPORTED DATA(reported1).

    COMMIT ENTITIES.

    IF ( sy-subrc = 0 ).
      out->write( `Root updated successfully.` ).
    ELSE.
      out->write( `An issue occurred.` ).
    ENDIF.

*   Update the 1st items of the 1st row:

    DATA it_item_update TYPE TABLE FOR UPDATE zi_simple_008\\Item.

    APPEND VALUE #(
        %is_draft = '00'            " Saved
        ItemUUID  = item-ItemUUID   " Key
        Field3  = 'Value10'
        Field4  = 'Value11'
    ) TO it_item_update.

    MODIFY ENTITIES OF zi_simple_008 " IN LOCAL MODE
        ENTITY Item
        UPDATE FIELDS ( Field3 Field4 )
        WITH it_item_update
        MAPPED DATA(mapped2)
        FAILED DATA(failed2)
        REPORTED DATA(reported2).

    COMMIT ENTITIES.

    IF ( sy-subrc = 0 ).
      out->write( `Items updated successfully.` ).
    ELSE.
      out->write( `An issue occurred.` ).
    ENDIF.

  ENDMETHOD. " use_eml_update

  METHOD use_eml_delete. " Delete

*   Get the 1st row:
    SELECT SINGLE * FROM zi_simple_008 WHERE ( SimpleID = '1' ) INTO @DATA(simple).

*   Get 1st and 2nd items from the 2st row:
    SELECT SINGLE * FROM zi_simple_008 WHERE ( SimpleID = '2' ) INTO @DATA(simple2).
    SELECT SINGLE * FROM zi_item_008 WHERE ( SimpleUUID = @simple2-SimpleUUID ) AND ( ItemID = '1' ) INTO @DATA(item1).
    SELECT SINGLE * FROM zi_item_008 WHERE ( SimpleUUID = @simple2-SimpleUUID ) AND ( ItemID = '2' ) INTO @DATA(item2).

*   Delete the 1st (root) row:

    DATA it_simple_delete TYPE TABLE FOR DELETE zi_simple_008\\Simple.

    APPEND VALUE #(
        %is_draft   = '00'              " Saved
        SimpleUUID  = simple-SimpleUUID " Key
    ) TO it_simple_delete.

    MODIFY ENTITIES OF zi_simple_008 " IN LOCAL MODE
      ENTITY Simple
      DELETE FROM it_simple_delete
      MAPPED DATA(mapped1)
      FAILED DATA(failed1)
      REPORTED DATA(reported1).

    COMMIT ENTITIES.

    IF ( sy-subrc = 0 ).
      out->write( `Root deleted successfully.` ).
    ELSE.
      out->write( `An issue occurred.` ).
    ENDIF.

*   Delete two items from 2nd row:

    DATA it_item_delete TYPE TABLE FOR DELETE zi_simple_008\\Item.

    APPEND VALUE #(
        %is_draft   = '00'              " Saved
        ItemUUID    = item1-ItemUUID    " Key
    ) TO it_item_delete.
    APPEND VALUE #(
        %is_draft   = '00'              " Saved
        ItemUUID    = item2-ItemUUID    " Key
    ) TO it_item_delete.

*   Delete items
    MODIFY ENTITIES OF zi_simple_008 " IN LOCAL MODE
      ENTITY Item
      DELETE FROM it_item_delete
      FAILED DATA(failed2)
      MAPPED DATA(mapped2)
      REPORTED DATA(reported2).

    COMMIT ENTITIES.

    IF ( sy-subrc = 0 ).
      out->write( `Items deleted successfully.` ).
    ELSE.
      out->write( `An issue occurred.` ).
    ENDIF.

  ENDMETHOD. " use_eml_delete

ENDCLASS.
