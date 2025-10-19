REPORT ymbh_video_store.

CLASS customer DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS get_rental_fee RETURNING VALUE(result) TYPE decfloat16.

    METHODS get_renter_points RETURNING VALUE(result) TYPE i.

    METHODS add_rental IMPORTING title TYPE string
                                 days  TYPE i.
  PRIVATE SECTION.
    DATA days TYPE i.

    METHODS apply_grace_period IMPORTING amount        TYPE i
                                         grace         TYPE i
                               RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS customer IMPLEMENTATION.

  METHOD get_rental_fee.
    result = apply_grace_period( amount = 150 grace = 3 ).
  ENDMETHOD.

  METHOD add_rental.
    me->days = days.
  ENDMETHOD.

  METHOD get_renter_points.
    result = apply_grace_period( amount =  1 grace = 3 ).
  ENDMETHOD.

  METHOD apply_grace_period.
    result = amount.
    IF days > grace.
      result = amount + amount * ( days - 3 ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS customer_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO customer.

    METHODS setup.
    METHODS assert_fee_and_points IMPORTING fee    TYPE i
                                            points TYPE i.

    METHODS regular_movie_one_day FOR TESTING.
    METHODS regular_movie_2nd_3rd_day_fee FOR TESTING.
    METHODS regular_movie_four_days FOR TESTING.
ENDCLASS.

CLASS customer_test IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD assert_fee_and_points.
    cl_abap_unit_assert=>assert_equals( exp = fee act = cut->get_rental_fee( ) ).
    cl_abap_unit_assert=>assert_equals( exp = points act = cut->get_renter_points( ) ).
  ENDMETHOD.

  METHOD regular_movie_one_day.
    cut->add_rental( title = |Regular Movie|
                     days  = 1 ).
    assert_fee_and_points( fee = 150 points = 1 ).
  ENDMETHOD.

  METHOD regular_movie_2nd_3rd_day_fee.
    cut->add_rental( title = |Regular_Movie| days = 2 ).
    assert_fee_and_points( fee = 150 points = 1 ).
    cut->add_rental( title = |Regular_Movie| days = 3 ).
    assert_fee_and_points( fee = 150 points = 1 ).
  ENDMETHOD.

  METHOD regular_movie_four_days.
    cut->add_rental( title = |Regular_Movie| days = 4 ).
    assert_fee_and_points( fee = 300 points = 2 ).
  ENDMETHOD.

ENDCLASS.
