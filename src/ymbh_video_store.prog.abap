REPORT ymbh_video_store.

CLASS video_registry DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ENUM VideoType BASE TYPE text6,
             regular   VALUE 'RegularMovie',
             childrens VALUE 'ChildrensMovie',
             none      VALUE IS INITIAL,
           END OF ENUM VideoType.

    TYPES: BEGIN OF registry_item,
             description TYPE string,
             video_type  TYPE videotype,
           END OF regiSTRY_ITEM.
    TYPES movie_items TYPE STANDARD TABLE OF registry_item WITH EMPTY KEY.

    CLASS-METHODS add_movie IMPORTING title      TYPE string
                                      video_type TYPE videotype.

    CLASS-METHODS get_video_type IMPORTING title         TYPE string
                                 RETURNING VALUE(result) TYPE videotype.

  PRIVATE SECTION.
    CLASS-DATA movie_registry TYPE movie_items.
ENDCLASS.

CLASS video_registry IMPLEMENTATION.
  METHOD get_video_type.
    result = VALUE #( movie_registry[ description = title ]-video_type DEFAULT none ).
  ENDMETHOD.

  METHOD add_movie.
    movie_registry = VALUE #( BASE movie_registry ( description = title
                                                    video_type  = video_type ) ).
  ENDMETHOD.

ENDCLASS.

CLASS rental DEFINITION.
  PUBLIC SECTION.
    DATA title TYPE string.
    DATA days TYPE i.
    DATA video_type TYPE video_registry=>videotype.

    METHODS constructor IMPORTING title TYPE string
                                  days  TYPE i.

ENDCLASS.

CLASS rental IMPLEMENTATION.

  METHOD constructor.
    me->title = title.
    me->days = days.
    me->video_type = video_registry=>get_video_type( title ).
  ENDMETHOD.

ENDCLASS.

CLASS customer DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS add_rental IMPORTING title TYPE string
                                 days  TYPE i.

    METHODS get_rental_fee RETURNING VALUE(result) TYPE i.

    METHODS get_renter_points RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA rentals TYPE TABLE OF REF TO rental.

    METHODS apply_grace_period IMPORTING amount        TYPE i
                                         grace         TYPE i
                                         days          TYPE i
                               RETURNING VALUE(result) TYPE i.

    METHODS fee_for IMPORTING rental        TYPE REF TO rental
                    RETURNING VALUE(result) TYPE i.

    METHODS points_for IMPORTING rental        TYPE REF TO rental
                       RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS customer IMPLEMENTATION.

  METHOD add_rental.
    rentals = VALUE #( BASE rentals ( NEW rental( title = title days = days ) ) ).
  ENDMETHOD.

  METHOD get_rental_fee.
    result = REDUCE #( INIT sum = 0
                       FOR rental IN rentals
                       NEXT sum += fee_for( rental ) ).
  ENDMETHOD.

  METHOD get_renter_points.
    result = REDUCE #( INIT sum = 0
                      FOR rental IN rentals
                      NEXT sum += points_for( rental ) ).
  ENDMETHOD.

  METHOD fee_for.
    result += SWITCH #( rental->video_type
                        WHEN video_registry=>regular
                            THEN apply_grace_period( amount = 150 days = rental->days grace = 3 )
                        ELSE rental->days * 100 ).
  ENDMETHOD.

  METHOD points_for.
    result += SWITCH #( rental->video_type
                            WHEN video_registry=>regular
                                THEN apply_grace_period( amount = 1 days = rental->days grace = 3 )
                            ELSE 1 ).
  ENDMETHOD.

  METHOD apply_grace_period.
    result = amount.
    IF days > grace.
      result = amount + amount * ( days - grace ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS customer_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO customer.

    CLASS-METHODS class_setup.
    METHODS setup.
    METHODS assert_fee_and_points IMPORTING fee    TYPE i
                                            points TYPE i.

    METHODS regular_movie_one_day FOR TESTING.
    METHODS regular_movie_2nd_day_fee FOR TESTING.
    METHODS regular_movie_3rd_day_Fee FOR TESTING.
    METHODS regular_movie_four_days FOR TESTING.
    METHODS childrens_movie_one_day FOR TESTING.
    METHODS childrens_movie_four_days FOR TESTING.
    METHODS one_reg_one_child_4_days FOR TESTING.
ENDCLASS.

CLASS customer_test IMPLEMENTATION.

  METHOD class_setup.
    video_registry=>add_movie( title = |RegularMovie| video_type = video_registry=>regular ).
    video_registry=>add_movie( title = |ChildrensMovie| video_type = video_registry=>childrens ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD assert_fee_and_points.
    cl_abap_unit_assert=>assert_equals( exp = fee act = cut->get_rental_fee( ) ).
    cl_abap_unit_assert=>assert_equals( exp = points act = cut->get_renter_points( ) ).
  ENDMETHOD.

  METHOD regular_movie_one_day.
    cut->add_rental( title = |RegularMovie|
                     days  = 1 ).
    assert_fee_and_points( fee = 150 points = 1 ).
  ENDMETHOD.

  METHOD regular_movie_2nd_day_fee.
    cut->add_rental( title = |RegularMovie| days = 2 ).
    assert_fee_and_points( fee = 150 points = 1 ).
  ENDMETHOD.

  METHOD regular_movie_3rd_day_fee.
    cut->add_rental( title = |RegularMovie| days = 3 ).
    assert_fee_and_points( fee = 150 points = 1 ).
  ENDMETHOD.

  METHOD regular_movie_four_days.
    cut->add_rental( title = |RegularMovie| days = 4 ).
    assert_fee_and_points( fee = 300 points = 2 ).
  ENDMETHOD.

  METHOD childrens_movie_one_day.
    cut->add_rental( title = |ChildrensMovie| days = 1 ).
    assert_fee_and_points( fee = 100 points = 1 ).
  ENDMETHOD.

  METHOD childrens_movie_four_days.
    cut->add_rental( title = |ChildrensMovie| days = 4 ).
    assert_fee_and_points( fee = 400 points = 1 ).
  ENDMETHOD.

  METHOD one_reg_one_child_4_days.
    cut->add_rental( title = |RegularMovie| days = 4 ).
    cut->add_rental( title = |ChildrensMovie| days = 4 ).
    assert_fee_and_points( fee = 700 points = 3 ).
  ENDMETHOD.

ENDCLASS.
