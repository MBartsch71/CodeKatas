REPORT ymbh_video_store.

CLASS customer DEFINITION FINAL.

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

    METHODS get_rental_fee RETURNING VALUE(result) TYPE decfloat16.

    METHODS get_renter_points RETURNING VALUE(result) TYPE i.

    METHODS constructor.
    METHODS add_rental IMPORTING title TYPE string
                                 days  TYPE i.
  PRIVATE SECTION.
    DATA days TYPE i.
    DATA title TYPE string.
    DATA movie_registry TYPE movie_items.

    METHODS apply_grace_period IMPORTING amount        TYPE i
                                         grace         TYPE i
                               RETURNING VALUE(result) TYPE i.

    METHODS get_video_type IMPORTING title         TYPE string
                           RETURNING VALUE(result) TYPE videotype.

ENDCLASS.

CLASS customer IMPLEMENTATION.

  METHOD constructor.
    movie_registry = VALUE #( ( description = |RegularMovie|   video_type = regular  )
                              ( description = |ChildrensMovie| video_type = childrens ) ).
  ENDMETHOD.

  METHOD get_rental_fee.
    IF get_video_type( title ) = regular.
      result = apply_grace_period( amount = 150 grace = 3 ).
    ELSE.
      result = 100.
    ENDIF.
  ENDMETHOD.

  METHOD add_rental.
    me->days = days.
    me->title = title.
  ENDMETHOD.

  METHOD get_renter_points.
    result = apply_grace_period( amount = 1 grace = 3 ).
  ENDMETHOD.

  METHOD apply_grace_period.
    result = amount.
    IF days > grace.
      result = amount + amount * ( days - 3 ).
    ENDIF.
  ENDMETHOD.

  METHOD get_video_type.
    result = VALUE #( movie_registry[ description = title ]-video_type DEFAULT none ).
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
    METHODS childrens_movie_one_day FOR TESTING.
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
    cut->add_rental( title = |RegularMovie|
                     days  = 1 ).
    assert_fee_and_points( fee = 150 points = 1 ).
  ENDMETHOD.

  METHOD regular_movie_2nd_3rd_day_fee.
    cut->add_rental( title = |RegularMovie| days = 2 ).
    assert_fee_and_points( fee = 150 points = 1 ).
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

ENDCLASS.
