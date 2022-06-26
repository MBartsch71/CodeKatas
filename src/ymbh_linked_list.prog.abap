"- A linked list contains of a head and the tail
"- the Head is the first element, the tail all the following items
"- Items have no key, just a value
"- every item knows his successor
"- the following operations can be performed
"   - append an element
"   - remove an element
"   - search an element
"   - get the head and the tail

##TODO
"* create a linked list with one item
"* create a linked list with two elements
"- create a linked list with multiple elements
"- remove the last element
"- remove an element in between
"- add one element at a position
"- get the head
"- get the tail

REPORT ymbh_linked_list.

CLASS item DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING value TYPE i.

    METHODS append      IMPORTING item TYPE REF TO item.
    METHODS get_next    RETURNING VALUE(result) TYPE REF TO item.
    METHODS get_value   RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA value TYPE REF TO ycl_mbh_integer.
    DATA next_item TYPE REF TO item.

ENDCLASS.

CLASS item IMPLEMENTATION.

  METHOD constructor.
    me->value = ycl_mbh_integer=>create( value ).
  ENDMETHOD.

  METHOD get_value.
    result = value->value( ).
  ENDMETHOD.

  METHOD get_next.
    result = next_item.
  ENDMETHOD.

  METHOD append.
    next_item = item.
  ENDMETHOD.

ENDCLASS.

CLASS tc_item DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO item.
    METHODS setup.

    METHODS create_item_with_value FOR TESTING.
    METHODS create_two_linked_items FOR TESTING.
ENDCLASS.


CLASS tc_item IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( 3 ).
  ENDMETHOD.

  METHOD create_item_with_value.
    cl_abap_unit_assert=>assert_equals(
        exp = 3
        act = cut->get_value( ) ).
  ENDMETHOD.

  METHOD create_two_linked_items.
    cut->append(  NEW item( 5 ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = 5
        act = cut->get_next( )->get_value( ) ).
  ENDMETHOD.

ENDCLASS.
