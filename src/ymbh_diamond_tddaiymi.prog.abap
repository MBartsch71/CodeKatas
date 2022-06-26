##TODO
"* print a diamond with 2 letters
"* print a diamond with 3 letters
"* calculate the difference between A and C
"* calculate index for a given letter
"* get the sequence till the expected letter
"- get the sequence in a table
"- print a diamond with given termination letter

REPORT ymbh_diamond_tddaiymi.

CLASS tc_diamond DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS print_diamond_of_2_letters  FOR TESTING.
    METHODS print_diamond_of_3_letters  FOR TESTING.
    METHODS calculate_index_of_letter_e FOR TESTING.

    METHODS get_sequence_till_e_as_table FOR TESTING.

    METHODS print_diamond_with_2_lines RETURNING VALUE(result) TYPE stringtab.
    METHODS print_diamond_with_3_lines RETURNING VALUE(result) TYPE stringtab.

    METHODS calculate_index_of IMPORTING letter        TYPE char1
                               RETURNING VALUE(result) TYPE i.

    METHODS get_sequence_table_till
      IMPORTING
        letter        TYPE char1
      RETURNING
        VALUE(result) TYPE stringtab.
    METHODS get_letter_from_index
      IMPORTING
                index         TYPE i
      RETURNING VALUE(result) TYPE char1.

ENDCLASS.


CLASS tc_diamond IMPLEMENTATION.

  METHOD print_diamond_of_2_letters.
    DATA(expected_values) = VALUE stringtab( ( | A | )
                                             ( |B B| )
                                             ( | A | ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = print_diamond_with_2_lines( ) ).
  ENDMETHOD.

  METHOD print_diamond_of_3_letters.
    DATA(expected_values) = VALUE stringtab( ( |  A  | )
                                             ( | B B | )
                                             ( |C   C| )
                                             ( | B B | )
                                             ( |  A  | ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = print_diamond_with_3_lines( ) ).
  ENDMETHOD.

  METHOD print_diamond_with_2_lines.
    result = VALUE #( ( | A | )
                      ( |B B| )
                      ( | A | ) ).
  ENDMETHOD.

  METHOD print_diamond_with_3_lines.
    result = VALUE #( ( |  A  | )
                      ( | B B | )
                      ( |C   C| )
                      ( | B B | )
                      ( |  A  | ) ).

  ENDMETHOD.

  METHOD calculate_index_of.
    FIND letter IN sy-abcde IGNORING CASE MATCH OFFSET DATA(offset).
    result = offset + 1.
  ENDMETHOD.

  METHOD calculate_index_of_letter_e.
    cl_abap_unit_assert=>assert_equals(
        exp = 5
        act = calculate_index_of( |E| ) ).
  ENDMETHOD.

  METHOD get_sequence_till_e_as_table.
    DATA(expected_values) = VALUE stringtab( ( |A| )
                                             ( |B| )
                                             ( |C| )
                                             ( |D| )
                                             ( |E| ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = get_sequence_table_till( |E| ) ).
  ENDMETHOD.


  METHOD get_sequence_table_till.
    result = VALUE #( FOR i = 0 THEN i + 1 UNTIL i = calculate_index_of( letter )
                            ( get_letter_from_index(  index  = i ) ) ).
  ENDMETHOD.

  METHOD get_letter_from_index.
    result = substring( val = sy-abcde off = index len = 1 ).
  ENDMETHOD.

ENDCLASS.
