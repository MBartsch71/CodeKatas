##TODO
"- print a diamond with 2 letters
"- print a diamond with 3 letters
"- calculate the difference between A and C
"- print a diamond with given termination letter

REPORT ymbh_diamond_tddaiymi.

CLASS tc_diamond DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS print_diamond_of_2_letters FOR TESTING.
    METHODS print_diamond_of_3_letters FOR TESTING.

    METHODS print_diamond_with_2_lines RETURNING VALUE(result) TYPE stringtab.
    METHODS print_diamond_with_3_lines RETURNING VALUE(result) TYPE stringtab.

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

ENDCLASS.
