##TODO
"- print a diamond with 2 letters
"- print a diamond with 3 letters
"- print a diamond with given termination letter

REPORT ymbh_diamond_tddaiymi.

CLASS tc_diamond DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS print_diamond_of_2_lines FOR TESTING.
    METHODS print_simple_diamond
      RETURNING
        VALUE(result) TYPE stringtab.
ENDCLASS.


CLASS tc_diamond IMPLEMENTATION.

  METHOD print_diamond_of_2_lines.
    DATA(expected_values) = VALUE stringtab( ( | A | )
                                             ( |B B| )
                                             ( | A | ) ).
    cl_abap_unit_assert=>assert_equals(
        exp = expected_values
        act = print_simple_diamond( ) ).
  ENDMETHOD.

  METHOD print_simple_diamond.
    result = VALUE #( ( | A | )
                      ( |B B| )
                      ( | A | ) ).
  ENDMETHOD.

ENDCLASS.
