REPORT ymbh_fire_alarm.

CLASS lcl_fire_alarm DEFINITION FINAL.

  PUBLIC SECTION.
    EVENTS the_roof_is_on_fire.

    METHODS raise_fire_alarm.
ENDCLASS.

CLASS lcl_fire_alarm IMPLEMENTATION.

  METHOD raise_fire_alarm.
    RAISE EVENT the_roof_is_on_fire.
    ##TODO "MEssage
  ENDMETHOD.

ENDCLASS.

CLASS lcl_kata_constraint DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES if_constraint.

    METHODS handle_fire_alarm FOR EVENT the_roof_is_on_fire OF lcl_fire_alarm.

  PRIVATE SECTION.
    DATA mv_alarm_handled TYPE abap_bool.
ENDCLASS.

CLASS lcl_kata_constraint IMPLEMENTATION.

  METHOD if_constraint~get_description.

  ENDMETHOD.

  METHOD if_constraint~is_valid.

    result = mv_alarm_handled.
  ENDMETHOD.

  METHOD handle_fire_alarm.
    mv_alarm_handled = abap_true.
  ENDMETHOD.

ENDCLASS.


CLASS ltc_fire_alarm DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS throw_event_fire_alarm FOR TESTING.
ENDCLASS.


CLASS ltc_fire_alarm IMPLEMENTATION.


  METHOD throw_event_fire_alarm.

    DATA lo_kata_constraint TYPE REF TO lcl_kata_constraint.
    DATA(lo_cut) = NEW lcl_fire_alarm( ).
    lo_kata_constraint = NEW #( ).
    SET HANDLER lo_kata_constraint->handle_fire_alarm FOR lo_cut.
    lo_cut->raise_fire_alarm( ).

    cl_abap_unit_assert=>assert_that(
      EXPORTING
        act              = |Fire alarm raised|
        exp              = lo_kata_constraint
        msg              = |The fire alarm should be raised.|
    ).
  ENDMETHOD.

ENDCLASS.
