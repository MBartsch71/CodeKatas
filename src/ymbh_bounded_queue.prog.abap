REPORT ymbh_bounded_queue.

INTERFACE lif_html_merger_dao.
  METHODS merge_html_site IMPORTING it_merge_table       TYPE swww_t_merge_table
                          RETURNING VALUE(rt_html_table) TYPE swww_t_html_table.

ENDINTERFACE.

CLASS lcl_html_merger_dao DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_html_merger_dao.
  PRIVATE SECTION.
    DATA: mc_html_template_name TYPE swww_t_template_name VALUE 'YBOUNDED_QUEUE'.
ENDCLASS.

CLASS lcl_html_merger_dao IMPLEMENTATION.

  METHOD lif_html_merger_dao~merge_html_site.
    DATA(lt_merge_table) = it_merge_table.
    CALL FUNCTION 'WWW_HTML_MERGER'
      EXPORTING
        template    = mc_html_template_name
      IMPORTING
        html_table  = rt_html_table
      CHANGING
        merge_table = lt_merge_table.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS build_website.
ENDCLASS.

CLASS lcl_application IMPLEMENTATION.

  METHOD build_website.
    DATA lt_merge_table TYPE swww_t_merge_table.
    DATA lv_url TYPE swk_url.

    DATA(lt_html_table) = NEW lcl_html_merger_dao( )->lif_html_merger_dao~merge_html_site( lt_merge_table ).
    DATA(lo_html_viewer) = NEW cl_gui_html_viewer( parent = cl_gui_container=>screen0 ).
    lo_html_viewer->load_data(
      IMPORTING
        assigned_url           = lv_url
      CHANGING
        data_table             = lt_html_table ).

    lo_html_viewer->show_url( EXPORTING url = lv_url ).

  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF SCREEN 1001.
SELECTION-SCREEN END OF SCREEN 1001.

START-OF-SELECTION.

  NEW lcl_application( )->build_website( ).
  CALL SCREEN 1001.
