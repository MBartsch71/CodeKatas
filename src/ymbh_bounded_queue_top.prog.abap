TYPES tt_textline TYPE STANDARD TABLE OF text60 WITH DEFAULT KEY.

DATA go_queue    TYPE REF TO lcl_queue.

DATA ok_code TYPE sy-ucomm.
DATA gv_sender_count TYPE i.
DATA gv_reader_count TYPE i.

DATA go_container TYPE REF TO cl_gui_custom_container.
DATA go_textedit  TYPE REF TO cl_gui_textedit.

DATA gt_outtab       TYPE tt_textline.
