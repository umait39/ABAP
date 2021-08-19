*&---------------------------------------------------------------------*
*& Report YTEST_POC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ytest_poc.
*DATA: gv_ucomm TYPE sy-ucomm,
*      gv_bukrs TYPE bukrs_d.
*
*SELECTION-SCREEN: BEGIN OF BLOCK b1 .
*SELECT-OPTIONS s_bukrs FOR gv_burks.
*SELECTION-SCREEN: END OF BLOCK b1.

* Include for data declerations

*INCLUDE /dcr/cbfc_mm_non_pt_inv_top.

* Include for selection screen
*INCLUDE /dcr/cbfc_mm_non_pt_inv_sel.

* Include for sub forms
*INCLUDE /dcr/cbfc_mm_non_pt_inv_forms

TYPES: BEGIN OF ts_rbkp ,
         belnr  TYPE belnr_d,
         gjahr  TYPE gjahr,
         bukrs  TYPE bukrs,
         lifnr  TYPE lifnr,
         bldat  TYPE bldat,
         budat  TYPE budat,
         blart  TYPE blart,
         bktxt  TYPE bktxt,
         rbstat TYPE rbstat,
         rmwwr  TYPE rmwwr,
         waers  TYPE waers,
         kursf  TYPE kursf,
       END OF ts_rbkp .

TYPES : BEGIN OF ts_lfa1 ,
          lifnr TYPE lifnr,
          name1 TYPE name1,
        END OF ts_lfa1 .

TYPES : BEGIN OF ts_lfb1 ,
          lifnr TYPE lifnr,
          bukrs TYPE bukrs,
          busab TYPE t001s-busab,
        END OF ts_lfb1.

TYPES : BEGIN OF ts_ekko ,
          ebeln TYPE ebeln,
          bsart TYPE bsart,
        END OF ts_ekko .

TYPES : BEGIN OF ts_ekpo ,
          ebeln TYPE ebeln,
          ebelp TYPE ebelp,
          wepos TYPE wepos,
          knttp TYPE knttp,
          matnr TYPE matnr,
          txz01 TYPE txz01,
        END OF ts_ekpo .

TYPES : BEGIN OF ts_ekkn ,
          ebeln TYPE ebeln,
          ebelp TYPE ebelp,
          sakto TYPE sakto,
          kostl TYPE kostl,
          aufnr TYPE aufnr,
        END OF ts_ekkn .

TYPES : BEGIN OF ts_rbsellifs ,
          belnr TYPE   belnr_d,
          gjahr TYPE gjahr,
          lfsnr TYPE   lfsnr1,
        END OF ts_rbsellifs.

TYPES: BEGIN OF ts_rbco ,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         saknr TYPE saknr,
         aufnr TYPE aufnr,
         kostl TYPE kostl,
       END OF ts_rbco .

TYPES: BEGIN OF ts_rbdrseg,
         rblnr TYPE re_belnr,
         rjahr TYPE gjahr,
         ebeln TYPE bstnr,
         ebelp TYPE ebelp,
         knttp TYPE knttp,
       END OF ts_rbdrseg .

TYPES: BEGIN OF ts_rbselbest,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
       END OF ts_rbselbest.

TYPES: BEGIN OF ts_bukrs,
         bukrs TYPE bukrs,
       END   OF ts_bukrs.

TYPES : BEGIN OF ts_aufk ,
          aufnr TYPE aufnr,
          kostv TYPE aufkostv,
        END OF ts_aufk.

TYPES : BEGIN OF ts_t001s ,    "For accounting clerk description
          bukrs TYPE  bukrs,
          busab TYPE busab,
          sname TYPE sname_001s,
          usnam TYPE fi_usr_nam,
        END OF ts_t001s.
***Used in final table preparation
TYPES : BEGIN OF ts_temp ,
          belnr TYPE belnr_d,
          ebeln TYPE ebeln,
          count TYPE int4,
        END OF ts_temp .
TYPES : BEGIN OF ts_t001 ,
          bukrs TYPE bukrs,
          waers TYPE waers,
        END OF ts_t001 .

DATA: gv_belnr   TYPE belnr_d,
      gv_gjahr   TYPE gjahr ,                               "#EC NEEDED
      gv_lifnr   TYPE lifnr ,                               "#EC NEEDED
      gv_bukrs   TYPE bukrs ,                               "#EC NEEDED
      gv_busab   TYPE busab ,                               "#EC NEEDED
      gv_bldat   TYPE bldat ,                               "#EC NEEDED
      gv_budat   TYPE budat ,                               "#EC NEEDED
      gv_blart   TYPE rbkp-blart ,                          "#EC NEEDED
      gv_bktxt   TYPE bktxt ,                               "#EC NEEDED
      gv_sgtxt   TYPE sgtxt ,                               "#EC NEEDED
      gv_xblnr   TYPE xblnr ,                               "#EC NEEDED
      gv_rbstat  TYPE rbstat ,                              "#EC NEEDED
      gv_ebeln   TYPE ebeln ,                               "#EC NEEDED
      gv_bsart   TYPE bsart ,                               "#EC NEEDED
      gv_wepos   TYPE wepos ,                               "#EC NEEDED
      gv_knttp   TYPE ekpo-knttp ,                          "#EC NEEDED
      gv_kostl   TYPE ekkn-kostl ,                          "#EC NEEDED
      gv_aufnr   TYPE aufk-aufnr ,                          "#EC NEEDED
      gv_rmwwr   TYPE rmwwr  ,                              "#EC NEEDED
      gv_bldat1  TYPE bldat ,                               "#EC NEEDED
      gv_budat1  TYPE budat ,                               "#EC NEEDED
      gv_blart1  TYPE rbkp-blart ,                          "#EC NEEDED
      gv_bktxt1  TYPE bktxt ,                               "#EC NEEDED
      gv_sgtxt1  TYPE sgtxt ,                               "#EC NEEDED
      gv_xblnr1  TYPE xblnr ,                               "#EC NEEDED
      gv_rbstat1 TYPE rbstat ,                              "#EC NEEDED
      gv_ebeln1  TYPE ebeln ,                               "#EC NEEDED
      gv_bsart1  TYPE bsart ,                               "#EC NEEDED
      gv_wepos1  TYPE wepos ,                               "#EC NEEDED
      gv_knttp1  TYPE ekpo-knttp ,                          "#EC NEEDED
      gv_kostl1  TYPE ekkn-kostl ,                          "#EC NEEDED
      gv_aufnr1  TYPE aufk-aufnr ,                          "#EC NEEDED
      gv_rmwwr1  TYPE rmwwr  ,                              "#EC NEEDED
      gv_ucomm   TYPE sy-ucomm.                             "#EC NEEDED

CONSTANTS : c_x(1)           TYPE c VALUE 'X',
            c_m(1)           TYPE c VALUE 'M',
            c_v(1)           TYPE c VALUE 'V',
*            c_err  TYPE char1  VALUE 'E',
            c_inf            TYPE char1  VALUE 'I',
            c_0(1)           TYPE c VALUE '0',
            c_1(1)           TYPE c VALUE '1',
            c_2(1)           TYPE c VALUE '2' ,             "#EC NEEDED
            c_4(1)           TYPE c VALUE '4' ,             "#EC NEEDED
            c_5(1)           TYPE c VALUE '5' ,             "#EC NEEDED
            c_space(1)       TYPE c VALUE ' ',
            c_authobj        TYPE char10  VALUE 'M_RECH_BUK',
            c_02             TYPE char2   VALUE '02',
            c_bukrs          TYPE char5   VALUE 'BUKRS',
            c_actvt          TYPE char5   VALUE 'ACTVT',
            c_po(4)          TYPE c VALUE 'P_PO',
            c_dn(4)          TYPE c VALUE 'P_DN',
            c_wr(4)          TYPE c VALUE 'P_WR',
            c_upd(5)         TYPE c VALUE 'P_UPD' ,         "#EC NEEDED
            c_ucomm(10)      TYPE c VALUE 'SJOB',
            c_ucomm_onli(10) TYPE c VALUE 'ONLI',
            c_rmwwr_l(20)    TYPE c VALUE 'S_RMWWR-LOW',
            c_rmwwr_h(20)    TYPE c VALUE 'S_RMWWR-HIGH',
            c_gjahr(10)      TYPE c VALUE 'P_GJAHR',
            c_bukrs_l(20)    TYPE c VALUE 'S_BUKRS-LOW',
            c_bukrs_h(20)    TYPE c VALUE 'S_BUKRS-HIGH',
            c_final(50)      TYPE c VALUE '/DCR/CBFC_MM_NON_PT_INVOICES', "#EC NEEDED
            c_flag           TYPE char4         VALUE 'FLAG', "#EC NEEDED
            c_tab_name       TYPE tabname       VALUE '/DCR/CBFC_MM_NON_PT_INVOICES',
            c_prog_name      TYPE syrepid       VALUE '/DCR/CBFC_MM_NON_PT_INVOICES',
            c_tab_log        TYPE tabname       VALUE '/DCR/CBFC_MM_NPT_INV_LOG',
            c_tab_upd_st     TYPE tabname       VALUE '/DCR/CBFC_MM_NPT_INV', "#EC NEEDED
            c_form_status    TYPE slis_formname VALUE 'SET_PF_STATUS', "#EC NEEDED
            c_form_command   TYPE slis_formname VALUE 'USER_COMMAND', "#EC NEEDED
            c_standard       TYPE sypfkey       VALUE 'STANDARD',
            c_display        TYPE sy-ucomm      VALUE 'DISPLAY', "#EC NEEDED
            c_ic1            TYPE sy-ucomm      VALUE '&IC1', "#EC NEEDED
*            c_report_title TYPE char25        VALUE 'Report for non posted invoices',
            c_ebeln          TYPE char5         VALUE 'EBELN',
            c_belnr          TYPE char5         VALUE 'BELNR',
            c_prog           TYPE syrepid VALUE '/DCR/CBFC_MM_NON_PT_INVOICES', "#EC NEEDED
            c_cons           TYPE char20  VALUE 'RBSTAT' .  "#EC NEEDED
TYPES: tt_extab TYPE slis_t_extab.      "Exclude table
DATA: gt_final TYPE STANDARD TABLE OF ekpo.
DATA : gt_rbkp        TYPE STANDARD TABLE OF ts_rbkp ,      "#EC NEEDED
       gt_lfa1        TYPE STANDARD TABLE OF ts_lfa1 ,      "#EC NEEDED
       gt_lfb1        TYPE STANDARD TABLE OF ts_lfb1 ,      "#EC NEEDED
       gt_ekkn        TYPE STANDARD TABLE OF ts_ekkn ,      "#EC NEEDED
       gt_ekko        TYPE STANDARD TABLE OF ts_ekko ,      "#EC NEEDED
       gt_ekpo        TYPE STANDARD TABLE OF ts_ekpo ,      "#EC NEEDED
       gt_ekpo_1      TYPE STANDARD TABLE OF ts_ekpo ,  " for ekpo-knttp is not intial "#EC NEEDED
       gt_ekpo_2      TYPE STANDARD TABLE OF ts_ekpo ,  " for ekpo-knttp is intial     "#EC NEEDED
       gt_rbsellifs   TYPE STANDARD TABLE OF ts_rbsellifs , "#EC NEEDED
       gt_rbsellifs_1 TYPE STANDARD TABLE OF ts_rbsellifs , "#EC NEEDED
       gt_rbco        TYPE STANDARD TABLE OF ts_rbco ,      "#EC NEEDED
       gt_rbco_1      TYPE STANDARD TABLE OF ts_rbco , "rbco kostl is initial  "#EC NEEDED
       gt_rbco_2      TYPE STANDARD TABLE OF ts_rbco , "rbco kostl is initial  "#EC NEEDED
       gt_rbdrseg     TYPE STANDARD TABLE OF ts_rbdrseg ,   "#EC NEEDED
       gt_rbselbest   TYPE STANDARD TABLE OF ts_rbselbest,  "#EC NEEDED
       gt_rbselbest_1 TYPE STANDARD TABLE OF ts_rbselbest,  "#EC NEEDED
       gt_aufk        TYPE STANDARD TABLE OF ts_aufk ,      "#EC NEEDED
       gt_t001s       TYPE STANDARD TABLE OF ts_t001s .     "#EC NEEDED
*       gt_log_success TYPE STANDARD TABLE OF /dcr/cbfc_mm_npt_inv_log , "#EC NEEDED
*       gt_log_failed  TYPE STANDARD TABLE OF /dcr/cbfc_mm_npt_inv_log , "#EC NEEDED
*       gt_update      TYPE STANDARD TABLE OF /dcr/cbfc_mm_npt_inv,
*       gt_config      TYPE /dcr/cbfc_config_tt.             "#EC NEEDED
*       gt_rbstat      TYPE STANDARD TABLE OF /dcr/cbfc_config-value.
TYPES: BEGIN OF ts_rbstat_range,
         sign(1),
         option(2) ,
         low       TYPE rbstat,
         high      TYPE rbstat,
       END OF ts_rbstat_range.
DATA: gt_rbstat_range TYPE STANDARD TABLE OF ts_rbstat_range.

FIELD-SYMBOLS :
  <gs_final>       TYPE ekpo ,                              "#EC NEEDED
  <gs_rbkp>        TYPE ts_rbkp ,                           "#EC NEEDED
  <gs_lfa1>        TYPE ts_lfa1 ,                           "#EC NEEDED
  <gs_lfb1>        TYPE ts_lfb1 ,                           "#EC NEEDED
  <gs_ekkn>        TYPE ts_ekkn ,                           "#EC NEEDED
  <gs_ekko>        TYPE ts_ekko ,                           "#EC NEEDED
  <gs_ekpo>        TYPE ts_ekpo ,                           "#EC NEEDED
  <gs_ekpo_1>      TYPE ts_ekpo,                            "#EC NEEDED
  <gs_ekpo_2>      TYPE ts_ekpo,                            "#EC NEEDED
  <gs_rbsellifs>   TYPE ts_rbsellifs ,                      "#EC NEEDED
  <gs_rbsellifs_1> TYPE ts_rbsellifs ,                      "#EC NEEDED
  <gs_rbco>        TYPE ts_rbco ,                           "#EC NEEDED
  <gs_rbco_1>      TYPE ts_rbco ,                           "#EC NEEDED
  <gs_rbco_2>      TYPE ts_rbco ,                           "#EC NEEDED
  <gs_rbdrseg>     TYPE ts_rbdrseg ,                        "#EC NEEDED
  <gs_rbselbest>   TYPE ts_rbselbest ,                      "#EC NEEDED
  <gs_rbselbest_1> TYPE ts_rbselbest ,                      "#EC NEEDED
  <gs_aufk>        TYPE ts_aufk ,                           "#EC NEEDED
  <gs_t001s>       TYPE ts_t001s .                          "#EC NEEDED
**                <gs_log_s>       TYPE /dcr/cbfc_mm_npt_inv_log , "#EC NEEDED
**                <gs_log_f>       TYPE /dcr/cbfc_mm_npt_inv_log , "#EC NEEDED
*                <gs_update>      TYPE /dcr/cbfc_mm_npt_inv, "#EC NEEDED
*                <gs_config>      TYPE /dcr/cbfc_config.     "#EC NEEDED


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
SELECT-OPTIONS: s_belnr FOR  gv_belnr .
PARAMETERS :    p_gjahr TYPE gjahr DEFAULT sy-datum(4) OBLIGATORY.
SELECT-OPTIONS: s_lifnr FOR  gv_lifnr ,
                s_bukrs FOR  gv_bukrs OBLIGATORY.
PARAMETERS :    p_busab TYPE t001s-busab.
SELECT-OPTIONS: s_bldat FOR  gv_bldat ,
                s_budat FOR  gv_budat ,
                s_blart FOR  gv_blart,
                s_bktxt FOR  gv_bktxt ,
                s_sgtxt FOR  gv_sgtxt ,
                s_xblnr FOR  gv_xblnr .
*                s_rbstat FOR gv_rbstat.
SELECTION-SCREEN END OF BLOCK b1.
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
SELECT-OPTIONS: s_ebeln FOR  gv_ebeln ,
                s_bsart FOR  gv_bsart .
PARAMETERS:     p_wepos TYPE wepos .
SELECT-OPTIONS: s_knttp FOR  gv_knttp ,
                s_kostl FOR  gv_kostl ,
                s_aufnr FOR  gv_aufnr .
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-s03.
PARAMETERS: p_po AS CHECKBOX DEFAULT c_x,
            p_dn AS CHECKBOX,
            p_wr AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b3.


SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-s04.
SELECT-OPTIONS: s_rmwwr FOR  gv_rmwwr .
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-s05.
PARAMETERS:     p_upd   AS CHECKBOX USER-COMMAND check_upd.
SELECTION-SCREEN END OF BLOCK b5.

AT SELECTION-SCREEN.
  CLEAR : gv_ucomm .
  gv_ucomm = sy-ucomm .
  CASE gv_ucomm.
    WHEN 'BCG'.
***Perform to initialize the screen according to the requirement
      PERFORM sjob_screen_mod.
    WHEN 'ONLI'.
**Perform validations
      PERFORM validations.
  ENDCASE.


AT SELECTION-SCREEN OUTPUT.
***Perform to initialize the screen according to the requirement
  PERFORM initialize_screen.


AT SELECTION-SCREEN ON s_bukrs.
  IF s_bukrs IS INITIAL OR p_gjahr  IS INITIAL .
    MESSAGE e009(yt).
    LEAVE LIST-PROCESSING.
  ELSE.
    PERFORM validate_company_code.
  ENDIF.

AT SELECTION-SCREEN ON s_rmwwr.
  IF p_upd = c_x AND s_rmwwr IS INITIAL.
    MESSAGE e011(/dcr/cbfc_mm_invstat) DISPLAY LIKE c_inf.
    LEAVE LIST-PROCESSING.
  ENDIF.



START-OF-SELECTION.

**Perform to accumulate all the data before displaying
  PERFORM gather_data.
***Perform to update table and generate log.
  IF gv_ucomm = c_ucomm OR sy-batch = c_x.
    PERFORM prepare_tab_for_update.
    PERFORM update_table.
    PERFORM update_log_spool.
  ELSE.
**Perform to display in foreground
    PERFORM display_alv.
  ENDIF.




FORM sjob_screen_mod .

  LOOP AT SCREEN .
    CASE screen-name .
      WHEN c_po .
        p_po = c_x.
      WHEN c_dn OR c_wr.
        p_dn = p_wr = c_space.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.


ENDFORM.                    " SJOB_SCREEN_MOD
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize_screen .

*** Enabling and disabling the screen according to the requirement .
*** Comapny code currency will be disabled initially and will be active
*** when UPdate table check box is active
  LOOP AT SCREEN .
    IF p_upd = c_x.
*** clear all the values of the other fields when P_UPD is selected.
      REFRESH: s_belnr , s_lifnr , s_bldat , s_budat , s_bsart ,s_sgtxt , s_xblnr , s_ebeln , s_bsart ,s_blart , s_knttp , s_kostl , s_aufnr.
      CLEAR: p_busab , p_dn , p_wr.
      IF screen-name = c_rmwwr_l OR screen-name = c_rmwwr_h OR screen-name = c_gjahr
                                OR screen-name = c_bukrs_l OR screen-name = c_bukrs_h OR screen-name = 'P_UPD' .
        screen-input = c_1 .
*        IF screen-name = c_rmwwr_l OR screen-name = c_rmwwr_h .
**          screen-required = c_1.
*        ENDIF.
      ELSE.
        screen-input = c_0 .
      ENDIF.
      MODIFY SCREEN.
    ELSEIF screen-name = c_rmwwr_l OR screen-name = c_rmwwr_h.
      screen-input = c_0.
    ELSE.
      screen-input = c_1.
      REFRESH: s_rmwwr.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " INITIALIZE_SCREEN
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_COMPANY_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_company_code .
* Local data decleration
  DATA: lt_bukrs TYPE STANDARD TABLE OF ts_bukrs,
        ls_bukrs TYPE                   ts_bukrs.

  SELECT bukrs FROM t001
    INTO TABLE lt_bukrs
      WHERE bukrs IN s_bukrs.
  IF sy-subrc <> 0 AND s_bukrs IS NOT INITIAL.
* Enter valid Company code
    MESSAGE e004(/dcr/cbfc_mm_invstat) .
  ENDIF.


*Authority check on Company code
  LOOP AT lt_bukrs INTO ls_bukrs.
* Authority check on Company code
    AUTHORITY-CHECK OBJECT c_authobj
                    ID c_bukrs FIELD ls_bukrs-bukrs
                    ID c_actvt FIELD c_02.

    IF sy-subrc <> 0.
* No Authorization for Company code &1
      MESSAGE e001(ytest) WITH ls_bukrs-bukrs.
      LEAVE LIST-PROCESSING .
    ENDIF.
  ENDLOOP.
ENDFORM.                    " VALIDATE_COMPANY_CODE
*&---------------------------------------------------------------------*
*&      Form  VALIDATIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validations .
*** check if the user is trying to run in foreground
*** when the update is checked
  IF  p_upd = c_x AND sy-batch <> c_x.
    MESSAGE e009(/dcr/cbfc_mm_invstat) DISPLAY LIKE c_inf.
    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM.                    " VALIDATIONS
*&---------------------------------------------------------------------*
*&      Form  GATHER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gather_data .
*** Select from the cbFC config  table first.
  PERFORM select_config_tab.

  BREAK-POINT.

  IF p_busab IS NOT INITIAL.
    PERFORM get_data_basedon_clerk.
***Prepare final table
    PERFORM prepare_final_tab.
*** If PO block data is not initial
  ELSEIF s_ebeln IS NOT INITIAL OR s_bsart IS NOT INITIAL OR s_knttp IS NOT INITIAL OR s_kostl IS NOT INITIAL OR s_aufnr IS NOT INITIAL.
    PERFORM get_data_basedon_po.
***Prepare final table
    PERFORM prepare_final_tab.
  ELSE.
*    IF s_ebeln IS INITIAL AND s_bsart IS INITIAL AND s_knttp IS INITIAL AND s_kostl IS INITIAL AND s_aufnr IS INITIAL.
***If accounting CLerk is initial
    PERFORM get_data_basedon_invoices.
***Prepare final table
    PERFORM prepare_final_tab.
  ENDIF.

**If the GR'ed item is not needed .
  IF p_wepos IS NOT INITIAL.
    DELETE gt_final WHERE wepos = c_x.
  ENDIF.

  IF gt_final IS INITIAL.
    MESSAGE i012(/dcr/cbfc_mm_invstat) DISPLAY LIKE c_inf.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.                    " GATHER_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
* Local data Declerations
  DATA: lt_fcat    TYPE slis_t_fieldcat_alv,
        ls_layout  TYPE slis_layout_alv,
        ls_variant TYPE disvariant.
  DATA: lv_line  TYPE char10,
        lv_count TYPE i.


  IF ls_variant IS INITIAL.
    ls_variant-report  = sy-repid.
*    ls_variant-variant = p_alvvar.
  ENDIF.

* Build field catalog
  PERFORM build_field_catalog CHANGING lt_fcat.

* Layout settings
*  ls_layout-box_fieldname = c_flag.


  DESCRIBE TABLE gt_final LINES lv_count.

  lv_line = lv_count.
  CONDENSE lv_line.
  CONCATENATE sy-title '(' lv_line ')' INTO sy-title SEPARATED BY space.

  IF sy-batch = c_x OR gv_ucomm = c_ucomm.

*    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*      EXPORTING
*        i_callback_program       = c_prog_name
*        i_callback_pf_status_set = c_form_status
*        i_callback_user_command  = c_form_command
*        it_fieldcat              = lt_fcat
*        is_layout                = ls_layout
*        is_variant               = ls_variant
*      TABLES
*        t_outtab                 = gt_log_success.

  ELSE.
* Display ALV in Grid format
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = c_prog_name
        i_callback_pf_status_set = c_form_status
        i_callback_user_command  = c_form_command
        it_fieldcat              = lt_fcat
        is_layout                = ls_layout
        is_variant               = ls_variant
      TABLES
        t_outtab                 = gt_final.
  ENDIF.

ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_FCAT  text
*----------------------------------------------------------------------*
FORM build_field_catalog  CHANGING xt_fcat TYPE slis_t_fieldcat_alv.
* local data declerations
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv.

  REFRESH: lt_fieldcat.

  IF sy-batch = c_x OR gv_ucomm = c_ucomm.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = c_prog_name
        i_structure_name       = c_tab_log
      CHANGING
        ct_fieldcat            = lt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
  ELSE.
*Build Field catalog with existing structure
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = c_prog_name
        i_structure_name       = c_tab_name
      CHANGING
        ct_fieldcat            = lt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

  ENDIF.
  IF sy-subrc EQ 0.
    xt_fcat[] = lt_fieldcat[].
  ENDIF.


  IF sy-batch <> c_x OR gv_ucomm <> c_ucomm.
** Modify required fields
    LOOP AT xt_fcat INTO ls_fieldcat.
      CASE ls_fieldcat-fieldname.
        WHEN c_belnr.
          ls_fieldcat-hotspot   = 'X'.
          MODIFY xt_fcat FROM ls_fieldcat.
        WHEN c_ebeln.
          ls_fieldcat-hotspot = 'X'.
          MODIFY xt_fcat FROM ls_fieldcat.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_BASEDON_CLERK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_basedon_clerk .
  SELECT lifnr bukrs busab FROM lfb1  INTO TABLE gt_lfb1
                           WHERE lifnr IN s_lifnr AND bukrs IN s_bukrs AND  busab = p_busab.

  IF sy-subrc = 0 .
    IF NOT gt_lfb1 IS INITIAL.

      SELECT belnr gjahr bukrs lifnr bldat budat blart bktxt rbstat rmwwr waers kursf
              FROM rbkp INTO TABLE gt_rbkp
              FOR ALL ENTRIES IN gt_lfb1
              WHERE belnr IN s_belnr AND gjahr = p_gjahr
              AND blart IN s_blart   AND bldat IN s_bldat
              AND budat IN s_budat  AND xblnr IN s_xblnr
              AND bukrs IN s_bukrs AND lifnr = gt_lfb1-lifnr
           "AND rmwwr IN s_rmwwr
              AND xrech = c_x AND bktxt IN s_bktxt  AND sgtxt IN s_sgtxt .
*rbstat NOT IN (c_2 , c_4 , c_5 )
*** Delete entries containing RBSTAT 2,4,6 retrived fron Config table
      IF gt_rbstat_range IS NOT INITIAL.
        DELETE gt_rbkp WHERE rbstat IN gt_rbstat_range.
      ENDIF.

      IF gt_rbkp IS NOT INITIAL.
        SELECT lifnr name1 FROM lfa1 INTO TABLE gt_lfa1 FOR ALL ENTRIES IN gt_rbkp WHERE lifnr = gt_rbkp-lifnr.
        PERFORM commom_select_process.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE i005(/dcr/cbfc_mm_invstat) DISPLAY LIKE c_inf.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.                    " GET_DATA_BASEDON_CLERK
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_BASEDON_INVOICES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_basedon_invoices .

  SELECT belnr gjahr bukrs lifnr bldat budat blart bktxt rbstat rmwwr waers kursf FROM rbkp INTO TABLE gt_rbkp
      WHERE belnr IN s_belnr AND gjahr = p_gjahr AND blart IN s_blart   AND bldat IN s_bldat
              AND budat IN s_budat  AND xblnr IN s_xblnr AND bukrs IN s_bukrs AND lifnr IN s_lifnr
*              AND rmwwr IN s_rmwwr
              AND xrech = c_x AND bktxt IN s_bktxt  AND sgtxt IN s_sgtxt .
*rbstat NOT IN (c_2 , c_4 , c_5 )
*** Delete entries containing RBSTAT 2,4,6 retrived fron Config table
  IF gt_rbstat_range IS NOT INITIAL.
    DELETE gt_rbkp WHERE rbstat IN gt_rbstat_range.
  ENDIF.
  IF gt_rbkp IS NOT INITIAL.
    SELECT lifnr name1 FROM lfa1 INTO TABLE gt_lfa1 FOR ALL ENTRIES IN gt_rbkp WHERE lifnr = gt_rbkp-lifnr.
*** Fetch aacounting clerk
    SELECT lifnr bukrs busab FROM lfb1  INTO TABLE gt_lfb1 FOR ALL ENTRIES IN gt_rbkp
    WHERE lifnr = gt_rbkp-lifnr AND bukrs IN s_bukrs.
** After this point the select queries for all the cases are same
    PERFORM commom_select_process.
  ENDIF.
ENDFORM.                    " GET_DATA_BASEDON_INVOICES
*&---------------------------------------------------------------------*
*&      Form  PREPARE_FINAL_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_final_tab .

  SORT gt_rbsellifs BY belnr ASCENDING.
  SORT gt_rbselbest BY belnr gjahr ebeln ebelp ASCENDING.

  SORT gt_ekko BY ebeln ASCENDING.
  SORT gt_ekpo BY ebeln ebelp ASCENDING.
  SORT gt_ekkn BY ebeln ebelp ASCENDING.


  SORT gt_aufk BY aufnr ASCENDING.

  SORT gt_rbco_1 BY belnr ASCENDING .
  SORT gt_rbco_2 BY belnr ASCENDING.

  SORT gt_rbdrseg BY rblnr rjahr ASCENDING.


**Prepare the final table
  LOOP AT gt_rbkp ASSIGNING <gs_rbkp>.
    APPEND INITIAL LINE TO gt_final ASSIGNING <gs_final> .
    MOVE-CORRESPONDING <gs_rbkp> TO <gs_final>.

*** The busab this way because if needed the text of busab can be added later.
*** As Busab is a 2 char field in output , hence updating only the code.
    READ TABLE gt_lfb1 ASSIGNING <gs_lfb1> WITH KEY lifnr = <gs_rbkp>-lifnr .
    IF sy-subrc = 0 .
*      READ TABLE gt_t001s ASSIGNING <GS_t001s> WITH KEY busab = <GS_lfb1>-busab .
*      IF sy-subrc = 0 .
*      <gs_final>-busab = <gs_lfb1>-busab.
*      ENDIF.
    ENDIF.
    READ TABLE gt_lfa1 ASSIGNING <gs_lfa1> WITH KEY lifnr = <gs_rbkp>-lifnr .
    IF sy-subrc = 0 .
*      <gs_final>-name1 = <gs_lfa1>-name1.
    ENDIF.

***Invoices with Delivery Note assignment
    READ TABLE gt_rbsellifs ASSIGNING <gs_rbsellifs> WITH KEY belnr = <gs_rbkp>-belnr BINARY SEARCH.
    IF sy-subrc = 0 .
*      <gs_final>-lfsnr = <gs_rbsellifs>-lfsnr .
    ENDIF.

**In case  EBELP = INITIAL in RBSELBEST (Invoice not assigned to a particular PO item),
**then the first item of the PO has to be taken for the further processing, provided EKPO–EREKZ
**is not active (Final invoice indicator is not set).
**If EKPO – EREKZ is active for this item, take next item where EKPO – EREKZ is not active
    READ TABLE gt_rbselbest ASSIGNING <gs_rbselbest> WITH KEY belnr = <gs_rbkp>-belnr gjahr = <gs_rbkp>-gjahr  BINARY SEARCH..
    IF sy-subrc = 0 .
      <gs_final>-ebeln = <gs_rbselbest>-ebeln.
      READ TABLE gt_ekko ASSIGNING <gs_ekko> WITH KEY ebeln = <gs_final>-ebeln BINARY SEARCH.
      IF sy-subrc = 0 .
*        <gs_final>-bsart = <gs_ekko>-bsart.
      ENDIF.
*      IF <GS_final>-ebelp IS INITIAL.
      READ TABLE gt_ekpo ASSIGNING <gs_ekpo> WITH KEY ebeln = <gs_final>-ebeln BINARY SEARCH.
      IF sy-subrc = 0 .
        <gs_final>-ebelp = <gs_ekpo>-ebelp.
        <gs_final>-matnr = <gs_ekpo>-matnr.
        <gs_final>-wepos = <gs_ekpo>-wepos.
        <gs_final>-knttp = <gs_ekpo>-knttp.
        <gs_final>-txz01 = <gs_ekpo>-txz01.
**If EKPO – KNTTP is not initial read from EKKN
        IF <gs_ekpo>-knttp IS NOT INITIAL .
          READ TABLE gt_ekkn ASSIGNING <gs_ekkn> WITH KEY ebeln = <gs_ekpo>-ebeln ebelp = <gs_ekpo>-ebelp BINARY SEARCH.
          IF sy-subrc = 0 .
*            <gs_final>-sakto = <gs_ekkn>-sakto. "GL Account in TC it is mentioned SAKNR
*            <gs_final>-kostl = <gs_ekkn>-kostl. " Cost center from EKKN table
*            IF <gs_final>-kostl IS INITIAL.
            READ TABLE gt_aufk ASSIGNING <gs_aufk> WITH KEY aufnr = <gs_ekkn>-aufnr BINARY SEARCH .
            IF sy-subrc = 0 .
**                <gs_final>-aufnr = <gs_aufk>-aufnr.
*                <gs_final>-kostl = <gs_aufk>-kostv.
            ENDIF.
*            ENDIF.
          ENDIF.
        ELSE.
**If EKPO – KNTTP is initial, read from RBCO
          READ TABLE gt_rbco_2 ASSIGNING <gs_rbco_2> WITH KEY belnr = <gs_rbkp>-belnr gjahr = <gs_rbkp>-gjahr BINARY SEARCH.
          IF sy-subrc = 0 .
*            <gs_final>-sakto = <gs_rbco_2>-saknr.
*            <gs_final>-kostl = <gs_rbco_2>-kostl.
*            IF <gs_final>-kostl IS INITIAL.
            READ TABLE gt_aufk ASSIGNING <gs_aufk> WITH KEY aufnr = <gs_rbco_2>-aufnr BINARY SEARCH .
            IF sy-subrc = 0 .
*                <gs_final>-aufnr = <gs_aufk>-aufnr.
*                <gs_final>-kostl = <gs_aufk>-kostv.
            ENDIF.
*            ENDIF.
          ENDIF.

        ENDIF.

      ENDIF.
*      ENDIF.
    ENDIF.

**If ‘Invoices w/o reference document’ is selected on the selection screen
    IF p_wr IS NOT INITIAL .
      READ TABLE gt_rbdrseg ASSIGNING <gs_rbdrseg> WITH KEY rblnr = <gs_rbkp>-belnr rjahr = <gs_rbkp>-gjahr BINARY SEARCH.
      IF sy-subrc = 0 .
        UNASSIGN : <gs_ekpo> , <gs_ekko>.
        READ TABLE gt_ekko ASSIGNING  <gs_ekko> WITH KEY ebeln = <gs_rbdrseg>-ebeln .
        IF sy-subrc = 0 .
*          <gs_final>-bsart = <gs_ekko>-bsart .
        ENDIF.
        READ TABLE gt_ekpo ASSIGNING <gs_ekpo> WITH KEY ebeln = <gs_final>-ebeln BINARY SEARCH.
        IF sy-subrc = 0 .
          <gs_final>-ebelp = <gs_ekpo>-ebelp.
          <gs_final>-matnr = <gs_ekpo>-matnr.
          <gs_final>-wepos = <gs_ekpo>-wepos.
          <gs_final>-knttp = <gs_ekpo>-knttp.
          <gs_final>-txz01 = <gs_ekpo>-txz01.
        ENDIF.
      ENDIF.
      READ TABLE gt_rbco_2 ASSIGNING <gs_rbco_1> WITH KEY belnr = <gs_rbkp>-belnr gjahr = <gs_rbkp>-gjahr BINARY SEARCH.
      IF sy-subrc = 0 .
*        <gs_final>-sakto = <gs_rbco_1>-saknr.
*        <gs_final>-kostl = <gs_rbco_1>-kostl.
*        IF <gs_final>-kostl IS INITIAL.
        READ TABLE gt_aufk ASSIGNING <gs_aufk> WITH KEY aufnr = <gs_rbco_1>-aufnr BINARY SEARCH .
        IF sy-subrc = 0 .
*            <gs_final>-aufnr = <gs_aufk>-aufnr.
*            <gs_final>-kostl = <gs_aufk>-kostv.
        ENDIF.
*        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

**Delete all the invoices when the delivery note invoices are not checked
**THis is where the del. note invoices are filtered depending on the check-box
  IF p_dn IS INITIAL .
*    DELETE gt_final WHERE lfsnr <> space.
  ENDIF.

ENDFORM.                    " PREPARE_FINAL_TAB
FORM set_pf_status                                          "#EC NEEDED
   USING it_extab TYPE tt_extab.                            "#EC CALLED

  SET PF-STATUS c_standard OF PROGRAM '/DCR/CBFC_MM_INV_STAT_REP'.

ENDFORM.                    " SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       User Command at Output
*----------------------------------------------------------------------*
*  -->  IV_UCOMM       User Command
*  <--  IS_SELFIELD    Selected content
*----------------------------------------------------------------------*
FORM user_command USING iv_ucomm    TYPE syucomm            "#EC CALLED
                        is_selfield TYPE slis_selfield.     "#EC NEEDED

  DATA: lo_grid  TYPE REF TO cl_gui_alv_grid,               "#EC NEEDED
        lt_index TYPE        lvc_t_row,                     "#EC NEEDED
        lv_lines TYPE        i,                             "#EC NEEDED
        ls_index TYPE        lvc_s_row,                     "#EC NEEDED
        ls_final TYPE        ekpo.                          "#EC NEEDED

  DATA: lv_bstyp TYPE char1.                                "#EC NEEDED
  DATA: lv_mjahr TYPE mjahr,                                "#EC NEEDED
        lv_mblnr TYPE mblnr,                                "#EC NEEDED
        lv_zeile TYPE mblpo.                                "#EC NEEDED

* check if the line is selected on the alv output
  CASE iv_ucomm.
    WHEN c_display OR c_ic1.

*.. Call FM to get the reference of the changed data in the Grid
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = lo_grid.

*.. Updated data will be availble after calling the below method.
      IF lo_grid IS NOT INITIAL.

        CALL METHOD lo_grid->check_changed_data( ).
* Get the selected row
        CALL METHOD lo_grid->get_selected_rows
          IMPORTING
            et_index_rows = lt_index.
* Get the no. of lines selected
        DESCRIBE TABLE lt_index LINES lv_lines.
* If user Selects PO document
        IF  is_selfield-fieldname = c_ebeln.

          CLEAR lv_bstyp.
          SELECT SINGLE bstyp FROM ekko INTO lv_bstyp
                 WHERE ebeln = is_selfield-value.
* If PO is a Scheduling agreement
          IF sy-subrc = 0.
            IF lv_bstyp = 'L'.
              SET PARAMETER ID 'SAG' FIELD is_selfield-value. "#EC CI_USE_WANTED
              CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN. "#EC CI_CALLTA.
* If PO is purchase order/Contract
            ELSE.
              SET PARAMETER ID 'BES' FIELD is_selfield-value. "#EC CI_USE_WANTED
              CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN. ""#EC CI_CALLTA.
            ENDIF.
          ENDIF.
* If user selected Invoice document
        ELSEIF  is_selfield-fieldname = c_belnr."MIR4 for invoice

          SET PARAMETER ID 'RBN' FIELD is_selfield-value. "#EC CI_USE_WANTED
          CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.  ""#EC CI_CALLTA.
*
        ENDIF.

      ENDIF.
  ENDCASE.

ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  COMMOM_SELECT_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM commom_select_process .
***Get the PO assigned to invoices from rbselbest table according to the selection criteria
  IF NOT gt_rbkp IS INITIAL AND sy-subrc = 0.
    SELECT belnr gjahr ebeln ebelp FROM rbselbest INTO TABLE gt_rbselbest FOR ALL ENTRIES IN gt_rbkp
      WHERE belnr = gt_rbkp-belnr AND gjahr = gt_rbkp-gjahr.
  ENDIF .

  BREAK-POINT.

  IF gt_rbselbest IS NOT INITIAL AND sy-subrc = 0 .
***Fetch the header data of PO's
    SELECT ebeln bsart FROM ekko INTO TABLE gt_ekko FOR ALL ENTRIES IN gt_rbselbest WHERE ebeln = gt_rbselbest-ebeln AND bsart IN s_bsart.

***Fetch the item data of PO where final invoice indicator is not set
    SELECT ebeln ebelp wepos knttp matnr txz01 FROM ekpo INTO TABLE gt_ekpo FOR ALL ENTRIES IN gt_rbselbest WHERE ebeln = gt_rbselbest-ebeln
     AND erekz = space.
    IF sy-subrc = 0 .
***Prepare a temporary table and delete all the values of KNTTP not equal space and then select with internal order field from AUFK
      gt_ekpo_1[] = gt_ekpo[] .
      SORT gt_ekpo_1 BY ebeln ebelp .
**If EKPO – KNTTP is NOT INITIAL,
**query table EKKN to fetch account assignment data (EBELN, EBELP).
**Read SAKNR – G/L account, KOSTL – Cost Center and AUFNR - Internal Order.
      DELETE gt_ekpo_1 WHERE knttp = space.
      IF NOT gt_ekpo_1 IS INITIAL.
        SELECT  ebeln ebelp sakto kostl aufnr FROM ekkn INTO TABLE gt_ekkn FOR ALL ENTRIES IN gt_ekpo_1
         WHERE ebeln = gt_ekpo_1-ebeln AND ebelp = gt_ekpo_1-ebelp .
      ENDIF.
      IF sy-subrc = 0 .
        SELECT aufnr kostv INTO TABLE gt_aufk FROM  aufk FOR ALL ENTRIES IN gt_ekkn WHERE aufnr = gt_ekkn-aufnr .
      ENDIF.
***If EKPO – KNTTP IS INITIAL, account assignment data should be read from table RBCO
***with RBKP – BELNR invoice doc number, RBKP – GJAHR invoice doc year.
***Account assignment should be read from the first item found in RBCO.
      gt_ekpo_2[] = gt_ekpo[] .
      DELETE gt_ekpo_2 WHERE knttp <> space.
**Since RBCO does not have PO/ PO item fields we need to find the BELNR again tracing back in the tables rbselbest & rbsellifs
**Matching the PO with the tables
**gt_ekpo_2 now holds all the items where KNTTP is initial
      LOOP AT gt_ekpo_2 ASSIGNING <gs_ekpo_2> .
        APPEND INITIAL LINE TO gt_rbselbest_1 ASSIGNING <gs_rbselbest_1>.
        READ TABLE gt_rbselbest ASSIGNING <gs_rbselbest> WITH KEY  ebeln = <gs_ekpo_2>-ebeln BINARY SEARCH.
        IF sy-subrc = 0 .
          MOVE-CORRESPONDING <gs_rbselbest> TO <gs_rbselbest_1>.
        ENDIF.
      ENDLOOP.
**Selecting all values from RBCO where related BELNR PO's KNTTP is intial
      IF  gt_rbselbest_1 IS NOT INITIAL.
        SELECT   belnr gjahr saknr aufnr kostl FROM rbco INTO TABLE gt_rbco FOR ALL ENTRIES IN gt_rbselbest_1
         WHERE belnr = gt_rbselbest_1-belnr AND gjahr = gt_rbselbest_1-gjahr.
        IF sy-subrc = 0 .
***delete all values where KOSTL in RBCO is not intial for case KNTTP is initial
          gt_rbco_2[] = gt_rbco[] .
          DELETE gt_rbco_2 WHERE kostl IS NOT INITIAL .
          SELECT aufnr kostv APPENDING TABLE gt_aufk FROM  aufk FOR ALL ENTRIES IN gt_rbco_2 WHERE aufnr = gt_rbco_2-aufnr .
        ENDIF.

      ENDIF.

    ENDIF.
  ENDIF.

**Invoices with Delivery Note assignment
**AND ( gv_ucomm <> c_ucomm or sy-batch  <> c_x )
*  IF p_dn IS NOT INITIAL AND gt_rbkp IS NOT INITIAL.
*** The leftover invoices which do not have PO assigment from RBKP table is searched in rbsellifs
*** And the delivery note no is assigned , this Del. note no will be useful in deleting from the
*** Final table by the condition del.note is not initial
  IF gt_rbkp IS NOT INITIAL .
    SELECT belnr gjahr lfsnr FROM rbsellifs INTO TABLE gt_rbsellifs FOR ALL ENTRIES IN gt_rbkp
    WHERE belnr = gt_rbkp-belnr AND gjahr = gt_rbkp-gjahr .

  ENDIF.

**Invoices w/o reference document
  IF p_wr IS NOT INITIAL AND gt_rbkp IS NOT INITIAL.
    SELECT rblnr rjahr ebeln ebelp knttp  FROM rbdrseg INTO TABLE gt_rbdrseg FOR ALL ENTRIES IN gt_rbkp
    WHERE rblnr = gt_rbkp-belnr AND gjahr = gt_rbkp-gjahr.
    IF sy-subrc <> 0 .
      SELECT   belnr gjahr saknr aufnr kostl FROM rbco APPENDING  TABLE gt_rbco FOR ALL ENTRIES IN gt_rbkp
      WHERE belnr = gt_rbkp-belnr AND gjahr = gt_rbkp-gjahr.

      IF sy-subrc = 0 .
***delete all values where KOSTL in RBCO is not intial
*** In case of Cost center the AUFNR will be not present
*** So deleting lines with cost center and directly comparing with the aufk table with aufnr
        gt_rbco_1[] = gt_rbco[] .
        DELETE gt_rbco_1 WHERE kostl IS NOT INITIAL .
        SELECT aufnr kostv APPENDING TABLE gt_aufk FROM  aufk FOR ALL ENTRIES IN gt_rbco_1 WHERE aufnr = gt_rbco_1-aufnr .

      ENDIF.
    ENDIF.
    IF sy-subrc = 0 .
***Fetch the header data of PO's
      SELECT ebeln bsart FROM ekko APPENDING TABLE gt_ekko FOR ALL ENTRIES IN gt_rbdrseg WHERE ebeln = gt_rbdrseg-ebeln AND bsart IN s_bsart .
***Fetch the item data of PO where final invoice indicator is not set
      SELECT ebeln ebelp wepos knttp matnr txz01 FROM ekpo APPENDING TABLE gt_ekpo FOR ALL ENTRIES IN gt_rbdrseg WHERE ebeln = gt_rbdrseg-ebeln
        AND erekz = space.
    ENDIF.
  ENDIF.

ENDFORM.                    " COMMOM_SELECT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_BASEDON_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_basedon_po .

  IF s_ebeln IS NOT INITIAL OR s_bsart IS NOT INITIAL .
***Fetch the header data of PO's
    SELECT ebeln bsart FROM ekko INTO TABLE gt_ekko  WHERE ebeln IN s_ebeln AND bsart IN s_bsart AND bukrs IN s_bukrs.

    IF sy-subrc = 0 AND gt_ekko IS NOT INITIAL.
***Fetch the item data of PO where final invoice indicator is not set
      SELECT ebeln ebelp wepos knttp matnr txz01 FROM ekpo INTO TABLE gt_ekpo FOR ALL ENTRIES IN gt_ekko WHERE ebeln = gt_ekko-ebeln
       AND bukrs IN s_bukrs AND knttp IN s_knttp AND  erekz = space.
    ENDIF.
***Fetch the invoices from the table rbselbest
    IF sy-subrc = 0 AND gt_ekpo IS NOT INITIAL.
      SELECT belnr gjahr ebeln ebelp FROM rbselbest INTO TABLE gt_rbselbest FOR ALL ENTRIES IN gt_ekpo
      WHERE  gjahr = p_gjahr AND  ebeln = gt_ekpo-ebeln .
*** Fetch data from RBKP when the invoices are derived
      IF NOT gt_rbselbest IS INITIAL AND sy-subrc = 0 .
***Use the cbfc config table for the constants
        SELECT belnr gjahr bukrs lifnr bldat budat blart bktxt rbstat rmwwr waers kursf FROM rbkp INTO TABLE gt_rbkp
          FOR ALL ENTRIES IN gt_rbselbest WHERE belnr = gt_rbselbest-belnr AND gjahr = gt_rbselbest-gjahr
          AND xrech = c_x .
*rbstat NOT IN (c_2 , c_4 , c_5 )
*** Delete entries containing RBSTAT 2,4,6 retrieved fron Config table
        IF gt_rbstat_range IS NOT INITIAL.
          DELETE gt_rbkp WHERE rbstat IN gt_rbstat_range.
        ENDIF.

***Get the account assigment for the invoices fetched from the PO
        SELECT  ebeln ebelp sakto kostl aufnr FROM ekkn INTO TABLE gt_ekkn FOR ALL ENTRIES IN gt_ekpo
         WHERE ebeln = gt_ekpo-ebeln AND ebelp = gt_ekpo-ebelp .
*** Get the internal order data when cost center is initial in EKKN
        IF NOT gt_ekkn IS INITIAL AND sy-subrc = 0.
          SORT gt_ekkn BY aufnr DESCENDING.
          SELECT aufnr kostv INTO TABLE gt_aufk FROM  aufk FOR ALL ENTRIES IN gt_ekkn WHERE aufnr = gt_ekkn-aufnr .
        ENDIF.
***Get invoice party name and accounting clerk
        IF NOT gt_rbkp IS INITIAL.
          SELECT lifnr name1 FROM lfa1 INTO TABLE gt_lfa1 FOR ALL ENTRIES IN gt_rbkp WHERE lifnr = gt_rbkp-lifnr.
          SELECT lifnr bukrs busab FROM lfb1  INTO TABLE gt_lfb1 FOR ALL ENTRIES IN gt_rbkp
          WHERE lifnr = gt_rbkp-lifnr AND bukrs IN s_bukrs.
        ENDIF.

*** Fetch the values for delivery note related invoices
        IF gt_rbkp IS NOT INITIAL AND p_dn IS NOT INITIAL.
          SELECT belnr gjahr lfsnr FROM rbsellifs INTO TABLE gt_rbsellifs FOR ALL ENTRIES IN gt_rbkp
          WHERE belnr = gt_rbkp-belnr AND gjahr = gt_rbkp-gjahr .
        ENDIF.
***Fetch the values for w/o reference invoices
        IF gt_rbkp IS NOT INITIAL AND p_wr IS NOT INITIAL.

          SELECT rblnr rjahr ebeln ebelp knttp  FROM rbdrseg INTO TABLE gt_rbdrseg FOR ALL ENTRIES IN gt_rbkp
           WHERE rblnr = gt_rbkp-belnr AND gjahr = gt_rbkp-gjahr.
          IF sy-subrc <> 0 .
            SELECT   belnr gjahr saknr aufnr kostl FROM rbco APPENDING  TABLE gt_rbco FOR ALL ENTRIES IN gt_rbkp
            WHERE belnr = gt_rbkp-belnr AND gjahr = gt_rbkp-gjahr.
            IF sy-subrc = 0 .
***delete all values where KOSTL in RBCO is not intial
*** In case of Cost center the AUFNR will be not present
*** So deleting lines with cost center and directly comparing with the aufk table with aufnr
              gt_rbco_1[] = gt_rbco[] .
              DELETE gt_rbco_1 WHERE kostl IS NOT INITIAL .
              SELECT aufnr kostv APPENDING TABLE gt_aufk FROM  aufk FOR ALL ENTRIES IN gt_rbco_1 WHERE aufnr = gt_rbco_1-aufnr .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.
*** if only cost center / internal order is provided in the selection screen
  ELSEIF s_kostl IS  NOT INITIAL OR s_aufnr IS NOT INITIAL.
***if the cost center is not initial then aufnr will be initial and vice-versa
    SELECT ebeln  ebelp sakto kostl aufnr  FROM ekkn INTO TABLE gt_ekkn WHERE kostl IN s_kostl AND aufnr IN s_aufnr.
    IF NOT gt_ekkn IS INITIAL AND sy-subrc = 0.
***Fetch the header data of PO's
      SELECT ebeln bsart FROM ekko INTO TABLE gt_ekko FOR ALL ENTRIES IN gt_ekkn WHERE ebeln = gt_ekkn-ebeln AND bukrs IN s_bukrs.
***Fetch the item data of PO where final invoice indicator is not set
      SELECT ebeln ebelp wepos knttp matnr txz01 FROM ekpo INTO TABLE gt_ekpo FOR ALL ENTRIES IN gt_ekkn
          WHERE ebeln = gt_ekkn-ebeln AND ebelp = gt_ekkn-ebelp AND bukrs IN s_bukrs AND knttp IN s_knttp AND  erekz = space.
***Fetch the internal order and then kostv ( cost center ) where CC is initial in EKKN
      SELECT aufnr kostv INTO TABLE gt_aufk  FROM aufk FOR ALL ENTRIES IN gt_ekkn WHERE aufnr = gt_ekkn-aufnr.
    ENDIF.
    IF gt_ekpo IS NOT INITIAL.
      SELECT belnr gjahr ebeln ebelp FROM rbselbest INTO TABLE gt_rbselbest FOR ALL ENTRIES IN gt_ekpo
      WHERE gjahr = p_gjahr AND ebeln = gt_ekpo-ebeln.
*** Fetch data from RBKP when the invoices are derived
      IF NOT gt_rbselbest IS INITIAL AND sy-subrc = 0 .
***Use the cbfc config table for the constants
        SELECT belnr gjahr bukrs lifnr bldat budat blart bktxt rbstat rmwwr waers kursf FROM rbkp INTO TABLE gt_rbkp
          FOR ALL ENTRIES IN gt_rbselbest WHERE belnr = gt_rbselbest-belnr AND gjahr = gt_rbselbest-gjahr
          AND bukrs IN s_bukrs AND xrech = c_x.
*rbstat NOT IN (c_2 , c_4 , c_5 )
*** Delete entries containing RBSTAT 2,4,6 retrived fron Config table
        IF gt_rbstat_range IS NOT INITIAL AND sy-subrc = 0.
          DELETE gt_rbkp WHERE rbstat IN gt_rbstat_range.
        ENDIF.
      ENDIF.
*** Fetch the values for delivery note related invoices and invoicing party / name
      IF gt_rbkp IS NOT INITIAL.
        IF p_dn IS NOT INITIAL.
          SELECT belnr gjahr lfsnr FROM rbsellifs INTO TABLE gt_rbsellifs FOR ALL ENTRIES IN gt_rbkp
          WHERE belnr = gt_rbkp-belnr AND gjahr = gt_rbkp-gjahr .
        ENDIF.
***Fetch the values for w/o reference invoices
        IF gt_rbkp IS NOT INITIAL AND p_wr IS NOT INITIAL.
          SELECT rblnr rjahr ebeln ebelp knttp  FROM rbdrseg INTO TABLE gt_rbdrseg FOR ALL ENTRIES IN gt_rbkp
              WHERE rblnr = gt_rbkp-belnr AND gjahr = gt_rbkp-gjahr.
          IF sy-subrc <> 0 .
            SELECT   belnr gjahr saknr aufnr kostl FROM rbco APPENDING  TABLE gt_rbco FOR ALL ENTRIES IN gt_rbkp
            WHERE belnr = gt_rbkp-belnr AND gjahr = gt_rbkp-gjahr.
            IF sy-subrc = 0 .
***delete all values where KOSTL in RBCO is not intial
*** In case of Cost center the AUFNR will be not present
*** So deleting lines with cost center and directly comparing with the aufk table with aufnr
              gt_rbco_1[] = gt_rbco[] .
              DELETE gt_rbco_1 WHERE kostl IS NOT INITIAL .
              SELECT aufnr kostv APPENDING TABLE gt_aufk FROM  aufk FOR ALL ENTRIES IN gt_rbco_1 WHERE aufnr = gt_rbco_1-aufnr .
            ENDIF.
          ENDIF.
        ENDIF.
***Get invoice party name and accounting clerk
        SELECT lifnr name1 FROM lfa1 INTO TABLE gt_lfa1 FOR ALL ENTRIES IN gt_rbkp WHERE lifnr = gt_rbkp-lifnr.
        SELECT lifnr bukrs busab FROM lfb1  INTO TABLE gt_lfb1 FOR ALL ENTRIES IN gt_rbkp
        WHERE lifnr = gt_rbkp-lifnr AND bukrs IN s_bukrs.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                    " GET_DATA_BASEDON_PO
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table .
*** Lock the table before performing deletion updation.

*** Lock object for the table
  CALL FUNCTION 'ENQUEUE_/DCR/E_MMNPT'
    EXPORTING
      mode_/dcr/cbfc_mm_npt = 'X'
      mandt                 = sy-mandt
    EXCEPTIONS
      foreign_lock          = 1
      system_failure        = 2
      OTHERS                = 3.
  IF sy-subrc = 0.
* Implement suitable error handling here
*  ENDIF.
*** Delete previous data based on company code and fiscal year
*    DELETE FROM /dcr/cbfc_mm_npt WHERE gjahr = p_gjahr AND bukrs IN s_bukrs. "#EC CI_NOFIRST

*    MODIFY /dcr/cbfc_mm_npt FROM TABLE gt_update.


    COMMIT WORK.

    CALL FUNCTION 'DEQUEUE_/DCR/E_MMNPT'
      EXPORTING
        mode_/dcr/cbfc_mm_npt = 'X'
        mandt                 = sy-mandt.
  ENDIF.

ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  PREPARE_TAB_FOR_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_tab_for_update .
  DATA: lv_date TYPE sy-datum.

  DATA: lt_temp TYPE STANDARD TABLE OF ts_temp ." WITH HEADER LINE.
  DATA: lt_t001 TYPE STANDARD TABLE OF ts_t001 ." WITH HEADER LINE.
  DATA: lv_count TYPE int4.
  DATA  lv_amount TYPE hslvt12.
  DATA  lv_rmwwr_low TYPE hslvt12.
  DATA  lv_rmwwr_high TYPE hslvt12.
  DATA: lv_temp_index TYPE sy-tabix.

  FIELD-SYMBOLS: <lv_old_curr> TYPE any,
                 <lv_new_curr> TYPE any,
*        <ls_date>     TYPE dats ,
                 <lv_amt>      TYPE any,
                 <lv_rate>     TYPE any,
                 <ls_temp>     TYPE ts_temp,
                 <ls_temp_1>   TYPE ts_temp,
                 <ls_t001>     TYPE ts_t001.

***Since only PO assingned objects are updated ,

***Select local currency for company code .
  IF NOT gt_final IS INITIAL.
    SELECT bukrs waers FROM t001 INTO TABLE lt_t001 FOR ALL ENTRIES IN gt_final WHERE bukrs = gt_final-bukrs.
  ENDIF.
*** Use the parallel cursor to get the counter values
***Collect all the values of ebeln and invoices in a temp table
  LOOP AT gt_final ASSIGNING <gs_final>.
    APPEND INITIAL LINE TO lt_temp ASSIGNING <ls_temp> .
    MOVE-CORRESPONDING <gs_final> TO <ls_temp>.
  ENDLOOP.

*** Sort the fields before processing
  SORT gt_final BY ebeln .
  SORT lt_temp  BY ebeln .

***Delete the values without PO assigment
  DELETE gt_final WHERE ebeln = space.

*** Append the values to final update table
  LOOP AT gt_final ASSIGNING <gs_final> .
*    APPEND INITIAL LINE TO gt_update ASSIGNING <gs_update>.
*    MOVE-CORRESPONDING <gs_final> TO <gs_update>.
*    <gs_update>-upddt = sy-datum .
*    CONCATENATE sy-datum sy-uzeit INTO <gs_update>-tims .
*** Parallel cursor processing to get the counter value
    READ TABLE lt_temp ASSIGNING <ls_temp> WITH KEY ebeln = <gs_final>-ebeln BINARY SEARCH.
    IF sy-subrc = 0.
      lv_temp_index = sy-tabix.
      LOOP AT lt_temp ASSIGNING <ls_temp_1> FROM lv_temp_index. "#EC CI_NESTED
        IF <ls_temp_1>-ebeln <> <gs_final>-ebeln.
          CLEAR : lv_count .
          EXIT.
        ENDIF.
        lv_count = lv_count + 1 .
*        <gs_update>-counter = lv_count.
      ENDLOOP.
    ENDIF.
*** Assign values to Pass that to FM below.
    lv_date = sy-datum. "This is passed as it is a mandatory parameter , currency translation date
*    ASSIGN <gs_final>-rmwwr TO <lv_amt>.
*    ASSIGN <gs_final>-waers TO <lv_old_curr>.
*    READ TABLE gt_rbkp ASSIGNING <gs_rbkp> WITH KEY belnr = <gs_final>-belnr.
*    IF sy-subrc = 0 .
    ASSIGN <gs_rbkp>-kursf TO <lv_rate>.
    READ TABLE lt_t001 ASSIGNING <ls_t001> WITH KEY bukrs = <gs_final>-bukrs .
    IF sy-subrc = 0 .
      ASSIGN <ls_t001>-waers TO <lv_new_curr>.
    ENDIF.
*    ENDIF.
    IF <lv_old_curr> IS ASSIGNED AND <lv_new_curr> IS  ASSIGNED AND <lv_rate> IS ASSIGNED AND lv_date IS NOT INITIAL.
*** Has to be convert to Company Code currency.
      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
          date             = lv_date
          foreign_amount   = <lv_amt>
          foreign_currency = <lv_old_curr>
          local_currency   = <lv_new_curr>
          rate             = <lv_rate>
          type_of_rate     = c_m
          read_tcurr       = c_x
        IMPORTING
          local_amount     = lv_amount
        EXCEPTIONS
          OTHERS           = 6.

      IF sy-subrc = 0.
*        <gs_update>-rmwwr = lv_amount.
      ENDIF.
    ENDIF.
  ENDLOOP.
***Delete the values without PO assigment
*  DELETE gt_final WHERE ebeln = space.
*  DELETE gt_update WHERE ebeln = space.

***Check whether the Invoice amount is within the range given in the selection screen.
*** THe update table needs to be checked and log table should be prepared
*  UNASSIGN: <gs_update> .
  CLEAR: lv_rmwwr_low , lv_rmwwr_high.
  lv_rmwwr_low = s_rmwwr-low.
  lv_rmwwr_high = s_rmwwr-high.

*  LOOP AT gt_update ASSIGNING <gs_update> .
*    IF <gs_update>-rmwwr < lv_rmwwr_low OR <gs_update>-rmwwr > lv_rmwwr_high.
*      APPEND INITIAL LINE TO gt_log_failed ASSIGNING <gs_log_f> .
*      READ TABLE gt_final ASSIGNING <gs_final> WITH KEY belnr = <gs_update>-belnr gjahr = <gs_update>-gjahr ebeln = <gs_update>-ebeln
*                                                        ebelp = <gs_update>-ebelp.
*      IF sy-subrc = 0 .
*        MOVE-CORRESPONDING <gs_final> TO <gs_log_f> .
*        <gs_log_f>-rmwwr = <gs_update>-rmwwr.
*        <gs_log_f>-message = TEXT-002.
*      ENDIF.

*    ELSE.
*      APPEND INITIAL LINE TO gt_log_success ASSIGNING <gs_log_s> .
*      READ TABLE gt_final ASSIGNING <gs_final> WITH KEY belnr = <gs_update>-belnr gjahr = <gs_update>-gjahr ebeln = <gs_update>-ebeln
*                                                     ebelp = <gs_update>-ebelp.
*      IF sy-subrc = 0 .
*        MOVE-CORRESPONDING <gs_final> TO <gs_log_s> .
*        <gs_log_s>-rmwwr = <gs_update>-rmwwr.
*        <gs_log_s>-message = TEXT-003.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

***Delete from the update table and then proceed to update .
*  DELETE gt_update WHERE rmwwr < lv_rmwwr_low .
*  DELETE gt_update WHERE rmwwr > lv_rmwwr_high .
ENDFORM.                    " PREPARE_TAB_FOR_UPDATE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_LOG_SPOOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_log_spool .
***As the structure is same append lines and then show in spool
*  APPEND LINES OF gt_log_failed TO gt_log_success.
  PERFORM display_alv.
ENDFORM.                    " UPDATE_LOG_SPOOL
*&---------------------------------------------------------------------*
*&      Form  SELECT_CONFIG_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_config_tab .
*  DATA:
*         ls_value   TYPE /dcr/cbfc_ca_multivalue_str,                  " Config record
*        lt_value   TYPE /dcr/cbfc_ca_multivalue_tbl.                  " Config table
  CONSTANTS: lc_rbstat(6) TYPE c VALUE 'RBSTAT'.

  CALL FUNCTION '/DCR/CBFC_CA_CONFIG_GET_MULTIP'                      " Get constants from config table
    EXPORTING
      program_name       = sy-repid
      constant           = lc_rbstat
      company_code       = s_bukrs-low
*    IMPORTING
*     multivalue         = lt_value
    EXCEPTIONS
      constant_not_found = 1
      OTHERS             = 2.

  IF sy-subrc NE 0.
**    Customizing entries in table /DCR/CBFC_CONFIG are missing
    MESSAGE e010(/dcr/cbfc_mm_invstat) DISPLAY LIKE c_inf.
    LEAVE LIST-PROCESSING .
  ELSE.
** To build the ranges table
    CALL FUNCTION '/DCR/CBFC_FILL_RANGE_TAB'
      EXPORTING
        iv_mode            = c_v
        iv_separator       = ','
        iv_conversion_exit = ' '
*       it_value_tab       = lt_value
        iv_i_eq_only       = c_x
      CHANGING
        xt_range_tab       = gt_rbstat_range
      EXCEPTIONS
        param_error        = 1
        value_error        = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECT_CONFIG_TAB
FORM select_config_tab1 .
*  DATA:
*         ls_value   TYPE /dcr/cbfc_ca_multivalue_str,                  " Config record
*        lt_value   TYPE /dcr/cbfc_ca_multivalue_tbl.                  " Config table
  CONSTANTS: lc_rbstat(6) TYPE c VALUE 'RBSTAT'.

  CALL FUNCTION '/DCR/CBFC_CA_CONFIG_GET_MULTIP'                      " Get constants from config table
    EXPORTING
      program_name       = sy-repid
      constant           = lc_rbstat
      company_code       = s_bukrs-low
*    IMPORTING
*     multivalue         = lt_value
    EXCEPTIONS
      constant_not_found = 1
      OTHERS             = 2.

  IF sy-subrc NE 0.
**    Customizing entries in table /DCR/CBFC_CONFIG are missing
    MESSAGE e010(/dcr/cbfc_mm_invstat) DISPLAY LIKE c_inf.
    LEAVE LIST-PROCESSING .
  ELSE.
** To build the ranges table
    CALL FUNCTION '/DCR/CBFC_FILL_RANGE_TAB'
      EXPORTING
        iv_mode            = c_v
        iv_separator       = ','
        iv_conversion_exit = ' '
*       it_value_tab       = lt_value
        iv_i_eq_only       = c_x
      CHANGING
        xt_range_tab       = gt_rbstat_range
      EXCEPTIONS
        param_error        = 1
        value_error        = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECT_CONFIG_TAB
FORM prepare_tab_for_update1 .
  DATA: lv_date TYPE sy-datum.

  DATA: lt_temp TYPE STANDARD TABLE OF ts_temp ." WITH HEADER LINE.
  DATA: lt_t001 TYPE STANDARD TABLE OF ts_t001 ." WITH HEADER LINE.
  DATA: lv_count TYPE int4.
  DATA  lv_amount TYPE hslvt12.
  DATA  lv_rmwwr_low TYPE hslvt12.
  DATA  lv_rmwwr_high TYPE hslvt12.
  DATA: lv_temp_index TYPE sy-tabix.

  FIELD-SYMBOLS: <lv_old_curr> TYPE any,
                 <lv_new_curr> TYPE any,
*        <ls_date>     TYPE dats ,
                 <lv_amt>      TYPE any,
                 <lv_rate>     TYPE any,
                 <ls_temp>     TYPE ts_temp,
                 <ls_temp_1>   TYPE ts_temp,
                 <ls_t001>     TYPE ts_t001.

***Since only PO assingned objects are updated ,

***Select local currency for company code .
  IF NOT gt_final IS INITIAL.
    SELECT bukrs waers FROM t001 INTO TABLE lt_t001 FOR ALL ENTRIES IN gt_final WHERE bukrs = gt_final-bukrs.
  ENDIF.
*** Use the parallel cursor to get the counter values
***Collect all the values of ebeln and invoices in a temp table
  LOOP AT gt_final ASSIGNING <gs_final>.
    APPEND INITIAL LINE TO lt_temp ASSIGNING <ls_temp> .
    MOVE-CORRESPONDING <gs_final> TO <ls_temp>.
  ENDLOOP.

*** Sort the fields before processing
  SORT gt_final BY ebeln .
  SORT lt_temp  BY ebeln .

***Delete the values without PO assigment
  DELETE gt_final WHERE ebeln = space.

*** Append the values to final update table
  LOOP AT gt_final ASSIGNING <gs_final> .
*    APPEND INITIAL LINE TO gt_update ASSIGNING <gs_update>.
*    MOVE-CORRESPONDING <gs_final> TO <gs_update>.
*    <gs_update>-upddt = sy-datum .
*    CONCATENATE sy-datum sy-uzeit INTO <gs_update>-tims .
*** Parallel cursor processing to get the counter value
    READ TABLE lt_temp ASSIGNING <ls_temp> WITH KEY ebeln = <gs_final>-ebeln BINARY SEARCH.
    IF sy-subrc = 0.
      lv_temp_index = sy-tabix.
      LOOP AT lt_temp ASSIGNING <ls_temp_1> FROM lv_temp_index. "#EC CI_NESTED
        IF <ls_temp_1>-ebeln <> <gs_final>-ebeln.
          CLEAR : lv_count .
          EXIT.
        ENDIF.
        lv_count = lv_count + 1 .
*        <gs_update>-counter = lv_count.
      ENDLOOP.
    ENDIF.
*** Assign values to Pass that to FM below.
    lv_date = sy-datum. "This is passed as it is a mandatory parameter , currency translation date
*    ASSIGN <gs_final>-rmwwr TO <lv_amt>.
*    ASSIGN <gs_final>-waers TO <lv_old_curr>.
*    READ TABLE gt_rbkp ASSIGNING <gs_rbkp> WITH KEY belnr = <gs_final>-belnr.
*    IF sy-subrc = 0 .
    ASSIGN <gs_rbkp>-kursf TO <lv_rate>.
    READ TABLE lt_t001 ASSIGNING <ls_t001> WITH KEY bukrs = <gs_final>-bukrs .
    IF sy-subrc = 0 .
      ASSIGN <ls_t001>-waers TO <lv_new_curr>.
    ENDIF.
*    ENDIF.
    IF <lv_old_curr> IS ASSIGNED AND <lv_new_curr> IS  ASSIGNED AND <lv_rate> IS ASSIGNED AND lv_date IS NOT INITIAL.
*** Has to be convert to Company Code currency.
      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
          date             = lv_date
          foreign_amount   = <lv_amt>
          foreign_currency = <lv_old_curr>
          local_currency   = <lv_new_curr>
          rate             = <lv_rate>
          type_of_rate     = c_m
          read_tcurr       = c_x
        IMPORTING
          local_amount     = lv_amount
        EXCEPTIONS
          OTHERS           = 6.

      IF sy-subrc = 0.
*        <gs_update>-rmwwr = lv_amount.
      ENDIF.
    ENDIF.
  ENDLOOP.
***Delete the values without PO assigment
*  DELETE gt_final WHERE ebeln = space.
*  DELETE gt_update WHERE ebeln = space.

***Check whether the Invoice amount is within the range given in the selection screen.
*** THe update table needs to be checked and log table should be prepared
*  UNASSIGN: <gs_update> .
  CLEAR: lv_rmwwr_low , lv_rmwwr_high.
  lv_rmwwr_low = s_rmwwr-low.
  lv_rmwwr_high = s_rmwwr-high.

*  LOOP AT gt_update ASSIGNING <gs_update> .
*    IF <gs_update>-rmwwr < lv_rmwwr_low OR <gs_update>-rmwwr > lv_rmwwr_high.
*      APPEND INITIAL LINE TO gt_log_failed ASSIGNING <gs_log_f> .
*      READ TABLE gt_final ASSIGNING <gs_final> WITH KEY belnr = <gs_update>-belnr gjahr = <gs_update>-gjahr ebeln = <gs_update>-ebeln
*                                                        ebelp = <gs_update>-ebelp.
*      IF sy-subrc = 0 .
*        MOVE-CORRESPONDING <gs_final> TO <gs_log_f> .
*        <gs_log_f>-rmwwr = <gs_update>-rmwwr.
*        <gs_log_f>-message = TEXT-002.
*      ENDIF.

*    ELSE.
*      APPEND INITIAL LINE TO gt_log_success ASSIGNING <gs_log_s> .
*      READ TABLE gt_final ASSIGNING <gs_final> WITH KEY belnr = <gs_update>-belnr gjahr = <gs_update>-gjahr ebeln = <gs_update>-ebeln
*                                                     ebelp = <gs_update>-ebelp.
*      IF sy-subrc = 0 .
*        MOVE-CORRESPONDING <gs_final> TO <gs_log_s> .
*        <gs_log_s>-rmwwr = <gs_update>-rmwwr.
*        <gs_log_s>-message = TEXT-003.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

***Delete from the update table and then proceed to update .
*  DELETE gt_update WHERE rmwwr < lv_rmwwr_low .
*  DELETE gt_update WHERE rmwwr > lv_rmwwr_high .
ENDFORM.                    " PREPARE_TAB_FOR_UPDATE1
