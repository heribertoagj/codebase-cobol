      *---------------------------------------------------------------*
      * COPYBOOK     : I#BVVE16                                       *
      * DESCRICAO    : DOMINIO DO CAMPO ARRANJO CIP                   *
      * DATA CRIACAO : NOVEMBRO DE 2019                               *
      * AUTOR        : ESTHER YOKOSAWA - RTB                          *
      * OBS.         : DE ACORDO COM O MANUAL CIP V3.0                *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      * DATA           NOME                   DESCRICAO               *
      * ==========  =================  ============================== *
      * DD/MM/AAAA  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      * ------------------------------------------------------------- *
      * ARRANJO BANDEIRA            TIPO     SIGLA                    *
      * 001 VAGO                    VAGO     VGO                      *
      * 002 VAGO                    VAGO     VGO                      *
      * 003 MASTERCARD              CREDITO  MCC                      *
      * 004 VISA                    CREDITO  VCC                      *
      * 005 DINERS CLUB             CREDITO  DCC                      *
      * 007 HIPER DEBITO            DEBITO   HPD                      *
      * 008 ELO                     CREDITO  ECC                      *
      * 009 ALELO                   PRE-PAGO ALP                      *
      * 010 CABAL                   CREDITO  CAC                      *
      * 011 AGIPLAN                 CREDITO  AGC                      *
      * 012 AURA                    CREDITO  AUC                      *
      * 013 BANESCARD               CREDITO  BCC                      *
      * 014 CALCARD                 CREDITO  CDC                      *
      * 015 CREDSYSTEM              CREDITO  MAC                      *
      * 016 CUP                     CREDITO  CUC                      *
      * 017 REDESPLAN               CREDITO  RDC                      *
      * 018 SICRED                  CREDITO  SIC                      *
      * 019 SOROCRED                CREDITO  SCC                      *
      * 020 VERDECARD               CREDITO  VDC                      *
      * 021 HIPERCARD               CREDITO  HCC                      *
      * 022 AVISTA                  CREDITO  AVC                      *
      * 023 CREDZ                   CREDITO  CZC                      *
      * 024 DISCOVER                CREDITO  DVC                      *
      * 025 MAESTRO                 DEBITO   MCD                      *
      * 026 VISA ELECTRON           DEBITO   VCD                      *
      * 028 SICREDI DEBITO          DEBITO   SID                      *
      * 029 HIPER CREDITO           CREDITO  HPC                      *
      * 030 CABAL DEBITO            DEBITO   CAD                      *
      * 031 JCB                     CREDITO  JCC                      *
      * 032 TICKET                  PRE-PAGO TCP                      *
      * 033 SODEXO                  PRE-PAGO SDP                      *
      * 034 VR                      PRE-PAGO VRP                      *
      * 035 POLICARD                PRE-PAGO PDP                      *
      * 036 VALECARD                PRE-PAGO VDP                      *
      * 037 GOODCARD                CREDITO  GDC                      *
      * 038 GREENCARD               PRE-PAGO GDP                      *
      * 039 COOPERCARD              PRE-PAGO CDP                      *
      * 040 VEROCHEQUE              PRE-PAGO VQP                      *
      * 041 NUTRICASH               PRE-PAGO NCP                      *
      * 042 BANRICARD               PRE-PAGO BCP                      *
      * 043 BANESCARD DEBITO        DEBITO   BCD                      *
      * 044 SOCORED PRE-PAGO        PRE-PAGO SCP                      *
      * 045 MASTERCARD PRE-P        PRE-PAGO MCP                      *
      * 046 VISA PRE-PAGO           PRE-PAGO VCP                      *
      * 047 OUROCARD                DEBITO   OCD                      *
      * 049 CABAL PRE               PRE-PAGO CAP                      *
      * 050 BEM VISA VALE PRE       PRE-PAGO BVP                      *
      * 051 VISA INTERN SAQUE ATM   CREDITO  VSC                      *
      * 052 VISA INTERN COMPRA CRED CREDITO  VIC                      *
      * 053 VISA INTERN COMPRA DEB  DEBITO   VID                      *
      * 054 CIELO ARRANJO FECH CRED CREDITO  CFC                      *
      * 055 CIELO ARRANJO FECH DEB  DEBITO   CFD                      *
      *                                                               *
      *===============================================================*
       01  TAB-ARRANJO.
           02  FILLER                  OCCURS 55.
               03  TAB-COD-ARRANJO     PIC  X(03).
               03  TAB-SGL-CACF        PIC  X(03).

       01  TAB-ARRANJO-DEXPARA.
           03  FILLER                  PIC  X(06) VALUE '001VGO'.
           03  FILLER                  PIC  X(06) VALUE '002VGO'.
           03  FILLER                  PIC  X(06) VALUE '003MCC'.
           03  FILLER                  PIC  X(06) VALUE '004VCC'.
           03  FILLER                  PIC  X(06) VALUE '005DCC'.
           03  FILLER                  PIC  X(06) VALUE '007HPD'.
           03  FILLER                  PIC  X(06) VALUE '008ECC'.
           03  FILLER                  PIC  X(06) VALUE '009ALP'.
           03  FILLER                  PIC  X(06) VALUE '010CAC'.
           03  FILLER                  PIC  X(06) VALUE '011AGC'.
           03  FILLER                  PIC  X(06) VALUE '012AUC'.
           03  FILLER                  PIC  X(06) VALUE '013BCC'.
           03  FILLER                  PIC  X(06) VALUE '014CDC'.
           03  FILLER                  PIC  X(06) VALUE '015MAC'.
           03  FILLER                  PIC  X(06) VALUE '016CUC'.
           03  FILLER                  PIC  X(06) VALUE '017RDC'.
           03  FILLER                  PIC  X(06) VALUE '018SIC'.
           03  FILLER                  PIC  X(06) VALUE '019SCC'.
           03  FILLER                  PIC  X(06) VALUE '020VDC'.
           03  FILLER                  PIC  X(06) VALUE '021HCC'.
           03  FILLER                  PIC  X(06) VALUE '022AVC'.
           03  FILLER                  PIC  X(06) VALUE '023CZC'.
           03  FILLER                  PIC  X(06) VALUE '024DVC'.
           03  FILLER                  PIC  X(06) VALUE '025MCD'.
           03  FILLER                  PIC  X(06) VALUE '026VCD'.
           03  FILLER                  PIC  X(06) VALUE '028SID'.
           03  FILLER                  PIC  X(06) VALUE '029HPC'.
           03  FILLER                  PIC  X(06) VALUE '030CAD'.
           03  FILLER                  PIC  X(06) VALUE '031JCC'.
           03  FILLER                  PIC  X(06) VALUE '032TCP'.
           03  FILLER                  PIC  X(06) VALUE '033SDP'.
           03  FILLER                  PIC  X(06) VALUE '034VRP'.
           03  FILLER                  PIC  X(06) VALUE '035PDP'.
           03  FILLER                  PIC  X(06) VALUE '036VDP'.
           03  FILLER                  PIC  X(06) VALUE '037GDC'.
           03  FILLER                  PIC  X(06) VALUE '038GDP'.
           03  FILLER                  PIC  X(06) VALUE '039CDP'.
           03  FILLER                  PIC  X(06) VALUE '040VQP'.
           03  FILLER                  PIC  X(06) VALUE '041NCP'.
           03  FILLER                  PIC  X(06) VALUE '042BCP'.
           03  FILLER                  PIC  X(06) VALUE '043BCD'.
           03  FILLER                  PIC  X(06) VALUE '044SCP'.
           03  FILLER                  PIC  X(06) VALUE '045MCP'.
           03  FILLER                  PIC  X(06) VALUE '046VCP'.
           03  FILLER                  PIC  X(06) VALUE '047OCD'.
           03  FILLER                  PIC  X(06) VALUE '049CAP'.
           03  FILLER                  PIC  X(06) VALUE '050BVP'.
           03  FILLER                  PIC  X(06) VALUE '051VSC'.
           03  FILLER                  PIC  X(06) VALUE '052VIC'.
           03  FILLER                  PIC  X(06) VALUE '053VID'.
           03  FILLER                  PIC  X(06) VALUE '054CFC'.
           03  FILLER                  PIC  X(06) VALUE '055CFD'.

