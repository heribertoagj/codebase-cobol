      ******************************************************************
      * NOME BOOK : GFCTWQFF - MOVIMENTO DIARIO DE CONTAS SALARIOS     *
      *            ATIVAS E PROCESSADAS VIA MULTIPAG.                  *
      *            ARQUIVO DE VINCULO ENVIADO AO GOTF (TARIFAS)        *
      * DATA      : MAR/2024                                           *
      * AUTOR     : MAURICIO ROQUE DA SILVA                            *
      * EMPRESA   : SIS                                                *
      * COMPONENTE: CONVIVENCIA E MIGRACAO PGIT X PFOR                 *
      * TAMANHO   : 250 BYTES                                          *
      ************************* DADOS **********************************
      *    LAYOUT COM BASE NA TABELA CSALB002 - TCTA_SALRL_DSTNO       *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
       01 GFCTWQFF-REGISTRO-MULTIPAG.
          05  GFCTWQFF-TIPO-REGISTRO.
              10  GFCTWQFF-TP-REG             PIC X(01).
          05  GFCTWQFF-CONTRATO-MULTIPAG.
              10  GFCTWQFF-NSEQ-CONTR-NEGOC   PIC 9(10).
          05 GFCTWQFF-CONTA-SALARIO.
              10  GFCTWQFF-CBCO-CTA-SALRL     PIC 9(03).
              10  GFCTWQFF-CAG-CTA-SALRL      PIC 9(05).
              10  GFCTWQFF-CCTA-SALRL         PIC 9(13).
              10  GFCTWQFF-CDIG-CTA-SALRL     PIC X(02).
              10  GFCTWQFF-CSIT-CTA-SALRL     PIC 9(02).
          05 GFCTWQFF-EMPRESA-PAGADORA.
ST2507*       10  GFCTWQFF-CCNPJ-EMPR-CONVN   PIC 9(09).
ST2507*       10  GFCTWQFF-CFLIAL-EMPR-CONVN  PIC 9(05).
ST2507        10  GFCTWQFF-CCNPJ-EMPR-CONVN   PIC X(09).
ST2507        10  GFCTWQFF-CFLIAL-EMPR-CONVN  PIC X(04).
              10  GFCTWQFF-CTRL-EMPR-CONVN    PIC 9(02).
              10  GFCTWQFF-CBCO-EMPR-CONVN    PIC 9(03).
              10  GFCTWQFF-CAG-EMPR-CONVN     PIC 9(05).
          05 GFCTWQFF-CTA-DSTNO-FUNC.
              10  GFCTWQFF-CCONVE-CTA-SALRL   PIC 9(09).
              10  GFCTWQFF-CBCO-DSTNO-SALRL   PIC 9(03).
              10  GFCTWQFF-CAG-DSTNO-SALRL    PIC 9(05).
              10  GFCTWQFF-CCTA-DSTNO-SALRL   PIC 9(13).
              10  GFCTWQFF-CDIG-CTA-DSTNO     PIC X(02).
              10  GFCTWQFF-CSIT-CTA-DSTNO     PIC 9(02).
              10  GFCTWQFF-CTPO-CTA-DSTNO     PIC X(02).
          05 GFCTWQFF-GRUPO-RAZAO.
              10 GFCTWQFF-CGRUPO-SALRL        PIC 9(03)  COMP-3.
              10 GFCTWQFF-CSUBGR-SALRL        PIC 9(03)  COMP-3.
          05 GFCTWQFF-GRUPO-RAZAO-DSTNO.
              10 GFCTWQFF-CGRUPO-DSTNO        PIC 9(03)  COMP-3.
              10 GFCTWQFF-CSUBGR-DSTNO        PIC 9(03)  COMP-3.
          05 GFCTWQFF-CONTA-SALRL-TRAG        PIC X(01).
          05 GFCTWQFF-CONTA-SALRL-ATU.
              10 GFCTWQFF-CAG-SALRL-ATU       PIC S9(05)  COMP-3.
              10 GFCTWQFF-CCTA-SALRL-ATU      PIC S9(13)  COMP-3.
              10 GFCTWQFF-CGRUPO-SALRL-ATU    PIC  9(03)  COMP-3.
          05 GFCTWQFF-CONTA-DSTNO-TRAG        PIC  X(01).
          05 GFCTWQFF-CONTA-DSTNO-ATU.
              10 GFCTWQFF-CAG-DSTNO-ATU       PIC S9(05)  COMP-3.
              10 GFCTWQFF-CCTA-DSTNO-ATU      PIC S9(13)  COMP-3.
              10 GFCTWQFF-CGRUPO-DSTNO-ATU    PIC  9(03)  COMP-3.
              10 GFCTWQFF-CSUBGR-DSTNO-ATU    PIC  9(03)  COMP-3.
          05 GFCTWQFF-MSG-ERRO                PIC  X(80).
ST2507*   05 FILLER                           PIC  X(23).
ST2507    05 FILLER                           PIC  X(24).
