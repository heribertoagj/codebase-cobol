      ******************************************************************
      * NOME BOOK : GFCTWQEF                                           *
      * DESCRICAO : ARQUIVO DE PAGAMENTO DO MULTIPAG                   *
      * DATA      : JAN_2024                                           *
      * AUTOR     : MAURICIO ROQUE                                     *
      * EMPRESA   : SIS                                                *
      * GRUPO     : GFCT                                               *
      * TAMANHO   : 250 bytes                                          *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01 GFCTWQEF-REGISTRO-MULTIPAG.
          05 GFCTWQEF-DATA-MVTO              PIC  9(09)        COMP-3.
          05 GFCTWQEF-CONTRATO-MULTIPAG.
             10 GFCTWQEF-NSEQ-CONTR-NEGOC    PIC S9(10)        COMP-3.
          05 GFCTWQEF-DADOS-EMPRESA.
             10 GFCTWQEF-NM-EMPR-PGDOR       PIC S9(03)        COMP-3.
ST2507*      10 GFCTWQEF-CCNPJ-EMPR          PIC S9(09)        COMP-3.
ST2507*      10 GFCTWQEF-CFLIAL-EMPR         PIC S9(05)        COMP-3.
ST2507       10 GFCTWQEF-CCNPJ-EMPR          PIC  x(09).
ST2507       10 GFCTWQEF-CFLIAL-EMPR         PIC  x(04).
             10 GFCTWQEF-CTRL-EMPR           PIC S9(02)        COMP-3.
             10 GFCTWQEF-CAG-EMPR            PIC S9(05)        COMP-3.
             10 GFCTWQEF-CCTA-SALRL          PIC S9(13)        COMP-3.
             10 GFCTWQEF-VLR-DEB             PIC S9(13)V9(02)  COMP-3.
          05 GFCTWQEF-CONTA-SALARIO.
             10 GFCTWQEF-CAG-CTA-SALRL       PIC S9(05)        COMP-3.
             10 GFCTWQEF-CCTA-SALRL          PIC S9(13)        COMP-3.
             10 GFCTWQEF-CDIG-CTA-SALRL      PIC  X(02).
             10 GFCTWQEF-CSIT-CTA-SALRL      PIC S9(02)        COMP-3.
          05 GFCTWQEF-DSTNO-RCDOR.
             10 GFCTWQEF-BCO-RCDOR           PIC S9(03)        COMP-3.
             10 GFCTWQEF-CAG-RCDOR           PIC S9(05)        COMP-3.
             10 GFCTWQEF-CCTA-RCDOR          PIC S9(13)        COMP-3.
             10 GFCTWQEF-DIG-CTA-RCDOR       PIC  X(02).
             10 GFCTWQEF-VLR-CRED            PIC S9(13)V9(02)  COMP-3.
             10 GFCTWQEF-SIT-CTA-RCDOR       PIC  X(02).
             10 GFCTWQEF-TIP-CTA-RCDOR       PIC  X(02).
ST2507*   05 FILLER                          PIC  X(169).
ST2507    05 FILLER                          PIC  X(164).
