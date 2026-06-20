      ******************************************************************
      * NOME BOOK : GFCTWQGF                                           *
      * DESCRICAO : BOOK PAGAMENTOS MULTIPAG                           *
      * DATA      : MARCO-2024                                         *
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
       01 GFCTWQGF-MULTIPAG-PGTO.
          03 GFCTWQGF-DADOS-PAGADOR.
             05 GFCTWQGF-DTMOVTO             PIC 9(08)        COMP-3.
             05 GFCTWQGF-NSEQ-CONTR-NEGOC    PIC 9(10)        COMP-3.
             05 GFCTWQGF-NM-EMPR-PGDOR       PIC X(35).
ST2507*      05 GFCTWQGF-CCNPJ-EMPR          PIC 9(09)        COMP-3.
ST2507*      05 GFCTWQGF-CFLIAL-EMPR         PIC 9(04)        COMP-3.
ST2507       05 GFCTWQGF-CCNPJ-EMPR          PIC x(09).
ST2507       05 GFCTWQGF-CFLIAL-EMPR         PIC x(04).
             05 GFCTWQGF-CTRL-EMPR           PIC 9(02)        COMP-3.
             05 GFCTWQGF-CAG-EMPR            PIC 9(05)        COMP-3.
             05 GFCTWQGF-CCTA-EMPR           PIC 9(13)        COMP-3.
             05 GFCTWQGF-VLR-DEB             PIC 9(13)V9(02)  COMP-3.
          03 GFCTWQGF-DADOS-CONTA-SALARIO.
             05 GFCTWQGF-AGEN-SALRL          PIC 9(05)        COMP-3.
             05 GFCTWQGF-CTA-SALRL           PIC 9(13)        COMP-3.
             05 GFCTWQGF-DIG-CTA-SALRL       PIC X(01).
             05 GFCTWQGF-SIT-CTA-SALRL       PIC 9(02)        COMP-3.
          03 GFCTWQGF-DADOS-CREDITO.
             05 GFCTWQGF-BCO-RCDOR           PIC 9(03)        COMP-3.
             05 GFCTWQGF-CAG-RCDOR           PIC 9(05)        COMP-3.
             05 GFCTWQGF-CCTA-RCDOR          PIC 9(13)        COMP-3.
             05 GFCTWQGF-DIG-CTA-RCDOR       PIC X(02).
             05 GFCTWQGF-VLR-CRED            PIC 9(13)V9(02)  COMP-3.
             05 GFCTWQGF-SIT-CTA-RCDOR       PIC 9(02)        COMP-3.
             05 GFCTWQGF-TIP-CTA-RCDOR       PIC 9(02)        COMP-3.
ST2507*      05 FILLER                       PIC X(137).
ST2507       05 FILLER                       PIC X(132).
