      ******************************************************************
      * NOME BOOK : GFCTWQHF                                           *
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
       01 GFCTWQHF-REGISTRO-PGTO-TRAG.
          03 GFCTWQHF-DADOS-PAGADOR.
             05 GFCTWQHF-DTMOVTO             PIC 9(08)        COMP-3.
             05 GFCTWQHF-NSEQ-CONTR-NEGOC    PIC 9(10)        COMP-3.
             05 GFCTWQHF-NM-EMPR-PGDOR       PIC X(35).
ST2507*      05 GFCTWQHF-CCNPJ-EMPR          PIC 9(09)        COMP-3.
ST2507*      05 GFCTWQHF-CFLIAL-EMPR         PIC 9(04)        COMP-3.
ST2507       05 GFCTWQHF-CCNPJ-EMPR          PIC x(09).
ST2507       05 GFCTWQHF-CFLIAL-EMPR         PIC x(04).
             05 GFCTWQHF-CTRL-EMPR           PIC 9(02)        COMP-3.
             05 GFCTWQHF-CAG-EMPR            PIC 9(05)        COMP-3.
             05 GFCTWQHF-CCTA-EMPR           PIC 9(13)        COMP-3.
             05 GFCTWQHF-VLR-DEB             PIC 9(13)V9(02)  COMP-3.
          03 GFCTWQHF-DADOS-CONTA-SALARIO.
             05 GFCTWQHF-AGEN-SALRL          PIC 9(05)        COMP-3.
             05 GFCTWQHF-CTA-SALRL           PIC 9(13)        COMP-3.
             05 GFCTWQHF-DIG-CTA-SALRL       PIC X(01).
             05 GFCTWQHF-SIT-CTA-SALRL       PIC 9(02)        COMP-3.
          03 GFCTWQHF-DADOS-CREDITO.
             05 GFCTWQHF-BCO-RCDOR           PIC 9(03)        COMP-3.
             05 GFCTWQHF-CAG-RCDOR           PIC 9(05)        COMP-3.
             05 GFCTWQHF-CCTA-RCDOR          PIC 9(13)        COMP-3.
             05 GFCTWQHF-DIG-CTA-RCDOR       PIC X(02).
             05 GFCTWQHF-VLR-CRED            PIC 9(13)V9(02)  COMP-3.
             05 GFCTWQHF-SIT-CTA-RCDOR       PIC 9(02)        COMP-3.
             05 GFCTWQHF-TIP-CTA-RCDOR       PIC 9(02)        COMP-3.
          03 GFCTWQHF-GRUPO-RAZAO.
             05 GFCTWQHF-CGRUPO-SALRL        PIC  9(03)        COMP-3.
             05 GFCTWQHF-CSUBGR-SALRL        PIC  9(03)        COMP-3.
          03 GFCTWQHF-GRUPO-RAZAO-DSTNO.
             05 GFCTWQHF-CGRUPO-DSTNO        PIC  9(03)        COMP-3.
             05 GFCTWQHF-CSUBGR-DSTNO        PIC  9(03)        COMP-3.
          03 GFCTWQHF-CONTA-SALRL-TRAG       PIC  X(01).
          03 GFCTWQHF-CONTA-SALRL-ATU.
             05 GFCTWQHF-CAG-SALRL-ATU       PIC S9(05)V USAGE COMP-3.
             05 GFCTWQHF-CCTA-SALRL-ATU      PIC S9(13)V USAGE COMP-3.
             05 GFCTWQHF-CGRUPO-SALRL-ATU    PIC  9(03)        COMP-3.
             05 GFCTWQHF-CSUBGR-SALRL-ATU    PIC  9(03)        COMP-3.
          03 GFCTWQHF-CONTA-DSTNO-TRAG       PIC  X(01).
          03 GFCTWQHF-CONTA-DSTNO-ATU.
             05 GFCTWQHF-CAG-DSTNO-ATU       PIC S9(05)V USAGE COMP-3.
             05 GFCTWQHF-CCTA-DSTNO-ATU      PIC S9(13)V USAGE COMP-3.
             05 GFCTWQHF-CGRUPO-DSTNO-ATU    PIC  9(03)        COMP-3.
             05 GFCTWQHF-CSUBGR-DSTNO-ATU    PIC  9(03)        COMP-3.
          03 GFCTWQHF-MSG-ERRO               PIC  X(80).
ST2507*   03 FILLER                          PIC  X(19).
ST2507    03 FILLER                          PIC  X(14).
