      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *     -> DCOMB0J8: DB2PRD.TSPROD_TX_ITNET   *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4J8S                                       *
      *================================================================*
      *                                                                *
       05 DCOMWJ8S-HEADER.
          10 DCOMWJ8S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWJ8S'.
          10 DCOMWJ8S-TAM-LAYOUT       PIC  9(005) VALUE 07726.
      *
          10 DCOMWJ8S-BLOCO-SAIDA.
             15 DCOMWJ8S-QTDE-TOTAL              PIC 9(010).
             15 DCOMWJ8S-QTDE-RETORNADA          PIC 9(003).
             15 DCOMWJ8S-OCORRENCIA   OCCURS 050 TIMES.
                20 DCOMWJ8S-CPRODT               PIC  9(03).
                20 DCOMWJ8S-CSPROD-DESC-COML     PIC  9(03).
                20 DCOMWJ8S-CMEIO-ENTRD-DESC     PIC  9(03).
                20 DCOMWJ8S-CSGMTO-CLI           PIC  9(03).
                20 DCOMWJ8S-CINDCD-PSSOA-TOMAD   PIC  X(01).
                20 DCOMWJ8S-HINIC-TX-OPER-DESC   PIC  X(26).
                20 DCOMWJ8S-NSEQ-BANDA-TX-REDE   PIC  9(05).
                20 DCOMWJ8S-HFIM-TX-OPER-DESC    PIC  X(26).
                20 DCOMWJ8S-VMIN-FAIXA-OPER      PIC  9(15)V9(2).
                20 DCOMWJ8S-VMAX-FAIXA-OPER      PIC  9(15)V9(2).
                20 DCOMWJ8S-CINDCD-FREQ-TX       PIC  X(02).
                20 DCOMWJ8S-PTX-MAX-JURO-DESC    PIC  9(03)V9(2).
                20 DCOMWJ8S-HULT-ATULZ           PIC  X(26).
                20 DCOMWJ8S-CFUNC-BDSCO          PIC  9(09).
                20 DCOMWJ8S-CTERM                PIC  X(08).
