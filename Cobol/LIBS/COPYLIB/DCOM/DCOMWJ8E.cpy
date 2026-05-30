      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0J8: DB2PRD.TSPROD_TX_ITNET                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4J8S                                       *
      *================================================================*
      *                                                                *
       05 DCOMWJ8E-HEADER.
          10 DCOMWJ8E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWJ8E'.
          10 DCOMWJ8E-TAM-LAYOUT       PIC  9(005) VALUE 00258.
      *
       05 DCOMWJ8E-BLOCO-ENTRADA.
          10 DCOMWJ8E-INSTRUCAO                     PIC  X(10).
          10 DCOMWJ8E-ENTRADA.
             15 DCOMWJ8E-CPRODT                     PIC  9(03).
             15 DCOMWJ8E-CSPROD-DESC-COML           PIC  9(03).
             15 DCOMWJ8E-CMEIO-ENTRD-DESC           PIC  9(03).
             15 DCOMWJ8E-CSGMTO-CLI                 PIC  9(03).
             15 DCOMWJ8E-CINDCD-PSSOA-TOMAD         PIC  X(01).
             15 DCOMWJ8E-HINIC-TX-OPER-DESC         PIC  X(26).
             15 DCOMWJ8E-NSEQ-BANDA-TX-REDE         PIC  9(05).
             15 DCOMWJ8E-HFIM-TX-OPER-DESC          PIC  X(26).
             15 DCOMWJ8E-VMIN-FAIXA-OPER            PIC  9(15)V9(2).
             15 DCOMWJ8E-VMAX-FAIXA-OPER            PIC  9(15)V9(2).
             15 DCOMWJ8E-CINDCD-FREQ-TX             PIC  X(02).
             15 DCOMWJ8E-PTX-MAX-JURO-DESC          PIC  9(03)V9(2).
             15 DCOMWJ8E-HULT-ATULZ                 PIC  X(26).
             15 DCOMWJ8E-CFUNC-BDSCO                PIC  9(09).
             15 DCOMWJ8E-CTERM                      PIC  X(08).
      *
           10 DCOMWJ8E-BLOCO-PAGINACAO.
              15 DCOMWJ8E-MAX-OCORR                 PIC  9(03).
              15 DCOMWJ8E-TOTALIZAR                 PIC  X(01).
              15 DCOMWJ8E-INDICADOR-PAGINACAO       PIC  X(01).
                 88 DCOMWJ8E-P-INICIAL              VALUE 'I'.
                 88 DCOMWJ8E-P-PRIMEIRA             VALUE 'P'.
                 88 DCOMWJ8E-P-SEGUINTE             VALUE 'S'.
                 88 DCOMWJ8E-P-ANTERIOR             VALUE 'A'.
                 88 DCOMWJ8E-P-ULTIMA               VALUE 'U'.
      *
           10 DCOMWJ8E-BLOCO-RESTART.
              15 DCOMWJ8E-CHAVE-INI.
                 20 DCOMWJ8E-I-CPRODT               PIC S9(03) COMP-3.
                 20 DCOMWJ8E-I-CSPROD-DESC-COML     PIC S9(03) COMP-3.
                 20 DCOMWJ8E-I-CMEIO-ENTRD-DESC     PIC S9(03) COMP-3.
                 20 DCOMWJ8E-I-CSGMTO-CLI           PIC S9(03) COMP-3.
                 20 DCOMWJ8E-I-CINDCD-PSSOA-TOMAD   PIC  X(01).
                 20 DCOMWJ8E-I-HINIC-VGCIA-NVEL     PIC  X(26).
                 20 DCOMWJ8E-I-NSEQ-BANDA-TX-REDE   PIC S9(05) COMP-3.
              15 DCOMWJ8E-CHAVE-FIM.
                 20 DCOMWJ8E-F-CPRODT               PIC S9(03) COMP-3.
                 20 DCOMWJ8E-F-CSPROD-DESC-COML     PIC S9(03) COMP-3.
                 20 DCOMWJ8E-F-CMEIO-ENTRD-DESC     PIC S9(03) COMP-3.
                 20 DCOMWJ8E-F-CSGMTO-CLI           PIC S9(03) COMP-3.
                 20 DCOMWJ8E-F-CINDCD-PSSOA-TOMAD   PIC  X(01).
                 20 DCOMWJ8E-F-HINIC-VGCIA-NVEL     PIC  X(26).
                 20 DCOMWJ8E-F-NSEQ-BANDA-TX-REDE   PIC S9(05) COMP-3.
