      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB064: DB2PRD.TSPROD_MEIO_ENTRD                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM464S                                       *
      *    -> FETCH   - DCOM464F                                       *
      *    -> INSERT  - DCOM464I                                       *
      *    -> UPDATE  - DCOM464U                                       *
      *    -> DELETE  - DCOM464D                                       *
      *================================================================*
      *                                                                *
       05 DCOMW64E-HEADER.
          10 DCOMW64E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMW64E'.
          10 DCOMW64E-TAM-LAYOUT       PIC  9(005) VALUE 00777.
      *
       05 DCOMW64E-BLOCO-ENTRADA.
          10 DCOMW64E-INSTRUCAO                    PIC  X(010).
          10 DCOMW64E-ENTRADA.
             15 DCOMW64E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMW64E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMW64E-CMEIO-ENTRD-DESC          PIC S9(003) COMP-3.
             15 DCOMW64E-HINIC-MEIO-ENTRD          PIC  X(026).
             15 DCOMW64E-CINDCD-MODLD-DESC         PIC  X(001).
             15 DCOMW64E-HFIM-MEIO-ENTRD           PIC  X(026).
             15 DCOMW64E-CSIT-DESC-COML            PIC S9(003) COMP-3.
             15 DCOMW64E-HSIT-DESC-COML            PIC  X(026).
             15 DCOMW64E-CINDCD-FORML-OPER         PIC  X(001).
             15 DCOMW64E-CINDCD-LIBRC-OPER         PIC  X(001).
             15 DCOMW64E-CINDCD-LIBRC-ANTCP        PIC  X(001).
             15 DCOMW64E-PMAX-LIBRC-ANTCP          PIC S9(003)V9(003)
                                                               COMP-3.
             15 DCOMW64E-TMIN-VCTO-TITLO           PIC S9(005) COMP-3.
             15 DCOMW64E-TMAX-VCTO-TITLO           PIC S9(005) COMP-3.
             15 DCOMW64E-CINDCD-PRZ-VCTO           PIC  X(002).
             15 DCOMW64E-TMAX-ACLTO-TITLO          PIC S9(003) COMP-3.
             15 DCOMW64E-CINDCD-PRZ-ACLTO          PIC  X(002).
             15 DCOMW64E-VMAX-DIVRG-TITLO          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMW64E-CINDCD-OBRIG-GARNT        PIC  X(001).
             15 DCOMW64E-CINDCD-OBRIG-PROMS        PIC  X(001).
             15 DCOMW64E-CCART-NORML-DESC          PIC  X(005).
             15 DCOMW64E-CCART-VENCD-DESC          PIC  X(005).
             15 DCOMW64E-CCART-DESC-PRODT          PIC  X(005).
             15 DCOMW64E-CINDCD-TARIF-TRNSF        PIC  X(001).
             15 DCOMW64E-VTARIF-TRNSF-CART         PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMW64E-CTARIF-TRNSF-CART         PIC S9(005) COMP-3.
             15 DCOMW64E-CCOBR-TAC-OPER            PIC S9(003) COMP-3.
             15 DCOMW64E-VTAC-OPER-DESC            PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMW64E-CTAC-OPER-DESC            PIC S9(005) COMP-3.
             15 DCOMW64E-CTAC-PERC-OPER            PIC S9(005) COMP-3.
             15 DCOMW64E-CTAC-MIN-OPER             PIC S9(005) COMP-3.
             15 DCOMW64E-CTAC-MAX-OPER             PIC S9(005) COMP-3.
             15 DCOMW64E-CCOBR-TAC-CONTR           PIC S9(003) COMP-3.
             15 DCOMW64E-VTAC-OPER-EFETU           PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMW64E-CTAC-OPER-EFETU           PIC S9(005) COMP-3.
             15 DCOMW64E-CTAC-PERC-CONTR           PIC S9(005) COMP-3.
             15 DCOMW64E-CTAC-MIN-CONTR            PIC S9(005) COMP-3.
             15 DCOMW64E-CTAC-MAX-CONTR            PIC S9(005) COMP-3.
             15 DCOMW64E-HULT-ATULZ                PIC  X(026).
             15 DCOMW64E-WCTRL-FLUXO-DIGTC         PIC  X(015).
             15 DCOMW64E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMW64E-CTERM                     PIC  X(008).
             15 DCOMW64E-CINDCD-TARIF-REG          PIC  X(001).
             15 DCOMW64E-CINDCD-PRZ-MAX            PIC  X(002).
      *
          10 DCOMW64E-BLOCO-VARIAVEL.
             15 DCOMW64E-V-FILLER                  PIC  X(300).
      *
          10 DCOMW64E-BLOCO-PAGINACAO.
             15 DCOMW64E-MAX-OCORR                 PIC  9(003).
             15 DCOMW64E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMW64E-P-INICIAL              VALUE 'I'.
                88 DCOMW64E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMW64E-P-SEGUINTE             VALUE 'S'.
                88 DCOMW64E-P-ANTERIOR             VALUE 'A'.
                88 DCOMW64E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMW64E-BLOCO-RESTART.
             15 DCOMW64E-CHAVE-INI.
                20 DCOMW64E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW64E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW64E-I-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMW64E-I-HINIC-MEIO-ENTRD     PIC  X(026).
             15 DCOMW64E-CHAVE-FIM.
                20 DCOMW64E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMW64E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW64E-F-CMEIO-ENTRD-DESC     PIC S9(003) COMP-3.
                20 DCOMW64E-F-HINIC-MEIO-ENTRD     PIC  X(026).
          10 DCOMW64E-FILLER                       PIC  X(138).
      *                                                                *
