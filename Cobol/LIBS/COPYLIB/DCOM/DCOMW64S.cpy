      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMW64S-HEADER.
          10 DCOMW64S-COD-LAYOUT   PIC  X(008) VALUE 'DCOMW64S'.
          10 DCOMW64S-TAM-LAYOUT   PIC  9(005) VALUE 12650.
      *
          10 DCOMW64S-BLOCO-SAIDA.
             15 DCOMW64S-QTDE-TOTAL            PIC  9(010).
             15 DCOMW64S-QTDE-RETORNADA        PIC  9(003).
             15 DCOMW64S-OCORRENCIA            OCCURS 050  TIMES.
                20 DCOMW64S-CPRODT             PIC S9(003) COMP-3.
                20 DCOMW64S-CSPROD-DESC-COML   PIC S9(003) COMP-3.
                20 DCOMW64S-CMEIO-ENTRD-DESC   PIC S9(003) COMP-3.
                20 DCOMW64S-HINIC-MEIO-ENTRD   PIC  X(026).
                20 DCOMW64S-CINDCD-MODLD-DESC  PIC  X(001).
                20 DCOMW64S-HFIM-MEIO-ENTRD    PIC  X(026).
                20 DCOMW64S-CSIT-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMW64S-HSIT-DESC-COML     PIC  X(026).
                20 DCOMW64S-CINDCD-FORML-OPER  PIC  X(001).
                20 DCOMW64S-CINDCD-LIBRC-OPER  PIC  X(001).
                20 DCOMW64S-CINDCD-LIBRC-ANTCP PIC  X(001).
                20 DCOMW64S-PMAX-LIBRC-ANTCP   PIC S9(003)V9(03)
                                                           COMP-3.
                20 DCOMW64S-TMIN-VCTO-TITLO    PIC S9(005) COMP-3.
                20 DCOMW64S-TMAX-VCTO-TITLO    PIC S9(005) COMP-3.
                20 DCOMW64S-CINDCD-PRZ-VCTO    PIC  X(002).
                20 DCOMW64S-TMAX-ACLTO-TITLO   PIC S9(003) COMP-3.
                20 DCOMW64S-CINDCD-PRZ-ACLTO   PIC  X(002).
                20 DCOMW64S-VMAX-DIVRG-TITLO   PIC S9(015)V9(02)
                                                           COMP-3.
                20 DCOMW64S-CINDCD-OBRIG-GARNT PIC  X(001).
                20 DCOMW64S-CINDCD-OBRIG-PROMS PIC  X(001).
                20 DCOMW64S-CCART-NORML-DESC   PIC  X(005).
                20 DCOMW64S-CCART-VENCD-DESC   PIC  X(005).
                20 DCOMW64S-CCART-DESC-PRODT   PIC  X(005).
                20 DCOMW64S-CINDCD-TARIF-TRNSF PIC  X(001).
                20 DCOMW64S-VTARIF-TRNSF-CART  PIC S9(015)V9(002)
                                                           COMP-3.
                20 DCOMW64S-CTARIF-TRNSF-CART  PIC S9(005) COMP-3.
                20 DCOMW64S-CCOBR-TAC-OPER     PIC S9(003) COMP-3.
                20 DCOMW64S-VTAC-OPER-DESC     PIC S9(015)V9(002)
                                                           COMP-3.
                20 DCOMW64S-CTAC-OPER-DESC     PIC S9(005) COMP-3.
                20 DCOMW64S-CTAC-PERC-OPER     PIC S9(005) COMP-3.
                20 DCOMW64S-CTAC-MIN-OPER      PIC S9(005) COMP-3.
                20 DCOMW64S-CTAC-MAX-OPER      PIC S9(005) COMP-3.
                20 DCOMW64S-CCOBR-TAC-CONTR    PIC S9(003) COMP-3.
                20 DCOMW64S-VTAC-OPER-EFETU    PIC S9(015)V9(002)
                                                           COMP-3.
                20 DCOMW64S-CTAC-OPER-EFETU    PIC S9(005) COMP-3.
                20 DCOMW64S-CTAC-PERC-CONTR    PIC S9(005) COMP-3.
                20 DCOMW64S-CTAC-MIN-CONTR     PIC S9(005) COMP-3.
                20 DCOMW64S-CTAC-MAX-CONTR     PIC S9(005) COMP-3.
                20 DCOMW64S-HULT-ATULZ         PIC  X(026).
                20 DCOMW64S-WCTRL-FLUXO-DIGTC  PIC  X(015).
                20 DCOMW64S-CFUNC-BDSCO        PIC S9(009) COMP-3.
                20 DCOMW64S-CTERM              PIC  X(008).
                20 DCOMW64S-CINDCD-TARIF-REG   PIC  X(001).
                20 DCOMW64S-CINDCD-PRZ-MAX     PIC  X(002).
             15 DCOMW64S-FILLER                PIC  X(224).
      *
