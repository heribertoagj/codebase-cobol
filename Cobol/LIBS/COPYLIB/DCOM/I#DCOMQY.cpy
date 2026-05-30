      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQY                                      *
      * TAMANHO : 1630 BYTES                                           *
      * ARQUIVO : ARQUIVO MOVIMENTACAO DESCONTO COMERCIAL              *
      * INCLUDE DA TABELA DCOMB0C3 - DB2PRD.TMOVTC_DESC_COML           *
      ******************************************************************
       01  QY-C3-REGISTRO.
           05 QY-C3-CELMTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
           05 QY-C3-CEVNTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
           05 QY-C3-HULT-ATULZ           PIC X(26).
           05 QY-C3-CINDCD-MANUT-REG     PIC X(1).
           05 QY-C3-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
           05 QY-C3-CCHAVE-ELMTO-DESC    PIC X(45).
           05 QY-C3-WREG-MOVTC-DESC.
              49 WREG-MOVTC-DESC-LEN  PIC S9(4) USAGE COMP.
              49 WREG-MOVTC-DESC-TEXT  PIC X(1500).
           05 QY-C3-CTRANS-PROG-ATULZ    PIC X(8).
           05 QY-C3-CINDCD-MODLD-PROCM   PIC X(1).
           05 QY-C3-CEMPR-INC            PIC S9(5)V USAGE COMP-3.
           05 QY-C3-CEMPR-INC-NULL       PIC X(01).
           05 QY-C3-CDEPDC               PIC S9(5)V USAGE COMP-3.
           05 QY-C3-CDEPDC-NULL          PIC X(01).
           05 QY-C3-DMOVTC-DESC-COML     PIC X(10).
           05 QY-C3-CCATLG-COMPS-REG     PIC X(8).
           05 QY-C3-NSEQ-PROCM-EVNTO     PIC S9(3)V USAGE COMP-3.
           05 QY-C3-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
           05 QY-C3-CTERM                PIC X(8).
************************************************************************
*******INICIO
*******CELMTO-DESC-COML                          1       2
*******CEVNTO-DESC-COML                          3       2
*******HULT-ATULZ                                5      26
*******CINDCD-MANUT-REG                         31       1
*******CSIT-DESC-COML                           32       2
*******CCHAVE-ELMTO-DESC                        34      45
*******MOVTC-DESC-LEN                           79       2
*******MOVTC-DESC-TEXT                          81    1500
*******CTRANS-PROG-ATULZ                      1581       8
*******CINDCD-MODLD-PROCM                     1589       1
*******CEMPR-INC                              1590       3
*******CDEPDC                                 1593       3
*******DMOVTC-DESC-COML                       1596      10
*******CCATLG-COMPS-REG                       1606       8
*******NSEQ-PROCM-EVNTO                       1614       2
*******CFUNC-BDSCO                            1616       5
*******CTERM                                  1621       8
