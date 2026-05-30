      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQZ                                      *
      * TAMANHO : 3108 BYTES                                           *
      * ARQUIVO : LOG DE OPERAÇOES                                     *
      * INCLUDE DA TABELA DCOMB031 - TLOG_COPLT_DESC                   *
      ******************************************************************
       01  QZ-31-REGISTRO.
           05  QZ-31-CELMTO-DESC-COML   PIC S9(3)V USAGE COMP-3.
           05  QZ-31-CEVNTO-DESC-COML   PIC S9(3)V USAGE COMP-3.
           05  QZ-31-HULT-ATULZ         PIC X(26).
           05  QZ-31-CINDCD-MANUT-REG   PIC X(1).
           05  QZ-31-CCHAVE-ELMTO-DESC.
               10  QZ-31-OPER.
                   15 QZ-31-DANO-OPER   PIC 9(4).
                   15 QZ-31-NSEQ-OPER   PIC 9(9).
               10  FILLER               PIC X(32).
           05  QZ-31-WREG-LOG-ANTES.
               49 QZ-31-LOG-ANTES-LEN   PIC S9(4) USAGE COMP.
               49 QZ-31-LOG-ANTES-TEXT  PIC X(1500).
           05  QZ-31-WREG-LOG-APOS.
               49 QZ-31-LOG-APOS-LEN    PIC S9(4) USAGE COMP.
               49 QZ-31-LOG-APOS-TEXT   PIC X(1500).
           05  QZ-31-CTRANS-PROG-ATULZ  PIC X(8).
           05  QZ-31-CINDCD-MODLD-PROCM PIC X(1).
           05  QZ-31-CEMPR-INC          PIC S9(5)V USAGE COMP-3.
           05  QZ-31-CDEPDC             PIC S9(5)V USAGE COMP-3.
           05  QZ-31-CFUNC-BDSCO        PIC S9(9)V USAGE COMP-3.
           05  QZ-31-CTERM              PIC X(8).
