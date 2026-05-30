      *===============================================================*
      *    FORMATACAO DA MENSAGEM ASCG023                             *
      *    BOOK = I#BVVEA7                TAMANHO = 100               *
      *===============================================================*
      *
       01  WRK-QTDE                        PIC 9(02) COMP.

       01  BVVEA7-ASCG023.
           03  BVVEA7-GRUPO-LIQUID         PIC X(006).
           03  BVVEA7-NUM-CTRL-IF          PIC X(020).
           03  BVVEA7-NUM-LIQUID           PIC X(021).
           03  BVVEA7-OCORRENCIAS          OCCURS 1 TO 5 TIMES
               DEPENDING ON WRK-QTDE.
               05  BVVEA7-GRUPO-OCOR        PIC X(006).
               05  BVVEA7-OCOR-ERRO         PIC X(002).
