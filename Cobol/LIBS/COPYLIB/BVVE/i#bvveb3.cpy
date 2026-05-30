      *===============================================================*
      *    FORMATACAO DA MENSAGEM ASLC023/25/33                       *
      *    BOOK = I#BVVEB3                TAMANHO = 103               *
      *===============================================================*
      *
       01  WRK-QTDE                        PIC 9(02) COMP.

       01  BVVEB3-ASLC.
           03  BVVEB3-GRUPO-LIQUID         PIC X(006).
           03  BVVEB3-IDENT-PART-PRINC     PIC X(008).
           03  BVVEB3-IDENT-PART-ADM       PIC X(008).
           03  BVVEB3-NUM-CTRL-IF          PIC X(020).
           03  BVVEB3-NUM-LIQUID           PIC X(021).
           03  BVVEB3-OCORRENCIAS          OCCURS 1 TO 5 TIMES
               DEPENDING ON WRK-QTDE.
               05  BVVEB3-GRUPO-OCOR        PIC X(006).
               05  BVVEB3-OCOR-ERRO         PIC X(002).
