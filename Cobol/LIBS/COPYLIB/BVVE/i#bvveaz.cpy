      *================================================================*
      *  I#BVVEAZ - IMAGEM DA TABELA BVVEB031                          *
      *             HISTORICO DE PRODUTOS DE CREDITO                   *
      *----------------------------------------------------------------*
      *  LRECL= 045                                                    *
      *================================================================*
       01  BVVEAZ-REGISTRO.
           05 BVVEAZ-BANCO             PIC S9(003)     COMP-3.
           05 BVVEAZ-AGENCIA           PIC S9(005)     COMP-3.
           05 BVVEAZ-CONTA             PIC S9(013)     COMP-3.
           05 BVVEAZ-PRODUTO           PIC S9(005)     COMP-3.
           05 BVVEAZ-DATA-BASE         PIC S9(006)     COMP-3.
           05 BVVEAZ-VALOR-CRED        PIC S9(015)V99  COMP-3.
           05 BVVEAZ-VALOR-DEB         PIC S9(015)V99  COMP-3.
           05 BVVEAZ-VALOR-GAR         PIC S9(013)V99  COMP-3.
