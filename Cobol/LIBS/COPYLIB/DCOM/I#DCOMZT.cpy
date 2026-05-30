      ******************************************************************
      * NOME BOOK : I#DCOMZT                                           *
      * DESCRICAO : B0OK PARA RELATORIO DE TAXAS DCOM 26               *
      * DATA      : 02/2021                                            *
      * AUTOR     : AMANDA BELTOSO                                     *
      * EMPRESA   : CAPGEMINI NEARSHORE                                *
      * TAMANHO   : 0084 BYTES                                         *
      ******************************************************************
       05  DCOMZT-REGISTRO.
           10 DCOMZT-AGENCIA           PIC 9(05)V   USAGE COMP-3.
           10 DCOMZT-CONTA             PIC 9(13)V   USAGE COMP-3.
           10 DCOMZT-PRODUTO           PIC 9(03)V   USAGE COMP-3.
           10 DCOMZT-SUBPRODUTO        PIC 9(03)V   USAGE COMP-3.
           10 DCOMZT-PERCENTUAL        PIC 9(03)V99 USAGE COMP-3.
           10 DCOMZT-DT-INICIAL        PIC X(10).
           10 DCOMZT-DT-FINAL          PIC X(10).
           10 DCOMZT-ACAO              PIC X(01).
           10 DCOMZT-DESCRICAO         PIC X(46).
