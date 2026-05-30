           SKIP2
       I#DCOMNG
       01        DCOMNG-OPERACOES.
      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMNG *
      * CRIACAO   : 02/2007                                   *
      * DESCRICAO : BVVE - ARQUIVO DE OPERACOES PROGRAMADAS   *
      *             RECEPCAO                                  *
      * TAMANHO   : LRECL  080                                *
      *********************************************************
           SKIP2
         05      DCOMNG-DADOS-ENTRADA.
           10    DCOMNG-CENTRO-CUSTO         PIC X(04).
           10    DCOMNG-DATA-MOVIMENTO       PIC 9(09) COMP-3.
           10    DCOMNG-AGENCIA-CONTA-LOJISTA.
             15  DCOMNG-BANCO-LOJISTA        PIC 9(03) COMP-3.
             15  DCOMNG-AGENCIA-LOJISTA      PIC 9(05) COMP-3.
             15  DCOMNG-CONTA-LOJISTA        PIC 9(13) COMP-3.
           10    DCOMNG-CODIGO-PROD-BAND     PIC 9(02).
           10    DCOMNG-ROTATIVO-PARCELADO   PIC X(01).
           10    DCOMNG-TAXA-JUROS           PIC 9(03)V9(05) COMP-3.
           10    DCOMNG-CODIGO-AUTORIZANTE   PIC 9(09) COMP-3.
           10    DCOMNG-NOME AUTORIZANTE     PIC X(40).
         05      FILLER                      PIC X(06).
      *
