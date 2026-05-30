           SKIP2
       I#DCOMNJ
       01        DCOMNG-OPERACOES.
      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMNJ *
      * CRIACAO   : 02/2007                                   *
      * DESCRICAO : BVVE - ARQUIVO DE OPERACOES PROGRAMADAS   *
      *             REMESSA                                   *
      * TAMANHO   : LRECL  232                                *
      *********************************************************
           SKIP2
         05      DCOMNJ-DADOS-ENTRADA.
           10    DCOMNJ-AGENCIA-CONTA-LOJISTA.
             15  DCOMNJ-BANCO-LOJISTA        PIC 9(03) COMP-3.
             15  DCOMNJ-AGENCIA-LOJISTA      PIC 9(05) COMP-3.
             15  DCOMNJ-CONTA-LOJISTA        PIC 9(13) COMP-3.
           10    DCOMNJ-CODIGO-PROD-BAND     PIC 9(02).
           10    DCOMNJ-ROTATIVO-PARCELADO   PIC X(01).
           10    DCOMNJ-INIC-OPERACAO        PIC X(10).
           10    DCOMNJ-FIM-OPERACAO         PIC X(10).
           10    DCOMNJ-QTDE-TITULOS         PIC 9(03)    COMP-3.
           10    DCOMNJ-VLR-OPERACAO         PIC 9(13)V99 COMP-3.
           10    DCOMNJ-TAXA-JUROS           PIC 9(03)V99 COMP-3.
           10    DCOMNJ-CODIGO-AUTORIZANTE   PIC 9(09)    COMP-3.
           10    DCOMNJ-NOME AUTORIZANTE     PIC X(40).
           10    DCOMNJ-NRO-OPERACAO         PIC 9(13)    COMP-3.
           10    DCOMNJ-VLR-IOF-OPERACAO     PIC 9(15)V99 COMP-3.
           10    DCOMNJ-VLR-TAC-OPERACAO     PIC 9(15)V99 COMP-3.
           10    DCOMNJ-VLR-JRS-OPERACAO     PIC 9(15)V99 COMP-3.
           10    DCOMNJ-VLR-LIQ-LIB          PIC 9(15)V99 COMP-3.
           10    DCOMNJ-COD-OCORRENCIA       PIC X(01).
             15  DCOMNJ-COD-RETORNO          PIC 9(04).
             15  DCOMNJ-MENSAGEM             PIC X(80).
         05      FILLER                      PIC X(11).
      *
