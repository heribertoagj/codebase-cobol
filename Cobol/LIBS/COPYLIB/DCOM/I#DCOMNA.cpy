      *****************************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMNA         *
      * DESCRICAO : APROPRIACAO EVOLUCAO DE PARCELA PARA DB2          *
      * TAMANHO   : LRECL = 056                                       *
      *****************************************************************

       01  DCOMNA-AREA.
           05  DCOMNA-DADOS-PARCELA.
               10  DCOMNA-ANO-OPER          PIC  9(004).
               10  DCOMNA-SEQ-OPER          PIC  9(009).
               10  DCOMNA-NUM-PARC          PIC  9(005).
               10  DCOMNA-SIT-DESC          PIC  9(003).
               10  DCOMNA-JURAPRO-ACT-REAL  PIC  9(015)V99 COMP-3.
               10  DCOMNA-VALOR-PRESENTE    PIC  9(015)V99 COMP-3.
               10  DCOMNA-DATA-ULTAPR       PIC  X(010).
               10  FILLER                   PIC  X(007).
