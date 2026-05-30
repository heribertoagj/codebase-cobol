      *****************************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMNE         *
      * DESCRICAO : EXPURGO DO CADASTRO DE APROPRIACAO                *
      * TAMANHO   : LRECL = 064                                       *
      *****************************************************************
       01  DCOMNE-DT-DADOS-PARCELA.
           05  DCOMNE-DT-ANO-OPER          PIC  9(004).
           05  DCOMNE-DT-SEQ-OPER          PIC  9(009).
           05  DCOMNE-DT-NUM-PARC          PIC  9(005).
           05  DCOMNE-DT-BCO-CED           PIC  9(003).
           05  DCOMNE-DT-AGE-CED           PIC  9(005).
           05  DCOMNE-DT-SIT-DESC          PIC  9(003).
           05  DCOMNM-DT-ULT-AT            PIC  X(026).
           05  FILLER                      PIC  X(009).
