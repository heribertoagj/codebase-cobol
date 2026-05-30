      ******************************************************************
      * NOME BOOK : CRURWIY1 - INTERFACE DOS DADOS DE INTERFACE        *
      * DESCRICAO.: AREA DE COMUNICACAO COM FUNCIONAL                  *
      * DATA......: 23/05/2014                                         *
      * AUTOR.....: RONALDO CORREA              - CAPGEMINI - ALPHA    *
      * EMPRESA...: CAPGEMINI                                          *
      * TAMANHO   : 552 BYTES                                          *
      ******************************************************************
      * DATA        AUTOR             DESCRICAO / MANUTENCAO           *
      ******************************************************************
      * XX/XX/XXXX  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  *
      ******************************************************************
       05 CRURWIY1-HEADER.
         10 CRURWIY1-COD-LAYOUT            PIC  X(008) VALUE 'CRURWIY1'.
         10 CRURWIY1-TAM-LAYOUT            PIC  9(005) VALUE 552.
       05 CRURWIY1-REGISTRO.
         10 CRURWIY1-BLOCO-RETORNO.
           15 CRURWIY1-COD-RETORNO             PIC  9(002).
           15 CRURWIY1-COD-ERRO                PIC  X(004).
           15 CRURWIY1-COD-MENSAGEM            PIC  X(008).
         10 CRURWIY1-BLOCO-ENTRADA.
           15 CRURWIY1-E-CPSSOA-JURID-CONTR    PIC  9(010).
           15 CRURWIY1-E-CTPO-CONTR-NEGOC      PIC  9(003).
           15 CRURWIY1-E-NSEQ-CONTR-NEGOC      PIC  9(010).
           15 CRURWIY1-E-CPSSOA-BNEFC          PIC  9(010).
           15 CRURWIY1-E-CPSSOA-JURID          PIC  9(010).
           15 CRURWIY1-E-NSEQ-UND-ORGNZ        PIC  9(008).
           15 CRURWIY1-E-DEPENDENCIA-OPER      PIC  9(005).
           15 CRURWIY1-E-USUARIO               PIC  X(009).
           15 CRURWIY1-E-EMPR-OPERANTE         PIC  9(010).
         10 CRURWIY1-BLOCO-SAIDA.
           15 CRURWIY1-S-MSG                   PIC  X(450).
