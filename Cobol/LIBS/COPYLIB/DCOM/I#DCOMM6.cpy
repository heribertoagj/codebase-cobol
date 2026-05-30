      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0M6 - DB2PRD.TTRASI_CATAO_ANTVL                      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7610 - SELECT         - DCOM7611 - FETCH             *
      *    - DCOM7612 - INSERT         - DCOM7613 - UPDATE            *
      *    - DCOM7614 - DELETE                                        *
      *===============================================================*

       01  M6-ARGUMENTOS-ENTRADA.

       05  M6-COLUNAS-DA-TABELA.
           10  M6-DANO-OPER-DESC       PIC S9(004)        COMP-3.
           10  M6-NSEQ-OPER-DESC       PIC S9(009)        COMP-3.
           10  M6-CADM-CATAO-CREDT     PIC S9(009)        COMP-3.
           10  M6-CINDCD-VDA-RCBVL     PIC  X(001).
           10  M6-VDISPN-CATAO-ANTVL   PIC S9(015)V9(002) COMP-3.
           10  M6-CINDCD-SELEC-DESC    PIC  X(001).
           10  M6-CSELEC-CATAO-CREDT   PIC  X(001).
           10  M6-HULT-ATULZ           PIC  X(026).
           10  M6-CFUNC-BDSCO          PIC S9(009)        COMP-3.
           10  M6-CTERM                PIC  X(008).
           10  M6-DINCL-REG            PIC  X(010).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  M6-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  M6-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  M6-RETORNO.
       05  M6-COD-RETORNO              PIC  9(004).
       05  M6-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  M6-PROXIMO-RESTART          PIC  9(005).
       05  M6-QTDE-RETORNADA           PIC  9(003).
       05  M6-QTDE-TOTAL               PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  M6-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TM6.
           10  M6RT-DANO-OPER-DESC     PIC S9(004)        COMP-3.
           10  M6RT-NSEQ-OPER-DESC     PIC S9(009)        COMP-3.
           10  M6RT-CADM-CATAO-CREDT   PIC S9(009)        COMP-3.
           10  M6RT-CINDCD-VDA-RCBVL   PIC  X(001).
           10  M6RT-VDISPN-CATAO-ANTVL PIC S9(015)V9(002) COMP-3.
           10  M6RT-CINDCD-SELEC-DESC  PIC  X(001).
           10  M6RT-CSELEC-CATAO-CREDT PIC  X(001).
           10  M6RT-HULT-ATULZ         PIC  X(026).
           10  M6RT-CFUNC-BDSCO        PIC S9(009)        COMP-3.
           10  M6RT-CTERM              PIC  X(008).
           10  M6RT-DINCL-REG          PIC  X(010).

