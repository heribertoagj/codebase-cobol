      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0H3 - DB2PRD.THIST_JUSTF_BAIXA      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
BSI001*                     BSI TECNOLOGIA - ALTERACAO                *
BSI001*---------------------------------------------------------------*
BSI001* DATA....:  08/06/2016                                         *
BSI001* OBJETIVO:  INCLUIR TRATAMENTO PARA BAIXA POR PREJUIZO.        *
BSI001* PROJETO.:  BSI - 16-0116-01 - BAIXA AMIGAVEL E JUDICIAL.      *
BSI001*---------------------------------------------------------------*
BSI001* MODULOS.:                                                     *
BSI001*    - DCOM7365 - SELECT         - DCOM7366 - FETCH             *
BSI001*    - DCOM7367 - INSERT         - DCOM7368 - UPDATE            *
BSI001*    - DCOM7369 - DELETE                                        *
BSI001*===============================================================*

       01  H3-ARGUMENTOS-ENTRADA.

       05  H3-COLUNAS-DA-TABELA.
           10  H3-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  H3-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  H3-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  H3-HBAIXA-PCELA-DESC            PIC X(26).
           10  H3-NLIN-JUSTF-BAIXA             PIC S9(3)V COMP-3.
           10  H3-HULT-ATULZ                   PIC X(26).
           10  H3-RJUSTF-BAIXA-PCELA           PIC X(79).
           10  H3-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  H3-CTERM                        PIC X(8).
BSI001     10  H3-CMOTVO-BAIXA-PREJ            PIC S9(3)V COMP-3.
BSI001     10  H3-CREGRA-CTBIL-PREJ            PIC S9(3)V COMP-3.
BSI001     10  H3-CFUNC-BDSCO-AUTRZ            PIC S9(9)V COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  H3-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  H3-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  H3-RETORNO.
       05  H3-COD-RETORNO                         PIC 9(04).
       05  H3-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  H3-PROXIMO-RESTART                     PIC 9(05).
       05  H3-QTDE-RETORNADA                      PIC 9(03).
       05  H3-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  H3-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TH3.
           10  H3RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  H3RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  H3RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  H3RT-HBAIXA-PCELA-DESC          PIC X(26).
           10  H3RT-NLIN-JUSTF-BAIXA           PIC S9(3)V COMP-3.
           10  H3RT-HULT-ATULZ                 PIC X(26).
           10  H3RT-RJUSTF-BAIXA-PCELA         PIC X(79).
           10  H3RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  H3RT-CTERM                      PIC X(8).
BSI001     10  H3RT-CMOTVO-BAIXA-PREJ          PIC S9(3)V COMP-3.
BSI001     10  H3RT-CREGRA-CTBIL-PREJ          PIC S9(3)V COMP-3.
BSI001     10  H3RT-CFUNC-BDSCO-AUTRZ          PIC S9(9)V COMP-3.
