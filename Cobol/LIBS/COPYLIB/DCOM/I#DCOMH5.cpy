      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0H5 - DB2PRD.TJUSTF_BAIXA_PCELA     *
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
BSI001*    - DCOM7375 - SELECT         - DCOM7376 - FETCH             *
BSI001*    - DCOM7377 - INSERT         - DCOM7378 - UPDATE            *
BSI001*    - DCOM7379 - DELETE                                        *
BSI001*===============================================================*

       01  H5-ARGUMENTOS-ENTRADA.

       05  H5-COLUNAS-DA-TABELA.
           10  H5-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  H5-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  H5-NPCELA-DESC-COML             PIC S9(5)V COMP-3.
           10  H5-HBAIXA-PCELA-DESC            PIC X(26).
           10  H5-NLIN-JUSTF-BAIXA             PIC S9(3)V COMP-3.
           10  H5-RJUSTF-BAIXA-PCELA           PIC X(79).
           10  H5-HULT-ATULZ                   PIC X(26).
           10  H5-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  H5-CTERM                        PIC X(8).
BSI001     10  H5-CMOTVO-BAIXA-PREJ            PIC S9(3)V COMP-3.
BSI001     10  H5-CREGRA-CTBIL-PREJ            PIC S9(3)V COMP-3.
BSI001     10  H5-CFUNC-BDSCO-AUTRZ            PIC S9(9)V COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  H5-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  H5-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  H5-RETORNO.
       05  H5-COD-RETORNO                         PIC 9(04).
       05  H5-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  H5-PROXIMO-RESTART                     PIC 9(05).
       05  H5-QTDE-RETORNADA                      PIC 9(03).
       05  H5-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  H5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TH5.
           10  H5RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  H5RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  H5RT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  H5RT-HBAIXA-PCELA-DESC          PIC X(26).
           10  H5RT-NLIN-JUSTF-BAIXA           PIC S9(3)V COMP-3.
           10  H5RT-RJUSTF-BAIXA-PCELA         PIC X(79).
           10  H5RT-HULT-ATULZ                 PIC X(26).
           10  H5RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  H5RT-CTERM                      PIC X(8).
BSI001     10  H5RT-CMOTVO-BAIXA-PREJ          PIC S9(3)V COMP-3.
BSI001     10  H5RT-CREGRA-CTBIL-PREJ          PIC S9(3)V COMP-3.
BSI001     10  H5RT-CFUNC-BDSCO-AUTRZ          PIC S9(9)V COMP-3.
