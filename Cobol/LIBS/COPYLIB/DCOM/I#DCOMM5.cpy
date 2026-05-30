      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0M5 - DB2PRD.TOPER_DESC_MGRAD       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  M5-ARGUMENTOS-ENTRADA.

       05  M5-COLUNAS-DA-TABELA.
           10  M5-DANO-OPER-DESC               PIC S9(004) COMP-3.
           10  M5-NSEQ-OPER-DESC               PIC S9(009) COMP-3.
           10  M5-HULT-ATULZ                   PIC  X(026).
           10  M5-NOPER-DESC-MGRAD             PIC S9(018) COMP-3.


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  M5-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  M5-INSTRUCAO                          PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  M5-RETORNO.
       05  M5-COD-RETORNO                         PIC 9(04).
       05  M5-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  M5-PROXIMO-RESTART                     PIC 9(05).
       05  M5-QTDE-RETORNADA                      PIC 9(03).
       05  M5-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  M5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TM5.
           10  M5RT-DANO-OPER-DESC             PIC S9(004) COMP-3.
           10  M5RT-NSEQ-OPER-DESC             PIC S9(009) COMP-3.
           10  M5RT-HULT-ATULZ                 PIC  X(026).
           10  M5RT-NOPER-DESC-MGRAD           PIC S9(018) COMP-3.
