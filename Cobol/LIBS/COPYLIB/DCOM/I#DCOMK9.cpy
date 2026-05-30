      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0K9 - DB2PRD.TTESTM_AG_OPER         *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  K9-ARGUMENTOS-ENTRADA.

       05  K9-COLUNAS-DA-TABELA.
           10  K9-CBCO                 PIC S9(3)V COMP-3.
           10  K9-CAG-BCRIA            PIC S9(5)V COMP-3.
           10  K9-NPSSOA-DESC-COML     PIC S9(9)V COMP-3.
           10  K9-DULT-UTILZ           PIC X(10).
           10  K9-HULT-ATULZ           PIC X(26).
           10  K9-DINCL-REG            PIC X(10).
           10  K9-CFUNC-BDSCO          PIC S9(9)V COMP-3.
           10  K9-CTERM                PIC X(8).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  K9-QTDE-A-DESPREZAR                   PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  K9-INSTRUCAO                          PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  K9-RETORNO.
       05  K9-COD-RETORNO                         PIC 9(04).
       05  K9-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  K9-PROXIMO-RESTART                     PIC 9(05).
       05  K9-QTDE-RETORNADA                      PIC 9(03).
       05  K9-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  K9-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TK9.
           10  K9RT-CBCO                 PIC S9(3)V COMP-3.
           10  K9RT-CAG-BCRIA            PIC S9(5)V COMP-3.
           10  K9RT-NPSSOA-DESC-COML     PIC S9(9)V COMP-3.
           10  K9RT-DULT-UTILZ           PIC X(10).
           10  K9RT-HULT-ATULZ           PIC X(26).
           10  K9RT-DINCL-REG            PIC X(10).
           10  K9RT-CFUNC-BDSCO          PIC S9(9)V COMP-3.
           10  K9RT-CTERM                PIC X(8).

