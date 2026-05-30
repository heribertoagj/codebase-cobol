      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0H1 - DB2PRD.TCONTR_LIM_ROTRO       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  H1-ARGUMENTOS-ENTRADA.

       05  H1-COLUNAS-DA-TABELA.
           10  H1-CCONTR-LIM-DESC              PIC S9(9)V COMP-3.
           10  H1-CVRSAO-CONTR-LIM             PIC S9(3)V COMP-3.
           10  H1-CTPO-AGPTO-ROTRO             PIC S9(3)V COMP-3.
           10  H1-CINDCD-OBRIG-AGPTO           PIC X(1).
           10  H1-CINDCD-SIT-AGPTO             PIC X(1).
           10  H1-CINDCD-CONCL-ROTRO           PIC X(1).
           10  H1-DVALDC-ROTRO-ELMTO           PIC X(10).
           10  H1-HULT-ATULZ                   PIC X(26).
           10  H1-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  H1-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  H1-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  H1-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  H1-RETORNO.
       05  H1-COD-RETORNO                         PIC 9(04).
       05  H1-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  H1-PROXIMO-RESTART                     PIC 9(05).
       05  H1-QTDE-RETORNADA                      PIC 9(03).
       05  H1-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  H1-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TH1.
           10  H1RT-CCONTR-LIM-DESC            PIC S9(9)V COMP-3.
           10  H1RT-CVRSAO-CONTR-LIM           PIC S9(3)V COMP-3.
           10  H1RT-CTPO-AGPTO-ROTRO           PIC S9(3)V COMP-3.
           10  H1RT-CINDCD-OBRIG-AGPTO         PIC X(1).
           10  H1RT-CINDCD-SIT-AGPTO           PIC X(1).
           10  H1RT-CINDCD-CONCL-ROTRO         PIC X(1).
           10  H1RT-DVALDC-ROTRO-ELMTO         PIC X(10).
           10  H1RT-HULT-ATULZ                 PIC X(26).
           10  H1RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  H1RT-CTERM                      PIC X(8).
