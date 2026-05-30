      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0A4 - DB2PRD.TGARNT_CONTR_LIM       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  A4-ARGUMENTOS-ENTRADA.

       05  A4-COLUNAS-DA-TABELA.
           10  A4-CCONTR-LIM-DESC              PIC S9(9)V COMP-3.
           10  A4-CVRSAO-CONTR-LIM             PIC S9(3)V COMP-3.
           10  A4-CTPO-GARNT                   PIC S9(3)V COMP-3.
           10  A4-CTERM                        PIC X(8).
           10  A4-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  A4-HULT-ATULZ                   PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  A4-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  A4-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  A4-RETORNO.
       05  A4-COD-RETORNO                         PIC 9(04).
       05  A4-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  A4-PROXIMO-RESTART                     PIC 9(05).
       05  A4-QTDE-RETORNADA                      PIC 9(03).
       05  A4-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  A4-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TA4.
           10  A4RT-CCONTR-LIM-DESC            PIC S9(9)V COMP-3.
           10  A4RT-CVRSAO-CONTR-LIM           PIC S9(3)V COMP-3.
           10  A4RT-CTPO-GARNT                 PIC S9(3)V COMP-3.
           10  A4RT-CTERM                      PIC X(8).
           10  A4RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  A4RT-HULT-ATULZ                 PIC X(26).
