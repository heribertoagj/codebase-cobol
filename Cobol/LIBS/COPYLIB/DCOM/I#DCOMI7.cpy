      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0I7 - DB2PRD.TCONVE_CONS_CREDT      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  I7-ARGUMENTOS-ENTRADA.

       05  I7-COLUNAS-DA-TABELA.
           10  I7-CCONVE-CLI-DESC              PIC S9(9)V COMP-3.
           10  I7-CVRSAO-CONVE-CLI             PIC S9(3)V COMP-3.
           10  I7-NSEQ-CONS-CREDT              PIC S9(3)V COMP-3.
           10  I7-DANO-BASE                    PIC S9(4)V COMP-3.
           10  I7-CDEPDC                       PIC S9(5)V COMP-3.
           10  I7-CSEQ-STUDO                   PIC S9(9)V COMP-3.
           10  I7-VCONS-NEGOC                  PIC S9(15)V9(2) COMP-3.
           10  I7-CCART-LIM-CREDT              PIC X(5).
           10  I7-HULT-ATULZ                   PIC X(26).
           10  I7-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  I7-CTERM                        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  I7-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  I7-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  I7-RETORNO.
       05  I7-COD-RETORNO                         PIC 9(04).
       05  I7-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  I7-PROXIMO-RESTART                     PIC 9(05).
       05  I7-QTDE-RETORNADA                      PIC 9(03).
       05  I7-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  I7-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TI7.
           10  I7RT-CCONVE-CLI-DESC            PIC S9(9)V COMP-3.
           10  I7RT-CVRSAO-CONVE-CLI           PIC S9(3)V COMP-3.
           10  I7RT-NSEQ-CONS-CREDT            PIC S9(3)V COMP-3.
           10  I7RT-DANO-BASE                  PIC S9(4)V COMP-3.
           10  I7RT-CDEPDC                     PIC S9(5)V COMP-3.
           10  I7RT-CSEQ-STUDO                 PIC S9(9)V COMP-3.
           10  I7RT-VCONS-NEGOC                PIC S9(15)V9(2) COMP-3.
           10  I7RT-CCART-LIM-CREDT            PIC X(5).
           10  I7RT-HULT-ATULZ                 PIC X(26).
           10  I7RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  I7RT-CTERM                      PIC X(8).
